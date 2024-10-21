// Copyright 2018-2024 the Deno authors. MIT license.

use crate::analyzer::DependencyDescriptor;
use crate::analyzer::DynamicArgument;
use crate::analyzer::DynamicDependencyDescriptor;
use crate::analyzer::DynamicTemplatePart;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleInfo;
use crate::analyzer::PositionRange;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::StaticDependencyDescriptor;
use crate::analyzer::TypeScriptReference;
use crate::graph::Position;
use crate::module_specifier::ModuleSpecifier;

use deno_ast::dep::DependencyComment;
use deno_ast::MultiThreadedComments;
use deno_ast::ProgramRef;
use deno_ast::SourcePos;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

use deno_ast::swc::common::comments::CommentKind;
use deno_ast::MediaType;
use deno_ast::ParseDiagnostic;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use once_cell::sync::Lazy;
use regex::Match;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

/// Matches a JSDoc import type reference (`{import("./example.js")}`
static JSDOC_IMPORT_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r#"\{[^}]*import\(['"]([^'"]+)['"]\)[^}]*}"#).unwrap()
});
/// Matches the `@jsxImportSource` pragma.
static JSX_IMPORT_SOURCE_RE: Lazy<Regex> =
  Lazy::new(|| Regex::new(r"(?i)^[\s*]*@jsxImportSource\s+(\S+)").unwrap());
/// Matches the `@jsxImportSourceTypes` pragma.
static JSX_IMPORT_SOURCE_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r"(?i)^[\s*]*@jsxImportSourceTypes\s+(\S+)").unwrap()
});
/// Matches a `/// <reference ... />` comment reference.
static TRIPLE_SLASH_REFERENCE_RE: Lazy<Regex> =
  Lazy::new(|| Regex::new(r"(?i)^/\s*<reference\s.*?/>").unwrap());
/// Matches a path reference, which adds a dependency to a module
static PATH_REFERENCE_RE: Lazy<Regex> =
  Lazy::new(|| Regex::new(r#"(?i)\spath\s*=\s*["']([^"']*)["']"#).unwrap());
/// Matches a types reference, which for JavaScript files indicates the
/// location of types to use when type checking a program that includes it as
/// a dependency.
static TYPES_REFERENCE_RE: Lazy<Regex> =
  Lazy::new(|| Regex::new(r#"(?i)\stypes\s*=\s*["']([^"']*)["']"#).unwrap());
/// Matches the `@ts-self-types` pragma.
static TS_SELF_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r#"(?i)^\s*@ts-self-types\s*=\s*["']([^"']+)["']"#).unwrap()
});
/// Matches the `@ts-types` pragma.
static TS_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r#"(?i)^\s*@ts-types\s*=\s*["']([^"']+)["']"#).unwrap()
});
/// Matches the `@deno-types` pragma.
pub static DENO_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#).unwrap()
});

pub struct ParseOptions<'a> {
  pub specifier: &'a ModuleSpecifier,
  pub source: Arc<str>,
  pub media_type: MediaType,
  pub scope_analysis: bool,
}

/// Parses modules to a ParsedSource.
pub trait ModuleParser {
  fn parse_module(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic>;
}

#[derive(Default, Clone)]
pub struct DefaultModuleParser;

impl ModuleParser for DefaultModuleParser {
  fn parse_module(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    deno_ast::parse_program(deno_ast::ParseParams {
      specifier: options.specifier.clone(),
      text: options.source,
      media_type: options.media_type,
      capture_tokens: options.scope_analysis,
      scope_analysis: options.scope_analysis,
      maybe_syntax: None,
    })
  }
}

/// Stores parsed sources.
///
/// Note: This interface is racy and not thread safe, as it's assumed
/// it will only store the latest changes or that the source text
/// will never change.
pub trait ParsedSourceStore {
  /// Sets the parsed source, potentially returning the previous value.
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource>;
  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource>;
  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    // todo(dsherret): remove this default implementation once a breaking change is done to deno_graph
    self.get_parsed_source(specifier)
  }
  /// Gets a `deno_ast::ParsedSource` from the store, upgrading it
  /// to have scope analysis if it doesn't already.
  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource>;
}

/// Default store that works on a single thread.
#[derive(Default)]
pub struct DefaultParsedSourceStore {
  store: RefCell<HashMap<ModuleSpecifier, ParsedSource>>,
}

impl ParsedSourceStore for DefaultParsedSourceStore {
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource> {
    self.store.borrow_mut().insert(specifier, parsed_source)
  }

  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.borrow().get(specifier).cloned()
  }

  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.borrow_mut().remove(specifier)
  }

  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    let mut store = self.store.borrow_mut();
    let parsed_source = store.get_mut(specifier)?;
    if parsed_source.has_scope_analysis() {
      Some(parsed_source.clone())
    } else {
      let parsed_source = store.remove(specifier).unwrap();
      let parsed_source = parsed_source.into_with_scope_analysis();
      store.insert(specifier.clone(), parsed_source.clone());
      Some(parsed_source.clone())
    }
  }
}

/// Stores parsed files in the provided store after parsing.
/// in a provided store. Parses that match the previous one
/// will be cached.
///
/// Note that this will insert into the store whatever was
/// last parsed, so if two threads race to parse, when they're
/// both done it will have whatever was last stored.
#[derive(Clone, Copy)]
pub struct CapturingModuleParser<'a> {
  parser: Option<&'a dyn ModuleParser>,
  store: &'a dyn ParsedSourceStore,
}

impl<'a> CapturingModuleParser<'a> {
  pub fn new(
    parser: Option<&'a dyn ModuleParser>,
    store: &'a dyn ParsedSourceStore,
  ) -> Self {
    Self { parser, store }
  }

  fn get_from_store_if_matches(
    &self,
    options: &ParseOptions,
  ) -> Option<ParsedSource> {
    let parsed_source = if options.scope_analysis {
      self
        .store
        .get_scope_analysis_parsed_source(options.specifier)?
    } else {
      self.store.get_parsed_source(options.specifier)?
    };
    if parsed_source.media_type() == options.media_type
      && parsed_source.text().as_ref() == options.source.as_ref()
    {
      Some(parsed_source)
    } else {
      None
    }
  }
}

impl<'a> ModuleParser for CapturingModuleParser<'a> {
  fn parse_module(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    if let Some(parsed_source) = self.get_from_store_if_matches(&options) {
      Ok(parsed_source)
    } else {
      let default_parser = DefaultModuleParser;
      let parser = self.parser.unwrap_or(&default_parser);
      let specifier = options.specifier.clone();
      let parsed_source = parser.parse_module(options)?;
      self
        .store
        .set_parsed_source(specifier, parsed_source.clone());
      Ok(parsed_source)
    }
  }
}

#[derive(Default)]
pub struct DefaultModuleAnalyzer;

#[async_trait::async_trait(?Send)]
impl ModuleAnalyzer for DefaultModuleAnalyzer {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ParseDiagnostic> {
    ParserModuleAnalyzer::default()
      .analyze(specifier, source, media_type)
      .await
  }
}

/// Default module analyzer that analyzes based on a deno_ast::ParsedSource.
pub struct ParserModuleAnalyzer<'a> {
  parser: &'a dyn ModuleParser,
}

impl<'a> ParserModuleAnalyzer<'a> {
  /// Creates a new module analyzer.
  pub fn new(parser: &'a dyn ModuleParser) -> Self {
    Self { parser }
  }

  /// Gets the module info from a parsed source.
  pub fn module_info(parsed_source: &ParsedSource) -> ModuleInfo {
    let program = parsed_source.program_ref();
    Self::module_info_from_swc(
      parsed_source.media_type(),
      program,
      parsed_source.text_info_lazy(),
      parsed_source.comments(),
    )
  }

  pub fn module_info_from_swc(
    media_type: MediaType,
    program: ProgramRef,
    text_info: &SourceTextInfo,
    comments: &MultiThreadedComments,
  ) -> ModuleInfo {
    let leading_comments = match program.body().next() {
      Some(item) => comments.get_leading(item.start()),
      None => match program.shebang() {
        Some(_) => comments.get_trailing(program.end()),
        None => comments.get_leading(program.start()),
      },
    };
    ModuleInfo {
      dependencies: analyze_dependencies(program, text_info, comments),
      ts_references: analyze_ts_references(text_info, leading_comments),
      self_types_specifier: analyze_ts_self_types(
        media_type,
        text_info,
        leading_comments,
      ),
      jsx_import_source: analyze_jsx_import_source(
        media_type,
        text_info,
        leading_comments,
      ),
      jsx_import_source_types: analyze_jsx_import_source_types(
        media_type,
        text_info,
        leading_comments,
      ),
      jsdoc_imports: analyze_jsdoc_imports(media_type, text_info, comments),
    }
  }

  pub fn analyze_sync(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ParseDiagnostic> {
    let parsed_source = self.parser.parse_module(ParseOptions {
      specifier,
      source,
      media_type,
      // scope analysis is not necessary for module parsing
      scope_analysis: false,
    })?;
    Ok(ParserModuleAnalyzer::module_info(&parsed_source))
  }
}

impl<'a> Default for ParserModuleAnalyzer<'a> {
  fn default() -> Self {
    Self {
      parser: &DefaultModuleParser,
    }
  }
}

#[async_trait::async_trait(?Send)]
impl<'a> ModuleAnalyzer for ParserModuleAnalyzer<'a> {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ParseDiagnostic> {
    self.analyze_sync(specifier, source, media_type)
  }
}

/// Helper struct for creating a single object that implements
/// `deno_graph::ModuleAnalyzer`, `deno_graph::ModuleParser`,
/// and `deno_graph::ParsedSourceStore`. All parses will be captured
/// to prevent them from occuring more than one time.
pub struct CapturingModuleAnalyzer {
  parser: Box<dyn ModuleParser>,
  store: Box<dyn ParsedSourceStore>,
}

impl Default for CapturingModuleAnalyzer {
  fn default() -> Self {
    Self::new(None, None)
  }
}

impl CapturingModuleAnalyzer {
  pub fn new(
    parser: Option<Box<dyn ModuleParser>>,
    store: Option<Box<dyn ParsedSourceStore>>,
  ) -> Self {
    Self {
      parser: parser.unwrap_or_else(|| Box::<DefaultModuleParser>::default()),
      store: store
        .unwrap_or_else(|| Box::<DefaultParsedSourceStore>::default()),
    }
  }

  pub fn as_capturing_parser(&self) -> CapturingModuleParser {
    CapturingModuleParser::new(Some(&*self.parser), &*self.store)
  }
}

#[async_trait::async_trait(?Send)]
impl ModuleAnalyzer for CapturingModuleAnalyzer {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ParseDiagnostic> {
    let capturing_parser = self.as_capturing_parser();
    let module_analyzer = ParserModuleAnalyzer::new(&capturing_parser);
    module_analyzer.analyze(specifier, source, media_type).await
  }
}

impl ModuleParser for CapturingModuleAnalyzer {
  fn parse_module(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    let capturing_parser = self.as_capturing_parser();
    capturing_parser.parse_module(options)
  }
}

impl ParsedSourceStore for CapturingModuleAnalyzer {
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource> {
    self.store.set_parsed_source(specifier, parsed_source)
  }

  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.get_parsed_source(specifier)
  }

  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.remove_parsed_source(specifier)
  }

  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.get_scope_analysis_parsed_source(specifier)
  }
}

fn analyze_dependencies(
  program: deno_ast::ProgramRef,
  text_info: &SourceTextInfo,
  comments: &MultiThreadedComments,
) -> Vec<DependencyDescriptor> {
  let deps = deno_ast::dep::analyze_program_dependencies(program, comments);

  deps
    .into_iter()
    .map(|d| match d {
      deno_ast::dep::DependencyDescriptor::Static(d) => {
        DependencyDescriptor::Static(StaticDependencyDescriptor {
          kind: d.kind,
          types_specifier: analyze_ts_or_deno_types(
            text_info,
            &d.leading_comments,
          ),
          specifier: d.specifier.to_string(),
          specifier_range: PositionRange::from_source_range(
            d.specifier_range,
            text_info,
          ),
          import_attributes: d.import_attributes,
        })
      }
      deno_ast::dep::DependencyDescriptor::Dynamic(d) => {
        DependencyDescriptor::Dynamic(DynamicDependencyDescriptor {
          kind: d.kind,
          types_specifier: analyze_ts_or_deno_types(
            text_info,
            &d.leading_comments,
          ),
          argument: match d.argument {
            deno_ast::dep::DynamicArgument::String(text) => {
              DynamicArgument::String(text.to_string())
            }
            deno_ast::dep::DynamicArgument::Template(parts) => {
              DynamicArgument::Template(
                parts
                  .into_iter()
                  .map(|part| match part {
                    deno_ast::dep::DynamicTemplatePart::String(text) => {
                      DynamicTemplatePart::String {
                        value: text.to_string(),
                      }
                    }
                    deno_ast::dep::DynamicTemplatePart::Expr => {
                      DynamicTemplatePart::Expr
                    }
                  })
                  .collect(),
              )
            }
            deno_ast::dep::DynamicArgument::Expr => DynamicArgument::Expr,
          },
          argument_range: PositionRange::from_source_range(
            d.argument_range,
            text_info,
          ),
          import_attributes: d.import_attributes,
        })
      }
    })
    .collect()
}

fn analyze_ts_references(
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Vec<TypeScriptReference> {
  let mut references = Vec::new();
  if let Some(c) = leading_comments {
    for comment in c {
      if comment.kind == CommentKind::Line
        && TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text)
      {
        let comment_start = comment.start();
        if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Path(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_source_to_position_range(
              comment_start,
              &m,
              text_info,
              false,
            ),
          }));
        } else if let Some(captures) =
          TYPES_REFERENCE_RE.captures(&comment.text)
        {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Types(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_source_to_position_range(
              comment_start,
              &m,
              text_info,
              false,
            ),
          }));
        }
      }
    }
  }
  references
}

fn analyze_jsx_import_source(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if !matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      if c.kind != CommentKind::Block {
        return None; // invalid
      }
      let captures = JSX_IMPORT_SOURCE_RE.captures(&c.text)?;
      let m = captures.get(1)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(c.start(), &m, text_info, true),
      })
    })
  })
}

fn analyze_jsx_import_source_types(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if !matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      if c.kind != CommentKind::Block {
        return None; // invalid
      }
      let captures = JSX_IMPORT_SOURCE_TYPES_RE.captures(&c.text)?;
      let m = captures.get(1)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(c.start(), &m, text_info, true),
      })
    })
  })
}

fn analyze_ts_self_types(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if media_type.is_typed() {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      let captures = TS_SELF_TYPES_RE.captures(&c.text)?;
      let m = captures.get(1)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(
          c.start(),
          &m,
          text_info,
          false,
        ),
      })
    })
  })
}

/// Searches comments for any `@ts-types` or `@deno-types` compiler hints.
pub fn analyze_ts_or_deno_types(
  text_info: &SourceTextInfo,
  leading_comments: &[DependencyComment],
) -> Option<SpecifierWithRange> {
  let comment = leading_comments.last()?;

  if let Some(captures) = TS_TYPES_RE.captures(&comment.text) {
    if let Some(m) = captures.get(1) {
      return Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(
          comment.range.start(),
          &m,
          text_info,
          false,
        ),
      });
    }
  }
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some(SpecifierWithRange {
      text: m.as_str().to_string(),
      range: comment_source_to_position_range(
        comment.range.start(),
        &m,
        text_info,
        false,
      ),
    })
  } else if let Some(m) = captures.get(2) {
    Some(SpecifierWithRange {
      text: m.as_str().to_string(),
      range: comment_source_to_position_range(
        comment.range.start(),
        &m,
        text_info,
        true,
      ),
    })
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
}

fn analyze_jsdoc_imports(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  comments: &MultiThreadedComments,
) -> Vec<SpecifierWithRange> {
  // Analyze any JSDoc type imports
  // We only analyze these on JavaScript types of modules, since they are
  // ignored by TypeScript when type checking anyway and really shouldn't be
  // there, but some people do strange things.
  if !matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  ) {
    return Vec::new();
  }

  let mut deps = Vec::new();
  for comment in comments.iter_unstable() {
    if comment.kind != CommentKind::Block || !comment.text.starts_with('*') {
      continue;
    }
    for captures in JSDOC_IMPORT_RE.captures_iter(&comment.text) {
      if let Some(m) = captures.get(1) {
        deps.push(SpecifierWithRange {
          text: m.as_str().to_string(),
          range: comment_source_to_position_range(
            comment.range().start,
            &m,
            text_info,
            false,
          ),
        });
      }
    }
  }
  deps.sort_by(|a, b| a.range.start.cmp(&b.range.start));
  deps
}

fn comment_source_to_position_range(
  comment_start: SourcePos,
  m: &Match,
  text_info: &SourceTextInfo,
  is_specifier_quoteless: bool,
) -> PositionRange {
  // the comment text starts after the double slash or slash star, so add 2
  let comment_start = comment_start + 2;
  // -1 and +1 to include the quotes, but not for pragmas that don't have quotes
  let padding = if is_specifier_quoteless { 0 } else { 1 };
  PositionRange {
    start: Position::from_source_pos(
      comment_start + m.start() - padding,
      text_info,
    ),
    end: Position::from_source_pos(
      comment_start + m.end() + padding,
      text_info,
    ),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_parse() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.tsx").expect("bad specifier");
    let source = r#"
    /// <reference path="./ref.d.ts" />
    /// <reference types="./types.d.ts" />
    // @jsxImportSource http://example.com/invalid
    /* @jsxImportSource http://example.com/preact */
    // @jsxImportSourceTypes http://example.com/invalidTypes
    /* @jsxImportSourceTypes http://example.com/preactTypes */
    import {
      A,
      B,
      C,
      D,
    } from "https://deno.land/x/example@v1.0.0/mod.ts";

    export * from "./mod.ts";

    import type { Component } from "https://esm.sh/preact";
    import { h, Fragment } from "https://esm.sh/preact";

    // other
    // @deno-types="https://deno.land/x/types/react/index.d.ts"
    import React from "https://cdn.skypack.dev/react";

    // @deno-types=https://deno.land/x/types/react/index.d.ts
    import React2 from "https://cdn.skypack.dev/react";

    // @deno-types="https://deno.land/x/types/react/index.d.ts"
    // other comment first
    import React3 from "https://cdn.skypack.dev/react";

    const a = await import("./a.ts");

    const React4 = await /* @deno-types="https://deno.land/x/types/react/index.d.ts" */ import("https://cdn.skypack.dev/react");
    "#;
    let parsed_source = DefaultModuleParser
      .parse_module(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::Tsx,
        scope_analysis: false,
      })
      .unwrap();
    let text_info = parsed_source.text_info_lazy();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 9);

    let ts_references = module_info.ts_references;
    assert_eq!(ts_references.len(), 2);
    match &ts_references[0] {
      TypeScriptReference::Path(specifier) => {
        assert_eq!(specifier.text, "./ref.d.ts");
        assert_eq!(
          text_info.range_text(&specifier.range.as_source_range(text_info)),
          r#""./ref.d.ts""#
        );
      }
      TypeScriptReference::Types(_) => panic!("expected path"),
    }
    match &ts_references[1] {
      TypeScriptReference::Path(_) => panic!("expected types"),
      TypeScriptReference::Types(specifier) => {
        assert_eq!(specifier.text, "./types.d.ts");
        assert_eq!(
          text_info.range_text(&specifier.range.as_source_range(text_info)),
          r#""./types.d.ts""#
        );
      }
    }

    let dep_deno_types = &dependencies[4]
      .as_static()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#""https://deno.land/x/types/react/index.d.ts""#
    );

    let dep_deno_types = &dependencies[5]
      .as_static()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#"https://deno.land/x/types/react/index.d.ts"#
    );

    assert!(dependencies[6]
      .as_static()
      .unwrap()
      .types_specifier
      .is_none());

    let dep_deno_types = &dependencies[8]
      .as_dynamic()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#""https://deno.land/x/types/react/index.d.ts""#
    );

    let jsx_import_source = module_info.jsx_import_source.unwrap();
    assert_eq!(jsx_import_source.text, "http://example.com/preact");
    assert_eq!(
      text_info.range_text(&jsx_import_source.range.as_source_range(text_info)),
      "http://example.com/preact"
    );

    let jsx_import_source_types = module_info.jsx_import_source_types.unwrap();
    assert_eq!(
      jsx_import_source_types.text,
      "http://example.com/preactTypes"
    );
    assert_eq!(
      text_info
        .range_text(&jsx_import_source_types.range.as_source_range(text_info)),
      "http://example.com/preactTypes"
    );

    assert!(module_info.self_types_specifier.is_none());
  }

  #[test]
  fn test_analyze_dependencies() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = r#"
    import * as a from "./a.ts";
    import "./b.ts";
    import { c } from "./c.ts";
    import d from "./d.ts";
    import e, { ee } from "./e.ts";
    const f = await import("./f.ts");
    export * from "./g.ts";
    export { h } from "./h.ts";

    import type { i } from "./i.d.ts";
    export type { j } from "./j.d.ts";
    "#;
    let parsed_source = DefaultModuleParser
      .parse_module(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::TypeScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let text_info = parsed_source.text_info_lazy();
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 10);
    let dep = dependencies[0].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./a.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./a.ts\""
    );
    let dep = dependencies[1].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./b.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./b.ts\""
    );
  }

  #[test]
  fn test_analyze_self_types() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.js").expect("bad specifier");
    let source = r#"
      // @ts-self-types="./self.d.ts"

      import * as a from "./a.ts";
    "#;
    let parsed_source = DefaultModuleParser
      .parse_module(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::JavaScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let text_info = parsed_source.text_info_lazy();
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 1);
    let dep = dependencies[0].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./a.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./a.ts\""
    );

    let self_types_specifier = module_info.self_types_specifier.unwrap();
    assert_eq!(self_types_specifier.text, "./self.d.ts");
    assert_eq!(
      text_info
        .range_text(&self_types_specifier.range.as_source_range(text_info)),
      "\"./self.d.ts\""
    );
  }

  #[test]
  fn test_analyze_dependencies_import_attributes() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    for keyword in ["assert", "with"] {
      let source = format!(
        "
      import a from \"./a.json\" {keyword} {{ type: \"json\" }};
      await import(\"./b.json\", {{ {keyword}: {{ type: \"json\" }} }});
      "
      );
      let parsed_source = DefaultModuleParser
        .parse_module(ParseOptions {
          specifier: &specifier,
          source: source.into(),
          media_type: MediaType::TypeScript,
          scope_analysis: false,
        })
        .unwrap();
      let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
      let dependencies = module_info.dependencies;
      assert_eq!(dependencies.len(), 2);
      let dep = dependencies[0].as_static().unwrap();
      assert_eq!(dep.specifier.to_string(), "./a.json");
      assert_eq!(dep.import_attributes.get("type"), Some(&"json".to_string()));
      let dep = dependencies[1].as_dynamic().unwrap();
      assert_eq!(
        dep.argument,
        DynamicArgument::String("./b.json".to_string())
      );
      assert_eq!(dep.import_attributes.get("type"), Some(&"json".to_string()));
    }
  }

  #[test]
  fn test_analyze_jsdoc_imports() {
    let specifier = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let source = r#"
/** @module */

/**
 * Some stuff here
 *
 * @type {import("./a.js").A}
 */
const a = "a";

/**
 * Some other stuff here
 *
 * @param {import('./b.js').C}
 * @returns {import("./d.js")}
 */
function b(c) {
  return;
}

/**
 * @type {Set<import("./e.js").F>}
 */
const f = new Set();
"#;
    let parsed_source = DefaultModuleParser
      .parse_module(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::JavaScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.jsdoc_imports;
    assert_eq!(
      dependencies,
      [
        SpecifierWithRange {
          text: "./a.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 6,
              character: 17
            },
            end: Position {
              line: 6,
              character: 25
            }
          }
        },
        SpecifierWithRange {
          text: "./b.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 13,
              character: 18
            },
            end: Position {
              line: 13,
              character: 26
            }
          }
        },
        SpecifierWithRange {
          text: "./d.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 14,
              character: 20
            },
            end: Position {
              line: 14,
              character: 28
            }
          }
        },
        SpecifierWithRange {
          text: "./e.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 21,
              character: 21
            },
            end: Position {
              line: 21,
              character: 29
            }
          }
        },
      ]
    );
  }

  #[tokio::test]
  async fn test_analyze_ts_references_and_jsx_import_source_with_shebang() {
    let specifier = ModuleSpecifier::parse("file:///a/test.tsx").unwrap();
    let source = r#"#!/usr/bin/env -S deno run
/// <reference path="./ref.d.ts" />
/* @jsxImportSource preact */
export {};
"#;
    let module_info = DefaultModuleAnalyzer
      .analyze(&specifier, source.into(), MediaType::Tsx)
      .await
      .unwrap();
    assert_eq!(
      module_info,
      ModuleInfo {
        dependencies: vec![],
        ts_references: vec![TypeScriptReference::Path(SpecifierWithRange {
          text: "./ref.d.ts".to_owned(),
          range: PositionRange {
            start: Position {
              line: 1,
              character: 20,
            },
            end: Position {
              line: 1,
              character: 32,
            },
          },
        })],
        self_types_specifier: None,
        jsx_import_source: Some(SpecifierWithRange {
          text: "preact".to_owned(),
          range: PositionRange {
            start: Position {
              line: 2,
              character: 20,
            },
            end: Position {
              line: 2,
              character: 26,
            },
          },
        }),
        jsx_import_source_types: None,
        jsdoc_imports: vec![],
      },
    );
  }
}
