// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::media_type::MediaType;
use crate::module_specifier::ModuleSpecifier;

use anyhow::Result;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use swc_common::comments::Comment;
use swc_common::comments::SingleThreadedComments;
use swc_common::BytePos;
use swc_common::Spanned;
use swc_ecmascript::ast::Module;
use swc_ecmascript::dep_graph::DependencyDescriptor;
use swc_ecmascript::dep_graph::DependencyKind;
use swc_ecmascript::parser::lexer::Lexer;
use swc_ecmascript::parser::EsConfig;
use swc_ecmascript::parser::JscTarget;
use swc_ecmascript::parser::Parser;
use swc_ecmascript::parser::StringInput;
use swc_ecmascript::parser::Syntax;
use swc_ecmascript::parser::TsConfig;
use text_lines::TextLines;

static TARGET: JscTarget = JscTarget::Es2021;

lazy_static! {
  /// Matched the `@deno-types` pragma.
  static ref DENO_TYPES_RE: Regex =
    Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#)
      .unwrap();
  /// Matches a `/// <reference ... />` comment reference.
  static ref TRIPLE_SLASH_REFERENCE_RE: Regex =
    Regex::new(r"(?i)^/\s*<reference\s.*?/>").unwrap();
  /// Matches a path reference, which adds a dependency to a module
  static ref PATH_REFERENCE_RE: Regex =
    Regex::new(r#"(?i)\spath\s*=\s*["']([^"']*)["']"#).unwrap();
  /// Matches a types reference, which for JavaScript files indicates the
  /// location of types to use when type checking a program that includes it as
  /// a dependency.
  static ref TYPES_REFERENCE_RE: Regex =
    Regex::new(r#"(?i)\stypes\s*=\s*["']([^"']*)["']"#).unwrap();
}

fn get_es_config(jsx: bool) -> EsConfig {
  EsConfig {
    class_private_methods: true,
    class_private_props: true,
    class_props: true,
    dynamic_import: true,
    export_default_from: true,
    export_namespace_from: true,
    import_meta: true,
    jsx,
    nullish_coalescing: true,
    num_sep: true,
    optional_chaining: true,
    top_level_await: true,
    ..EsConfig::default()
  }
}

fn get_ts_config(tsx: bool, dts: bool) -> TsConfig {
  TsConfig {
    decorators: true,
    dts,
    dynamic_import: true,
    tsx,
    ..TsConfig::default()
  }
}

impl<'a> From<MediaType> for Syntax {
  fn from(media_type: MediaType) -> Self {
    match media_type {
      MediaType::JavaScript => Self::Es(get_es_config(false)),
      MediaType::Jsx => Self::Es(get_es_config(true)),
      MediaType::TypeScript => Self::Typescript(get_ts_config(false, false)),
      MediaType::Dts => Self::Typescript(get_ts_config(false, true)),
      MediaType::Tsx => Self::Typescript(get_ts_config(true, false)),
      _ => Self::Es(get_es_config(false)),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Position {
  /// The 0-indexed line index.
  pub line: usize,
  /// The 0-indexed character index.
  pub character: usize,
}

impl Position {
  pub fn from_pos(text_lines: &TextLines, pos: BytePos) -> Self {
    let line_and_column_index =
      text_lines.line_and_column_index(pos.0 as usize);
    Position {
      line: line_and_column_index.line_index,
      character: line_and_column_index.column_index,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Range {
  pub start: Position,
  pub end: Position,
}

impl Default for Range {
  fn default() -> Self {
    Self {
      start: Position {
        line: 0,
        character: 0,
      },
      end: Position {
        line: 0,
        character: 0,
      },
    }
  }
}

impl Range {
  pub(crate) fn from_span(
    parsed_ast: &dyn ParsedAst,
    span: &swc_common::Span,
  ) -> Range {
    Range {
      start: parsed_ast.get_position(span.lo),
      end: parsed_ast.get_position(span.hi),
    }
  }

  fn from_comment_match(
    comment: &Comment,
    m: &Match,
    get_position: impl Fn(BytePos) -> Position,
  ) -> Self {
    Self {
      start: get_position(comment.span.lo + BytePos((m.start() + 1) as u32)),
      end: get_position(comment.span.lo + BytePos((m.end() + 1) as u32)),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Span {
  #[serde(skip_serializing)]
  pub specifier: ModuleSpecifier,
  #[serde(flatten)]
  pub range: Range,
}

impl fmt::Display for Span {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}:{}:{}",
      self.specifier,
      self.range.start.line + 1,
      self.range.start.character + 1
    )
  }
}

#[derive(Debug, Clone)]
pub struct Location {
  pub specifier: ModuleSpecifier,
  pub position: Position,
}

impl fmt::Display for Location {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}:{}:{}",
      self.specifier,
      self.position.line + 1,
      self.position.character + 1
    )
  }
}

pub enum TypeScriptReference {
  Path(String, Range),
  Types(String, Range),
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
  pub location: Location,
  pub message: String,
}

impl std::error::Error for Diagnostic {}

impl fmt::Display for Diagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} at {}", self.message, self.location)
  }
}

/// A parsed module with comments.
pub trait ParsedAst {
  /// Gets the module specifier of the module.
  fn specifier(&self) -> &ModuleSpecifier;
  /// Gets the media type of the module.
  fn media_type(&self) -> MediaType;
  /// Gets the text content of the module.
  fn source(&self) -> Arc<String>;
  /// Gets the comments found in the source file.
  fn comments(&self) -> &dyn swc_common::comments::Comments;
  /// Gets the parsed module—AST.
  fn module(&self) -> &swc_ecmascript::ast::Module;
  /// Gets a position from the specified byte position.
  /// Note: This should/will panic when the byte position is
  // outside the bounds of the source file.
  fn get_position(&self, pos: BytePos) -> Position;

  /// Get the module's leading comments, where triple slash directives might
  /// be located.
  fn get_leading_comments(&self) -> Vec<Comment> {
    self
      .comments()
      .get_leading(self.module().span.lo)
      .unwrap_or_else(Vec::new)
  }

  /// Gets all the dependencies of this module.
  fn analyze_dependencies(&self) -> Vec<DependencyDescriptor> {
    swc_ecmascript::dep_graph::analyze_dependencies(
      self.module(),
      self.comments(),
    )
    .into_iter()
    .filter(|desc| desc.kind != DependencyKind::Require)
    .collect()
  }

  /// Searches comments for any `@deno-types` compiler hints.
  fn analyze_deno_types(
    &self,
    desc: &DependencyDescriptor,
  ) -> Option<(String, Range)> {
    let comment = desc.leading_comments.last()?;
    let captures = DENO_TYPES_RE.captures(&comment.text)?;
    if let Some(m) = captures.get(1) {
      Some((
        m.as_str().to_string(),
        Range::from_comment_match(comment, &m, |pos| self.get_position(pos)),
      ))
    } else if let Some(m) = captures.get(2) {
      Some((
        m.as_str().to_string(),
        Range::from_comment_match(comment, &m, |pos| self.get_position(pos)),
      ))
    } else {
      unreachable!("Unexpected captures from deno types regex")
    }
  }

  /// Searches comments for any triple slash references.
  fn analyze_ts_references(&self) -> Vec<TypeScriptReference> {
    let mut references = Vec::new();
    for comment in self.get_leading_comments().iter() {
      if TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text) {
        if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Path(
            m.as_str().to_string(),
            Range::from_comment_match(comment, &m, |pos| {
              self.get_position(pos)
            }),
          ));
        } else if let Some(captures) =
          TYPES_REFERENCE_RE.captures(&comment.text)
        {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Types(
            m.as_str().to_string(),
            Range::from_comment_match(comment, &m, |pos| {
              self.get_position(pos)
            }),
          ));
        }
      }
    }
    references
  }
}

/// Parses modules.
pub trait AstParser {
  /// Parses the provided information to a `ParsedAst` and returns a reference.
  fn parse(
    &mut self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<&dyn ParsedAst, Diagnostic>;
}

pub struct DefaultParsedAst {
  specifier: ModuleSpecifier,
  media_type: MediaType,
  source: Arc<String>,
  comments: SingleThreadedComments,
  module: Module,
  text_lines: TextLines,
}

impl ParsedAst for DefaultParsedAst {
  fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  fn media_type(&self) -> MediaType {
    self.media_type
  }

  fn source(&self) -> Arc<String> {
    self.source.clone()
  }

  fn comments(&self) -> &dyn swc_common::comments::Comments {
    &self.comments
  }

  fn module(&self) -> &swc_ecmascript::ast::Module {
    &self.module
  }

  fn get_position(&self, pos: BytePos) -> Position {
    Position::from_pos(&self.text_lines, pos)
  }
}

/// An implementation of `AstParser` that stores the parsed ASTs.
#[derive(Default)]
pub struct CapturingAstParser {
  modules: HashMap<ModuleSpecifier, DefaultParsedAst>,
}

#[cfg(feature = "rust")]
impl CapturingAstParser {
  pub fn new() -> Self {
    Self {
      modules: HashMap::new(),
    }
  }

  /// Gets an AST by a module specifier if it was previously parsed.
  pub fn get_ast(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&DefaultParsedAst> {
    self.modules.get(specifier)
  }
}

impl AstParser for CapturingAstParser {
  fn parse(
    &mut self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<&dyn ParsedAst, Diagnostic> {
    let ast = parse_default_ast(specifier, source, media_type)?;

    self.modules.insert(specifier.clone(), ast);

    Ok(self.modules.get(specifier).unwrap())
  }
}

/// The default implementation of `AstParser` used by this crate.
#[derive(Default)]
pub struct DefaultAstParser {
  module: Option<DefaultParsedAst>,
}

impl DefaultAstParser {
  pub fn new() -> Self {
    Self { module: None }
  }
}

impl AstParser for DefaultAstParser {
  fn parse(
    &mut self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<&dyn ParsedAst, Diagnostic> {
    let ast = parse_default_ast(specifier, source, media_type)?;

    self.module.replace(ast);

    Ok(self.module.as_ref().unwrap())
  }
}

fn parse_default_ast(
  specifier: &ModuleSpecifier,
  source: Arc<String>,
  media_type: MediaType,
) -> Result<DefaultParsedAst, Diagnostic> {
  let text_lines = TextLines::new(&source);
  let input =
    StringInput::new(&source, BytePos(0), BytePos(source.len() as u32));
  let comments = SingleThreadedComments::default();
  let lexer = Lexer::new(media_type.into(), TARGET, input, Some(&comments));
  let mut parser = Parser::new_from(lexer);
  let module = parser.parse_module().map_err(|err| Diagnostic {
    location: Location {
      specifier: specifier.clone(),
      position: Position::from_pos(&text_lines, err.span().lo),
    },
    message: err.into_kind().msg().to_string(),
  })?;

  Ok(DefaultParsedAst {
    specifier: specifier.clone(),
    media_type,
    source,
    comments,
    module,
    text_lines,
  })
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = Arc::new(
      r#"import {
      A,
      B,
      C,
      D,
    } from "https://deno.land/x/example@v1.0.0/mod.ts";

    export * from "./mod.ts";

    import type { Component } from "https://esm.sh/preact";
    import { h, Fragment } from "https://esm.sh/preact";

    // @deno-types="https://deno.land/x/types/react/index.d.ts";
    import React from "https://cdn.skypack.dev/react";

    const a = await import("./a.ts");
    "#
      .to_string(),
    );
    let mut parser = DefaultAstParser::new();
    let result = parser.parse(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_ast = result.unwrap();
    assert_eq!(parsed_ast.analyze_dependencies().len(), 6);
    assert_eq!(parsed_ast.analyze_ts_references().len(), 0);
  }

  #[test]
  fn test_analyze_dependencies() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = Arc::new(
      r#"
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
    "#
      .to_string(),
    );
    let mut parser = DefaultAstParser::new();
    let result = parser.parse(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_ast = result.unwrap();
    let dependencies = parsed_ast.analyze_dependencies();
    assert_eq!(dependencies.len(), 10);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.ts");
    let range = Range::from_span(parsed_ast, &dependencies[0].specifier_span);
    assert_eq!(
      range,
      Range {
        start: Position {
          line: 1,
          character: 23
        },
        end: Position {
          line: 1,
          character: 31
        },
      }
    );
    assert!(!dependencies[0].is_dynamic);
    assert_eq!(dependencies[1].specifier.to_string(), "./b.ts");
    let range = Range::from_span(parsed_ast, &dependencies[1].specifier_span);
    assert_eq!(
      range,
      Range {
        start: Position {
          line: 2,
          character: 11
        },
        end: Position {
          line: 2,
          character: 19
        },
      }
    );
    assert!(!dependencies[1].is_dynamic);
  }
}
