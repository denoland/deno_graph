// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use crate::analyzer::ByteRange;
use crate::analyzer::Comment;
use crate::analyzer::DependencyDescriptor;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleAnalyzerProvider;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::TypeScriptReference;
use crate::graph::ModuleGraphError;
use crate::graph::Position;
use crate::module_specifier::ModuleSpecifier;

use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;

use anyhow::Result;
use deno_ast::parse_module;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::Diagnostic;
use deno_ast::MediaType;
use deno_ast::ParseParams;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use std::sync::Arc;

lazy_static! {
  /// Matches the `@deno-types` pragma.
  static ref DENO_TYPES_RE: Regex =
    Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#)
      .unwrap();
  /// Matches a JSDoc import type reference (`{import("./example.js")}`
  static ref JSDOC_IMPORT_RE: Regex = Regex::new(r#"\{[^}]*import\(['"]([^'"]+)['"]\)[^}]*}"#).unwrap();
  /// Matches the `@jsxImportSource` pragma.
  static ref JSX_IMPORT_SOURCE_RE: Regex = Regex::new(r#"(?i)^[\s*]*@jsxImportSource\s+(\S+)"#).unwrap();
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

#[derive(Default, Clone)]
pub struct ParsedSourceAnalyzerProvider;

impl ParsedSourceAnalyzerProvider {
  pub fn get_concrete_analyzer(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ParsedSourceAnalyzer, Diagnostic> {
    let parsed_source = parse_module(ParseParams {
      specifier: specifier.to_string(),
      text_info: SourceTextInfo::new(source),
      media_type,
      capture_tokens: false,
      scope_analysis: false,
      maybe_syntax: None,
    })?;
    Ok(ParsedSourceAnalyzer(parsed_source))
  }
}

impl ModuleAnalyzerProvider for ParsedSourceAnalyzerProvider {
  fn get_analyzer(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<Box<dyn ModuleAnalyzer>, ModuleGraphError> {
    let result = self.get_concrete_analyzer(specifier, source, media_type);
    match result {
      Ok(analyzer) => Ok(Box::new(analyzer)),
      Err(diagnostic) => {
        Err(ModuleGraphError::ParseErr(specifier.clone(), diagnostic))
      }
    }
  }
}

pub struct ParsedSourceAnalyzer(ParsedSource);

impl ParsedSourceAnalyzer {
  pub fn new(source: ParsedSource) -> Self {
    Self(source)
  }

  pub fn parsed_source(&self) -> &ParsedSource {
    &self.0
  }
}

impl ModuleAnalyzer for ParsedSourceAnalyzer {
  fn byte_index_to_position(&self, byte_index: usize) -> Position {
    let text_info = self.0.text_info();
    let pos = text_info.range().start + byte_index;
    let line_and_column_index = text_info.line_and_column_index(pos);
    Position {
      line: line_and_column_index.line_index,
      character: line_and_column_index.column_index,
    }
  }

  fn analyze_dependencies(&self) -> Vec<DependencyDescriptor> {
    deno_ast::swc::dep_graph::analyze_dependencies(
      self.0.module(),
      &self.0.comments().as_swc_comments(),
    )
    .into_iter()
    .filter(|desc| {
      desc.kind != deno_ast::swc::dep_graph::DependencyKind::Require
    })
    .map(|d| DependencyDescriptor {
      kind: d.kind.into(),
      is_dynamic: d.is_dynamic,
      leading_comments: d
        .leading_comments
        .into_iter()
        .map(|c| Comment::from_swc(c, &self.0))
        .collect(),
      // ok to use this because we received this span from swc
      range: ByteRange::from_source_range(
        SourceRange::unsafely_from_span(d.span),
        &self.0,
      ),
      specifier: d.specifier.to_string(),
      specifier_range: ByteRange::from_source_range(
        SourceRange::unsafely_from_span(d.specifier_span),
        &self.0,
      ),
      import_assertions: d.import_assertions.into(),
    })
    .collect()
  }

  fn analyze_ts_references(&self) -> Vec<crate::analyzer::TypeScriptReference> {
    let mut references = Vec::new();
    for comment in self.0.get_leading_comments().iter() {
      if TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text) {
        let comment_start = comment
          .start()
          .as_byte_index(self.0.text_info().range().start);
        if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Path(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_match_to_byte_range(comment_start, &m),
          }));
        } else if let Some(captures) =
          TYPES_REFERENCE_RE.captures(&comment.text)
        {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Types(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_match_to_byte_range(comment_start, &m),
          }));
        }
      }
    }
    references
  }

  fn analyze_jsx_import_source(&self) -> Option<SpecifierWithRange> {
    match self.0.media_type() {
      MediaType::Jsx | MediaType::Tsx => {
        self.0.get_leading_comments().iter().find_map(|c| {
          let captures = JSX_IMPORT_SOURCE_RE.captures(&c.text)?;
          let m = captures.get(1)?;
          Some(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_match_to_byte_range(
              c.start().as_byte_index(self.0.text_info().range().start),
              &m,
            ),
          })
        })
      }
      _ => None,
    }
  }

  fn analyze_jsdoc_imports(&self) -> Vec<SpecifierWithRange> {
    let mut deps = Vec::new();
    for comment in self.0.comments().get_vec().iter() {
      if comment.kind != CommentKind::Block || !comment.text.starts_with('*') {
        continue;
      }
      for captures in JSDOC_IMPORT_RE.captures_iter(&comment.text) {
        if let Some(m) = captures.get(1) {
          deps.push(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_match_to_byte_range(
              comment
                .range()
                .start
                .as_byte_index(self.0.text_info().range().start),
              &m,
            ),
          });
        }
      }
    }
    deps
  }
}

/// Searches comments for any `@deno-types` compiler hints.
pub fn analyze_deno_types(
  desc: &DependencyDescriptor,
) -> Option<(String, ByteRange)> {
  let comment = desc.leading_comments.last()?;
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some((
      m.as_str().to_string(),
      comment_match_to_byte_range(comment.range.start, &m),
    ))
  } else if let Some(m) = captures.get(2) {
    Some((
      m.as_str().to_string(),
      comment_match_to_byte_range(comment.range.start, &m),
    ))
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
}

fn comment_match_to_byte_range(comment_start: usize, m: &Match) -> ByteRange {
  // the comment text starts after the double slash or slash star, so add 2
  let comment_start = comment_start + 2;
  (comment_start + m.start()..comment_start + m.end()).into()
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
    /* @jsxImportSource http://example.com/preact */
    import {
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
    "#;
    let analyzer_provider = ParsedSourceAnalyzerProvider::default();
    let analyzer = analyzer_provider
      .get_concrete_analyzer(&specifier, source.into(), MediaType::Tsx)
      .unwrap();
    let file_text = analyzer.parsed_source().text_info().text_str();
    let dependencies = analyzer.analyze_dependencies();
    assert_eq!(dependencies.len(), 6);

    let ts_references = analyzer.analyze_ts_references();
    assert_eq!(ts_references.len(), 2);
    match &ts_references[0] {
      TypeScriptReference::Path(specifier) => {
        assert_eq!(specifier.text, "./ref.d.ts");
        assert_eq!(&file_text[specifier.range.into_std()], "./ref.d.ts");
      }
      TypeScriptReference::Types(_) => panic!("expected path"),
    }
    match &ts_references[1] {
      TypeScriptReference::Path(_) => panic!("expected types"),
      TypeScriptReference::Types(specifier) => {
        assert_eq!(specifier.text, "./types.d.ts");
        assert_eq!(&file_text[specifier.range.into_std()], "./types.d.ts");
      }
    }

    let dep_deno_types = analyze_deno_types(&dependencies[4]).unwrap();
    assert_eq!(
      dep_deno_types.0,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      &file_text[dep_deno_types.1.into_std()],
      "https://deno.land/x/types/react/index.d.ts"
    );

    let specifier = analyzer.analyze_jsx_import_source().unwrap();
    assert_eq!(specifier.text, "http://example.com/preact");
    assert_eq!(
      &file_text[specifier.range.into_std()],
      "http://example.com/preact"
    );
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
    let analyzer_provider = ParsedSourceAnalyzerProvider::default();
    let analyzer = analyzer_provider
      .get_concrete_analyzer(&specifier, source.into(), MediaType::TypeScript)
      .unwrap();
    let file_text = analyzer.parsed_source().text_info().text_str();
    let dependencies = analyzer.analyze_dependencies();
    assert_eq!(dependencies.len(), 10);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.ts");
    assert_eq!(
      &file_text[dependencies[0].specifier_range.into_std()],
      "\"./a.ts\""
    );
    assert!(!dependencies[0].is_dynamic);
    assert_eq!(dependencies[1].specifier.to_string(), "./b.ts");
    assert_eq!(
      &file_text[dependencies[1].specifier_range.into_std()],
      "\"./b.ts\""
    );
    assert!(!dependencies[1].is_dynamic);
  }

  #[test]
  fn test_analyze_dependencies_import_assertions() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = r#"
    import a from "./a.json" assert { type: "json" };
    await import("./b.json", { assert: { type: "json" } });
    "#;
    let analyzer_provider = ParsedSourceAnalyzerProvider::default();
    let analyzer = analyzer_provider
      .get_concrete_analyzer(&specifier, source.into(), MediaType::TypeScript)
      .unwrap();
    let dependencies = analyzer.analyze_dependencies();
    assert_eq!(dependencies.len(), 2);
    assert!(!dependencies[0].is_dynamic);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.json");
    assert_eq!(
      dependencies[0].import_assertions.get("type"),
      Some(&"json".to_string())
    );
    assert!(dependencies[1].is_dynamic);
    assert_eq!(dependencies[1].specifier.to_string(), "./b.json");
    assert_eq!(
      dependencies[1].import_assertions.get("type"),
      Some(&"json".to_string())
    );
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
    let analyzer_provider = ParsedSourceAnalyzerProvider::default();
    let analyzer = analyzer_provider
      .get_concrete_analyzer(&specifier, source.into(), MediaType::TypeScript)
      .unwrap();
    let dependencies = analyzer.analyze_jsdoc_imports();
    assert_eq!(
      dependencies,
      [
        SpecifierWithRange {
          text: "./a.js".to_string(),
          range: ByteRange { start: 61, end: 67 }
        },
        SpecifierWithRange {
          text: "./b.js".to_string(),
          range: ByteRange {
            start: 144,
            end: 150,
          }
        },
        SpecifierWithRange {
          text: "./d.js".to_string(),
          range: ByteRange {
            start: 177,
            end: 183,
          }
        },
        SpecifierWithRange {
          text: "./e.js".to_string(),
          range: ByteRange {
            start: 246,
            end: 252,
          }
        },
      ]
    );
  }
}
