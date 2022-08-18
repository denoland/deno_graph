// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use crate::analyzer::Comment;
use crate::analyzer::DependencyDescriptor;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleInfo;
use crate::analyzer::PositionRange;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::TypeScriptReference;
use crate::graph::ModuleGraphError;
use crate::graph::Position;
use crate::module_specifier::ModuleSpecifier;

use deno_ast::SourcePos;
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
pub struct ParsedSourceAnalyzer;

impl ParsedSourceAnalyzer {
  /// Parse a module with the settings necessary for analysis.
  pub fn parse_module(
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ParsedSource, Diagnostic> {
    parse_module(ParseParams {
      specifier: specifier.to_string(),
      text_info: SourceTextInfo::new(source),
      media_type,
      capture_tokens: false,
      scope_analysis: false,
      maybe_syntax: None,
    })
  }

  /// Gets the module info from a parsed source.
  pub fn module_info(parsed_source: &ParsedSource) -> ModuleInfo {
    ModuleInfo {
      dependencies: analyze_dependencies(parsed_source),
      ts_references: analyze_ts_references(parsed_source),
      jsx_import_source: analyze_jsx_import_source(parsed_source),
      jsdoc_imports: analyze_jsdoc_imports(parsed_source),
    }
  }
}

impl ModuleAnalyzer for ParsedSourceAnalyzer {
  fn analyze(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ModuleGraphError> {
    let result =
      ParsedSourceAnalyzer::parse_module(specifier, source, media_type);
    match result {
      Ok(parsed_source) => {
        Ok(ParsedSourceAnalyzer::module_info(&parsed_source))
      }
      Err(diagnostic) => {
        Err(ModuleGraphError::ParseErr(specifier.clone(), diagnostic))
      }
    }
  }
}

fn analyze_dependencies(
  parsed_source: &ParsedSource,
) -> Vec<DependencyDescriptor> {
  deno_ast::swc::dep_graph::analyze_dependencies(
    parsed_source.module(),
    &parsed_source.comments().as_swc_comments(),
  )
  .into_iter()
  .filter(|desc| desc.kind != deno_ast::swc::dep_graph::DependencyKind::Require)
  .map(|d| DependencyDescriptor {
    kind: d.kind.into(),
    is_dynamic: d.is_dynamic,
    leading_comments: d
      .leading_comments
      .into_iter()
      .map(|c| Comment::from_swc(c, parsed_source.text_info()))
      .collect(),
    // ok to use this because we received this span from swc
    range: PositionRange::from_source_range(
      SourceRange::unsafely_from_span(d.span),
      parsed_source.text_info(),
    ),
    specifier: d.specifier.to_string(),
    specifier_range: PositionRange::from_source_range(
      SourceRange::unsafely_from_span(d.specifier_span),
      parsed_source.text_info(),
    ),
    import_assertions: d.import_assertions.into(),
  })
  .collect()
}

fn analyze_ts_references(
  parsed_source: &ParsedSource,
) -> Vec<crate::analyzer::TypeScriptReference> {
  let mut references = Vec::new();
  for comment in parsed_source.get_leading_comments().iter() {
    if TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text) {
      let comment_start = comment.start();
      if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
        let m = captures.get(1).unwrap();
        references.push(TypeScriptReference::Path(SpecifierWithRange {
          text: m.as_str().to_string(),
          range: comment_source_to_position_range(
            comment_start,
            &m,
            parsed_source.text_info(),
          ),
        }));
      } else if let Some(captures) = TYPES_REFERENCE_RE.captures(&comment.text)
      {
        let m = captures.get(1).unwrap();
        references.push(TypeScriptReference::Types(SpecifierWithRange {
          text: m.as_str().to_string(),
          range: comment_source_to_position_range(
            comment_start,
            &m,
            parsed_source.text_info(),
          ),
        }));
      }
    }
  }
  references
}

fn analyze_jsx_import_source(
  parsed_source: &ParsedSource,
) -> Option<SpecifierWithRange> {
  match parsed_source.media_type() {
    MediaType::Jsx | MediaType::Tsx => {
      parsed_source.get_leading_comments().iter().find_map(|c| {
        let captures = JSX_IMPORT_SOURCE_RE.captures(&c.text)?;
        let m = captures.get(1)?;
        Some(SpecifierWithRange {
          text: m.as_str().to_string(),
          range: comment_source_to_position_range(
            c.start(),
            &m,
            parsed_source.text_info(),
          ),
        })
      })
    }
    _ => None,
  }
}

fn analyze_jsdoc_imports(
  parsed_source: &ParsedSource,
) -> Vec<SpecifierWithRange> {
  // Analyze any JSDoc type imports
  // We only analyze these on JavaScript types of modules, since they are
  // ignored by TypeScript when type checking anyway and really shouldn't be
  // there, but some people do strange things.
  if !matches!(
    parsed_source.media_type(),
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  ) {
    return Vec::new();
  }

  let mut deps = Vec::new();
  for comment in parsed_source.comments().get_vec().iter() {
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
            parsed_source.text_info(),
          ),
        });
      }
    }
  }
  deps
}

fn comment_source_to_position_range(
  comment_start: SourcePos,
  m: &Match,
  text_info: &SourceTextInfo,
) -> PositionRange {
  // the comment text starts after the double slash or slash star, so add 2
  let comment_start = comment_start + 2;
  PositionRange {
    start: Position::from_source_pos(comment_start + m.start(), text_info),
    end: Position::from_source_pos(comment_start + m.end(), text_info),
  }
}

#[cfg(test)]
mod tests {
  use crate::analyzer::analyze_deno_types;

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
    let parsed_source = ParsedSourceAnalyzer::parse_module(
      &specifier,
      source.into(),
      MediaType::Tsx,
    )
    .unwrap();
    let text_info = parsed_source.text_info();
    let module_info = ParsedSourceAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 6);

    let ts_references = module_info.ts_references;
    assert_eq!(ts_references.len(), 2);
    match &ts_references[0] {
      TypeScriptReference::Path(specifier) => {
        assert_eq!(specifier.text, "./ref.d.ts");
        assert_eq!(
          text_info.range_text(&specifier.range.as_source_range(text_info)),
          "./ref.d.ts"
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
          "./types.d.ts"
        );
      }
    }

    let dep_deno_types = analyze_deno_types(&dependencies[4]).unwrap();
    assert_eq!(
      dep_deno_types.0,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.1.as_source_range(text_info)),
      "https://deno.land/x/types/react/index.d.ts"
    );

    let jsx_import_source = module_info.jsx_import_source.unwrap();
    assert_eq!(jsx_import_source.text, "http://example.com/preact");
    assert_eq!(
      text_info.range_text(&jsx_import_source.range.as_source_range(text_info)),
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
    let parsed_source = ParsedSourceAnalyzer::parse_module(
      &specifier,
      source.into(),
      MediaType::TypeScript,
    )
    .unwrap();
    let module_info = ParsedSourceAnalyzer::module_info(&parsed_source);
    let text_info = parsed_source.text_info();
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 10);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.ts");
    assert_eq!(
      text_info.range_text(
        &dependencies[0].specifier_range.as_source_range(text_info)
      ),
      "\"./a.ts\""
    );
    assert!(!dependencies[0].is_dynamic);
    assert_eq!(dependencies[1].specifier.to_string(), "./b.ts");
    assert_eq!(
      text_info.range_text(
        &dependencies[1].specifier_range.as_source_range(text_info)
      ),
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
    let parsed_source = ParsedSourceAnalyzer::parse_module(
      &specifier,
      source.into(),
      MediaType::TypeScript,
    )
    .unwrap();
    let module_info = ParsedSourceAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.dependencies;
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
    let parsed_source = ParsedSourceAnalyzer::parse_module(
      &specifier,
      source.into(),
      MediaType::JavaScript,
    )
    .unwrap();
    let module_info = ParsedSourceAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.jsdoc_imports;
    assert_eq!(
      dependencies,
      [
        SpecifierWithRange {
          text: "./a.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 6,
              character: 18
            },
            end: Position {
              line: 6,
              character: 24
            }
          }
        },
        SpecifierWithRange {
          text: "./b.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 13,
              character: 19
            },
            end: Position {
              line: 13,
              character: 25
            }
          }
        },
        SpecifierWithRange {
          text: "./d.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 14,
              character: 21
            },
            end: Position {
              line: 14,
              character: 27
            }
          }
        },
        SpecifierWithRange {
          text: "./e.js".to_string(),
          range: PositionRange {
            start: Position {
              line: 21,
              character: 22
            },
            end: Position {
              line: 21,
              character: 28
            }
          }
        },
      ]
    );
  }
}
