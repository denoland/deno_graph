// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

pub use deno_ast::swc::dep_graph::DependencyDescriptor;
pub use deno_ast::swc::dep_graph::DependencyKind;

use anyhow::Result;
use deno_ast::parse_module;
use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::BytePos;
use deno_ast::swc::common::Span;
use deno_ast::Diagnostic;
use deno_ast::MediaType;
use deno_ast::ParseParams;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

lazy_static! {
  /// Matches the `@deno-types` pragma.
  static ref DENO_TYPES_RE: Regex =
    Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#)
      .unwrap();
  /// Matches a JSDoc import type reference (`{import("./example.js")}`).
  static ref JSDOC_IMPORT_RE: Regex = Regex::new(r#"\{\s*import\(['"]([^'"]+)['"]\)[^}]*}"#).unwrap();
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

pub enum TypeScriptReference {
  Path(String, Span),
  Types(String, Span),
}

/// Gets all the dependencies of this module.
pub fn analyze_dependencies(
  source: &ParsedSource,
) -> Vec<DependencyDescriptor> {
  deno_ast::swc::dep_graph::analyze_dependencies(
    source.module(),
    source.comments(),
  )
  .into_iter()
  .filter(|desc| desc.kind != DependencyKind::Require)
  .collect()
}

/// Searches comments for any `@deno-types` compiler hints.
pub fn analyze_deno_types(
  desc: &DependencyDescriptor,
) -> Option<(String, Span)> {
  let comment = desc.leading_comments.last()?;
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some((
      m.as_str().to_string(),
      comment_match_to_swc_span(comment, &m),
    ))
  } else if let Some(m) = captures.get(2) {
    Some((
      m.as_str().to_string(),
      comment_match_to_swc_span(comment, &m),
    ))
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
}

/// Searches JSDoc comment blocks for type imports
/// (e.g. `{import("./types.d.ts").Type}`) and returns a vector of tuples of
/// the specifier and the span of the import.
pub fn analyze_jsdoc_imports(
  parsed_source: &ParsedSource,
) -> Vec<(String, Span)> {
  let mut deps = Vec::new();
  for comment in parsed_source.comments().get_vec().iter() {
    if comment.kind != CommentKind::Block || !comment.text.starts_with('*') {
      continue;
    }
    for captures in JSDOC_IMPORT_RE.captures_iter(&comment.text) {
      if let Some(m) = captures.get(1) {
        deps.push((
          m.as_str().to_string(),
          comment_match_to_swc_span(comment, &m),
        ));
      }
    }
  }
  deps
}

/// Searches comments for a `@jsxImportSource` pragma on JSX/TSX media types
pub fn analyze_jsx_import_sources(
  parsed_source: &ParsedSource,
) -> Option<(String, Span)> {
  match parsed_source.media_type() {
    MediaType::Jsx | MediaType::Tsx => {
      parsed_source.get_leading_comments().iter().find_map(|c| {
        let captures = JSX_IMPORT_SOURCE_RE.captures(&c.text)?;
        let m = captures.get(1)?;
        Some((m.as_str().to_string(), comment_match_to_swc_span(c, &m)))
      })
    }
    _ => None,
  }
}

fn comment_match_to_swc_span(comment: &Comment, m: &Match) -> Span {
  // the comment text starts after the double slash or slash star, so add 2
  let comment_start = comment.span.lo + BytePos(2);
  Span::new(
    comment_start + BytePos(m.start() as u32),
    comment_start + BytePos(m.end() as u32),
    Default::default(),
  )
}

/// Searches comments for any triple slash references.
pub fn analyze_ts_references(
  parsed_source: &ParsedSource,
) -> Vec<TypeScriptReference> {
  let mut references = Vec::new();
  for comment in parsed_source.get_leading_comments().iter() {
    if TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text) {
      if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
        let m = captures.get(1).unwrap();
        references.push(TypeScriptReference::Path(
          m.as_str().to_string(),
          comment_match_to_swc_span(comment, &m),
        ));
      } else if let Some(captures) = TYPES_REFERENCE_RE.captures(&comment.text)
      {
        let m = captures.get(1).unwrap();
        references.push(TypeScriptReference::Types(
          m.as_str().to_string(),
          comment_match_to_swc_span(comment, &m),
        ));
      }
    }
  }
  references
}

/// Parses text to a `ParsedSource`.
pub trait SourceParser {
  /// Parses the provided module to a `ParsedSource`.
  fn parse_module(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<ParsedSource, Diagnostic>;
}

// TODO(@dsherret) remove CapturingSourceParser

/// An implementation of `SourceParser` that stores the parsed ASTs.
#[derive(Default)]
pub struct CapturingSourceParser {
  modules: RefCell<HashMap<ModuleSpecifier, ParsedSource>>,
}

#[cfg(feature = "rust")]
impl CapturingSourceParser {
  pub fn new() -> Self {
    Self {
      modules: RefCell::new(HashMap::new()),
    }
  }

  /// Gets a parsed source by module specifier if it was previously parsed.
  pub fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.modules.borrow().get(specifier).map(|m| m.to_owned())
  }
}

impl SourceParser for CapturingSourceParser {
  fn parse_module(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<ParsedSource, Diagnostic> {
    let module = parse_module(ParseParams {
      specifier: specifier.to_string(),
      source: SourceTextInfo::new(source),
      media_type,
      capture_tokens: false,
      scope_analysis: false,
      maybe_syntax: None,
    })?;

    self
      .modules
      .borrow_mut()
      .insert(specifier.clone(), module.clone());

    Ok(module)
  }
}

/// The default implementation of `SourceParser` used by this crate.
#[derive(Default)]
pub struct DefaultSourceParser;

impl DefaultSourceParser {
  pub fn new() -> Self {
    Self
  }
}

impl SourceParser for DefaultSourceParser {
  fn parse_module(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<String>,
    media_type: MediaType,
  ) -> Result<ParsedSource, Diagnostic> {
    parse_module(ParseParams {
      specifier: specifier.to_string(),
      source: SourceTextInfo::new(source),
      media_type,
      capture_tokens: false,
      scope_analysis: false,
      maybe_syntax: None,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;

  #[test]
  fn test_parse() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.tsx").expect("bad specifier");
    let source = Arc::new(
      r#"
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
    "#
      .to_string(),
    );
    let parser = DefaultSourceParser::new();
    let result = parser.parse_module(&specifier, source, MediaType::Tsx);
    assert!(result.is_ok());
    let parsed_source = result.unwrap();
    let dependencies = analyze_dependencies(&parsed_source);
    assert_eq!(dependencies.len(), 6);

    let ts_references = analyze_ts_references(&parsed_source);
    assert_eq!(ts_references.len(), 2);
    match &ts_references[0] {
      TypeScriptReference::Path(text, span) => {
        assert_eq!(text, "./ref.d.ts");
        assert_eq!(parsed_source.source().span_text(span), "./ref.d.ts");
      }
      TypeScriptReference::Types(_, _) => panic!("expected path"),
    }
    match &ts_references[1] {
      TypeScriptReference::Path(_, _) => panic!("expected types"),
      TypeScriptReference::Types(text, span) => {
        assert_eq!(text, "./types.d.ts");
        assert_eq!(parsed_source.source().span_text(span), "./types.d.ts");
      }
    }

    let dep_deno_types = analyze_deno_types(&dependencies[4]).unwrap();
    assert_eq!(
      dep_deno_types.0,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      parsed_source.source().span_text(&dep_deno_types.1),
      "https://deno.land/x/types/react/index.d.ts"
    );

    let (specifier, span) = analyze_jsx_import_sources(&parsed_source).unwrap();
    assert_eq!(specifier, "http://example.com/preact");
    assert_eq!(
      parsed_source.source().span_text(&span),
      "http://example.com/preact"
    );
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
    let parser = DefaultSourceParser::new();
    let result = parser.parse_module(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_source = result.unwrap();
    let dependencies = analyze_dependencies(&parsed_source);
    assert_eq!(dependencies.len(), 10);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.ts");
    assert_eq!(
      parsed_source
        .source()
        .span_text(&dependencies[0].specifier_span),
      "\"./a.ts\""
    );
    assert!(!dependencies[0].is_dynamic);
    assert_eq!(dependencies[1].specifier.to_string(), "./b.ts");
    assert_eq!(
      parsed_source
        .source()
        .span_text(&dependencies[1].specifier_span),
      "\"./b.ts\""
    );
    assert!(!dependencies[1].is_dynamic);
  }

  #[test]
  fn test_analyze_dependencies_import_assertions() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = Arc::new(
      r#"
    import a from "./a.json" assert { type: "json" };
    await import("./b.json", { assert: { type: "json" } });
    "#
      .to_string(),
    );
    let parser = DefaultSourceParser::new();
    let result = parser.parse_module(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_source = result.unwrap();
    let dependencies = analyze_dependencies(&parsed_source);
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
    let source = Arc::new(
      r#"
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
"#
      .to_string(),
    );
    let parser = DefaultSourceParser::new();
    let result = parser.parse_module(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_source = result.unwrap();
    let dependencies = analyze_jsdoc_imports(&parsed_source);
    assert_eq!(
      json!(dependencies),
      json!([
        [
          "./a.js",
          {
            "start": 62,
            "end": 68,
            "ctxt": 0
          }
        ],
        [
          "./b.js",
          {
            "start": 146,
            "end": 152,
            "ctxt": 0
          }
        ],
        [
          "./d.js",
          {
            "start": 179,
            "end": 185,
            "ctxt": 0
          }
        ]
      ])
    );
  }
}
