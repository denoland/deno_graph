// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

use anyhow::Result;
use deno_ast::parse_module;
use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::common::BytePos;
pub use deno_ast::swc::dep_graph::DependencyDescriptor;
pub use deno_ast::swc::dep_graph::DependencyKind;
use deno_ast::Diagnostic;
use deno_ast::MediaType;
use deno_ast::ParseParams;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

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

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct Position {
  /// The 0-indexed line index.
  pub line: usize,
  /// The 0-indexed character index.
  pub character: usize,
}

impl Position {
  fn from_pos(parsed_source: &ParsedSource, pos: BytePos) -> Self {
    let line_and_column_index =
      parsed_source.source().line_and_column_index(pos);
    Self {
      line: line_and_column_index.line_index,
      character: line_and_column_index.column_index,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct Range {
  #[serde(default)]
  pub start: Position,
  #[serde(default)]
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
    parsed_source: &ParsedSource,
    span: &deno_ast::swc::common::Span,
  ) -> Range {
    Range {
      start: Position::from_pos(parsed_source, span.lo),
      end: Position::from_pos(parsed_source, span.hi),
    }
  }

  fn from_comment_match(
    comment: &Comment,
    m: &Match,
    parsed_source: &ParsedSource,
  ) -> Self {
    Self {
      start: Position::from_pos(
        parsed_source,
        comment.span.lo + BytePos((m.start() + 1) as u32),
      ),
      end: Position::from_pos(
        parsed_source,
        comment.span.lo + BytePos((m.end() + 1) as u32),
      ),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
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
  parsed_source: &ParsedSource,
  desc: &DependencyDescriptor,
) -> Option<(String, Range)> {
  let comment = desc.leading_comments.last()?;
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some((
      m.as_str().to_string(),
      Range::from_comment_match(comment, &m, parsed_source),
    ))
  } else if let Some(m) = captures.get(2) {
    Some((
      m.as_str().to_string(),
      Range::from_comment_match(comment, &m, parsed_source),
    ))
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
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
          Range::from_comment_match(comment, &m, parsed_source),
        ));
      } else if let Some(captures) = TYPES_REFERENCE_RE.captures(&comment.text)
      {
        let m = captures.get(1).unwrap();
        references.push(TypeScriptReference::Types(
          m.as_str().to_string(),
          Range::from_comment_match(comment, &m, parsed_source),
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
    let parser = DefaultSourceParser::new();
    let result = parser.parse_module(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_source = result.unwrap();
    assert_eq!(analyze_dependencies(&parsed_source).len(), 6);
    assert_eq!(analyze_ts_references(&parsed_source).len(), 0);
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
    let range =
      Range::from_span(&parsed_source, &dependencies[0].specifier_span);
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
    let range =
      Range::from_span(&parsed_source, &dependencies[1].specifier_span);
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
