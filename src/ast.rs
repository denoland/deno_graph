// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::media_type::MediaType;
use crate::module_specifier::ModuleSpecifier;
use crate::text_encoding::strip_bom;

use anyhow::Result;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use serde::Serialize;
use std::fmt;
use swc_common::comments::Comment;
use swc_common::comments::Comments;
use swc_common::comments::SingleThreadedComments;
use swc_common::BytePos;
use swc_common::Spanned;
use swc_ecmascript::ast::Module;
use swc_ecmascript::dep_graph::analyze_dependencies;
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
  fn from_comment_match(
    comment: &Comment,
    parsed_module: &ParsedModule,
    m: &Match,
  ) -> Self {
    Self {
      start: parsed_module
        .get_position(comment.span.lo + BytePos((m.start() + 1) as u32)),
      end: parsed_module
        .get_position(comment.span.lo + BytePos((m.end() + 1) as u32)),
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

pub(crate) struct ParsedModule {
  comments: SingleThreadedComments,
  module: Module,
  text_lines: TextLines,
}

impl ParsedModule {
  pub fn analyze_dependencies(&self) -> Vec<DependencyDescriptor> {
    analyze_dependencies(&self.module, &self.comments)
      .into_iter()
      .filter(|desc| desc.kind != DependencyKind::Require)
      .collect()
  }

  pub fn analyze_deno_types(
    &self,
    desc: &DependencyDescriptor,
  ) -> Option<(String, Range)> {
    let comment = desc.leading_comments.last()?;
    let captures = DENO_TYPES_RE.captures(&comment.text)?;
    if let Some(m) = captures.get(1) {
      Some((
        m.as_str().to_string(),
        Range::from_comment_match(comment, self, &m),
      ))
    } else if let Some(m) = captures.get(2) {
      Some((
        m.as_str().to_string(),
        Range::from_comment_match(comment, self, &m),
      ))
    } else {
      unreachable!("Unexpected captures from deno types regex")
    }
  }

  pub fn analyze_ts_references(&self) -> Vec<TypeScriptReference> {
    let mut references = Vec::new();
    for comment in self.get_leading_comments().iter() {
      if TRIPLE_SLASH_REFERENCE_RE.is_match(&comment.text) {
        if let Some(captures) = PATH_REFERENCE_RE.captures(&comment.text) {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Path(
            m.as_str().to_string(),
            Range::from_comment_match(comment, self, &m),
          ));
        } else if let Some(captures) =
          TYPES_REFERENCE_RE.captures(&comment.text)
        {
          let m = captures.get(1).unwrap();
          references.push(TypeScriptReference::Types(
            m.as_str().to_string(),
            Range::from_comment_match(comment, self, &m),
          ));
        }
      }
    }
    references
  }

  /// Get the module's leading comments, where triple slash directives might
  /// be located.
  pub fn get_leading_comments(&self) -> Vec<Comment> {
    self
      .comments
      .get_leading(self.module.span.lo)
      .unwrap_or_else(Vec::new)
  }

  pub fn range_from_span(&self, span: &swc_common::Span) -> Range {
    Range {
      start: self.get_position(span.lo),
      end: self.get_position(span.hi),
    }
  }

  pub fn get_position(&self, pos: BytePos) -> Position {
    Position::from_pos(&self.text_lines, pos)
  }
}

pub(crate) fn parse(
  specifier: &ModuleSpecifier,
  source: &str,
  media_type: MediaType,
) -> Result<ParsedModule, Diagnostic> {
  let source = strip_bom(source);
  let text_lines = TextLines::new(source);
  let input =
    StringInput::new(source, BytePos(0), BytePos(source.len() as u32));
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

  Ok(ParsedModule {
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
    let source = r#"import {
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
    let result = parse(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_module = result.unwrap();
    assert_eq!(parsed_module.analyze_dependencies().len(), 6);
    assert_eq!(parsed_module.analyze_ts_references().len(), 0);
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
    let result = parse(&specifier, source, MediaType::TypeScript);
    assert!(result.is_ok());
    let parsed_module = result.unwrap();
    let dependencies = parsed_module.analyze_dependencies();
    assert_eq!(dependencies.len(), 10);
    assert_eq!(dependencies[0].specifier.to_string(), "./a.ts");
    let range = parsed_module.range_from_span(&dependencies[0].specifier_span);
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
    let range = parsed_module.range_from_span(&dependencies[1].specifier_span);
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
