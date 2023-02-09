// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_ast::Diagnostic;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use serde::Deserialize;
use serde::Serialize;

use crate::graph::Position;

/// A `@deno-types` pragma.
pub struct DenoTypesPragma {
  pub specifier: String,
  pub range: PositionRange,
}

/// Searches comments for any `@deno-types` compiler hints.
pub fn analyze_deno_types(
  desc: &DependencyDescriptor,
) -> Option<DenoTypesPragma> {
  let comment = desc.leading_comments.last()?;
  // @deno-types cannot be on a multi-line comment
  if comment.range.start.line != comment.range.end.line {
    return None;
  }

  let deno_types = parse_deno_types_pragma(&comment.text).ok()?;
  // the comment text starts after the double slash or slash star, so add 2
  let start_char = comment.range.start.character + 2;
  Some(DenoTypesPragma {
    specifier: deno_types.text.to_string(),
    range: PositionRange {
      start: Position {
        line: comment.range.start.line,
        character: start_char + deno_types.quote_start,
      },
      end: Position {
        line: comment.range.start.line,
        character: start_char + deno_types.quote_end,
      },
    },
  })
}

struct ParsedDenoTypes<'a> {
  text: &'a str,
  quote_start: usize,
  quote_end: usize,
}

fn parse_deno_types_pragma(
  input: &str,
) -> Result<ParsedDenoTypes, monch::ParseError> {
  use monch::*;

  let original_input = input;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = tag("@deno-types")(input)?;
  let (input, _) = ch('=')(input)?;
  let quote_start_input = input;
  let (input, quote_char) = or(ch('"'), ch('\"'))(input)?;
  let (input, text) = take_while(|c| c != quote_char)(input)?;
  Ok(ParsedDenoTypes {
    text,
    quote_start: original_input.len() - quote_start_input.len(),
    quote_end: original_input.len() - input.len() + 1,
  })
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PositionRange {
  pub start: Position,
  pub end: Position,
}

impl PositionRange {
  pub fn from_source_range(
    range: SourceRange,
    text_info: &SourceTextInfo,
  ) -> Self {
    Self {
      start: Position::from_source_pos(range.start, text_info),
      end: Position::from_source_pos(range.end, text_info),
    }
  }

  pub fn as_source_range(&self, text_info: &SourceTextInfo) -> SourceRange {
    SourceRange::new(
      self.start.as_source_pos(text_info),
      self.end.as_source_pos(text_info),
    )
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum DependencyKind {
  Import,
  ImportType,
  ImportEquals,
  Export,
  ExportType,
  ExportEquals,
  Require,
}

impl DependencyKind {
  // can't use this type directly because we need to make it serialize & deserialize
  pub fn from_swc(value: deno_ast::swc::dep_graph::DependencyKind) -> Self {
    use deno_ast::swc::dep_graph::DependencyKind::*;
    match value {
      Import => DependencyKind::Import,
      ImportType => DependencyKind::ImportType,
      ImportEquals => DependencyKind::ImportEquals,
      Export => DependencyKind::Export,
      ExportType => DependencyKind::ExportType,
      ExportEquals => DependencyKind::ExportEquals,
      Require => DependencyKind::Require,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ImportAssertion {
  /// The value of this assertion could not be statically analyzed.
  Unknown,
  /// The value of this assertion is a statically analyzed string.
  Known(String),
}

impl ImportAssertion {
  // can't use swc's type directly because we need to make it serialize & deserialize
  pub fn from_swc(value: deno_ast::swc::dep_graph::ImportAssertion) -> Self {
    use deno_ast::swc::dep_graph::ImportAssertion::*;
    match value {
      Unknown => ImportAssertion::Unknown,
      Known(value) => ImportAssertion::Known(value),
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ImportAssertions {
  /// There was no import assertions object literal.
  None,
  /// The set of assertion keys could not be statically analyzed.
  Unknown,
  /// The set of assertion keys is statically analyzed, though each respective
  /// value may or may not not be for dynamic imports.
  Known(HashMap<String, ImportAssertion>),
}

impl ImportAssertions {
  // can't use this type directly because we need to make it serialize & deserialize
  pub fn from_swc(value: deno_ast::swc::dep_graph::ImportAssertions) -> Self {
    use deno_ast::swc::dep_graph::ImportAssertions::*;
    match value {
      None => ImportAssertions::None,
      Unknown => ImportAssertions::Unknown,
      Known(value) => ImportAssertions::Known(
        value
          .into_iter()
          .map(|(key, value)| (key, ImportAssertion::from_swc(value)))
          .collect(),
      ),
    }
  }

  pub fn get(&self, key: &str) -> Option<&String> {
    match self {
      ImportAssertions::Known(map) => match map.get(key) {
        Some(ImportAssertion::Known(value)) => Some(value),
        _ => None,
      },
      _ => None,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Comment {
  pub text: String,
  pub range: PositionRange,
}

impl Comment {
  pub fn from_swc(
    comment: deno_ast::swc::common::comments::Comment,
    text_info: &SourceTextInfo,
  ) -> Comment {
    let range = PositionRange::from_source_range(comment.range(), text_info);
    Comment {
      text: comment.text.to_string(),
      range,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct DependencyDescriptor {
  pub kind: DependencyKind,
  /// A flag indicating if the import is dynamic or not.
  pub is_dynamic: bool,
  /// Any leading comments associated with the dependency.  This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<Comment>,
  /// The range of the import/export statement.
  pub range: PositionRange,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_range: PositionRange,
  /// Import assertions for this dependency.
  pub import_assertions: ImportAssertions,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecifierWithRange {
  pub text: String,
  pub range: PositionRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeScriptReference {
  Path(SpecifierWithRange),
  Types(SpecifierWithRange),
}

/// Information about the module.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleInfo {
  /// Dependencies of the module.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub dependencies: Vec<DependencyDescriptor>,
  /// Triple slash references.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub ts_references: Vec<TypeScriptReference>,
  /// Comment with a `@jsxImportSource` pragma on JSX/TSX media types
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub jsx_import_source: Option<SpecifierWithRange>,
  /// Type imports in JSDoc comment blocks (e.g. `{import("./types.d.ts").Type}`).
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub jsdoc_imports: Vec<SpecifierWithRange>,
}

/// Analyzes the provided module.
///
/// It can be assumed that the source has not changed since
/// it was loaded by deno_graph.
pub trait ModuleAnalyzer {
  /// Analyzes the module.
  fn analyze(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, Diagnostic>;
}
