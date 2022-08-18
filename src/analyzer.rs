// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use lazy_static::lazy_static;
use regex::Match;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

use crate::graph::ModuleGraphError;
use crate::graph::Position;

lazy_static! {
  /// Matches the `@deno-types` pragma.
  static ref DENO_TYPES_RE: Regex =
    Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#)
      .unwrap();
}

/// Searches comments for any `@deno-types` compiler hints.
pub fn analyze_deno_types(
  desc: &DependencyDescriptor,
) -> Option<(String, PositionRange)> {
  fn comment_position_to_position_range(
    mut comment_start: Position,
    m: &Match,
  ) -> PositionRange {
    // the comment text starts after the double slash or slash star, so add 2
    comment_start.character += 2;
    PositionRange {
      // this will always be on the same line
      start: Position {
        line: comment_start.line,
        character: comment_start.character + m.start(),
      },
      end: Position {
        line: comment_start.line,
        character: comment_start.character + m.end(),
      },
    }
  }

  let comment = desc.leading_comments.last()?;
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some((
      m.as_str().to_string(),
      comment_position_to_position_range(comment.range.start.clone(), &m),
    ))
  } else if let Some(m) = captures.get(2) {
    Some((
      m.as_str().to_string(),
      comment_position_to_position_range(comment.range.start.clone(), &m),
    ))
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
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

  #[cfg(feature = "rust")]
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

// can't use this type directly because we need to make it serialize & deserialize
impl From<deno_ast::swc::dep_graph::DependencyKind> for DependencyKind {
  fn from(value: deno_ast::swc::dep_graph::DependencyKind) -> Self {
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

// can't use this type directly because we need to make it serialize & deserialize
impl From<deno_ast::swc::dep_graph::ImportAssertion> for ImportAssertion {
  fn from(value: deno_ast::swc::dep_graph::ImportAssertion) -> Self {
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

// can't use this type directly because we need to make it serialize & deserialize
impl From<deno_ast::swc::dep_graph::ImportAssertions> for ImportAssertions {
  fn from(value: deno_ast::swc::dep_graph::ImportAssertions) -> Self {
    use deno_ast::swc::dep_graph::ImportAssertions::*;
    match value {
      None => ImportAssertions::None,
      Unknown => ImportAssertions::Unknown,
      Known(value) => ImportAssertions::Known(
        value
          .into_iter()
          .map(|(key, value)| (key, value.into()))
          .collect(),
      ),
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
      text: comment.text,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeScriptReference {
  Path(SpecifierWithRange),
  Types(SpecifierWithRange),
}

/// Information about the module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInfo {
  /// Dependencies of the module.
  pub dependencies: Vec<DependencyDescriptor>,
  /// Triple slash references.
  pub ts_references: Vec<TypeScriptReference>,
  /// Comment with a `@jsxImportSource` pragma on JSX/TSX media types
  pub jsx_import_source: Option<SpecifierWithRange>,
  /// Type imports in JSDoc comment blocks (e.g. `{import("./types.d.ts").Type}`).
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
  ) -> Result<ModuleInfo, ModuleGraphError>;
}
