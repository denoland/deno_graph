// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;

use crate::graph::ModuleGraphError;
use crate::graph::Position;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ByteRange {
  pub start: usize,
  pub end: usize,
}

impl ByteRange {
  pub fn from_source_range(
    range: SourceRange,
    parsed_source: &ParsedSource,
  ) -> Self {
    let byte_range =
      range.as_byte_range(parsed_source.text_info().range().start);
    byte_range.into()
  }

  #[cfg(feature = "rust")]
  pub fn into_std(&self) -> std::ops::Range<usize> {
    self.start..self.end
  }
}

impl From<std::ops::Range<usize>> for ByteRange {
  fn from(range: std::ops::Range<usize>) -> Self {
    Self {
      start: range.start,
      end: range.end,
    }
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
  pub range: ByteRange,
}

impl Comment {
  pub fn from_swc(
    comment: deno_ast::swc::common::comments::Comment,
    parsed_source: &ParsedSource,
  ) -> Comment {
    let range = ByteRange::from_source_range(comment.range(), parsed_source);
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
  pub range: ByteRange,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_range: ByteRange,
  /// Import assertions for this dependency.
  pub import_assertions: ImportAssertions,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecifierWithRange {
  pub text: String,
  pub range: ByteRange,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeScriptReference {
  Path(SpecifierWithRange),
  Types(SpecifierWithRange),
}

/// Provides a ModuleAnalyzer for the provided specifier.
///
/// It can be assumed that the source has not changed since
/// it was loaded by deno_graph.
pub trait ModuleAnalyzerProvider {
  fn get_analyzer(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<Box<dyn ModuleAnalyzer>, ModuleGraphError>;
}

pub trait ModuleAnalyzer {
  /// Converts a byte index to a `deno_graph::Position`.
  fn byte_index_to_position(&self, byte_index: usize) -> Position;

  fn analyze_dependencies(&self) -> Vec<DependencyDescriptor>;

  /// Searches comments for any triple slash references.
  fn analyze_ts_references(&self) -> Vec<TypeScriptReference>;

  /// Searches comments for a `@jsxImportSource` pragma on JSX/TSX media types
  fn analyze_jsx_import_source(&self) -> Option<SpecifierWithRange>;

  /// Searches JSDoc comment blocks for type imports
  /// (e.g. `{import("./types.d.ts").Type}`) and returns a vector of tuples of
  /// the specifier and the span of the import.
  fn analyze_jsdoc_imports(&self) -> Vec<SpecifierWithRange>;
}
