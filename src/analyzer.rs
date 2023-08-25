// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_ast::Diagnostic;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use once_cell::sync::Lazy;
use regex::Match;
use regex::Regex;
use serde::ser::SerializeTuple;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;

use crate::graph::Position;

/// Matches the `@deno-types` pragma.
static DENO_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
  Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#).unwrap()
});

/// A `@deno-types` pragma.
pub struct DenoTypesPragma {
  pub specifier: String,
  pub range: PositionRange,
}

/// Searches comments for any `@deno-types` compiler hints.
pub fn analyze_deno_types(
  desc: &DependencyDescriptor,
) -> Option<DenoTypesPragma> {
  fn comment_position_to_position_range(
    mut comment_start: Position,
    m: &Match,
  ) -> PositionRange {
    // the comment text starts after the double slash or slash star, so add 2
    comment_start.character += 2;
    PositionRange {
      // This will always be on the same line.
      // Does -1 and +1 to include the quotes
      start: Position {
        line: comment_start.line,
        character: comment_start.character + m.start() - 1,
      },
      end: Position {
        line: comment_start.line,
        character: comment_start.character + m.end() + 1,
      },
    }
  }

  let comment = desc.leading_comments.last()?;
  let captures = DENO_TYPES_RE.captures(&comment.text)?;
  if let Some(m) = captures.get(1) {
    Some(DenoTypesPragma {
      specifier: m.as_str().to_string(),
      range: comment_position_to_position_range(
        comment.range.start.clone(),
        &m,
      ),
    })
  } else if let Some(m) = captures.get(2) {
    Some(DenoTypesPragma {
      specifier: m.as_str().to_string(),
      range: comment_position_to_position_range(
        comment.range.start.clone(),
        &m,
      ),
    })
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
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

// Custom serialization to serialize to an array. Interestingly we
// don't need to implement custom deserialization logic that does
// the same thing, and serde_json will handle it fine.
impl Serialize for PositionRange {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    struct PositionSerializer<'a>(&'a Position);

    impl Serialize for PositionSerializer<'_> {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: Serializer,
      {
        let mut seq = serializer.serialize_tuple(2)?;
        seq.serialize_element(&self.0.line)?;
        seq.serialize_element(&self.0.character)?;
        seq.end()
      }
    }

    let mut seq = serializer.serialize_tuple(2)?;
    seq.serialize_element(&PositionSerializer(&self.start))?;
    seq.serialize_element(&PositionSerializer(&self.end))?;
    seq.end()
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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
#[serde(rename_all = "camelCase")]
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
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum ImportAssertions {
  /// There was no import assertions object literal.
  None,
  /// The set of assertion keys could not be statically analyzed.
  Unknown,
  /// The set of assertion keys is statically analyzed, though each respective
  /// value may or may not not be for dynamic imports.
  Known(HashMap<String, ImportAssertion>),
}

impl Default for ImportAssertions {
  fn default() -> Self {
    Self::None
  }
}

impl ImportAssertions {
  pub fn is_none(&self) -> bool {
    matches!(self, ImportAssertions::None)
  }

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
#[serde(rename_all = "camelCase")]
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

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DependencyDescriptor {
  pub kind: DependencyKind,
  /// A flag indicating if the import is dynamic or not.
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_dynamic: bool,
  /// Any leading comments associated with the dependency.  This is used for
  /// further processing of supported pragma that impact the dependency.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub leading_comments: Vec<Comment>,
  /// The range of the import/export statement.
  pub range: PositionRange,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_range: PositionRange,
  /// Import assertions for this dependency.
  #[serde(skip_serializing_if = "ImportAssertions::is_none", default)]
  pub import_assertions: ImportAssertions,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SpecifierWithRange {
  pub text: String,
  pub range: PositionRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum TypeScriptReference {
  Path(SpecifierWithRange),
  Types(SpecifierWithRange),
}

/// Information about the module.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;
  use serde::de::DeserializeOwned;
  use serde_json::json;

  use super::*;

  #[test]
  fn module_info_serialization_empty() {
    // empty
    let module_info = ModuleInfo {
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      jsx_import_source: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(&module_info, json!({}));
  }

  #[test]
  fn module_info_serialization_deps() {
    // with dependencies
    let module_info = ModuleInfo {
      dependencies: Vec::from([DependencyDescriptor {
        kind: DependencyKind::ImportEquals,
        is_dynamic: false,
        leading_comments: Vec::new(),
        range: PositionRange {
          start: Position {
            line: 4,
            character: 3,
          },
          end: Position {
            line: 2,
            character: 1,
          },
        },
        specifier: "./test".to_string(),
        specifier_range: PositionRange {
          start: Position {
            line: 1,
            character: 2,
          },
          end: Position {
            line: 3,
            character: 4,
          },
        },
        import_assertions: ImportAssertions::None,
      }]),
      ts_references: Vec::new(),
      jsx_import_source: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      json!({
        "dependencies": [{
          "kind": "importEquals",
          "range": [[4, 3], [2, 1]],
          "specifier": "./test",
          "specifierRange": [[1, 2], [3, 4]],
        }]
      }),
    );
  }

  #[test]
  fn module_info_serialization_ts_references() {
    let module_info = ModuleInfo {
      dependencies: Vec::new(),
      ts_references: Vec::from([
        TypeScriptReference::Path(SpecifierWithRange {
          text: "a".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }),
        TypeScriptReference::Types(SpecifierWithRange {
          text: "b".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }),
      ]),
      jsx_import_source: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      json!({
        "tsReferences": [{
          "kind": "path",
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }, {
          "kind": "types",
          "text": "b",
          "range": [[0, 0], [0, 0]],
        }]
      }),
    );
  }

  #[test]
  fn module_info_serialization_jsx_import_source() {
    let module_info = ModuleInfo {
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      jsx_import_source: Some(SpecifierWithRange {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }),
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      json!({
        "jsxImportSource": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }
      }),
    );
  }

  #[test]
  fn module_info_jsdoc_imports() {
    let module_info = ModuleInfo {
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      jsx_import_source: None,
      jsdoc_imports: Vec::from([SpecifierWithRange {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }]),
    };
    run_serialization_test(
      &module_info,
      json!({
        "jsdocImports": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }]
      }),
    );
  }

  #[test]
  fn dependency_descriptor_serialization() {
    // with dependencies
    let module_info = DependencyDescriptor {
      kind: DependencyKind::ExportEquals,
      is_dynamic: true,
      leading_comments: Vec::from([Comment {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }]),
      range: PositionRange {
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      specifier: "./test".to_string(),
      specifier_range: PositionRange {
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      import_assertions: ImportAssertions::Unknown,
    };
    run_serialization_test(
      &module_info,
      json!({
        "kind": "exportEquals",
        "isDynamic": true,
        "leadingComments": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }],
        "range": [[0, 0], [0, 0]],
        "specifier": "./test",
        "specifierRange": [[0, 0], [0, 0]],
        "importAssertions": {
          "kind": "unknown"
        }
      }),
    );
  }

  #[track_caller]
  fn run_serialization_test<
    T: DeserializeOwned + Serialize + std::fmt::Debug + PartialEq + Eq,
  >(
    value: &T,
    expected_json: serde_json::Value,
  ) {
    let json = serde_json::to_value(value).unwrap();
    assert_eq!(json, expected_json);
    let deserialized_value = serde_json::from_value::<T>(json).unwrap();
    assert_eq!(deserialized_value, *value);
  }
}
