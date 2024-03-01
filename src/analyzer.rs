// Copyright 2018-2024 the Deno authors. MIT license.

use std::sync::Arc;

use deno_ast::dep::DependencyKind;
use deno_ast::dep::ImportAttributes;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParseDiagnostic;
use deno_ast::SourceRange;
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
  leading_comments: &[Comment],
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

  let comment = leading_comments.last()?;
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
  pub fn zeroed() -> Self {
    Self {
      start: Position::zeroed(),
      end: Position::zeroed(),
    }
  }

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
pub struct Comment {
  pub text: String,
  pub range: PositionRange,
}

impl Comment {
  pub fn from_dep_comment(
    comment: deno_ast::dep::DependencyComment,
    text_info: &SourceTextInfo,
  ) -> Comment {
    let range = PositionRange::from_source_range(comment.range, text_info);
    Comment {
      text: comment.text.to_string(),
      range,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", tag = "type")]
pub enum DependencyDescriptor {
  Static(StaticDependencyDescriptor),
  Dynamic(DynamicDependencyDescriptor),
}

impl DependencyDescriptor {
  pub fn as_static(&self) -> Option<&StaticDependencyDescriptor> {
    match self {
      Self::Static(descriptor) => Some(descriptor),
      Self::Dynamic(_) => None,
    }
  }

  pub fn as_dynamic(&self) -> Option<&DynamicDependencyDescriptor> {
    match self {
      Self::Static(_) => None,
      Self::Dynamic(d) => Some(d),
    }
  }

  pub fn leading_comments(&self) -> &Vec<Comment> {
    match self {
      DependencyDescriptor::Static(d) => &d.leading_comments,
      DependencyDescriptor::Dynamic(d) => &d.leading_comments,
    }
  }

  pub fn import_attributes(&self) -> &ImportAttributes {
    match self {
      DependencyDescriptor::Static(d) => &d.import_attributes,
      DependencyDescriptor::Dynamic(d) => &d.import_attributes,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StaticDependencyDescriptor {
  pub kind: DependencyKind,
  /// Any leading comments associated with the dependency.  This is used for
  /// further processing of supported pragma that impact the dependency.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub leading_comments: Vec<Comment>,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_range: PositionRange,
  /// Import attributes for this dependency.
  #[serde(skip_serializing_if = "ImportAttributes::is_none", default)]
  pub import_attributes: ImportAttributes,
}

impl From<StaticDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: StaticDependencyDescriptor) -> Self {
    DependencyDescriptor::Static(descriptor)
  }
}

#[derive(Default, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", untagged)]
pub enum DynamicArgument {
  String(String),
  Template(Vec<DynamicTemplatePart>),
  /// An expression that could not be analyzed.
  #[default]
  Expr,
}

impl DynamicArgument {
  pub fn is_expr(&self) -> bool {
    matches!(self, DynamicArgument::Expr)
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", tag = "type")]
pub enum DynamicTemplatePart {
  String {
    value: String,
  },
  /// An expression that could not be analyzed.
  Expr,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DynamicDependencyDescriptor {
  /// Any leading comments associated with the dependency.  This is used for
  /// further processing of supported pragma that impact the dependency.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub leading_comments: Vec<Comment>,
  /// The argument associated with the dynamic import.
  #[serde(skip_serializing_if = "DynamicArgument::is_expr", default)]
  pub argument: DynamicArgument,
  /// The range of the argument.
  pub argument_range: PositionRange,
  /// Import attributes for this dependency.
  #[serde(skip_serializing_if = "ImportAttributes::is_none", default)]
  pub import_attributes: ImportAttributes,
}

impl From<DynamicDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: DynamicDependencyDescriptor) -> Self {
    DependencyDescriptor::Dynamic(descriptor)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SpecifierWithRange {
  pub text: String,
  pub range: PositionRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
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
  ) -> Result<ModuleInfo, ParseDiagnostic>;
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use deno_ast::dep::ImportAttribute;
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
      dependencies: Vec::from([
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportEquals,
          leading_comments: vec![Comment {
            text: "a".to_string(),
            range: PositionRange {
              start: Position {
                line: 9,
                character: 7,
              },
              end: Position {
                line: 5,
                character: 3,
              },
            },
          }],
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
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: vec![],
          argument: DynamicArgument::String("./test2".to_string()),
          argument_range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
          import_attributes: ImportAttributes::Known(HashMap::from([
            ("key".to_string(), ImportAttribute::Unknown),
            (
              "key2".to_string(),
              ImportAttribute::Known("value".to_string()),
            ),
            ("kind".to_string(), ImportAttribute::Unknown),
          ])),
        }
        .into(),
      ]),
      ts_references: Vec::new(),
      jsx_import_source: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      json!({
        "dependencies": [{
          "type": "static",
          "kind": "importEquals",
          "leadingComments": [{
            "text": "a",
            "range": [[9, 7], [5, 3]],
          }],
          "specifier": "./test",
          "specifierRange": [[1, 2], [3, 4]],
        }, {
          "type": "dynamic",
          "argument": "./test2",
          "argumentRange": [[0, 0], [0, 0]],
          "importAttributes": {
            "known": {
              "key": null,
              "kind": null,
              "key2": "value",
            }
          }
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
          "type": "path",
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }, {
          "type": "types",
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
  fn static_dependency_descriptor_serialization() {
    // with dependencies
    let descriptor = DependencyDescriptor::Static(StaticDependencyDescriptor {
      kind: DependencyKind::ExportEquals,
      leading_comments: Vec::from([Comment {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }]),
      specifier: "./test".to_string(),
      specifier_range: PositionRange {
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      import_attributes: ImportAttributes::Unknown,
    });
    run_serialization_test(
      &descriptor,
      json!({
        "type": "static",
        "kind": "exportEquals",
        "leadingComments": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }],
        "specifier": "./test",
        "specifierRange": [[0, 0], [0, 0]],
        "importAttributes": "unknown",
      }),
    );
  }

  #[test]
  fn dynamic_dependency_descriptor_serialization() {
    run_serialization_test(
      &DependencyDescriptor::Dynamic(DynamicDependencyDescriptor {
        leading_comments: Vec::from([Comment {
          text: "a".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }]),
        argument: DynamicArgument::Expr,
        argument_range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
        import_attributes: ImportAttributes::Unknown,
      }),
      json!({
        "type": "dynamic",
        "leadingComments": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }],
        "argumentRange": [[0, 0], [0, 0]],
        "importAttributes": "unknown",
      }),
    );

    run_serialization_test(
      &DependencyDescriptor::Dynamic(DynamicDependencyDescriptor {
        leading_comments: Vec::from([Comment {
          text: "a".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }]),
        argument: DynamicArgument::String("test".to_string()),
        argument_range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
        import_attributes: ImportAttributes::Unknown,
      }),
      json!({
        "type": "dynamic",
        "leadingComments": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }],
        "argument": "test",
        "argumentRange": [[0, 0], [0, 0]],
        "importAttributes": "unknown",
      }),
    );
  }

  #[test]
  fn test_dynamic_argument_serialization() {
    run_serialization_test(
      &DynamicArgument::String("test".to_string()),
      json!("test"),
    );
    run_serialization_test(
      &DynamicArgument::Template(vec![
        DynamicTemplatePart::String {
          value: "test".to_string(),
        },
        DynamicTemplatePart::Expr,
      ]),
      json!([{
        "type": "string",
        "value": "test",
      }, {
        "type": "expr",
      }]),
    );
  }

  #[test]
  fn test_import_attributes_serialization() {
    run_serialization_test(&ImportAttributes::Unknown, json!("unknown"));
    run_serialization_test(
      &ImportAttributes::Known(HashMap::from([(
        "type".to_string(),
        ImportAttribute::Unknown,
      )])),
      json!({
        "known": {
          "type": null,
        }
      }),
    );
    run_serialization_test(
      &ImportAttributes::Known(HashMap::from([(
        "type".to_string(),
        ImportAttribute::Known("test".to_string()),
      )])),
      json!({
        "known": {
          "type": "test",
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
