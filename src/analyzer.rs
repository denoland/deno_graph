// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_error::JsErrorBox;
use deno_media_type::MediaType;
use once_cell::sync::Lazy;
use regex::Regex;
use serde::ser::SerializeTuple;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;

use crate::graph::Position;
use crate::source::ResolutionMode;
use crate::ModuleSpecifier;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Hash)]
pub struct PositionRange {
  #[serde(default = "Position::zeroed")]
  pub start: Position,
  #[serde(default = "Position::zeroed")]
  pub end: Position,
}

impl PositionRange {
  pub fn zeroed() -> Self {
    Self {
      start: Position::zeroed(),
      end: Position::zeroed(),
    }
  }

  /// Determines if a given position is within the range.
  pub fn includes(&self, position: Position) -> bool {
    (position >= self.start) && (position <= self.end)
  }

  #[cfg(feature = "deno_ast")]
  pub fn from_source_range(
    range: deno_ast::SourceRange,
    text_info: &deno_ast::SourceTextInfo,
  ) -> Self {
    Self {
      start: Position::from_source_pos(range.start, text_info),
      end: Position::from_source_pos(range.end, text_info),
    }
  }

  #[cfg(feature = "deno_ast")]
  pub fn as_source_range(
    &self,
    text_info: &deno_ast::SourceTextInfo,
  ) -> deno_ast::SourceRange {
    deno_ast::SourceRange::new(
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

  pub fn import_attributes(&self) -> &ImportAttributes {
    match self {
      DependencyDescriptor::Static(d) => &d.import_attributes,
      DependencyDescriptor::Dynamic(d) => &d.import_attributes,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum ImportAttribute {
  /// The value of this attribute could not be statically analyzed.
  Unknown,
  /// The value of this attribute is a statically analyzed string.
  Known(String),
}

#[derive(Clone, Default, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ImportAttributes {
  /// There was no import attributes object literal.
  #[default]
  None,
  /// The set of attribute keys could not be statically analyzed.
  Unknown,
  /// The set of attribute keys is statically analyzed, though each respective
  /// value may or may not not be for dynamic imports.
  Known(HashMap<String, ImportAttribute>),
}

impl ImportAttributes {
  pub fn is_none(&self) -> bool {
    matches!(self, ImportAttributes::None)
  }

  pub fn get(&self, key: &str) -> Option<&String> {
    match self {
      ImportAttributes::Known(map) => match map.get(key) {
        Some(ImportAttribute::Known(value)) => Some(value),
        _ => None,
      },
      _ => None,
    }
  }
}

#[derive(
  Default, Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize,
)]
#[serde(rename_all = "camelCase")]
pub enum DynamicDependencyKind {
  #[default]
  Import,
  Require,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum StaticDependencyKind {
  Import,
  ImportType,
  ImportEquals,
  Export,
  ExportType,
  ExportEquals,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StaticDependencyDescriptor {
  /// The kind of dependency.
  pub kind: StaticDependencyKind,
  /// An optional specifier overriding the types associated with the
  /// import/export statement, if any.
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub types_specifier: Option<SpecifierWithRange>,
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
  #[serde(skip_serializing_if = "is_dynamic_esm", default)]
  pub kind: DynamicDependencyKind,
  /// An optional specifier overriding the types associated with the
  /// import/export statement, if any.
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub types_specifier: Option<SpecifierWithRange>,
  /// The argument associated with the dynamic import.
  #[serde(skip_serializing_if = "DynamicArgument::is_expr", default)]
  pub argument: DynamicArgument,
  /// The range of the argument.
  pub argument_range: PositionRange,
  /// Import attributes for this dependency.
  #[serde(skip_serializing_if = "ImportAttributes::is_none", default)]
  pub import_attributes: ImportAttributes,
}

fn is_dynamic_esm(kind: &DynamicDependencyKind) -> bool {
  *kind == DynamicDependencyKind::Import
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
pub enum TypeScriptTypesResolutionMode {
  Require,
  Import,
}

impl TypeScriptTypesResolutionMode {
  #[allow(clippy::should_implement_trait)]
  pub fn from_str(text: &str) -> Option<Self> {
    match text {
      "import" => Some(Self::Import),
      "require" => Some(Self::Require),
      _ => None,
    }
  }

  pub fn as_deno_graph(&self) -> ResolutionMode {
    match self {
      Self::Require => ResolutionMode::Require,
      Self::Import => ResolutionMode::Import,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum TypeScriptReference {
  Path(SpecifierWithRange),
  #[serde(rename_all = "camelCase")]
  Types {
    #[serde(flatten)]
    specifier: SpecifierWithRange,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    resolution_mode: Option<TypeScriptTypesResolutionMode>,
  },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsDocImportInfo {
  #[serde(flatten)]
  pub specifier: SpecifierWithRange,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub resolution_mode: Option<TypeScriptTypesResolutionMode>,
}

/// Information about JS/TS module.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModuleInfo {
  /// If the module has nothing that makes it for sure an ES module
  /// (no TLA, imports, exports, import.meta).
  #[serde(skip_serializing_if = "is_false", default, rename = "script")]
  pub is_script: bool,
  /// Dependencies of the module.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub dependencies: Vec<DependencyDescriptor>,
  /// Triple slash references.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub ts_references: Vec<TypeScriptReference>,
  /// Comment with `@ts-self-types` pragma.
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub self_types_specifier: Option<SpecifierWithRange>,
  /// Comment with a `@jsxImportSource` pragma on JSX/TSX media types
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub jsx_import_source: Option<SpecifierWithRange>,
  /// Comment with a `@jsxImportSourceTypes` pragma on JSX/TSX media types
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub jsx_import_source_types: Option<SpecifierWithRange>,
  /// Type imports in JSDoc comment blocks (e.g. `{import("./types.d.ts").Type}`)
  /// or `@import { SomeType } from "npm:some-module"`.
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub jsdoc_imports: Vec<JsDocImportInfo>,
}

pub fn module_graph_1_to_2(module_info: &mut serde_json::Value) {
  #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
  #[serde(rename_all = "camelCase")]
  struct Comment {
    text: String,
    range: PositionRange,
  }

  /// Searches comments for any `@deno-types` compiler hints.
  fn analyze_deno_types(
    leading_comments: &[Comment],
  ) -> Option<SpecifierWithRange> {
    fn comment_position_to_position_range(
      mut comment_start: Position,
      range: std::ops::Range<usize>,
    ) -> PositionRange {
      // the comment text starts after the double slash or slash star, so add 2
      comment_start.character += 2;
      PositionRange {
        // This will always be on the same line.
        // Does -1 and +1 to include the quotes
        start: Position {
          line: comment_start.line,
          character: comment_start.character + range.start - 1,
        },
        end: Position {
          line: comment_start.line,
          character: comment_start.character + range.end + 1,
        },
      }
    }

    let comment = leading_comments.last()?;
    let deno_types = find_deno_types(&comment.text)?;
    Some(SpecifierWithRange {
      text: deno_types.text.to_string(),
      range: comment_position_to_position_range(
        comment.range.start,
        deno_types.range,
      ),
    })
  }

  // To support older module graphs, we need to convert the module graph 1
  // to the new format. To do this, we need to extract the types specifier
  // from the leading comments and add it to the dependency object.
  if let serde_json::Value::Object(module_info) = module_info {
    if let Some(dependencies) = module_info
      .get_mut("dependencies")
      .and_then(|v| v.as_array_mut())
    {
      for dependency in dependencies {
        if let Some(dependency) = dependency.as_object_mut() {
          if let Some(leading_comments) = dependency
            .get("leadingComments")
            .and_then(|v| v.as_array())
            .and_then(|v| {
              v.iter()
                .map(|v| serde_json::from_value(v.clone()).ok())
                .collect::<Option<Vec<Comment>>>()
            })
          {
            if let Some(deno_types) = analyze_deno_types(&leading_comments) {
              dependency.insert(
                "typesSpecifier".to_string(),
                serde_json::to_value(deno_types).unwrap(),
              );
            }
            dependency.remove("leadingComments");
          }
        }
      }
    }
  };
}

/// Analyzes the provided module.
///
/// It can be assumed that the source has not changed since
/// it was loaded by deno_graph.
#[async_trait::async_trait(?Send)]
pub trait ModuleAnalyzer {
  /// Analyzes the module.
  async fn analyze(
    &self,
    specifier: &ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, JsErrorBox>;
}

impl<'a> Default for &'a dyn ModuleAnalyzer {
  fn default() -> &'a dyn ModuleAnalyzer {
    #[cfg(feature = "deno_ast")]
    {
      &crate::ast::DefaultModuleAnalyzer
    }
    #[cfg(not(feature = "deno_ast"))]
    {
      panic!("Provide a module analyzer or turn on the deno_ast feature of deno_graph.");
    }
  }
}

/// Matches a `/// <reference ... />` comment reference.
pub fn is_comment_triple_slash_reference(comment_text: &str) -> bool {
  static TRIPLE_SLASH_REFERENCE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^/\s*<reference\s.*?/>").unwrap());
  TRIPLE_SLASH_REFERENCE_RE.is_match(comment_text)
}

/// Matches a path reference, which adds a dependency to a module
pub fn find_path_reference(text: &str) -> Option<regex::Match> {
  static PATH_REFERENCE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"(?i)\spath\s*=\s*["']([^"']*)["']"#).unwrap());
  PATH_REFERENCE_RE
    .captures(text)
    .and_then(|captures| captures.get(1))
}

/// Matches a types reference, which for JavaScript files indicates the
/// location of types to use when type checking a program that includes it as
/// a dependency.
pub fn find_types_reference(text: &str) -> Option<regex::Match> {
  static TYPES_REFERENCE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"(?i)\stypes\s*=\s*["']([^"']*)["']"#).unwrap());
  TYPES_REFERENCE_RE.captures(text).and_then(|c| c.get(1))
}

/// Ex. `resolution-mode="require"` in `/// <reference types="pkg" resolution-mode="require" />`
pub fn find_resolution_mode(text: &str) -> Option<regex::Match> {
  static RESOLUTION_MODE_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"(?i)\sresolution-mode\s*=\s*["']([^"']*)["']"#).unwrap()
  });
  RESOLUTION_MODE_RE.captures(text).and_then(|m| m.get(1))
}

/// Matches the `@jsxImportSource` pragma.
pub fn find_jsx_import_source(text: &str) -> Option<regex::Match> {
  static JSX_IMPORT_SOURCE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^[\s*]*@jsxImportSource\s+(\S+)").unwrap());
  JSX_IMPORT_SOURCE_RE.captures(text).and_then(|c| c.get(1))
}

/// Matches the `@jsxImportSourceTypes` pragma.
pub fn find_jsx_import_source_types(text: &str) -> Option<regex::Match> {
  static JSX_IMPORT_SOURCE_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?i)^[\s*]*@jsxImportSourceTypes\s+(\S+)").unwrap()
  });
  JSX_IMPORT_SOURCE_TYPES_RE
    .captures(text)
    .and_then(|c| c.get(1))
}

/// Matches the `@ts-self-types` pragma.
pub fn find_ts_self_types(text: &str) -> Option<regex::Match> {
  static TS_SELF_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"(?i)^\s*@ts-self-types\s*=\s*["']([^"']+)["']"#).unwrap()
  });
  TS_SELF_TYPES_RE.captures(text).and_then(|c| c.get(1))
}

/// Matches the `@ts-types` pragma.
pub fn find_ts_types(text: &str) -> Option<regex::Match> {
  static TS_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"(?i)^\s*@ts-types\s*=\s*["']([^"']+)["']"#).unwrap()
  });
  TS_TYPES_RE.captures(text).and_then(|c| c.get(1))
}

pub struct DenoTypesPragmaMatch<'a> {
  pub text: &'a str,
  pub range: std::ops::Range<usize>,
  pub is_quoteless: bool,
}

pub fn find_deno_types(text: &str) -> Option<DenoTypesPragmaMatch> {
  /// Matches the `@deno-types` pragma.
  static DENO_TYPES_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"(?i)^\s*@deno-types\s*=\s*(?:["']([^"']+)["']|(\S+))"#)
      .unwrap()
  });

  let captures = DENO_TYPES_RE.captures(text)?;

  if let Some(m) = captures.get(1) {
    Some(DenoTypesPragmaMatch {
      text: m.as_str(),
      range: m.range(),
      is_quoteless: false,
    })
  } else if let Some(m) = captures.get(2) {
    Some(DenoTypesPragmaMatch {
      text: m.as_str(),
      range: m.range(),
      is_quoteless: true,
    })
  } else {
    unreachable!("Unexpected captures from deno types regex")
  }
}

fn is_false(v: &bool) -> bool {
  !v
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use pretty_assertions::assert_eq;
  use serde::de::DeserializeOwned;
  use serde_json::json;

  use super::*;

  #[test]
  fn module_info_serialization_empty() {
    // empty
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(&module_info, json!({}));
  }

  #[test]
  fn module_info_serialization_deps() {
    // with dependencies
    let module_info = ModuleInfo {
      is_script: true,
      dependencies: Vec::from([
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportEquals,
          types_specifier: Some(SpecifierWithRange {
            text: "a".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          }),
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
          kind: DynamicDependencyKind::Import,
          types_specifier: None,
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
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Require,
          types_specifier: None,
          argument: DynamicArgument::String("./test3".to_string()),
          argument_range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
          import_attributes: ImportAttributes::None,
        }
        .into(),
      ]),
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "script": true,
        "dependencies": [{
          "type": "static",
          "kind": "importEquals",
          "typesSpecifier": {
            "text": "a",
            "range": [[0, 0], [0, 0]],
          },
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
        }, {
          "type": "dynamic",
          "kind": "require",
          "argument": "./test3",
          "argumentRange": [[0, 0], [0, 0]]
        }]
      }),
    );
  }

  #[test]
  fn module_info_serialization_ts_references() {
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::from([
        TypeScriptReference::Path(SpecifierWithRange {
          text: "a".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }),
        TypeScriptReference::Types {
          specifier: SpecifierWithRange {
            text: "b".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: None,
        },
        TypeScriptReference::Types {
          specifier: SpecifierWithRange {
            text: "node".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Require),
        },
        TypeScriptReference::Types {
          specifier: SpecifierWithRange {
            text: "node-esm".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Import),
        },
      ]),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "tsReferences": [{
          "type": "path",
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }, {
          "type": "types",
          "text": "b",
          "range": [[0, 0], [0, 0]],
        }, {
          "type": "types",
          "text": "node",
          "range": [[0, 0], [0, 0]],
          "resolutionMode": "require",
        }, {
          "type": "types",
          "text": "node-esm",
          "range": [[0, 0], [0, 0]],
          "resolutionMode": "import",
        }]
      }),
    );
  }

  #[test]
  fn module_info_serialization_self_types_specifier() {
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      self_types_specifier: Some(SpecifierWithRange {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }),
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "selfTypesSpecifier": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }
      }),
    );
  }

  #[test]
  fn module_info_serialization_jsx_import_source() {
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: Some(SpecifierWithRange {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }),
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    run_serialization_test(
      &module_info,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "jsxImportSource": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }
      }),
    );
  }

  #[test]
  fn module_info_serialization_jsx_import_source_types() {
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: Some(SpecifierWithRange {
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
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "jsxImportSourceTypes": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }
      }),
    );
  }

  #[test]
  fn module_info_jsdoc_imports() {
    let module_info = ModuleInfo {
      is_script: false,
      dependencies: Vec::new(),
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::from([
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "a".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "b".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Import),
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "c".to_string(),
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Require),
        },
      ]),
    };
    run_serialization_test(
      &module_info,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "jsdocImports": [{
          "text": "a",
          "range": [[0, 0], [0, 0]],
        }, {
          "text": "b",
          "range": [[0, 0], [0, 0]],
          "resolutionMode": "import",
        }, {
          "text": "c",
          "range": [[0, 0], [0, 0]],
          "resolutionMode": "require",
        }]
      }),
    );
  }

  #[test]
  fn static_dependency_descriptor_serialization() {
    // with dependencies
    let descriptor = DependencyDescriptor::Static(StaticDependencyDescriptor {
      kind: StaticDependencyKind::ExportEquals,
      types_specifier: Some(SpecifierWithRange {
        text: "a".to_string(),
        range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      }),
      specifier: "./test".to_string(),
      specifier_range: PositionRange {
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      import_attributes: ImportAttributes::Unknown,
    });
    run_serialization_test(
      &descriptor,
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "type": "static",
        "kind": "exportEquals",
        "typesSpecifier": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        },
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
        kind: DynamicDependencyKind::Import,
        types_specifier: Some(SpecifierWithRange {
          text: "a".to_string(),
          range: PositionRange {
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        }),
        argument: DynamicArgument::Expr,
        argument_range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
        import_attributes: ImportAttributes::Unknown,
      }),
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "type": "dynamic",
        "typesSpecifier": {
          "text": "a",
          "range": [[0, 0], [0, 0]],
        },
        "argumentRange": [[0, 0], [0, 0]],
        "importAttributes": "unknown",
      }),
    );

    run_serialization_test(
      &DependencyDescriptor::Dynamic(DynamicDependencyDescriptor {
        kind: DynamicDependencyKind::Import,
        types_specifier: None,
        argument: DynamicArgument::String("test".to_string()),
        argument_range: PositionRange {
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
        import_attributes: ImportAttributes::Unknown,
      }),
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
      json!({
        "type": "dynamic",
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
      // WARNING: Deserialization MUST be backwards compatible in order
      // to load data from JSR.
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

  #[test]
  fn test_v1_to_v2_deserialization_with_leading_comment() {
    let expected = ModuleInfo {
      is_script: false,
      dependencies: vec![DependencyDescriptor::Static(
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          specifier: "./a.js".to_string(),
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
          types_specifier: Some(SpecifierWithRange {
            text: "./a.d.ts".to_string(),
            range: PositionRange {
              start: Position {
                line: 0,
                character: 15,
              },
              end: Position {
                line: 0,
                character: 25,
              },
            },
          }),
          import_attributes: ImportAttributes::None,
        },
      )],
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    let json = json!({
      "dependencies": [{
        "type": "static",
        "kind": "import",
        "specifier": "./a.js",
        "specifierRange": [[1, 2], [3, 4]],
        "leadingComments": [{
          "text": " @deno-types=\"./a.d.ts\"",
          "range": [[0, 0], [0, 25]],
        }]
      }]
    });
    run_v1_deserialization_test(json, &expected);
  }

  #[test]
  fn test_v1_to_v2_deserialization_no_leading_comment() {
    let expected = ModuleInfo {
      is_script: false,
      dependencies: vec![DependencyDescriptor::Static(
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          specifier: "./a.js".to_string(),
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
          types_specifier: None,
          import_attributes: ImportAttributes::None,
        },
      )],
      ts_references: Vec::new(),
      self_types_specifier: None,
      jsx_import_source: None,
      jsx_import_source_types: None,
      jsdoc_imports: Vec::new(),
    };
    let json = json!({
      "dependencies": [{
        "type": "static",
        "kind": "import",
        "specifier": "./a.js",
        "specifierRange": [[1, 2], [3, 4]],
      }]
    });
    run_v1_deserialization_test(json, &expected);
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

  #[track_caller]
  fn run_v1_deserialization_test<
    T: DeserializeOwned + Serialize + std::fmt::Debug + PartialEq + Eq,
  >(
    mut json: serde_json::Value,
    value: &T,
  ) {
    module_graph_1_to_2(&mut json);
    let deserialized_value = serde_json::from_value::<T>(json).unwrap();
    assert_eq!(deserialized_value, *value);
  }
}
