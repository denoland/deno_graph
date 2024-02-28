// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::analyzer::analyze_deno_types;
use crate::analyzer::DependencyDescriptor;
use crate::analyzer::DynamicArgument;
use crate::analyzer::DynamicTemplatePart;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleInfo;
use crate::analyzer::PositionRange;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::TypeScriptReference;
#[cfg(feature = "fast_check")]
use crate::fast_check::FastCheckDtsModule;
use crate::CapturingModuleAnalyzer;
use crate::ModuleParser;
use crate::ReferrerImports;

use crate::fast_check::FastCheckDiagnostic;
use crate::module_specifier::is_fs_root_specifier;
use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::module_specifier::SpecifierError;
use crate::packages::resolve_version;
use crate::packages::JsrPackageInfo;
use crate::packages::JsrPackageVersionInfo;
use crate::packages::PackageSpecifiers;
use crate::rt::spawn;
use crate::rt::Executor;
use crate::rt::JoinHandle;
use crate::source::*;

use anyhow::anyhow;
use deno_ast::dep::DependencyKind;
use deno_ast::dep::ImportAttributes;
use deno_ast::LineAndColumnIndex;
use deno_ast::MediaType;
use deno_ast::ParseDiagnostic;
use deno_ast::SourcePos;
use deno_ast::SourceTextInfo;
use deno_semver::jsr::JsrDepPackageReq;
use deno_semver::jsr::JsrPackageReqReference;
use deno_semver::npm::NpmPackageNvReference;
use deno_semver::npm::NpmPackageReqReference;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageNvReference;
use deno_semver::package::PackageReq;
use deno_semver::package::PackageReqReferenceParseError;
use deno_semver::RangeSetOrTag;
use deno_semver::Version;
use futures::future::LocalBoxFuture;
use futures::future::Shared;
use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::FutureExt;
use indexmap::IndexMap;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::sync::Arc;
use thiserror::Error;
use url::Url;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Position {
  /// The 0-indexed line index.
  pub line: usize,
  /// The 0-indexed character index.
  pub character: usize,
}

impl PartialOrd for Position {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Position {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.line.cmp(&other.line) {
      Ordering::Equal => self.character.cmp(&other.character),
      Ordering::Greater => Ordering::Greater,
      Ordering::Less => Ordering::Less,
    }
  }
}

impl Position {
  pub fn zeroed() -> Self {
    Self {
      line: 0,
      character: 0,
    }
  }

  pub fn from_source_pos(pos: SourcePos, text_info: &SourceTextInfo) -> Self {
    let line_and_column_index = text_info.line_and_column_index(pos);
    Self {
      line: line_and_column_index.line_index,
      character: line_and_column_index.column_index,
    }
  }

  pub fn as_source_pos(&self, text_info: &SourceTextInfo) -> SourcePos {
    text_info.loc_to_source_pos(LineAndColumnIndex {
      line_index: self.line,
      column_index: self.character,
    })
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Range {
  #[serde(skip_serializing)]
  pub specifier: ModuleSpecifier,
  #[serde(default = "Position::zeroed")]
  pub start: Position,
  #[serde(default = "Position::zeroed")]
  pub end: Position,
}

impl fmt::Display for Range {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}:{}:{}",
      self.specifier,
      self.start.line + 1,
      self.start.character + 1
    )
  }
}

impl Range {
  pub(crate) fn from_position_range(
    specifier: ModuleSpecifier,
    range: PositionRange,
  ) -> Range {
    Range {
      specifier,
      start: range.start,
      end: range.end,
    }
  }

  /// Determines if a given position is within the range.
  pub fn includes(&self, position: &Position) -> bool {
    (position >= &self.start) && (position <= &self.end)
  }
}

#[derive(Debug, Clone)]
pub enum ModuleError {
  LoadingErr(ModuleSpecifier, Option<Range>, Arc<anyhow::Error>),
  Missing(ModuleSpecifier, Option<Range>),
  MissingDynamic(ModuleSpecifier, Range),
  MissingWorkspaceMemberExports {
    specifier: ModuleSpecifier,
    maybe_range: Option<Range>,
    nv: PackageNv,
  },
  UnknownPackage {
    specifier: ModuleSpecifier,
    maybe_range: Option<Range>,
    package_name: String,
  },
  UnknownPackageReq {
    specifier: ModuleSpecifier,
    maybe_range: Option<Range>,
    package_req: PackageReq,
  },
  UnknownExport {
    specifier: ModuleSpecifier,
    maybe_range: Option<Range>,
    nv: PackageNv,
    export_name: String,
    exports: Vec<String>,
  },
  ParseErr(ModuleSpecifier, deno_ast::ParseDiagnostic),
  UnsupportedMediaType(ModuleSpecifier, MediaType, Option<Range>),
  InvalidTypeAssertion {
    specifier: ModuleSpecifier,
    range: Range,
    actual_media_type: MediaType,
    expected_media_type: MediaType,
  },
  UnsupportedImportAttributeType {
    specifier: ModuleSpecifier,
    range: Range,
    kind: String,
  },
}

impl ModuleError {
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::LoadingErr(s, _, _)
      | Self::ParseErr(s, _)
      | Self::UnsupportedMediaType(s, _, _)
      | Self::Missing(s, _)
      | Self::MissingDynamic(s, _)
      | Self::MissingWorkspaceMemberExports { specifier: s, .. }
      | Self::UnknownExport { specifier: s, .. }
      | Self::UnknownPackage { specifier: s, .. }
      | Self::UnknownPackageReq { specifier: s, .. }
      | Self::InvalidTypeAssertion { specifier: s, .. }
      | Self::UnsupportedImportAttributeType { specifier: s, .. } => s,
    }
  }

  pub fn maybe_referrer(&self) -> Option<&Range> {
    match self {
      Self::LoadingErr(_, maybe_referrer, _) => maybe_referrer.as_ref(),
      Self::Missing(_, maybe_referrer) => maybe_referrer.as_ref(),
      Self::MissingDynamic(_, range) => Some(range),
      Self::MissingWorkspaceMemberExports { maybe_range, .. } => {
        maybe_range.as_ref()
      }
      Self::UnknownExport { maybe_range, .. } => maybe_range.as_ref(),
      Self::UnknownPackage { maybe_range, .. } => maybe_range.as_ref(),
      Self::UnknownPackageReq { maybe_range, .. } => maybe_range.as_ref(),
      Self::UnsupportedMediaType(_, _, maybe_referrer) => {
        maybe_referrer.as_ref()
      }
      Self::ParseErr(_, _) => None,
      Self::InvalidTypeAssertion { range, .. } => Some(range),
      Self::UnsupportedImportAttributeType { range, .. } => Some(range),
    }
  }

  /// Converts the error into a string along with the range related to the error.
  pub fn to_string_with_range(&self) -> String {
    if let Some(range) = self.maybe_referrer() {
      format!("{self:#}\n    at {range}")
    } else {
      format!("{self:#}")
    }
  }
}

impl std::error::Error for ModuleError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::LoadingErr(_, _, err) => Some(err.as_ref().as_ref()),
      Self::Missing(_, _)
      | Self::MissingDynamic(_, _)
      | Self::ParseErr(_, _)
      | Self::MissingWorkspaceMemberExports { .. }
      | Self::UnknownExport { .. }
      | Self::UnknownPackage { .. }
      | Self::UnknownPackageReq { .. }
      | Self::UnsupportedMediaType(_, _, _)
      | Self::InvalidTypeAssertion { .. }
      | Self::UnsupportedImportAttributeType { .. } => None,
    }
  }
}

impl fmt::Display for ModuleError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LoadingErr(_, _, err) => err.fmt(f),
      Self::ParseErr(_, diagnostic) => write!(f, "The module's source code could not be parsed: {diagnostic}"),
      Self::UnknownExport { export_name, exports, nv, specifier, .. } => {
        let exports_text = exports.iter().map(|e| format!(" * {}", e)).collect::<Vec<_>>().join("\n");
        write!(f, "Unknown export '{export_name}' for '{nv}'.\n  Specifier: {specifier}\n  Package exports:\n{exports_text}")
      }
      Self::UnknownPackage { package_name, specifier, .. } =>
        write!(f, "Unknown package: {package_name}\n  Specifier: {specifier}"),
      Self::UnknownPackageReq { package_req, specifier, .. } =>
        write!(f, "Could not find constraint in the list of versions: {package_req}\n  Specifier: {specifier}"),
      Self::UnsupportedMediaType(specifier, MediaType::Json, ..) => write!(f, "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import attribute with the type of \"json\".\n  Specifier: {specifier}"),
      Self::UnsupportedMediaType(specifier, media_type, ..) => write!(f, "Expected a JavaScript or TypeScript module, but identified a {media_type} module. Importing these types of modules is currently not supported.\n  Specifier: {specifier}"),
      Self::Missing(specifier, _) => write!(f, "Module not found \"{specifier}\"."),
      Self::MissingDynamic(specifier, _) => write!(f, "Dynamic import not found \"{specifier}\"."),
      Self::MissingWorkspaceMemberExports { nv, specifier, .. } => {
        write!(f, "Expected workspace package '{nv}' to define exports in its deno.json.\n  Specifier: {specifier}")
      }
      Self::InvalidTypeAssertion { specifier, actual_media_type: MediaType::Json, expected_media_type, .. } =>
        write!(f, "Expected a {expected_media_type} module, but identified a Json module. Consider importing Json modules with an import attribute with the type of \"json\".\n  Specifier: {specifier}"),
      Self::InvalidTypeAssertion { specifier, actual_media_type, expected_media_type, .. } =>
        write!(f, "Expected a {expected_media_type} module, but identified a {actual_media_type} module.\n  Specifier: {specifier}"),
      Self::UnsupportedImportAttributeType { specifier, kind, .. } =>
        write!(f, "The import attribute type of \"{kind}\" is unsupported.\n  Specifier: {specifier}"),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ModuleGraphError {
  ModuleError(ModuleError),
  ResolutionError(ResolutionError),
  TypesResolutionError(ResolutionError),
}

impl ModuleGraphError {
  fn for_resolution_mode(mode: ResolutionMode, error: ResolutionError) -> Self {
    match mode {
      ResolutionMode::Execution => Self::ResolutionError(error),
      ResolutionMode::Types => Self::TypesResolutionError(error),
    }
  }

  /// Converts the error into a string along with the range related to the error.
  ///
  /// We don't include the range in the error messages by default because they're
  /// not useful in cases like the LSP where the range is given by the editor itself.
  pub fn to_string_with_range(&self) -> String {
    match self {
      ModuleGraphError::ModuleError(err) => err.to_string_with_range(),
      ModuleGraphError::ResolutionError(err)
      | ModuleGraphError::TypesResolutionError(err) => {
        err.to_string_with_range()
      }
    }
  }

  pub fn maybe_range(&self) -> Option<&Range> {
    match self {
      Self::ModuleError(err) => err.maybe_referrer(),
      Self::ResolutionError(err) | Self::TypesResolutionError(err) => {
        Some(err.range())
      }
    }
  }
}

impl std::error::Error for ModuleGraphError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::ModuleError(ref err) => Some(err),
      Self::ResolutionError(ref err) | Self::TypesResolutionError(ref err) => {
        Some(err)
      }
    }
  }
}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::ModuleError(err) => err.fmt(f),
      Self::ResolutionError(err) => err.fmt(f),
      Self::TypesResolutionError(err) => {
        f.write_str("Failed resolving types. ")?;
        err.fmt(f)
      }
    }
  }
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
  InvalidDowngrade {
    specifier: ModuleSpecifier,
    range: Range,
  },
  InvalidLocalImport {
    specifier: ModuleSpecifier,
    range: Range,
  },
  InvalidSpecifier {
    error: SpecifierError,
    range: Range,
  },
  ResolverError {
    error: Arc<ResolveError>,
    specifier: String,
    range: Range,
  },
}

impl ResolutionError {
  /// Return a reference to the range that the error applies to.
  pub fn range(&self) -> &Range {
    match self {
      Self::InvalidDowngrade { range, .. }
      | Self::InvalidLocalImport { range, .. }
      | Self::InvalidSpecifier { range, .. }
      | Self::ResolverError { range, .. } => range,
    }
  }

  /// Converts the error into a string along with the range related to the error.
  pub fn to_string_with_range(&self) -> String {
    format!("{}\n    at {}", self, self.range())
  }
}

impl std::error::Error for ResolutionError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::InvalidDowngrade { .. } | Self::InvalidLocalImport { .. } => None,
      Self::InvalidSpecifier { ref error, .. } => Some(error),
      Self::ResolverError { error, .. } => Some(error.as_ref()),
    }
  }
}

impl PartialEq for ResolutionError {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (
        Self::ResolverError {
          specifier: a,
          range: a_range,
          ..
        },
        Self::ResolverError {
          specifier: b,
          range: b_range,
          ..
        },
      ) => a == b && a_range == b_range,
      (
        Self::InvalidDowngrade {
          specifier: a,
          range: a_range,
          ..
        },
        Self::InvalidDowngrade {
          specifier: b,
          range: b_range,
          ..
        },
      )
      | (
        Self::InvalidLocalImport {
          specifier: a,
          range: a_range,
          ..
        },
        Self::InvalidLocalImport {
          specifier: b,
          range: b_range,
          ..
        },
      ) => a == b && a_range == b_range,
      (
        Self::InvalidSpecifier {
          error: a,
          range: a_range,
          ..
        },
        Self::InvalidSpecifier {
          error: b,
          range: b_range,
          ..
        },
      ) => a == b && a_range == b_range,
      _ => false,
    }
  }
}

impl Eq for ResolutionError {}

impl fmt::Display for ResolutionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::InvalidDowngrade { specifier, .. } => write!(f, "Modules imported via https are not allowed to import http modules.\n  Importing: {specifier}"),
      Self::InvalidLocalImport { specifier, .. } => write!(f, "Remote modules are not allowed to import local modules. Consider using a dynamic import instead.\n  Importing: {specifier}"),
      Self::ResolverError { error, .. } => error.fmt(f),
      Self::InvalidSpecifier { error, .. } => error.fmt(f),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolutionResolved {
  /// Specifier to.
  pub specifier: ModuleSpecifier,
  /// Referrer range.
  pub range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolution {
  None,
  Ok(Box<ResolutionResolved>),
  Err(Box<ResolutionError>),
}

impl Resolution {
  pub fn from_resolve_result(
    result: Result<ModuleSpecifier, ResolveError>,
    specifier_text: &str,
    range: Range,
  ) -> Self {
    match result {
      Ok(specifier) => {
        Resolution::Ok(Box::new(ResolutionResolved { specifier, range }))
      }
      Err(err) => {
        let resolution_error =
          if let ResolveError::Specifier(specifier_error) = err {
            ResolutionError::InvalidSpecifier {
              error: specifier_error.clone(),
              range,
            }
          } else {
            ResolutionError::ResolverError {
              error: Arc::new(err),
              specifier: specifier_text.to_string(),
              range,
            }
          };
        Self::Err(Box::new(resolution_error))
      }
    }
  }

  pub fn includes(&self, position: &Position) -> Option<&Range> {
    match self {
      Self::Ok(resolution) if resolution.range.includes(position) => {
        Some(&resolution.range)
      }
      Self::Err(err) => {
        let range = err.range();
        if range.includes(position) {
          Some(range)
        } else {
          None
        }
      }
      _ => None,
    }
  }

  pub fn is_none(&self) -> bool {
    matches!(self, Self::None)
  }

  pub fn maybe_specifier(&self) -> Option<&ModuleSpecifier> {
    self.ok().map(|r| &r.specifier)
  }

  pub fn maybe_range(&self) -> Option<&Range> {
    match self {
      Resolution::None => None,
      Resolution::Ok(r) => Some(&r.range),
      Resolution::Err(e) => Some(e.range()),
    }
  }

  pub fn ok(&self) -> Option<&ResolutionResolved> {
    if let Resolution::Ok(resolved) = self {
      Some(&**resolved)
    } else {
      None
    }
  }

  pub fn err(&self) -> Option<&ResolutionError> {
    if let Resolution::Err(err) = self {
      Some(&**err)
    } else {
      None
    }
  }
}

impl Default for Resolution {
  fn default() -> Self {
    Self::None
  }
}

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Clone, Copy, Debug, Serialize, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum ImportKind {
  /// `import`/`export`
  Es,
  /// `import type`/`export type`
  TsType,
  /// `/// <reference path="..." />`
  TsReferencePath,
  /// `/// <reference types="..." />`
  TsReferenceTypes,
  /// `/** @jsxImportSource ... */`
  JsxImportSource,
  /// `/** @typedef { import("./types").Pet } Pet */`
  JsDoc,
}

impl ImportKind {
  pub fn is_runtime(&self) -> bool {
    match self {
      ImportKind::Es | ImportKind::JsxImportSource => true,
      ImportKind::TsType
      | ImportKind::TsReferencePath
      | ImportKind::TsReferenceTypes
      | ImportKind::JsDoc => false,
    }
  }

  fn is_es(&self) -> bool {
    matches!(self, ImportKind::Es)
  }
}

#[derive(Clone, Debug, Serialize)]
#[cfg_attr(test, derive(Eq, PartialEq))]
#[serde(rename_all = "camelCase")]
pub struct Import {
  pub specifier: String,
  #[serde(skip_serializing_if = "ImportKind::is_es")]
  pub kind: ImportKind,
  pub range: Range,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  // Don't include attributes in `deno info --json` until someone has a need.
  // Attribute error strings eventually will be included in a separate `Import::errors`, however.
  #[serde(skip_serializing)]
  pub attributes: ImportAttributes,
}

#[derive(Debug, Default, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
  #[serde(rename = "code", skip_serializing_if = "Resolution::is_none")]
  pub maybe_code: Resolution,
  #[serde(rename = "type", skip_serializing_if = "Resolution::is_none")]
  pub maybe_type: Resolution,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  // todo(dsherret): rename to attributeType in 2.0
  #[serde(rename = "assertionType", skip_serializing_if = "Option::is_none")]
  pub maybe_attribute_type: Option<String>,
  // TODO(nayeemrmn): Replace `maybe_attribute_type` with this in the serialization
  // for 2.0.
  #[serde(skip_serializing)]
  pub imports: Vec<Import>,
}

impl Dependency {
  /// Optionally return the module specifier in the module graph that points to
  /// the "code" dependency in the graph.
  pub fn get_code(&self) -> Option<&ModuleSpecifier> {
    self.maybe_code.maybe_specifier()
  }

  /// Optionally return the module specifier in the module graph that points to
  /// the type only dependency in the graph.
  pub fn get_type(&self) -> Option<&ModuleSpecifier> {
    self.maybe_type.maybe_specifier()
  }

  /// Check to see if the position falls within the range of the code or types
  /// entry for the dependency, returning a reference to the range if true,
  /// otherwise none.
  pub fn includes(&self, position: &Position) -> Option<&Range> {
    for import in &self.imports {
      if import.range.includes(position) {
        return Some(&import.range);
      }
    }
    // `@deno-types` directives won't be associated with an import.
    if let Some(range) = self.maybe_type.includes(position) {
      return Some(range);
    }
    None
  }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TypesDependency {
  pub specifier: String,
  pub dependency: Resolution,
}

fn is_media_type_unknown(media_type: &MediaType) -> bool {
  matches!(media_type, MediaType::Unknown)
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceMember {
  pub base: Url,
  pub nv: PackageNv,
  pub exports: IndexMap<String, String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum Module {
  // todo(#239): remove this when updating the --json output for 2.0
  #[serde(rename = "esm")]
  Js(JsModule),
  // todo(#239): remove this when updating the --json output for 2.0
  #[serde(rename = "asserted")]
  Json(JsonModule),
  Npm(NpmModule),
  Node(BuiltInNodeModule),
  External(ExternalModule),
}

impl Module {
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Module::Js(module) => &module.specifier,
      Module::Json(module) => &module.specifier,
      Module::Npm(module) => &module.specifier,
      Module::Node(module) => &module.specifier,
      Module::External(module) => &module.specifier,
    }
  }

  pub fn json(&self) -> Option<&JsonModule> {
    if let Module::Json(module) = &self {
      Some(module)
    } else {
      None
    }
  }

  pub fn js(&self) -> Option<&JsModule> {
    if let Module::Js(module) = &self {
      Some(module)
    } else {
      None
    }
  }

  pub fn npm(&self) -> Option<&NpmModule> {
    if let Module::Npm(module) = &self {
      Some(module)
    } else {
      None
    }
  }

  pub fn node(&self) -> Option<&BuiltInNodeModule> {
    if let Module::Node(module) = &self {
      Some(module)
    } else {
      None
    }
  }

  pub fn external(&self) -> Option<&ExternalModule> {
    if let Module::External(module) = &self {
      Some(module)
    } else {
      None
    }
  }

  pub fn source(&self) -> Option<&Arc<str>> {
    match self {
      crate::Module::Js(m) => Some(&m.source),
      crate::Module::Json(m) => Some(&m.source),
      crate::Module::Npm(_)
      | crate::Module::Node(_)
      | crate::Module::External(_) => None,
    }
  }
}

/// An npm package entrypoint.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NpmModule {
  pub specifier: ModuleSpecifier,
  #[serde(skip_serializing)]
  pub nv_reference: NpmPackageNvReference,
}

/// Represents a module which is not statically analyzed and is only available
/// at runtime. It is up to the implementor to ensure that the module is
/// loaded and available as a dependency. The module does not contain source
/// code and will have no dependencies.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExternalModule {
  pub specifier: ModuleSpecifier,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuiltInNodeModule {
  /// Specifier (ex. "node:fs")
  pub specifier: ModuleSpecifier,
  /// Module name (ex. "fs")
  pub module_name: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JsonModule {
  pub specifier: ModuleSpecifier,
  #[serde(flatten, skip_serializing_if = "Option::is_none")]
  pub maybe_cache_info: Option<CacheInfo>,
  #[serde(rename = "size", serialize_with = "serialize_source")]
  pub source: Arc<str>,
  // todo(#240): This will always be MediaType::Json, but it's currently
  // used in the --json output. It's redundant though.
  pub media_type: MediaType,
}

impl JsonModule {
  /// Return the size in bytes of the content of the JSON module.
  pub fn size(&self) -> usize {
    self.source.as_bytes().len()
  }
}

#[derive(Debug, Clone)]
pub enum FastCheckTypeModuleSlot {
  Module(Box<FastCheckTypeModule>),
  Error(Vec<FastCheckDiagnostic>),
}

#[derive(Debug, Clone)]
pub struct FastCheckTypeModule {
  pub dependencies: IndexMap<String, Dependency>,
  pub source: Arc<str>,
  pub source_map: Arc<[u8]>,
  #[cfg(feature = "fast_check")]
  pub dts: Option<FastCheckDtsModule>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JsModule {
  #[serde(
    skip_serializing_if = "IndexMap::is_empty",
    serialize_with = "serialize_dependencies"
  )]
  pub dependencies: IndexMap<String, Dependency>,
  #[serde(flatten, skip_serializing_if = "Option::is_none")]
  pub maybe_cache_info: Option<CacheInfo>,
  #[serde(rename = "size", serialize_with = "serialize_source")]
  pub source: Arc<str>,
  #[serde(rename = "typesDependency", skip_serializing_if = "Option::is_none")]
  pub maybe_types_dependency: Option<TypesDependency>,
  #[serde(skip_serializing_if = "is_media_type_unknown")]
  pub media_type: MediaType,
  pub specifier: ModuleSpecifier,
  #[serde(skip_serializing)]
  pub fast_check: Option<FastCheckTypeModuleSlot>,
}

impl JsModule {
  fn new(specifier: ModuleSpecifier, source: Arc<str>) -> Self {
    Self {
      dependencies: Default::default(),
      maybe_cache_info: None,
      source,
      maybe_types_dependency: None,
      media_type: MediaType::Unknown,
      specifier,
      fast_check: None,
    }
  }

  /// Return the size in bytes of the content of the module.
  pub fn size(&self) -> usize {
    self.source.as_bytes().len()
  }

  pub fn fast_check_diagnostics(&self) -> Option<&Vec<FastCheckDiagnostic>> {
    let module_slot = self.fast_check.as_ref()?;
    match module_slot {
      FastCheckTypeModuleSlot::Module(_) => None,
      FastCheckTypeModuleSlot::Error(d) => Some(d),
    }
  }

  pub fn fast_check_module(&self) -> Option<&FastCheckTypeModule> {
    let module_slot = self.fast_check.as_ref()?;
    match module_slot {
      FastCheckTypeModuleSlot::Module(m) => Some(m),
      FastCheckTypeModuleSlot::Error(_) => None,
    }
  }

  pub fn dependencies_prefer_fast_check(
    &self,
  ) -> &IndexMap<String, Dependency> {
    match self.fast_check_module() {
      Some(fast_check) => &fast_check.dependencies,
      None => &self.dependencies,
    }
  }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub(crate) enum ModuleSlot {
  /// A module, with source code.
  Module(Module),
  /// When trying to load or parse the module, an error occurred.
  Err(ModuleError),
  /// An internal state set when loading a module asynchronously.
  Pending,
}

impl ModuleSlot {
  #[cfg(test)]
  pub fn module(&self) -> Option<&Module> {
    if let ModuleSlot::Module(module) = self {
      Some(module)
    } else {
      None
    }
  }
}

type ModuleResult<'a> =
  (&'a ModuleSpecifier, Result<&'a Module, &'a ModuleError>);

/// Convert a module slot entry into a result which contains the resolved
/// module specifier, module kind, and media type or the module graph error.
fn to_result<'a>(
  (specifier, module_slot): (&'a ModuleSpecifier, &'a ModuleSlot),
) -> Option<ModuleResult<'a>> {
  match module_slot {
    ModuleSlot::Err(err) => Some((specifier, Err(err))),
    ModuleSlot::Module(module) => Some((specifier, Ok(module))),
    ModuleSlot::Pending => None,
  }
}

/// Provides a way for imports, through configuration, to be imported to the
/// module graph without requiring the dependencies to be analyzed. This is
/// intended to be used for importing type dependencies or other externally
/// defined dependencies, like JSX runtimes.
#[derive(Debug, Clone, Serialize)]
pub struct GraphImport {
  /// A map of resolved dependencies, where the key is the value originally
  /// provided for the import and the value is the resolved dependency.
  #[serde(serialize_with = "serialize_dependencies")]
  pub dependencies: IndexMap<String, Dependency>,
}

impl GraphImport {
  pub fn new(
    referrer: &ModuleSpecifier,
    imports: Vec<String>,
    maybe_resolver: Option<&dyn Resolver>,
    maybe_npm_resolver: Option<&dyn NpmResolver>,
  ) -> Self {
    let dependencies = imports
      .into_iter()
      .map(|import| {
        let referrer_range = Range {
          specifier: referrer.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        };
        let maybe_type = resolve(
          &import,
          referrer_range,
          ResolutionMode::Types,
          maybe_resolver,
          maybe_npm_resolver,
        );
        (
          import,
          Dependency {
            is_dynamic: false,
            maybe_code: Resolution::None,
            maybe_type,
            maybe_attribute_type: None,
            imports: vec![],
          },
        )
      })
      .collect();
    Self { dependencies }
  }
}

#[cfg(feature = "fast_check")]
#[derive(Debug, Default)]
pub enum WorkspaceFastCheckOption<'a> {
  #[default]
  Disabled,
  Enabled(&'a [WorkspaceMember]),
}

#[cfg(feature = "fast_check")]
#[derive(Default)]
pub struct BuildFastCheckTypeGraphOptions<'a> {
  pub fast_check_cache: Option<&'a dyn crate::fast_check::FastCheckCache>,
  pub fast_check_dts: bool,
  pub jsr_url_provider: Option<&'a dyn JsrUrlProvider>,
  pub module_parser: Option<&'a dyn ModuleParser>,
  pub resolver: Option<&'a dyn Resolver>,
  pub npm_resolver: Option<&'a dyn NpmResolver>,
  /// Whether to fill workspace members with fast check TypeScript data.
  pub workspace_fast_check: WorkspaceFastCheckOption<'a>,
}

#[derive(Default)]
pub struct BuildOptions<'a> {
  pub is_dynamic: bool,
  /// Additional imports that should be brought into the scope of
  /// the module graph to add to the graph's "imports". This may
  /// be extra modules such as TypeScript's "types" option or JSX
  /// runtime types.
  pub imports: Vec<ReferrerImports>,
  pub file_system: Option<&'a dyn FileSystem>,
  pub jsr_url_provider: Option<&'a dyn JsrUrlProvider>,
  pub resolver: Option<&'a dyn Resolver>,
  pub npm_resolver: Option<&'a dyn NpmResolver>,
  pub module_analyzer: Option<&'a dyn ModuleAnalyzer>,
  pub module_parser: Option<&'a dyn ModuleParser>,
  pub reporter: Option<&'a dyn Reporter>,
  pub workspace_members: &'a [WorkspaceMember],
  pub executor: &'a dyn Executor,
}

#[derive(Debug, Copy, Clone)]
pub enum ModuleEntryRef<'a> {
  Module(&'a Module),
  Err(&'a ModuleError),
  Redirect(&'a ModuleSpecifier),
}

#[derive(Debug, Clone)]
pub struct WalkOptions {
  pub follow_dynamic: bool,
  pub follow_type_only: bool,
  pub check_js: bool,
}

impl Default for WalkOptions {
  fn default() -> Self {
    Self {
      follow_dynamic: false,
      follow_type_only: true,
      check_js: true,
    }
  }
}

pub struct ModuleEntryIterator<'a> {
  graph: &'a ModuleGraph,
  seen: HashSet<&'a ModuleSpecifier>,
  visiting: VecDeque<&'a ModuleSpecifier>,
  follow_dynamic: bool,
  follow_type_only: bool,
  check_js: bool,
  previous_module: Option<ModuleEntryRef<'a>>,
}

impl<'a> ModuleEntryIterator<'a> {
  fn new(
    graph: &'a ModuleGraph,
    roots: &'a [ModuleSpecifier],
    options: WalkOptions,
  ) -> Self {
    let mut seen =
      HashSet::<&'a ModuleSpecifier>::with_capacity(graph.specifiers_count());
    let mut visiting = VecDeque::<&'a ModuleSpecifier>::new();
    for root in roots {
      seen.insert(root);
      visiting.push_back(root);
    }
    for (_, dep) in graph.imports.values().flat_map(|i| &i.dependencies) {
      let mut resolutions = Vec::with_capacity(2);
      resolutions.push(&dep.maybe_code);
      if options.follow_type_only {
        resolutions.push(&dep.maybe_type);
      }
      #[allow(clippy::manual_flatten)]
      for resolution in resolutions {
        if let Resolution::Ok(resolved) = resolution {
          let specifier = &resolved.specifier;
          if seen.insert(specifier) {
            visiting.push_front(specifier);
          }
        }
      }
    }

    Self {
      graph,
      seen,
      visiting,
      follow_dynamic: options.follow_dynamic,
      follow_type_only: options.follow_type_only,
      check_js: options.check_js,
      previous_module: None,
    }
  }

  /// Skips analyzing the dependencies of the previously returned module.
  pub fn skip_previous_dependencies(&mut self) {
    self.previous_module = None;
  }

  /// An iterator over all the errors found when walking this iterator.
  ///
  /// This can be useful in scenarios where you want to filter or ignore an error.
  pub fn errors(self) -> ModuleGraphErrorIterator<'a> {
    ModuleGraphErrorIterator::new(self)
  }

  /// Consumes the iterator validating all the items for any resolution
  /// or module graph errors.
  ///
  /// This is different than calling `.valid()` on a module graph because
  /// it only applies to the roots filtered by the iterator with the provided
  /// options.
  #[allow(clippy::result_large_err)]
  pub fn validate(self) -> Result<(), ModuleGraphError> {
    if let Some(err) = self.errors().next() {
      Err(err)
    } else {
      Ok(())
    }
  }
}

impl<'a> Iterator for ModuleEntryIterator<'a> {
  type Item = (&'a ModuleSpecifier, ModuleEntryRef<'a>);

  fn next(&mut self) -> Option<Self::Item> {
    match self.previous_module.take() {
      Some(ModuleEntryRef::Module(module)) => match module {
        Module::Js(module) => {
          let check_types = (self.check_js
            || !matches!(
              module.media_type,
              MediaType::JavaScript
                | MediaType::Mjs
                | MediaType::Cjs
                | MediaType::Jsx
            ))
            && self.follow_type_only;
          if check_types {
            if let Some(Resolution::Ok(resolved)) = module
              .maybe_types_dependency
              .as_ref()
              .map(|d| &d.dependency)
            {
              let specifier = &resolved.specifier;
              if self.seen.insert(specifier) {
                self.visiting.push_front(specifier);
              }
            }
          }
          let module_deps = if check_types {
            module.dependencies_prefer_fast_check()
          } else {
            &module.dependencies
          };
          for dep in module_deps.values().rev() {
            if !dep.is_dynamic || self.follow_dynamic {
              let mut resolutions = Vec::with_capacity(2);
              resolutions.push(&dep.maybe_code);
              if check_types {
                resolutions.push(&dep.maybe_type);
              }
              #[allow(clippy::manual_flatten)]
              for resolution in resolutions {
                if let Resolution::Ok(resolved) = resolution {
                  let specifier = &resolved.specifier;
                  if self.seen.insert(specifier) {
                    self.visiting.push_front(specifier);
                  }
                }
              }
            }
          }
        }
        Module::Json(_)
        | Module::External(_)
        | Module::Npm(_)
        | Module::Node(_) => {}
      },
      Some(ModuleEntryRef::Redirect(specifier)) => {
        if self.seen.insert(specifier) {
          self.visiting.push_front(specifier);
        }
      }
      Some(ModuleEntryRef::Err(_)) | None => {}
    }

    let (specifier, module_entry) = loop {
      let specifier = self.visiting.pop_front()?;
      match self.graph.module_slots.get_key_value(specifier) {
        Some((specifier, module_slot)) => {
          match module_slot {
            ModuleSlot::Pending => {
              // ignore
            }
            ModuleSlot::Module(module) => {
              break (specifier, ModuleEntryRef::Module(module))
            }
            ModuleSlot::Err(err) => {
              break (specifier, ModuleEntryRef::Err(err))
            }
          }
        }
        None => {
          if let Some((specifier, to)) =
            self.graph.redirects.get_key_value(specifier)
          {
            break (specifier, ModuleEntryRef::Redirect(to));
          }
        }
      }
    };

    self.previous_module = Some(module_entry);

    Some((specifier, module_entry))
  }
}

pub struct ModuleGraphErrorIterator<'a> {
  iterator: ModuleEntryIterator<'a>,
  next_errors: Vec<ModuleGraphError>,
}

impl<'a> ModuleGraphErrorIterator<'a> {
  pub fn new(iterator: ModuleEntryIterator<'a>) -> Self {
    Self {
      iterator,
      next_errors: Default::default(),
    }
  }

  fn check_resolution(
    &self,
    module: &JsModule,
    mode: ResolutionMode,
    specifier_text: &str,
    resolution: &Resolution,
    is_dynamic: bool,
  ) -> Option<ModuleGraphError> {
    match resolution {
      Resolution::Ok(resolved) => {
        let referrer_scheme = module.specifier.scheme();
        let specifier_scheme = resolved.specifier.scheme();
        if referrer_scheme == "https" && specifier_scheme == "http" {
          Some(ModuleGraphError::for_resolution_mode(
            mode,
            ResolutionError::InvalidDowngrade {
              specifier: resolved.specifier.clone(),
              range: resolved.range.clone(),
            },
          ))
        } else if matches!(referrer_scheme, "https" | "http")
          && matches!(specifier_scheme, "file")
          && specifier_text.to_lowercase().starts_with("file://")
        {
          Some(ModuleGraphError::for_resolution_mode(
            mode,
            ResolutionError::InvalidLocalImport {
              specifier: resolved.specifier.clone(),
              range: resolved.range.clone(),
            },
          ))
        } else if self.iterator.follow_dynamic {
          let resolved_specifier =
            self.iterator.graph.resolve(&resolved.specifier);
          let module_slot =
            self.iterator.graph.module_slots.get(&resolved_specifier);
          if let Some(ModuleSlot::Err(ModuleError::Missing(
            specifier,
            maybe_range,
          ))) = module_slot
          {
            // we want to surface module missing errors as dynamic missing errors
            if is_dynamic {
              Some(ModuleGraphError::ModuleError(ModuleError::MissingDynamic(
                specifier.clone(),
                resolved.range.clone(),
              )))
            } else {
              Some(ModuleGraphError::ModuleError(ModuleError::Missing(
                specifier.clone(),
                maybe_range.clone(),
              )))
            }
          } else {
            None
          }
        } else {
          None
        }
      }
      Resolution::Err(err) => {
        Some(ModuleGraphError::for_resolution_mode(mode, *err.clone()))
      }
      Resolution::None => None,
    }
  }
}

impl<'a> Iterator for ModuleGraphErrorIterator<'a> {
  type Item = ModuleGraphError;

  fn next(&mut self) -> Option<Self::Item> {
    while self.next_errors.is_empty() {
      let follow_type_only = self.iterator.follow_type_only;
      let check_js = self.iterator.check_js;
      let follow_dynamic = self.iterator.follow_dynamic;

      if let Some((_, module_entry)) = self.iterator.next() {
        match module_entry {
          ModuleEntryRef::Module(Module::Js(module)) => {
            let check_types = (check_js
              || !matches!(
                module.media_type,
                MediaType::JavaScript
                  | MediaType::Mjs
                  | MediaType::Cjs
                  | MediaType::Jsx
              ))
              && follow_type_only;
            if check_types {
              if let Some(dep) = module.maybe_types_dependency.as_ref() {
                if let Some(err) = self.check_resolution(
                  module,
                  ResolutionMode::Types,
                  &dep.specifier,
                  &dep.dependency,
                  false,
                ) {
                  self.next_errors.push(err);
                }
              }
            }
            let module_deps = if follow_type_only {
              module.dependencies_prefer_fast_check()
            } else {
              &module.dependencies
            };
            for (specifier_text, dep) in module_deps {
              if follow_dynamic || !dep.is_dynamic {
                if let Some(err) = self.check_resolution(
                  module,
                  ResolutionMode::Execution,
                  specifier_text,
                  &dep.maybe_code,
                  dep.is_dynamic,
                ) {
                  self.next_errors.push(err);
                }
                if check_types {
                  if let Some(err) = self.check_resolution(
                    module,
                    ResolutionMode::Types,
                    specifier_text,
                    &dep.maybe_type,
                    dep.is_dynamic,
                  ) {
                    self.next_errors.push(err);
                  }
                }
              }
            }
          }
          ModuleEntryRef::Err(error) => {
            // ignore missing modules when following dynamic imports
            // because they will be resolved in place
            let should_ignore =
              follow_dynamic && matches!(error, ModuleError::Missing { .. });
            if !should_ignore {
              self
                .next_errors
                .push(ModuleGraphError::ModuleError(error.clone()));
            }
          }
          _ => {}
        }
      } else {
        break; // no more modules, stop searching
      }
    }

    self.next_errors.pop()
  }
}

/// The structure which represents a module graph, which can be serialized as
/// well as "printed". The roots of the graph represent the "starting" point
/// which can be located in the module "slots" in the graph. The graph also
/// contains any redirects where the requested module specifier was redirected
/// to another module specifier when being loaded.
#[derive(Debug, Clone, Serialize)]
pub struct ModuleGraph {
  #[serde(skip_serializing)]
  graph_kind: GraphKind,
  pub roots: Vec<ModuleSpecifier>,
  #[serde(rename = "modules")]
  #[serde(serialize_with = "serialize_module_slots")]
  pub(crate) module_slots: BTreeMap<ModuleSpecifier, ModuleSlot>,
  #[serde(skip_serializing_if = "IndexMap::is_empty")]
  #[serde(serialize_with = "serialize_graph_imports")]
  pub imports: IndexMap<ModuleSpecifier, GraphImport>,
  pub redirects: BTreeMap<ModuleSpecifier, ModuleSpecifier>,
  #[serde(skip_serializing)]
  pub npm_packages: Vec<PackageNv>,
  #[serde(skip_serializing)]
  pub has_node_specifier: bool,
  #[serde(rename = "packages")]
  #[serde(skip_serializing_if = "PackageSpecifiers::is_empty")]
  pub packages: PackageSpecifiers,
}

impl ModuleGraph {
  pub fn new(graph_kind: GraphKind) -> Self {
    Self {
      graph_kind,
      roots: Default::default(),
      module_slots: Default::default(),
      imports: Default::default(),
      redirects: Default::default(),
      npm_packages: Default::default(),
      has_node_specifier: false,
      packages: Default::default(),
    }
  }

  pub fn graph_kind(&self) -> GraphKind {
    self.graph_kind
  }

  pub async fn build<'a>(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    loader: &mut dyn Loader,
    options: BuildOptions<'a>,
  ) -> Vec<BuildDiagnostic> {
    let default_jsr_url_provider = DefaultJsrUrlProvider;
    let default_module_parser = CapturingModuleAnalyzer::default();
    #[cfg(not(target_arch = "wasm32"))]
    let file_system = RealFileSystem;
    #[cfg(target_arch = "wasm32")]
    let file_system = NullFileSystem;
    Builder::build(
      self,
      roots,
      options.imports,
      options.is_dynamic,
      options.file_system.unwrap_or(&file_system),
      options
        .jsr_url_provider
        .unwrap_or(&default_jsr_url_provider),
      options.resolver,
      options.npm_resolver,
      loader,
      options.module_analyzer.unwrap_or(&default_module_parser),
      options.reporter,
      options.workspace_members,
      options.executor,
    )
    .await
  }

  #[cfg(feature = "fast_check")]
  pub fn build_fast_check_type_graph(
    &mut self,
    options: BuildFastCheckTypeGraphOptions,
  ) {
    if !self.graph_kind().include_types() {
      return;
    }

    let mut pending_nvs = self
      .packages
      .top_level_packages()
      .iter()
      .cloned()
      .collect::<VecDeque<_>>();
    if let WorkspaceFastCheckOption::Enabled(workspace_members) =
      options.workspace_fast_check
    {
      pending_nvs.extend(workspace_members.iter().map(|n| n.nv.clone()));
    }
    if pending_nvs.is_empty() {
      return;
    }

    let default_module_parser = CapturingModuleAnalyzer::default();
    let root_symbol = crate::symbols::RootSymbol::new(
      self,
      options.module_parser.unwrap_or(&default_module_parser),
    );

    let default_jsr_url_provider = DefaultJsrUrlProvider;
    let modules = crate::fast_check::build_fast_check_type_graph(
      options.fast_check_cache,
      options
        .jsr_url_provider
        .unwrap_or(&default_jsr_url_provider),
      self,
      &root_symbol,
      pending_nvs,
      &crate::fast_check::TransformOptions {
        workspace_members: match options.workspace_fast_check {
          WorkspaceFastCheckOption::Disabled => &[],
          WorkspaceFastCheckOption::Enabled(members) => members,
        },
        should_error_on_first_diagnostic: match options.workspace_fast_check {
          WorkspaceFastCheckOption::Disabled => true,
          WorkspaceFastCheckOption::Enabled(_) => false,
        },
        dts: options.fast_check_dts,
      },
    );
    for (specifier, fast_check_module_result) in modules {
      let module_slot = self.module_slots.get_mut(&specifier).unwrap();
      let module = match module_slot {
        ModuleSlot::Module(m) => match m {
          Module::Js(m) => m,
          _ => continue,
        },
        ModuleSlot::Err(_) | ModuleSlot::Pending => continue,
      };
      module.fast_check = Some(match fast_check_module_result {
        Ok(fast_check_module) => {
          let mut dependencies: IndexMap<String, Dependency> =
            Default::default();
          fill_module_dependencies(
            GraphKind::TypesOnly,
            match Arc::try_unwrap(fast_check_module.module_info) {
              Ok(module_info) => module_info.dependencies,
              Err(module_info) => module_info.dependencies.clone(),
            },
            &module.specifier,
            &mut dependencies,
            // no need to resolve dynamic imports
            &NullFileSystem,
            options.resolver,
            options.npm_resolver,
          );
          FastCheckTypeModuleSlot::Module(Box::new(FastCheckTypeModule {
            dependencies,
            source: fast_check_module.text,
            source_map: fast_check_module.source_map,
            dts: fast_check_module.dts,
          }))
        }
        Err(diagnostic) => FastCheckTypeModuleSlot::Error(diagnostic),
      });
    }
  }

  /// Creates a new cloned module graph from the provided roots.
  pub fn segment(&self, roots: &[ModuleSpecifier]) -> Self {
    if roots == self.roots {
      // perf - do a straight clone since the roots are the same
      return self.clone();
    }

    let mut new_graph = ModuleGraph::new(self.graph_kind);
    let entries = self.walk(
      roots,
      WalkOptions {
        follow_dynamic: true,
        follow_type_only: true,
        check_js: true,
      },
    );

    for (specifier, module_entry) in entries {
      match module_entry {
        ModuleEntryRef::Module(module) => {
          new_graph
            .module_slots
            .insert(specifier.clone(), ModuleSlot::Module(module.clone()));
        }
        ModuleEntryRef::Err(err) => {
          new_graph
            .module_slots
            .insert(specifier.clone(), ModuleSlot::Err(err.clone()));
        }
        ModuleEntryRef::Redirect(specifier_to) => {
          new_graph
            .redirects
            .insert(specifier.clone(), specifier_to.clone());
        }
      }
    }
    new_graph.imports = self.imports.clone();
    new_graph.roots = roots.iter().map(|r| (*r).to_owned()).collect();
    new_graph.npm_packages = self.npm_packages.clone();
    // todo(dsherret): it should be a bit smarter about this, but this is not terrible
    new_graph.packages = self.packages.clone();
    new_graph.has_node_specifier = self.has_node_specifier;

    new_graph
  }

  /// Iterates over all the module entries in the module graph searching from the provided roots.
  pub fn walk<'a>(
    &'a self,
    roots: &'a [ModuleSpecifier],
    options: WalkOptions,
  ) -> ModuleEntryIterator<'a> {
    ModuleEntryIterator::new(self, roots, options)
  }

  /// Returns `true` if the specifier resolves to a module within a graph,
  /// otherwise returns `false`.
  pub fn contains(&self, specifier: &ModuleSpecifier) -> bool {
    let specifier = self.resolve(specifier);
    self
      .module_slots
      .get(&specifier)
      .map_or(false, |ms| matches!(ms, ModuleSlot::Module(_)))
  }

  /// Returns any module errors found in the graph.
  ///
  /// NOTE: This does not return any resolution errors.
  pub fn module_errors(&self) -> impl Iterator<Item = &ModuleError> {
    self.module_slots.values().filter_map(|ms| match ms {
      ModuleSlot::Err(err) => Some(err),
      ModuleSlot::Module(_) | ModuleSlot::Pending => None,
    })
  }

  /// Get a module from the module graph, returning `None` if the module is not
  /// part of the graph, or if when loading the module it errored. If any module
  /// resolution error is needed, then use the `try_get()` method which will
  /// return any resolution error as the error in the result.
  pub fn get(&self, specifier: &ModuleSpecifier) -> Option<&Module> {
    let specifier = self.resolve(specifier);
    match self.module_slots.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Some(module),
      _ => None,
    }
  }

  /// Return a vector of references to module objects in the graph. Only modules
  /// that were fully resolved are present, as "errors" are omitted. If
  /// you need to know what errors are in the graph, walk the graph via `.walk`
  /// or if you just need to check if everything is "ok" with the graph, use the
  /// `.valid()` method.
  pub fn modules(&self) -> impl Iterator<Item = &Module> {
    self.module_slots.values().filter_map(|ms| match ms {
      ModuleSlot::Module(m) => Some(m),
      _ => None,
    })
  }

  /// Resolve a specifier from the module graph following any possible redirects
  /// returning the "final" module.
  pub fn resolve(&self, specifier: &ModuleSpecifier) -> ModuleSpecifier {
    let mut redirected_specifier = specifier;
    let max_redirects = 10;
    let mut seen = HashSet::with_capacity(max_redirects);
    seen.insert(redirected_specifier);
    while let Some(specifier) = self.redirects.get(redirected_specifier) {
      if !seen.insert(specifier) {
        log::warn!("An infinite loop of redirections detected.\n  Original specifier: {specifier}");
        break;
      }
      redirected_specifier = specifier;
      if seen.len() >= max_redirects {
        log::warn!("An excessive number of redirections detected.\n  Original specifier: {specifier}");
        break;
      }
    }
    redirected_specifier.clone()
  }

  /// Resolve a dependency of a referring module providing the string specifier
  /// of the dependency and returning an optional fully qualified module
  /// specifier.
  ///
  /// The `prefer_types` flags indicates if a type dependency is preferred over
  /// a code dependency. If `true`, a type dependency will be returned in favor
  /// of a code dependency. If `false` a code dependency will be returned in
  /// favor of a type dependency. The value should be set to `true` when
  /// resolving specifiers for type checking, or otherwise `false`.
  pub fn resolve_dependency(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
    prefer_types: bool,
  ) -> Option<ModuleSpecifier> {
    let referrer = self.resolve(referrer);
    if let Some(ModuleSlot::Module(referring_module)) =
      self.module_slots.get(&referrer)
    {
      self.resolve_dependency_from_module(
        specifier,
        referring_module,
        prefer_types,
      )
    } else if let Some(graph_import) = self.imports.get(&referrer) {
      let dependency = graph_import.dependencies.get(specifier)?;
      self.resolve_dependency_from_dep(dependency, prefer_types)
    } else {
      None
    }
  }

  pub fn resolve_dependency_from_module(
    &self,
    specifier: &str,
    referring_module: &Module,
    prefer_types: bool,
  ) -> Option<ModuleSpecifier> {
    match referring_module {
      Module::Js(referring_module) => {
        let dependency = referring_module.dependencies.get(specifier)?;
        self.resolve_dependency_from_dep(dependency, prefer_types)
      }
      Module::Json(_)
      | Module::Npm(_)
      | Module::Node(_)
      | Module::External(_) => None,
    }
  }

  pub fn resolve_dependency_from_dep(
    &self,
    dependency: &Dependency,
    prefer_types: bool,
  ) -> Option<ModuleSpecifier> {
    let (maybe_first, maybe_second) = if prefer_types {
      (&dependency.maybe_type, &dependency.maybe_code)
    } else {
      (&dependency.maybe_code, &dependency.maybe_type)
    };
    let unresolved_specifier = maybe_first
      .maybe_specifier()
      .or_else(|| maybe_second.maybe_specifier())?;
    let resolved_specifier = self.resolve(unresolved_specifier);
    // Even if we resolved the specifier, it doesn't mean the module is actually
    // there, so check in the module slots
    match self.module_slots.get(&resolved_specifier) {
      Some(ModuleSlot::Module(Module::Js(module))) if prefer_types => {
        // check for if this module has a types dependency
        if let Some(Resolution::Ok(resolved)) = module
          .maybe_types_dependency
          .as_ref()
          .map(|d| &d.dependency)
        {
          let resolved_specifier = self.resolve(&resolved.specifier);
          if matches!(
            self.module_slots.get(&resolved_specifier),
            Some(ModuleSlot::Module(_))
          ) {
            return Some(resolved_specifier);
          }
        }
        Some(resolved_specifier)
      }
      Some(ModuleSlot::Module(_)) => Some(resolved_specifier),
      _ => None,
    }
  }

  /// Return the entries of the specifiers in the graph, where the first value
  /// is a module specifier and the second value is a result that contains a tuple of
  /// the module specifier, module kind, and media type, or the module graph
  /// error.
  pub fn specifiers(
    &self,
  ) -> impl Iterator<Item = (&ModuleSpecifier, Result<&Module, &ModuleError>)>
  {
    self.module_slots.iter().filter_map(to_result).chain(
      self.redirects.iter().filter_map(|(specifier, found)| {
        let module_slot = self.module_slots.get(found)?;
        to_result((specifier, module_slot))
      }),
    )
  }

  /// Retrieve a module from the module graph. If the module identified as a
  /// dependency of the graph, but resolving or loading that module resulted in
  /// an error, the error will be returned as the `Err` of the result. If the
  /// module is not part of the graph, or the module is missing from the graph,
  /// the result will be `Ok` with the option of the module.
  pub fn try_get(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<&Module>, &ModuleError> {
    let specifier = self.resolve(specifier);
    match self.module_slots.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Ok(Some(module)),
      Some(ModuleSlot::Err(err)) => Err(err),
      _ => Ok(None),
    }
  }

  /// Similar to `try_get`, but will prefer resolving to the types dependency if
  /// the module has one.
  pub fn try_get_prefer_types(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<&Module>, &ModuleError> {
    let Some(module) = self.try_get(specifier)? else {
      return Ok(None);
    };

    if let Some(specifier) = module.js().and_then(|m| {
      m.maybe_types_dependency
        .as_ref()
        .and_then(|d| d.dependency.ok())
        .map(|r| &r.specifier)
    }) {
      self.try_get(specifier)
    } else {
      Ok(Some(module))
    }
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-type only, non-dynamic imports. The first error is
  /// returned as as error result, otherwise ok if there are no errors.
  #[allow(clippy::result_large_err)]
  pub fn valid(&self) -> Result<(), ModuleGraphError> {
    self
      .walk(
        &self.roots,
        WalkOptions {
          check_js: true,
          follow_type_only: false,
          follow_dynamic: false,
        },
      )
      .validate()
  }

  /// Gets the approximate number of specifiers in the graph.
  ///
  /// This is useful for pre-allocating actions that will take
  /// place on the graph.
  pub fn specifiers_count(&self) -> usize {
    self.module_slots.len() + self.redirects.len() + self.imports.len()
  }
}

/// Resolve a string specifier from a referring module, using the resolver if
/// present, returning the resolution result.
fn resolve(
  specifier_text: &str,
  referrer_range: Range,
  mode: ResolutionMode,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) -> Resolution {
  let response = if let Some(resolver) = maybe_resolver {
    resolver.resolve(specifier_text, &referrer_range, mode)
  } else {
    resolve_import(specifier_text, &referrer_range.specifier)
      .map_err(|err| err.into())
  };
  if let Some(npm_resolver) = maybe_npm_resolver {
    if npm_resolver.enables_bare_builtin_node_module() {
      use import_map::ImportMapError;
      use ResolveError::*;
      use SpecifierError::*;
      let res_ref = response.as_ref();
      if matches!(res_ref, Err(Specifier(ImportPrefixMissing(_, _))))
        || matches!(res_ref, Err(Other(e)) if matches!(e.downcast_ref::<ImportMapError>(), Some(&ImportMapError::UnmappedBareSpecifier(_, _))))
      {
        if let Ok(specifier) =
          ModuleSpecifier::parse(&format!("node:{}", specifier_text))
        {
          if npm_resolver.resolve_builtin_node_module(&specifier).is_ok() {
            npm_resolver.on_resolve_bare_builtin_node_module(
              specifier_text,
              &referrer_range,
            );
            return Resolution::from_resolve_result(
              Ok(specifier),
              specifier_text,
              referrer_range,
            );
          }
        }
      }
    }
  }
  Resolution::from_resolve_result(response, specifier_text, referrer_range)
}

fn serialize_module_slots<S>(
  module_slots: &BTreeMap<ModuleSpecifier, ModuleSlot>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  let mut seq = serializer.serialize_seq(Some(module_slots.len()))?;
  for (specifier, slot) in module_slots.iter() {
    match slot {
      ModuleSlot::Module(module) => seq.serialize_element(module)?,
      ModuleSlot::Err(err) => seq.serialize_element(&serde_json::json!({
        "specifier": specifier,
        "error": err.to_string(),
      }))?,
      ModuleSlot::Pending => seq.serialize_element(&serde_json::json!({
        "specifier": specifier,
        "error": "[INTERNAL ERROR] A pending module load never completed.",
      }))?,
    };
  }
  seq.end()
}

fn serialize_graph_imports<S>(
  graph_imports: &IndexMap<ModuleSpecifier, GraphImport>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  #[derive(Serialize)]
  struct GraphImportWithReferrer<'a> {
    referrer: &'a ModuleSpecifier,
    #[serde(flatten)]
    graph_import: &'a GraphImport,
  }
  let mut seq = serializer.serialize_seq(Some(graph_imports.len()))?;
  for (referrer, graph_import) in graph_imports {
    seq.serialize_element(&GraphImportWithReferrer {
      referrer,
      graph_import,
    })?
  }
  seq.end()
}

/// With the provided information, parse a module and return its "module slot"
#[allow(clippy::too_many_arguments)]
#[allow(clippy::result_large_err)]
pub(crate) fn parse_module(
  graph_kind: GraphKind,
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: Arc<[u8]>,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_referrer: Option<Range>,
  file_system: &dyn FileSystem,
  maybe_resolver: Option<&dyn Resolver>,
  module_analyzer: &dyn ModuleAnalyzer,
  is_root: bool,
  is_dynamic_branch: bool,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) -> Result<Module, ModuleError> {
  let (media_type, maybe_charset) =
    resolve_media_type_and_charset_from_headers(specifier, maybe_headers);

  // here we check any media types that should have assertions made against them
  // if they aren't the root and add them to the graph, otherwise we continue
  if media_type == MediaType::Json
    && (is_root
      || is_dynamic_branch
      || matches!(
        maybe_attribute_type.as_ref().map(|t| t.kind.as_str()),
        Some("json")
      ))
  {
    let text = crate::source::decode_source(specifier, content, maybe_charset)
      .map_err(|err| {
        ModuleError::LoadingErr(specifier.clone(), None, Arc::new(err.into()))
      })?;
    return Ok(Module::Json(JsonModule {
      maybe_cache_info: None,
      source: text,
      media_type: MediaType::Json,
      specifier: specifier.clone(),
    }));
  }

  if let Some(attribute_type) = maybe_attribute_type {
    if attribute_type.kind == "json" {
      return Err(ModuleError::InvalidTypeAssertion {
        specifier: specifier.clone(),
        range: attribute_type.range,
        actual_media_type: media_type,
        expected_media_type: MediaType::Json,
      });
    } else {
      return Err(ModuleError::UnsupportedImportAttributeType {
        specifier: specifier.clone(),
        range: attribute_type.range,
        kind: attribute_type.kind,
      });
    }
  }

  // Here we check for known ES Modules that we will analyze the dependencies of
  match media_type {
    MediaType::JavaScript
    | MediaType::Mjs
    | MediaType::Cjs
    | MediaType::Jsx
    | MediaType::TypeScript
    | MediaType::Mts
    | MediaType::Cts
    | MediaType::Tsx
    | MediaType::Dts
    | MediaType::Dmts
    | MediaType::Dcts => {
      let source = new_source_with_text(specifier, content, maybe_charset)
        .map_err(|err| *err)?;
      match module_analyzer.analyze(specifier, source.clone(), media_type) {
        Ok(module_info) => {
          // Return the module as a valid module
          Ok(Module::Js(parse_js_module_from_module_info(
            graph_kind,
            specifier,
            media_type,
            maybe_headers,
            module_info,
            source,
            file_system,
            maybe_resolver,
            maybe_npm_resolver,
          )))
        }
        Err(diagnostic) => {
          Err(ModuleError::ParseErr(specifier.clone(), diagnostic))
        }
      }
    }
    MediaType::Unknown if is_root => {
      let source = new_source_with_text(specifier, content, maybe_charset)
        .map_err(|err| *err)?;
      match module_analyzer.analyze(
        specifier,
        source.clone(),
        MediaType::JavaScript,
      ) {
        Ok(module_info) => {
          // Return the module as a valid module
          Ok(Module::Js(parse_js_module_from_module_info(
            graph_kind,
            specifier,
            media_type,
            maybe_headers,
            module_info,
            source,
            file_system,
            maybe_resolver,
            maybe_npm_resolver,
          )))
        }
        Err(diagnostic) => {
          Err(ModuleError::ParseErr(specifier.clone(), diagnostic))
        }
      }
    }
    MediaType::Json
    | MediaType::Wasm
    | MediaType::TsBuildInfo
    | MediaType::SourceMap
    | MediaType::Unknown => Err(ModuleError::UnsupportedMediaType(
      specifier.clone(),
      media_type,
      maybe_referrer,
    )),
  }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn parse_js_module_from_module_info(
  graph_kind: GraphKind,
  specifier: &ModuleSpecifier,
  media_type: MediaType,
  maybe_headers: Option<&HashMap<String, String>>,
  module_info: ModuleInfo,
  source: Arc<str>,
  file_system: &dyn FileSystem,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) -> JsModule {
  let mut module = JsModule::new(specifier.clone(), source);
  module.media_type = media_type;

  // Analyze the TypeScript triple-slash references
  if graph_kind.include_types() {
    for reference in module_info.ts_references {
      match reference {
        TypeScriptReference::Path(specifier) => {
          let dep = module
            .dependencies
            .entry(specifier.text.clone())
            .or_default();
          let range = Range::from_position_range(
            module.specifier.clone(),
            specifier.range,
          );
          if dep.maybe_type.is_none() {
            dep.maybe_type = resolve(
              &specifier.text,
              range.clone(),
              ResolutionMode::Types,
              maybe_resolver,
              maybe_npm_resolver,
            );
          }
          dep.imports.push(Import {
            specifier: specifier.text,
            kind: ImportKind::TsReferencePath,
            range,
            is_dynamic: false,
            attributes: Default::default(),
          });
        }
        TypeScriptReference::Types(specifier) => {
          let range = Range::from_position_range(
            module.specifier.clone(),
            specifier.range,
          );
          let dep_resolution = resolve(
            &specifier.text,
            range.clone(),
            ResolutionMode::Types,
            maybe_resolver,
            maybe_npm_resolver,
          );
          if is_untyped(&module.media_type) {
            module.maybe_types_dependency = Some(TypesDependency {
              specifier: specifier.text.clone(),
              dependency: dep_resolution,
            });
          } else {
            let dep = module
              .dependencies
              .entry(specifier.text.clone())
              .or_default();
            if dep.maybe_type.is_none() {
              dep.maybe_type = dep_resolution;
            }
            dep.imports.push(Import {
              specifier: specifier.text,
              kind: ImportKind::TsReferenceTypes,
              range,
              is_dynamic: false,
              attributes: Default::default(),
            });
          }
        }
      }
    }
  }

  // Inject the JSX import source dependency if needed. This is done as follows:
  // 1. Check that the module is a JSX or TSX module.
  // 2. If the module has a @jsxImportSource pragma, use that as the import
  //    source.
  // 3. If the resolver has a default JSX import source, use that as the import
  //    source.
  // 4. If none of the above are true, do not inject a dependency.
  if matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    let res = module_info.jsx_import_source.or_else(|| {
      maybe_resolver.and_then(|r| {
        r.default_jsx_import_source()
          .map(|import_source| SpecifierWithRange {
            text: import_source,
            range: PositionRange {
              start: Position::zeroed(),
              end: Position::zeroed(),
            },
          })
      })
    });
    if let Some(import_source) = res {
      let jsx_import_source_module = maybe_resolver
        .map(|r| r.jsx_import_source_module())
        .unwrap_or(DEFAULT_JSX_IMPORT_SOURCE_MODULE);
      let specifier_text =
        format!("{}/{}", import_source.text, jsx_import_source_module);
      let dep = module
        .dependencies
        .entry(specifier_text.clone())
        .or_default();
      let range = Range::from_position_range(
        module.specifier.clone(),
        import_source.range,
      );
      if dep.maybe_code.is_none() {
        dep.maybe_code = resolve(
          &specifier_text,
          range.clone(),
          ResolutionMode::Execution,
          maybe_resolver,
          maybe_npm_resolver,
        );
      }
      dep.imports.push(Import {
        specifier: specifier_text,
        kind: ImportKind::JsxImportSource,
        range,
        is_dynamic: false,
        attributes: Default::default(),
      });
    }
  }

  // Analyze any JSDoc type imports
  if graph_kind.include_types() {
    for specifier in module_info.jsdoc_imports {
      let dep = module
        .dependencies
        .entry(specifier.text.clone())
        .or_default();
      let range =
        Range::from_position_range(module.specifier.clone(), specifier.range);
      if dep.maybe_type.is_none() {
        dep.maybe_type = resolve(
          &specifier.text,
          range.clone(),
          ResolutionMode::Types,
          maybe_resolver,
          maybe_npm_resolver,
        );
      }
      dep.imports.push(Import {
        specifier: specifier.text,
        kind: ImportKind::JsDoc,
        range,
        is_dynamic: false,
        attributes: Default::default(),
      });
    }
  }

  // Analyze the X-TypeScript-Types header
  if graph_kind.include_types() && module.maybe_types_dependency.is_none() {
    if let Some(headers) = maybe_headers {
      if let Some(types_header) = headers.get("x-typescript-types") {
        let range = Range {
          specifier: module.specifier.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        };
        module.maybe_types_dependency = Some(TypesDependency {
          specifier: types_header.to_string(),
          dependency: resolve(
            types_header,
            range,
            ResolutionMode::Types,
            maybe_resolver,
            maybe_npm_resolver,
          ),
        });
      }
    }
  }

  // Use resolve_types from maybe_resolver
  if let Some(resolver) = maybe_resolver {
    // this will only get called if there is no other types dependency and
    // the media type is untyped.
    if graph_kind.include_types()
      && module.maybe_types_dependency.is_none()
      && is_untyped(&module.media_type)
    {
      module.maybe_types_dependency =
        match resolver.resolve_types(&module.specifier) {
          Ok(Some((specifier, maybe_range))) => {
            let specifier_text = module.specifier.to_string();
            Some(TypesDependency {
              specifier: specifier_text,
              dependency: Resolution::Ok(Box::new(ResolutionResolved {
                specifier: specifier.clone(),
                range: maybe_range.unwrap_or_else(|| Range {
                  specifier,
                  start: Position::zeroed(),
                  end: Position::zeroed(),
                }),
              })),
            })
          }
          Ok(None) => None,
          Err(err) => Some(TypesDependency {
            specifier: module.specifier.to_string(),
            dependency: Resolution::Err(Box::new(
              ResolutionError::ResolverError {
                error: Arc::new(err),
                specifier: module.specifier.to_string(),
                range: Range {
                  specifier: module.specifier.clone(),
                  start: Position::zeroed(),
                  end: Position::zeroed(),
                },
              },
            )),
          }),
        };
    }
  }

  // Analyze ES dependencies
  fill_module_dependencies(
    graph_kind,
    module_info.dependencies,
    &module.specifier,
    &mut module.dependencies,
    file_system,
    maybe_resolver,
    maybe_npm_resolver,
  );

  // Return the module as a valid module
  module
}

fn fill_module_dependencies(
  graph_kind: GraphKind,
  dependencies: Vec<DependencyDescriptor>,
  module_specifier: &ModuleSpecifier,
  module_dependencies: &mut IndexMap<String, Dependency>,
  file_system: &dyn FileSystem,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) {
  for desc in dependencies {
    let (imports, leading_comments) = match desc {
      DependencyDescriptor::Static(desc) => {
        let is_import_or_export_type = matches!(
          desc.kind,
          DependencyKind::ImportType | DependencyKind::ExportType
        );
        if is_import_or_export_type && !graph_kind.include_types() {
          continue; // skip
        }
        let range = Range::from_position_range(
          module_specifier.clone(),
          desc.specifier_range.clone(),
        );

        (
          vec![Import {
            specifier: desc.specifier,
            kind: match is_import_or_export_type {
              true => ImportKind::TsType,
              false => ImportKind::Es,
            },
            range,
            is_dynamic: false,
            attributes: desc.import_attributes,
          }],
          desc.leading_comments,
        )
      }
      DependencyDescriptor::Dynamic(desc) => {
        let import_attributes = desc.import_attributes;
        let specifiers = match desc.argument {
          DynamicArgument::String(text) => {
            vec![text]
          }
          DynamicArgument::Template(parts)
            if module_specifier.scheme() == "file" =>
          {
            let mut parts = analyze_dynamic_arg_template_parts(
              &parts,
              module_specifier,
              &desc.argument_range,
              &import_attributes,
              file_system,
            );
            // operating systems won't always traverse directories in
            // the same order, so sort here to ensure output is stable
            parts.sort();
            parts
          }
          _ => continue,
        };
        let range = Range::from_position_range(
          module_specifier.clone(),
          desc.argument_range.clone(),
        );
        (
          specifiers
            .into_iter()
            .map(|specifier| Import {
              specifier,
              kind: ImportKind::Es,
              range: range.clone(),
              is_dynamic: true,
              attributes: import_attributes.clone(),
            })
            .collect::<Vec<_>>(),
          desc.leading_comments,
        )
      }
    };

    for import in imports {
      let dep = module_dependencies
        .entry(import.specifier.clone())
        .or_default();
      // TODO(nayeemrmn): Import attributes should be visited and checked for
      // every import, not one per specifier.
      if dep.maybe_attribute_type.is_none() {
        dep.maybe_attribute_type = import.attributes.get("type").cloned();
      }

      if import.kind == ImportKind::TsType {
        if dep.maybe_type.is_none() {
          dep.maybe_type = resolve(
            &import.specifier,
            import.range.clone(),
            ResolutionMode::Types,
            maybe_resolver,
            maybe_npm_resolver,
          );
        }
      } else if dep.maybe_code.is_none() {
        // This is a code import, the first one of that specifier in this module.
        // Resolve and determine the initial `is_dynamic` value from it.
        dep.maybe_code = resolve(
          &import.specifier,
          import.range.clone(),
          ResolutionMode::Execution,
          maybe_resolver,
          maybe_npm_resolver,
        );
        dep.is_dynamic = import.is_dynamic;
      } else {
        // This is a code import, but not the first one of that specifier in this
        // module. Maybe update the `is_dynamic` value. Static imports take
        // precedence. Note that `@jsxImportSource` and `/// <reference path />`
        // count as static imports for this purpose.
        dep.is_dynamic = dep.is_dynamic && import.is_dynamic;
      }
      if graph_kind.include_types() && dep.maybe_type.is_none() {
        let specifier = module_specifier.clone();
        let maybe_type =
          if let Some(pragma) = analyze_deno_types(&leading_comments) {
            resolve(
              &pragma.specifier,
              Range::from_position_range(specifier, pragma.range),
              ResolutionMode::Types,
              maybe_resolver,
              maybe_npm_resolver,
            )
          } else {
            let range = import.range.clone();
            // only check if the code resolution is for the same range
            if Some(&range) == dep.maybe_code.maybe_range() {
              let types_resolution = resolve(
                &import.specifier,
                range,
                ResolutionMode::Types,
                maybe_resolver,
                maybe_npm_resolver,
              );
              // only bother setting if the resolved specifier
              // does not match the code specifier
              if types_resolution.maybe_specifier()
                != dep.maybe_code.maybe_specifier()
              {
                types_resolution
              } else {
                Resolution::None
              }
            } else {
              Resolution::None
            }
          };
        dep.maybe_type = maybe_type;
      }
      dep.imports.push(import);
    }
  }
}

fn analyze_dynamic_arg_template_parts(
  parts: &[DynamicTemplatePart],
  referrer: &Url,
  referrer_range: &PositionRange,
  import_attributes: &ImportAttributes,
  file_system: &dyn FileSystem,
) -> Vec<String> {
  fn resolve_initial_dir_path(
    parts: &[DynamicTemplatePart],
    referrer: &Url,
  ) -> Option<ModuleSpecifier> {
    match parts.first()? {
      DynamicTemplatePart::String { value } => {
        if value.starts_with("./") {
          referrer.join(value).ok()
        } else if value.starts_with("file://") {
          ModuleSpecifier::parse(value).ok()
        } else {
          None
        }
      }
      // could be a remote module, so ignore
      DynamicTemplatePart::Expr => None,
    }
  }

  fn validate_string_parts(
    string_parts: &[&String],
    is_last_string: bool,
  ) -> bool {
    fn validate_part(
      index: usize,
      part: &str,
      path_parts: &[&String],
      is_last_string: bool,
    ) -> bool {
      !part.contains("/../")
        && if index == 0 {
          let valid_start = part.starts_with("./") || part.starts_with('/');
          let valid_end = part.ends_with('/');
          valid_start && valid_end
        } else if is_last_string && index == path_parts.len() - 1 {
          part.starts_with('/') || !part.contains('/')
        } else {
          part.starts_with('/') && part.ends_with('/')
        }
    }

    string_parts.iter().enumerate().all(|(index, part)| {
      validate_part(index, part, string_parts, is_last_string)
    })
  }

  let Some(dir_path) = resolve_initial_dir_path(parts, referrer) else {
    return Vec::new();
  };

  let string_parts = parts
    .iter()
    .enumerate()
    .filter_map(|(i, p)| match p {
      DynamicTemplatePart::String { value } => {
        if i == 0 && value.starts_with("file://") {
          None // don't do comparisons on the base
        } else {
          Some(value)
        }
      }
      DynamicTemplatePart::Expr => None,
    })
    .collect::<Vec<_>>();
  let is_last_string =
    matches!(parts.last(), Some(DynamicTemplatePart::String { .. }));
  if !validate_string_parts(&string_parts, is_last_string) {
    return Vec::new(); // don't search for this
  }

  let matching_media_types =
    if import_attributes.get("type").map(|t| t.as_str()) == Some("json") {
      vec![MediaType::Json]
    } else {
      vec![
        MediaType::JavaScript,
        MediaType::TypeScript,
        MediaType::Jsx,
        MediaType::Tsx,
        MediaType::Mjs,
        MediaType::Mts,
      ]
    };
  let mut specifiers = Vec::new();
  // skip the root specifier
  if is_fs_root_specifier(&dir_path) {
    return specifiers;
  }
  let mut pending_dirs = VecDeque::from([dir_path]);
  while let Some(dir_path) = pending_dirs.pop_front() {
    let entries = file_system.read_dir(&dir_path);
    for entry in entries {
      match entry.kind {
        DirEntryKind::File => {
          let url = &entry.url;
          if matching_media_types.contains(&MediaType::from_specifier(url)) {
            if url == referrer {
              continue; // found itself, so skip
            }
            if let Some(specifier) = referrer.make_relative(url) {
              let specifier = if !specifier.starts_with("../") {
                format!("./{}", specifier)
              } else {
                specifier
              };
              let mut valid = true;
              let mut last_index = 0;
              for part in &string_parts {
                if let Some(index) = &specifier[last_index..].find(*part) {
                  last_index += index + part.len();
                } else {
                  valid = false;
                  break;
                }
              }
              if valid {
                if let Some(DynamicTemplatePart::String { value }) =
                  parts.last()
                {
                  if specifier.ends_with(value) {
                    specifiers.push(specifier);
                  }
                } else {
                  specifiers.push(specifier);
                }
              }
            }
          }
        }
        DirEntryKind::Dir => {
          // ignore hidden directories and any node_modules/vendor folders
          let is_allowed_dir = entry
            .url
            .path()
            .rsplit('/')
            .find(|c| !c.is_empty())
            .map(|c| {
              !c.starts_with('.') && c != "node_modules" && c != "vendor"
            })
            .unwrap_or(true);
          if is_allowed_dir {
            pending_dirs.push_back(entry.url);
          }
        }
        DirEntryKind::Symlink => {
          // ignore
        }
        DirEntryKind::Error(err) => {
          // For now, errors are swallowed and not stored in the graph.
          // If we decide to represent these in the graph, we'll need to
          // figure out what to do with errors like directory errors.
          // Additionally, take note that these are dynamic import errors,
          // so they shouldn't be eagerly surfaced.
          log::warn!(
            "Graph failed resolving '{}'. {:#}\n    at {}:{}:{}",
            entry.url,
            err,
            referrer,
            referrer_range.start.line + 1,
            referrer_range.start.character + 1,
          );
        }
      };
    }
  }

  specifiers
}

/// Determine if a media type is "untyped" and should be checked to see if there
/// are types provided.
fn is_untyped(media_type: &MediaType) -> bool {
  matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  )
}

/// The kind of module graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphKind {
  /// All types of dependencies should be analyzed and included in the graph.
  All,
  /// Only code dependencies should be analyzed and included in the graph. This
  /// is useful when transpiling and running code, but not caring about type
  /// only dependnecies.
  CodeOnly,
  /// Only type dependencies should be analyzed and included in the graph. This
  /// is useful when assessing types, like documentation or type checking, when
  /// the code will not be executed.
  ///
  /// Note that code which is overloaded with types upon access (like the
  /// `X-TypeScript-Types` header or types defined in the code itself) will
  /// still be loaded into the graph, but further code only dependencies will
  /// not be followed.
  TypesOnly,
}

impl Default for GraphKind {
  fn default() -> Self {
    Self::All
  }
}

impl GraphKind {
  pub fn include_types(&self) -> bool {
    matches!(self, Self::All | Self::TypesOnly)
  }
}

enum PendingInfoResponse {
  External {
    specifier: ModuleSpecifier,
  },
  Module {
    content_or_module_info: ContentOrModuleInfo,
    specifier: ModuleSpecifier,
    maybe_headers: Option<HashMap<String, String>>,
  },
}

impl PendingInfoResponse {
  fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::External { specifier } => specifier,
      Self::Module { specifier, .. } => specifier,
    }
  }
}

impl From<LoadResponse> for PendingInfoResponse {
  fn from(load_response: LoadResponse) -> Self {
    match load_response {
      LoadResponse::External { specifier } => Self::External { specifier },
      LoadResponse::Module {
        content,
        specifier,
        maybe_headers,
      } => Self::Module {
        content_or_module_info: ContentOrModuleInfo::Content(content),
        specifier,
        maybe_headers,
      },
    }
  }
}

#[derive(Clone)]
struct JsrPackageVersionInfoExt {
  base_url: Url,
  inner: Arc<JsrPackageVersionInfo>,
}

struct PendingInfo {
  specifier: ModuleSpecifier,
  maybe_range: Option<Range>,
  result: Result<Option<PendingInfoResponse>, anyhow::Error>,
  maybe_version_info: Option<JsrPackageVersionInfoExt>,
}

type PendingInfoFuture = LocalBoxFuture<'static, PendingInfo>;

#[derive(PartialEq, Eq, Hash)]
pub(crate) struct AttributeTypeWithRange {
  range: Range,
  /// The kind of attribute (ex. "json").
  kind: String,
}

struct PendingNpmRegistryInfoLoad {
  package_name: String,
  result: Result<(), Arc<anyhow::Error>>,
}

type PendingNpmRegistryInfoLoadFutures =
  FuturesUnordered<LocalBoxFuture<'static, PendingNpmRegistryInfoLoad>>;

#[derive(Default)]
struct PendingNpmState {
  requested_registry_info_loads: HashSet<String>,
  pending_registry_info_loads: PendingNpmRegistryInfoLoadFutures,
  pending_resolutions: Vec<PendingNpmResolutionItem>,
}

struct PendingJsrResolutionItem {
  specifier: ModuleSpecifier,
  package_ref: JsrPackageReqReference,
  maybe_range: Option<Range>,
}

struct PendingContentLoadItem {
  specifier: ModuleSpecifier,
  maybe_range: Option<Range>,
  result: LoadResult,
  module_info: ModuleInfo,
}

#[derive(Clone)]
struct PendingJsrPackageVersionInfoLoadItem {
  checksum: String,
  info: Arc<JsrPackageVersionInfo>,
}

type PendingResult<T> = Shared<JoinHandle<Result<T, Arc<anyhow::Error>>>>;

#[derive(Default)]
struct PendingJsrState {
  pending_package_info_loads:
    HashMap<String, PendingResult<Option<Arc<JsrPackageInfo>>>>,
  pending_package_version_info_loads:
    HashMap<PackageNv, PendingResult<PendingJsrPackageVersionInfoLoadItem>>,
  pending_resolutions: Vec<PendingJsrResolutionItem>,
  pending_content_loads:
    FuturesUnordered<LocalBoxFuture<'static, PendingContentLoadItem>>,
}

struct PendingDynamicBranch {
  range: Range,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_version_info: Option<JsrPackageVersionInfoExt>,
}

#[derive(Default)]
struct PendingState {
  pending: FuturesOrdered<PendingInfoFuture>,
  jsr: PendingJsrState,
  npm: PendingNpmState,
  pending_specifiers:
    HashMap<ModuleSpecifier, HashSet<Option<AttributeTypeWithRange>>>,
  dynamic_branches: HashMap<ModuleSpecifier, PendingDynamicBranch>,
}

#[derive(Clone)]
enum ContentOrModuleInfo {
  Content(Arc<[u8]>),
  ModuleInfo {
    info: ModuleInfo,
    /// The checksum to use when loading the content
    checksum: LoaderChecksum,
  },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FillPassMode {
  AllowRestart,
  NoRestart,
  CacheBusting,
}

impl FillPassMode {
  fn to_cache_setting(self) -> CacheSetting {
    if self == FillPassMode::CacheBusting {
      CacheSetting::Reload
    } else {
      CacheSetting::Use
    }
  }
}

#[derive(Debug, Clone)]
pub struct BuildDiagnostic {
  pub maybe_range: Option<Range>,
  pub kind: BuildDiagnosticKind,
}

impl std::fmt::Display for BuildDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.kind)?;
    if let Some(range) = &self.maybe_range {
      write!(f, "\n    at {range}")?;
    }
    Ok(())
  }
}

#[derive(Debug, Clone)]
pub enum BuildDiagnosticKind {
  ConstraintNotMatchedWorkspaceVersion {
    reference: JsrPackageReqReference,
    workspace_version: Version,
  },
}

impl std::fmt::Display for BuildDiagnosticKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::ConstraintNotMatchedWorkspaceVersion {
        reference,
        workspace_version,
      } => {
        write!(
          f,
          "Version constraint '{}' did not match workspace member version '{}'",
          reference.req(),
          workspace_version,
        )
      }
    }
  }
}

struct Builder<'a, 'graph> {
  in_dynamic_branch: bool,
  file_system: &'a dyn FileSystem,
  jsr_url_provider: &'a dyn JsrUrlProvider,
  loader: &'a mut dyn Loader,
  resolver: Option<&'a dyn Resolver>,
  npm_resolver: Option<&'a dyn NpmResolver>,
  module_analyzer: &'a dyn ModuleAnalyzer,
  reporter: Option<&'a dyn Reporter>,
  graph: &'graph mut ModuleGraph,
  state: PendingState,
  fill_pass_mode: FillPassMode,
  workspace_members: &'a [WorkspaceMember],
  diagnostics: Vec<BuildDiagnostic>,
  executor: &'a dyn Executor,
}

impl<'a, 'graph> Builder<'a, 'graph> {
  #[allow(clippy::too_many_arguments)]
  pub async fn build(
    graph: &'graph mut ModuleGraph,
    roots: Vec<ModuleSpecifier>,
    imports: Vec<ReferrerImports>,
    is_dynamic: bool,
    file_system: &'a dyn FileSystem,
    jsr_url_provider: &'a dyn JsrUrlProvider,
    resolver: Option<&'a dyn Resolver>,
    npm_resolver: Option<&'a dyn NpmResolver>,
    loader: &'a mut dyn Loader,
    module_analyzer: &'a dyn ModuleAnalyzer,
    reporter: Option<&'a dyn Reporter>,
    workspace_members: &'a [WorkspaceMember],
    executor: &'a dyn Executor,
  ) -> Vec<BuildDiagnostic> {
    let fill_pass_mode = match graph.roots.is_empty() {
      true => FillPassMode::AllowRestart,
      false => FillPassMode::NoRestart,
    };
    let mut builder = Self {
      in_dynamic_branch: is_dynamic,
      file_system,
      jsr_url_provider,
      loader,
      resolver,
      npm_resolver,
      module_analyzer,
      reporter,
      graph,
      state: PendingState::default(),
      fill_pass_mode,
      workspace_members,
      diagnostics: Vec::new(),
      executor,
    };
    builder.fill(roots, imports).await;
    builder.diagnostics
  }

  async fn fill(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    imports: Vec<ReferrerImports>,
  ) {
    let provided_roots = roots;
    let provided_imports = imports;
    let roots = provided_roots
      .iter()
      .filter(|r| !self.graph.roots.contains(r))
      .cloned()
      .collect::<Vec<_>>();
    let imports = provided_imports
      .iter()
      .filter(|r| !self.graph.imports.contains_key(&r.referrer))
      .cloned()
      .collect::<Vec<_>>();

    self.graph.roots.extend(roots.clone());

    for root in roots {
      self.load(&root, None, self.in_dynamic_branch, None, None);
    }

    // process any imports that are being added to the graph.
    self.handle_provided_imports(imports);

    loop {
      let specifier = match self.state.pending.next().await {
        Some(PendingInfo {
          specifier,
          maybe_range,
          result,
          maybe_version_info,
        }) => match result {
          Ok(Some(response)) => {
            if maybe_version_info.is_none()
              && self
                .jsr_url_provider
                .package_url_to_nv(response.specifier())
                .is_some()
            {
              self.graph.module_slots.insert(
                specifier.clone(),
                ModuleSlot::Err(ModuleError::LoadingErr(
                  specifier.clone(),
                  maybe_range,
                  // Two tasks we need to do before removing this error message:
                  // 1. If someone imports a package via an HTTPS URL then we should probably
                  //    bail completely on fast check because it could expose additional types
                  //    not found in fast check, which might cause strange behaviour.
                  // 2. For HTTPS URLS imported from the registry, we should probably still
                  //    compare it against the checksums found in the registry otherwise it might
                  //    not end up in the lockfile causing a security issue.
                  Arc::new(anyhow!(concat!(
                    "Importing a JSR package via an HTTPS URL is not implemented. ",
                    "Use a jsr: specifier instead for the time being."
                  )),
                ))),
              );
            } else {
              let attribute_types =
                self.state.pending_specifiers.remove(&specifier).unwrap();
              for maybe_attribute_type in attribute_types {
                self.visit(
                  &specifier,
                  &response,
                  maybe_attribute_type,
                  maybe_range.clone(),
                  maybe_version_info.as_ref(),
                )
              }
            }
            Some(specifier)
          }
          Ok(None) => {
            self.graph.module_slots.insert(
              specifier.clone(),
              ModuleSlot::Err(ModuleError::Missing(
                specifier.clone(),
                maybe_range,
              )),
            );
            Some(specifier)
          }
          Err(err) => {
            self.graph.module_slots.insert(
              specifier.clone(),
              ModuleSlot::Err(ModuleError::LoadingErr(
                specifier.clone(),
                maybe_range,
                Arc::new(err),
              )),
            );
            Some(specifier)
          }
        },
        None => None,
      };

      if let (Some(specifier), Some(reporter)) = (specifier, self.reporter) {
        let modules_total = self.graph.module_slots.len();
        let modules_done = modules_total - self.state.pending.len();
        reporter.on_load(&specifier, modules_done, modules_total);
      }

      if self.state.pending.is_empty() {
        let should_restart = self.resolve_pending_jsr_specifiers().await;
        if should_restart {
          self.restart(provided_roots, provided_imports).await;
          return;
        }

        if self.state.pending.is_empty() {
          self.resolve_dynamic_branches();

          if self.state.pending.is_empty() {
            break;
          }
        }
      }
    }

    // handle any pending content loads from the Deno registry
    self.handle_jsr_registry_pending_content_loads().await;

    // enrich with cache info from the loader
    self.fill_graph_with_cache_info();

    // resolve any npm package requirements
    NpmSpecifierResolver::fill_builder(self).await;
  }

  fn handle_provided_imports(&mut self, imports: Vec<ReferrerImports>) {
    for referrer_imports in imports {
      let referrer = referrer_imports.referrer;
      let imports = referrer_imports.imports;
      let graph_import =
        GraphImport::new(&referrer, imports, self.resolver, self.npm_resolver);
      for dep in graph_import.dependencies.values() {
        if let Resolution::Ok(resolved) = &dep.maybe_type {
          self.load(
            &resolved.specifier,
            Some(&resolved.range),
            self.in_dynamic_branch,
            None,
            None,
          );
        }
      }
      self.graph.imports.insert(referrer, graph_import);
    }
  }

  async fn resolve_pending_jsr_specifiers(&mut self) -> bool {
    // first load the package information
    let pending_resolutions =
      std::mem::take(&mut self.state.jsr.pending_resolutions);
    let mut pending_version_resolutions =
      Vec::with_capacity(pending_resolutions.len());
    let should_collect_top_level_nvs =
      self.graph.packages.top_level_packages().is_empty()
        && self.graph.graph_kind.include_types();
    for pending_resolution in pending_resolutions {
      let package_name = &pending_resolution.package_ref.req().name;
      let fut = self
        .state
        .jsr
        .pending_package_info_loads
        .get(package_name)
        .unwrap()
        .clone();
      match fut.await {
        Ok(Some(info)) => {
          // resolve the best version out of the existing versions first
          let package_req = pending_resolution.package_ref.req();
          match self.resolve_jsr_version(&info, package_req) {
            Some(version) => {
              // now queue a pending load for that version information
              let package_nv = PackageNv {
                name: package_name.clone(),
                version,
              };
              self
                .graph
                .packages
                .add_nv(package_req.clone(), package_nv.clone());

              self.queue_load_package_version_info(&package_nv);
              pending_version_resolutions
                .push((package_nv, pending_resolution));
            }
            None => {
              if self.fill_pass_mode == FillPassMode::AllowRestart {
                return true; // restart
              } else {
                self.graph.module_slots.insert(
                  pending_resolution.specifier.clone(),
                  ModuleSlot::Err(ModuleError::UnknownPackageReq {
                    specifier: pending_resolution.specifier.clone(),
                    maybe_range: pending_resolution.maybe_range.clone(),
                    package_req: package_req.clone(),
                  }),
                );
              }
            }
          }
        }
        Ok(None) => {
          self.graph.module_slots.insert(
            pending_resolution.specifier.clone(),
            ModuleSlot::Err(ModuleError::UnknownPackage {
              specifier: pending_resolution.specifier.clone(),
              maybe_range: pending_resolution.maybe_range.clone(),
              package_name: package_name.clone(),
            }),
          );
        }
        Err(err) => {
          self.graph.module_slots.insert(
            pending_resolution.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              pending_resolution.specifier,
              pending_resolution.maybe_range,
              err.clone(),
            )),
          );
        }
      }
    }

    // now resolve the version information
    for (nv, resolution_item) in pending_version_resolutions {
      let version_info_result = self
        .state
        .jsr
        .pending_package_version_info_loads
        .get_mut(&nv)
        .unwrap()
        .clone()
        .await
        .map(|info| (info, resolution_item.package_ref.sub_path()));
      match version_info_result {
        Ok((version_info_load_item, sub_path)) => {
          let version_info = version_info_load_item.info;
          self
            .graph
            .packages
            .ensure_package(nv.clone(), version_info_load_item.checksum);
          let base_url = self.jsr_url_provider.package_url(&nv);
          let export_name = normalize_export_name(sub_path);
          match version_info.export(&export_name) {
            Some(export_value) => {
              self.graph.packages.add_export(
                &nv,
                (
                  normalize_export_name(resolution_item.package_ref.sub_path())
                    .to_string(),
                  export_value.to_string(),
                ),
              );
              if should_collect_top_level_nvs {
                self.graph.packages.add_top_level_package(nv.clone());
              }

              let specifier = base_url.join(export_value).unwrap();
              self
                .graph
                .redirects
                .insert(resolution_item.specifier, specifier.clone());
              let version_info = JsrPackageVersionInfoExt {
                base_url,
                inner: version_info,
              };
              self.load(
                &specifier,
                resolution_item.maybe_range.as_ref(),
                self.in_dynamic_branch,
                None,
                Some(&version_info),
              );
            }
            None => {
              self.graph.module_slots.insert(
                resolution_item.specifier.clone(),
                ModuleSlot::Err(ModuleError::UnknownExport {
                  specifier: resolution_item.specifier,
                  maybe_range: resolution_item.maybe_range,
                  nv,
                  export_name: export_name.to_string(),
                  exports: version_info
                    .exports()
                    .map(|(k, _)| k.to_string())
                    .collect::<Vec<_>>(),
                }),
              );
            }
          }
        }
        Err(err) => {
          self.graph.module_slots.insert(
            resolution_item.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              resolution_item.specifier,
              resolution_item.maybe_range,
              err.clone(),
            )),
          );
        }
      }
    }

    false // no restart
  }

  fn resolve_dynamic_branches(&mut self) {
    // Start visiting queued up dynamic branches. We do this in a separate
    // pass after all static dependencies have been visited because:
    // - If a module is both statically and dynamically imported, we want
    //   the static import to take precedence and only load it with
    //   `is_dynamic: false`.
    // - It's more convenient for tracking whether or not we are currently
    //   visiting a dynamic branch.
    if !self.in_dynamic_branch {
      self.in_dynamic_branch = true;
      for (specifier, dynamic_branch) in
        std::mem::take(&mut self.state.dynamic_branches)
      {
        if !self.graph.module_slots.contains_key(&specifier) {
          self.load(
            &specifier,
            Some(&dynamic_branch.range),
            true,
            dynamic_branch.maybe_attribute_type,
            dynamic_branch.maybe_version_info.as_ref(),
          );
        }
      }
    }
  }

  async fn handle_jsr_registry_pending_content_loads(&mut self) {
    while let Some(item) = self.state.jsr.pending_content_loads.next().await {
      match item.result {
        Ok(Some(response)) => {
          match response {
            LoadResponse::External { .. } => {
              // this should never happen, and if it does it indicates the loader
              // was setup incorrectly, so return an error
              self.graph.module_slots.insert(
                item.specifier.clone(),
                ModuleSlot::Err(
                  ModuleError::LoadingErr(
                    item.specifier,
                    item.maybe_range,
                    Arc::new(anyhow!("Loader should never return an external specifier for a jsr: specifier.")),
                  ),
                ),
              );
            }
            LoadResponse::Module {
              content,
              specifier,
              maybe_headers: _maybe_headers,
            } => {
              if specifier == item.specifier {
                self.loader.cache_module_info(
                  &specifier,
                  &content,
                  &item.module_info,
                );
                // fill the existing module slot with the loaded source
                let slot = self.graph.module_slots.get_mut(&specifier).unwrap();
                match slot {
                  ModuleSlot::Module(module) => {
                    match module {
                      Module::Js(module) => {
                        match new_source_with_text(
                          &module.specifier,
                          content,
                          None, // no charset for JSR
                        ) {
                          Ok(source) => {
                            module.source = source;
                          }
                          Err(err) => *slot = ModuleSlot::Err(*err),
                        }
                      }
                      Module::Json(module) => {
                        match new_source_with_text(
                          &module.specifier,
                          content,
                          None, // no charset for JSR
                        ) {
                          Ok(source) => {
                            module.source = source;
                          }
                          Err(err) => *slot = ModuleSlot::Err(*err),
                        }
                      }
                      Module::Npm(_)
                      | Module::Node(_)
                      | Module::External(_) => {
                        unreachable!(); // should not happen by design
                      }
                    }
                  }
                  ModuleSlot::Err(_) => {
                    // the module errored some other way, so ignore
                  }
                  ModuleSlot::Pending => {
                    unreachable!(); // should not happen by design
                  }
                }
              } else {
                // redirects are not supported
                self.graph.module_slots.insert(
                  item.specifier.clone(),
                  ModuleSlot::Err(ModuleError::LoadingErr(
                    item.specifier,
                    item.maybe_range,
                    Arc::new(anyhow!(
                      "Redirects are not supported for the Deno registry."
                    )),
                  )),
                );
              }
            }
          }
        }
        Ok(None) => {
          self.graph.module_slots.insert(
            item.specifier.clone(),
            ModuleSlot::Err(ModuleError::Missing(
              item.specifier,
              item.maybe_range,
            )),
          );
        }
        Err(err) => {
          self.graph.module_slots.insert(
            item.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              item.specifier,
              item.maybe_range,
              Arc::new(err),
            )),
          );
        }
      }
    }
  }

  fn fill_graph_with_cache_info(&mut self) {
    for slot in self.graph.module_slots.values_mut() {
      if let ModuleSlot::Module(ref mut module) = slot {
        match module {
          Module::Json(module) => {
            module.maybe_cache_info =
              self.loader.get_cache_info(&module.specifier);
          }
          Module::Js(module) => {
            module.maybe_cache_info =
              self.loader.get_cache_info(&module.specifier);
          }
          Module::External(_) | Module::Npm(_) | Module::Node(_) => {}
        }
      }
    }
  }

  fn restart(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    imports: Vec<ReferrerImports>,
  ) -> LocalBoxFuture<()> {
    // if restarting is allowed, then the graph will have been empty at the start
    *self.graph = ModuleGraph::new(self.graph.graph_kind);
    self.state = PendingState::default();
    self.fill_pass_mode = FillPassMode::CacheBusting;

    // boxed due to async recursion
    async move { self.fill(roots, imports).await }.boxed_local()
  }

  fn resolve_jsr_version(
    &mut self,
    package_info: &JsrPackageInfo,
    package_req: &PackageReq,
  ) -> Option<Version> {
    // 1. try to resolve with the list of existing versions
    if let Some(existing_versions) =
      self.graph.packages.versions_by_name(&package_req.name)
    {
      if let Some(version) = resolve_version(
        &package_req.version_req,
        existing_versions.iter().map(|nv| &nv.version),
      ) {
        let is_yanked = package_info
          .versions
          .get(version)
          .map(|i| i.yanked)
          .unwrap_or(false);
        let version = version.clone();
        if is_yanked {
          self.graph.packages.add_used_yanked_package(PackageNv {
            name: package_req.name.clone(),
            version: version.clone(),
          });
        }
        return Some(version);
      }
    }

    // 2. attempt to resolve with the unyanked versions
    let unyanked_versions =
      package_info.versions.iter().filter_map(|(v, i)| {
        if !i.yanked {
          Some(v)
        } else {
          None
        }
      });
    if let Some(version) =
      resolve_version(&package_req.version_req, unyanked_versions)
    {
      return Some(version.clone());
    }

    // 3. attempt to resolve with the the yanked versions
    let yanked_versions = package_info.versions.iter().filter_map(|(v, i)| {
      if i.yanked {
        Some(v)
      } else {
        None
      }
    });
    if let Some(version) =
      resolve_version(&package_req.version_req, yanked_versions)
    {
      self.graph.packages.add_used_yanked_package(PackageNv {
        name: package_req.name.clone(),
        version: version.clone(),
      });
      return Some(version.clone());
    }

    None
  }

  /// Checks if the specifier is redirected or not and updates any redirects in
  /// the graph.
  fn check_specifier(
    &mut self,
    requested_specifier: &ModuleSpecifier,
    specifier: &ModuleSpecifier,
  ) {
    // If the response was redirected, then we add the module to the redirects
    if requested_specifier != specifier {
      // remove a potentially pending redirect that will never resolve
      if let Some(slot) = self.graph.module_slots.get(requested_specifier) {
        if matches!(slot, ModuleSlot::Pending) {
          self.graph.module_slots.remove(requested_specifier);
        }
      }
      self
        .graph
        .redirects
        .insert(requested_specifier.clone(), specifier.clone());
    }
  }

  /// Enqueue a request to load the specifier via the loader.
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    maybe_range: Option<&Range>,
    is_dynamic: bool,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) {
    let specifier = self.graph.redirects.get(specifier).unwrap_or(specifier);
    self
      .state
      .pending_specifiers
      .entry(specifier.clone())
      .or_default()
      .insert(maybe_attribute_type);
    if self.graph.module_slots.contains_key(specifier) {
      return;
    }

    if let Some(version_info) = maybe_version_info {
      let base_url = version_info.base_url.as_str();
      let base_url = base_url.strip_suffix('/').unwrap_or(base_url);
      if let Some(sub_path) = specifier.as_str().strip_prefix(base_url) {
        let checksum = match version_info.inner.manifest.get(sub_path) {
          Some(manifest_entry) => {
            match manifest_entry.checksum.strip_prefix("sha256-") {
              Some(checksum) => checksum.to_string(),
              None => {
                self.graph.module_slots.insert(
                  specifier.clone(),
                  ModuleSlot::Err(ModuleError::LoadingErr(
                    specifier.clone(),
                    maybe_range.cloned(),
                    Arc::new(anyhow!(
                      "Unsupported checksum in package manifest. Maybe try upgrading deno?",
                    )),
                  )),
                );
                return;
              }
            }
          }
          // If the checksum is missing then leave it up to the loader to error
          // by providing this special checksum value. For example, someone may
          // be making modifications to their vendor folder in which case this
          // checksum will be ignored and if not, then a loading error will
          // occur about an incorrect checksum.
          None => "package-manifest-missing-checksum".to_string(),
        };
        let checksum = LoaderChecksum::new(checksum.clone());
        if let Some(module_info) = version_info.inner.module_info(sub_path) {
          // Check if this specifier is in the cache. If it is, then
          // don't use the module information as it may be out of date
          // with what's in the cache
          let fut = self.loader.load(
            specifier,
            LoadOptions {
              is_dynamic: self.in_dynamic_branch,
              cache_setting: CacheSetting::Only,
              maybe_checksum: Some(checksum.clone()),
            },
          );
          self.state.pending.push_back({
            let specifier = specifier.clone();
            let maybe_range = maybe_range.cloned();
            let version_info = version_info.clone();
            async move {
              let response = fut.await;
              match response {
                Ok(None) => PendingInfo {
                  specifier: specifier.clone(),
                  maybe_range,
                  result: Ok(Some(PendingInfoResponse::Module {
                    content_or_module_info: ContentOrModuleInfo::ModuleInfo {
                      info: module_info,
                      checksum,
                    },
                    specifier,
                    maybe_headers: None,
                  })),
                  maybe_version_info: Some(version_info),
                },
                response => PendingInfo {
                  specifier,
                  maybe_range,
                  result: response.map(|r| r.map(Into::into)),
                  maybe_version_info: Some(version_info),
                },
              }
            }
            .boxed_local()
          });
          self
            .graph
            .module_slots
            .insert(specifier.clone(), ModuleSlot::Pending);
          return;
        } else {
          self.load_pending_module(
            specifier.clone(),
            maybe_range.map(ToOwned::to_owned),
            specifier.clone(),
            is_dynamic,
            Some(checksum),
            Some(version_info.clone()),
          );
          return;
        }
      }
    }

    let maybe_range = maybe_range.map(ToOwned::to_owned);
    if specifier.scheme() == "jsr" {
      self.load_jsr_specifier(specifier.clone(), maybe_range, is_dynamic);
      return;
    } else if let Some(npm_resolver) = self.npm_resolver {
      if specifier.scheme() == "npm" {
        self.load_npm_specifier(specifier.clone(), npm_resolver, maybe_range);
        return;
      }

      match npm_resolver.resolve_builtin_node_module(specifier) {
        Ok(Some(module_name)) => {
          self.graph.has_node_specifier = true;
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Module(Module::Node(BuiltInNodeModule {
              specifier: specifier.clone(),
              module_name,
            })),
          );
          return;
        }
        Err(err) => {
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              specifier.clone(),
              maybe_range,
              Arc::new(err.into()),
            )),
          );
          return;
        }
        Ok(None) => {
          // ignore, not a builtin node module name
        }
      }
    }

    self.load_pending_module(
      specifier.clone(),
      maybe_range,
      specifier.clone(),
      is_dynamic,
      None,
      None,
    );
  }

  fn load_jsr_specifier(
    &mut self,
    specifier: Url,
    maybe_range: Option<Range>,
    is_dynamic: bool,
  ) {
    match validate_jsr_specifier(&specifier) {
      Ok(package_ref) => {
        if let Some(range) = &maybe_range {
          if let Some(nv) =
            self.jsr_url_provider.package_url_to_nv(&range.specifier)
          {
            self.graph.packages.add_dependency(
              &nv,
              JsrDepPackageReq::jsr(package_ref.req().clone()),
            );
          }
        }
        for workspace_member in self.workspace_members {
          if workspace_member.nv.name == package_ref.req().name {
            if package_ref
              .req()
              .version_req
              .matches(&workspace_member.nv.version)
            {
              let result = self.resolve_workspace_jsr_specifier(
                &specifier,
                maybe_range.as_ref(),
                &package_ref,
                workspace_member,
              );
              match result {
                Ok(load_specifier) => {
                  self.load_pending_module(
                    specifier.clone(),
                    maybe_range.clone(),
                    load_specifier,
                    is_dynamic,
                    None,
                    None,
                  );
                }
                Err(err) => {
                  self
                    .graph
                    .module_slots
                    .insert(specifier.clone(), ModuleSlot::Err(*err));
                }
              }
              return;
            } else {
              self.diagnostics.push(BuildDiagnostic {
                maybe_range: maybe_range.clone(),
                kind:
                  BuildDiagnosticKind::ConstraintNotMatchedWorkspaceVersion {
                    reference: package_ref.clone(),
                    workspace_version: workspace_member.nv.version.clone(),
                  },
              });
            }
          }
        }

        let package_name = &package_ref.req().name;
        let specifier = specifier.clone();
        self.queue_load_package_info(package_name);
        self
          .state
          .jsr
          .pending_resolutions
          .push(PendingJsrResolutionItem {
            specifier,
            package_ref,
            maybe_range,
          });
      }
      Err(err) => {
        self.graph.module_slots.insert(
          specifier.clone(),
          ModuleSlot::Err(ModuleError::LoadingErr(
            specifier,
            maybe_range,
            Arc::new(err.into()),
          )),
        );
      }
    }
  }

  fn resolve_workspace_jsr_specifier(
    &self,
    specifier: &Url,
    maybe_range: Option<&Range>,
    package_ref: &JsrPackageReqReference,
    workspace_member: &WorkspaceMember,
  ) -> Result<ModuleSpecifier, Box<ModuleError>> {
    if workspace_member.exports.is_empty() {
      return Err(Box::new(ModuleError::MissingWorkspaceMemberExports {
        specifier: specifier.clone(),
        maybe_range: maybe_range.map(ToOwned::to_owned),
        nv: workspace_member.nv.clone(),
      }));
    }

    let export_name = normalize_export_name(package_ref.sub_path());
    if let Some(sub_path) = workspace_member.exports.get(export_name.as_ref()) {
      match workspace_member.base.join(sub_path) {
        Ok(load_specifier) => Ok(load_specifier),
        Err(err) => {
          let err: anyhow::Error = err.into();
          Err(Box::new(ModuleError::LoadingErr(
            specifier.clone(),
            maybe_range.map(ToOwned::to_owned),
            Arc::new(err.context(format!(
              "Failed joining '{}' to '{}'.",
              sub_path, workspace_member.base
            ))),
          )))
        }
      }
    } else {
      Err(Box::new(ModuleError::UnknownExport {
        specifier: specifier.clone(),
        maybe_range: maybe_range.map(ToOwned::to_owned),
        nv: workspace_member.nv.clone(),
        export_name: export_name.to_string(),
        exports: workspace_member.exports.keys().cloned().collect(),
      }))
    }
  }

  fn load_npm_specifier(
    &mut self,
    specifier: Url,
    npm_resolver: &dyn NpmResolver,
    maybe_range: Option<Range>,
  ) {
    match NpmPackageReqReference::from_specifier(&specifier) {
      Ok(package_ref) => {
        if let Some(range) = &maybe_range {
          if let Some(nv) =
            self.jsr_url_provider.package_url_to_nv(&range.specifier)
          {
            self.graph.packages.add_dependency(
              &nv,
              JsrDepPackageReq::npm(package_ref.req().clone()),
            );
          }
        }

        if self
          .state
          .npm
          .requested_registry_info_loads
          .insert(package_ref.req().name.clone())
        {
          // request to load
          let package_name = package_ref.req().name.clone();
          let fut = npm_resolver.load_and_cache_npm_package_info(&package_name);
          self.state.npm.pending_registry_info_loads.push(Box::pin(
            async move {
              PendingNpmRegistryInfoLoad {
                package_name,
                result: fut.await.map_err(Arc::new),
              }
            },
          ));
        }
        self
          .state
          .npm
          .pending_resolutions
          .push(PendingNpmResolutionItem {
            specifier,
            package_ref,
            maybe_range,
          });
      }
      Err(err) => {
        self.graph.module_slots.insert(
          specifier.clone(),
          ModuleSlot::Err(ModuleError::LoadingErr(
            specifier,
            maybe_range,
            Arc::new(err.into()),
          )),
        );
      }
    }
  }

  fn load_pending_module(
    &mut self,
    requested_specifier: Url,
    maybe_range: Option<Range>,
    load_specifier: Url,
    is_dynamic: bool,
    maybe_checksum: Option<LoaderChecksum>,
    maybe_version_info: Option<JsrPackageVersionInfoExt>,
  ) {
    self
      .graph
      .module_slots
      .insert(requested_specifier.clone(), ModuleSlot::Pending);
    let fut = self
      .loader
      .load(
        &load_specifier,
        LoadOptions {
          is_dynamic,
          cache_setting: CacheSetting::Use,
          maybe_checksum,
        },
      )
      .map(move |result| PendingInfo {
        specifier: requested_specifier,
        maybe_range,
        result: result.map(|r| r.map(Into::into)),
        maybe_version_info,
      });
    self.state.pending.push_back(Box::pin(fut));
  }

  fn queue_load_package_info(&mut self, package_name: &str) {
    if self
      .state
      .jsr
      .pending_package_info_loads
      .contains_key(package_name)
    {
      return; // already queued
    }

    // request to load
    let specifier = self
      .jsr_url_provider
      .url()
      .join(&format!("{}/meta.json", package_name))
      .unwrap();
    let fut = self.loader.load(
      &specifier,
      LoadOptions {
        is_dynamic: false,
        cache_setting: self.fill_pass_mode.to_cache_setting(),
        maybe_checksum: None,
      },
    );
    let fut = spawn(
      self.executor,
      async move {
        let data = fut.await.map_err(Arc::new)?;
        match data {
          Some(LoadResponse::Module { content, .. }) => {
            let package_info: JsrPackageInfo = serde_json::from_slice(&content)
              .map_err(|e| Arc::new(e.into()))?;
            Ok(Some(Arc::new(package_info)))
          }
          _ => Ok(None),
        }
      }
      .boxed_local(),
    );
    self
      .state
      .jsr
      .pending_package_info_loads
      .insert(package_name.to_string(), fut.shared());
  }

  fn queue_load_package_version_info(&mut self, package_nv: &PackageNv) {
    if self
      .state
      .jsr
      .pending_package_version_info_loads
      .contains_key(package_nv)
    {
      return; // already queued
    }

    let specifier = self
      .jsr_url_provider
      .url()
      .join(&format!(
        "{}/{}_meta.json",
        package_nv.name, package_nv.version
      ))
      .unwrap();
    let maybe_expected_checksum = self
      .graph
      .packages
      .get_manifest_checksum(package_nv)
      .map(|checksum| LoaderChecksum::new(checksum.clone()));
    let fut = self.loader.load(
      &specifier,
      LoadOptions {
        is_dynamic: false,
        cache_setting: self.fill_pass_mode.to_cache_setting(),
        // we won't have a checksum when loading this the
        // first time or when not using a lockfile
        maybe_checksum: maybe_expected_checksum.clone(),
      },
    );
    let fut = spawn(
      self.executor,
      async move {
        let data = fut.await.map_err(Arc::new)?;
        match data {
          Some(LoadResponse::Module { content, .. }) => {
            // if we have the expected checksum, then we can re-use that here
            let checksum = maybe_expected_checksum
              .map(|c| c.into_string())
              .unwrap_or_else(|| LoaderChecksum::gen(&content));
            let version_info: JsrPackageVersionInfo =
              serde_json::from_slice(&content)
                .map_err(|e| Arc::new(e.into()))?;
            Ok(PendingJsrPackageVersionInfoLoadItem {
              checksum,
              info: Arc::new(version_info),
            })
          }
          _ => Err(Arc::new(anyhow!("Not found: {}", specifier))),
        }
      }
      .boxed_local(),
    );
    self
      .state
      .jsr
      .pending_package_version_info_loads
      .insert(package_nv.clone(), fut.shared());
  }

  fn roots_contain(&self, specifier: &ModuleSpecifier) -> bool {
    for root in &self.graph.roots {
      if root == specifier {
        return true;
      }
    }
    false
  }

  fn visit(
    &mut self,
    requested_specifier: &ModuleSpecifier,
    response: &PendingInfoResponse,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_referrer: Option<Range>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) {
    let (specifier, module_slot) = match response {
      PendingInfoResponse::External { specifier } => {
        self.check_specifier(requested_specifier, specifier);
        let module_slot =
          ModuleSlot::Module(Module::External(ExternalModule {
            specifier: specifier.clone(),
          }));
        (specifier, module_slot)
      }
      PendingInfoResponse::Module {
        specifier,
        content_or_module_info,
        maybe_headers,
      } => {
        self.check_specifier(requested_specifier, specifier);
        (
          specifier,
          self.visit_module(
            specifier,
            maybe_headers.as_ref(),
            content_or_module_info.clone(),
            maybe_attribute_type,
            maybe_referrer,
            maybe_version_info,
          ),
        )
      }
    };
    self
      .graph
      .module_slots
      .insert(specifier.clone(), module_slot);
  }

  /// Visit a module, parsing it and resolving any dependencies.
  fn visit_module(
    &mut self,
    specifier: &ModuleSpecifier,
    maybe_headers: Option<&HashMap<String, String>>,
    content_or_module_info: ContentOrModuleInfo,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_referrer: Option<Range>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) -> ModuleSlot {
    struct ProvidedModuleAnalyzer(RefCell<Option<ModuleInfo>>);

    impl ModuleAnalyzer for ProvidedModuleAnalyzer {
      fn analyze(
        &self,
        _specifier: &ModuleSpecifier,
        _source: Arc<str>,
        _media_type: MediaType,
      ) -> Result<ModuleInfo, ParseDiagnostic> {
        Ok(self.0.borrow_mut().take().unwrap()) // will only be called once
      }
    }

    use std::borrow::BorrowMut;
    let is_root = self.roots_contain(specifier);

    let (content, maybe_module_analyzer) = match content_or_module_info {
      ContentOrModuleInfo::Content(content) => (content, None),
      ContentOrModuleInfo::ModuleInfo { info, checksum } => {
        self.state.jsr.pending_content_loads.push({
          let specifier = specifier.clone();
          let maybe_range = maybe_referrer.clone();
          let module_info = info.clone();
          let fut = self.loader.load(
            &specifier,
            LoadOptions {
              is_dynamic: self.in_dynamic_branch,
              cache_setting: CacheSetting::Use,
              maybe_checksum: Some(checksum),
            },
          );
          async move {
            let result = fut.await;
            PendingContentLoadItem {
              specifier,
              maybe_range,
              result,
              module_info,
            }
          }
          .boxed_local()
        });
        (
          Arc::new([]) as Arc<[u8]>,
          Some(ProvidedModuleAnalyzer(RefCell::new(Some(info)))),
        )
      }
    };

    let mut module_slot = match parse_module(
      self.graph.graph_kind,
      specifier,
      maybe_headers,
      content,
      maybe_attribute_type,
      maybe_referrer,
      self.file_system,
      self.resolver,
      maybe_module_analyzer
        .as_ref()
        .map(|r| r as &dyn ModuleAnalyzer)
        .unwrap_or(self.module_analyzer),
      is_root,
      self.in_dynamic_branch,
      self.npm_resolver,
    ) {
      Ok(module) => ModuleSlot::Module(module),
      Err(err) => ModuleSlot::Err(err),
    };

    if let ModuleSlot::Module(Module::Js(module)) = module_slot.borrow_mut() {
      if matches!(self.graph.graph_kind, GraphKind::All | GraphKind::CodeOnly)
        || module.maybe_types_dependency.is_none()
      {
        for dep in module.dependencies.values_mut() {
          if matches!(
            self.graph.graph_kind,
            GraphKind::All | GraphKind::CodeOnly
          ) || dep.maybe_type.is_none()
          {
            if let Resolution::Ok(resolved) = &dep.maybe_code {
              let specifier = &resolved.specifier;
              let range = &resolved.range;
              let maybe_attribute_type =
                dep.maybe_attribute_type.as_ref().map(|assert_type| {
                  AttributeTypeWithRange {
                    range: range.clone(),
                    kind: assert_type.clone(),
                  }
                });
              if dep.is_dynamic && !self.in_dynamic_branch {
                self.state.dynamic_branches.insert(
                  specifier.clone(),
                  PendingDynamicBranch {
                    range: range.clone(),
                    maybe_attribute_type,
                    maybe_version_info: maybe_version_info
                      .map(ToOwned::to_owned),
                  },
                );
              } else {
                self.load(
                  specifier,
                  Some(range),
                  self.in_dynamic_branch,
                  maybe_attribute_type,
                  maybe_version_info,
                );
              }
            }
          } else {
            dep.maybe_code = Resolution::None;
          }

          if self.graph.graph_kind.include_types() {
            if let Resolution::Ok(resolved) = &dep.maybe_type {
              let specifier = &resolved.specifier;
              let range = &resolved.range;
              let maybe_attribute_type =
                dep.maybe_attribute_type.as_ref().map(|assert_type| {
                  AttributeTypeWithRange {
                    range: range.clone(),
                    kind: assert_type.clone(),
                  }
                });
              if dep.is_dynamic && !self.in_dynamic_branch {
                self.state.dynamic_branches.insert(
                  specifier.clone(),
                  PendingDynamicBranch {
                    range: range.clone(),
                    maybe_attribute_type,
                    maybe_version_info: maybe_version_info
                      .map(ToOwned::to_owned),
                  },
                );
              } else {
                self.load(
                  specifier,
                  Some(range),
                  self.in_dynamic_branch,
                  maybe_attribute_type,
                  maybe_version_info,
                );
              }
            }
          } else {
            dep.maybe_type = Resolution::None;
          }
        }
      } else {
        module.dependencies.clear();
      }

      if self.graph.graph_kind.include_types() {
        if let Some(Resolution::Ok(resolved)) = module
          .maybe_types_dependency
          .as_ref()
          .map(|d| &d.dependency)
        {
          self.load(
            &resolved.specifier,
            Some(&resolved.range),
            false,
            None,
            maybe_version_info,
          );
        }
      } else {
        module.maybe_types_dependency = None;
      }
    }
    module_slot
  }
}

#[derive(Error, Debug, Clone)]
pub enum JsrPackageFormatError {
  #[error(transparent)]
  JsrPackageParseError(PackageReqReferenceParseError),
  #[error("Version tag not supported in jsr specifiers.")]
  VersionTagNotSupported,
}

fn validate_jsr_specifier(
  specifier: &Url,
) -> Result<JsrPackageReqReference, JsrPackageFormatError> {
  let package_ref = JsrPackageReqReference::from_specifier(specifier)
    .map_err(JsrPackageFormatError::JsrPackageParseError)?;
  match package_ref.req().version_req.inner() {
    RangeSetOrTag::Tag(_) => Err(JsrPackageFormatError::VersionTagNotSupported),
    RangeSetOrTag::RangeSet(_) => Ok(package_ref),
  }
}

fn normalize_export_name(sub_path: Option<&str>) -> Cow<str> {
  let Some(sub_path) = sub_path else {
    return Cow::Borrowed(".");
  };
  if sub_path.is_empty() || matches!(sub_path, "/" | ".") {
    Cow::Borrowed(".")
  } else {
    let sub_path = if sub_path.starts_with('/') {
      Cow::Owned(format!(".{}", sub_path))
    } else if !sub_path.starts_with("./") {
      Cow::Owned(format!("./{}", sub_path))
    } else {
      Cow::Borrowed(sub_path)
    };
    if let Some(prefix) = sub_path.strip_suffix('/') {
      Cow::Owned(prefix.to_string())
    } else {
      sub_path
    }
  }
}

/// Pending information to insert into the module graph once
/// npm specifier resolution has been finalized.
struct NpmSpecifierBuildPendingInfo {
  specifier_resolutions: HashMap<ModuleSpecifier, PackageNv>,
  module_slots: HashMap<ModuleSpecifier, ModuleSlot>,
  redirects: HashMap<ModuleSpecifier, ModuleSpecifier>,
}

impl NpmSpecifierBuildPendingInfo {
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      specifier_resolutions: HashMap::with_capacity(capacity),
      module_slots: HashMap::with_capacity(capacity),
      redirects: HashMap::with_capacity(capacity),
    }
  }

  pub fn clear(&mut self) {
    self.specifier_resolutions.clear();
    self.module_slots.clear();
    self.redirects.clear();
  }
}

struct PendingNpmResolutionItem {
  specifier: ModuleSpecifier,
  package_ref: NpmPackageReqReference,
  maybe_range: Option<Range>,
}

struct NpmSpecifierResolver<'a> {
  npm_resolver: Option<&'a dyn NpmResolver>,
  /// Ordered npm specifiers.
  npm_specifiers: Vec<ModuleSpecifier>,
  pending_info: NpmSpecifierBuildPendingInfo,
  pending_npm_by_name: HashMap<String, VecDeque<PendingNpmResolutionItem>>,
}

impl<'a> NpmSpecifierResolver<'a> {
  pub async fn fill_builder(builder: &mut Builder<'a, '_>) {
    let mut npm_specifier_resolver = NpmSpecifierResolver::new(
      builder.npm_resolver,
      std::mem::take(&mut builder.state.npm.pending_resolutions),
    );

    npm_specifier_resolver
      .resolve(std::mem::take(
        &mut builder.state.npm.pending_registry_info_loads,
      ))
      .await;

    npm_specifier_resolver.fill_graph(builder.graph);
  }

  fn new(
    npm_resolver: Option<&'a dyn NpmResolver>,
    mut pending_npm_specifiers: Vec<PendingNpmResolutionItem>,
  ) -> Self {
    let capacity = pending_npm_specifiers.len();
    let mut pending_npm_by_name = HashMap::with_capacity(capacity);
    let mut npm_specifiers = Vec::with_capacity(capacity);

    let mut seen_specifiers = HashSet::with_capacity(capacity);
    for item in pending_npm_specifiers.drain(..) {
      npm_specifiers.push(item.specifier.clone());
      if seen_specifiers.insert(item.specifier.clone()) {
        let items: &mut VecDeque<_> = pending_npm_by_name
          .entry(item.package_ref.req().name.clone())
          .or_default();
        items.push_back(item);
      }
    }

    Self {
      npm_resolver,
      pending_info: NpmSpecifierBuildPendingInfo::with_capacity(capacity),
      pending_npm_by_name,
      npm_specifiers,
    }
  }

  async fn resolve(
    &mut self,
    mut pending_npm_registry_info_loads: PendingNpmRegistryInfoLoadFutures,
  ) {
    let mut previously_restarted = false;
    while let Some(pending_load) = pending_npm_registry_info_loads.next().await
    {
      let items = self
        .pending_npm_by_name
        .get_mut(&pending_load.package_name)
        .unwrap();
      if let Err(err) = pending_load.result {
        for item in items {
          // load failure
          self.pending_info.module_slots.insert(
            item.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              item.specifier.clone(),
              item.maybe_range.clone(),
              err.clone(),
            )),
          );
        }
      } else {
        while let Some(item) = items.pop_front() {
          if let Some(npm_resolver) = &self.npm_resolver {
            let resolution = npm_resolver.resolve_npm(item.package_ref.req());
            match resolution {
              NpmPackageReqResolution::Ok(pkg_nv) => {
                self
                  .pending_info
                  .specifier_resolutions
                  .insert(item.specifier.clone(), pkg_nv.clone());
                let pkg_id_ref =
                  NpmPackageNvReference::new(PackageNvReference {
                    nv: pkg_nv,
                    sub_path: item
                      .package_ref
                      .sub_path()
                      .map(ToOwned::to_owned),
                  });
                let resolved_specifier = pkg_id_ref.as_specifier();
                if resolved_specifier != item.specifier {
                  self
                    .pending_info
                    .redirects
                    .insert(item.specifier, resolved_specifier.clone());
                }
                self.pending_info.module_slots.insert(
                  resolved_specifier.clone(),
                  ModuleSlot::Module(Module::Npm(NpmModule {
                    specifier: resolved_specifier,
                    nv_reference: pkg_id_ref,
                  })),
                );
              }
              NpmPackageReqResolution::ReloadRegistryInfo(_)
                if !previously_restarted =>
              {
                // the implementer should ideally never return this more than once,
                // but in case they do, have this safety
                previously_restarted = true;

                // clear the current pending information and restart from scratch
                pending_npm_registry_info_loads.clear();
                self.pending_info.clear();

                // add back the failed item so it can be retried
                items.push_front(item);

                // reload all the npm registry information
                for package_name in self.pending_npm_by_name.keys() {
                  let package_name = package_name.clone();
                  let fut =
                    npm_resolver.load_and_cache_npm_package_info(&package_name);
                  pending_npm_registry_info_loads.push(Box::pin(async move {
                    PendingNpmRegistryInfoLoad {
                      package_name,
                      result: fut.await.map_err(Arc::new),
                    }
                  }));
                }
                break;
              }
              NpmPackageReqResolution::Err(err)
              | NpmPackageReqResolution::ReloadRegistryInfo(err) => {
                self.pending_info.module_slots.insert(
                  item.specifier.clone(),
                  ModuleSlot::Err(ModuleError::LoadingErr(
                    item.specifier,
                    item.maybe_range,
                    Arc::new(err),
                  )),
                );
              }
            }
          } else {
            self.pending_info.module_slots.insert(
              item.specifier.clone(),
              ModuleSlot::Err(ModuleError::LoadingErr(
                item.specifier,
                item.maybe_range,
                Arc::new(anyhow::anyhow!(
                  "npm specifiers are not supported in this environment"
                )),
              )),
            );
          }
        }
      }
    }
  }

  fn fill_graph(self, graph: &mut ModuleGraph) {
    let pending_info = self.pending_info;
    let npm_specifiers = self.npm_specifiers;

    // update the graph with the pending information
    for (key, value) in pending_info.module_slots {
      // always keep the existing information in the graph
      // in case it was already used in the runtime
      graph.module_slots.entry(key).or_insert(value);
    }
    for (key, value) in pending_info.redirects {
      graph.redirects.entry(key).or_insert(value);
    }

    // we resolve the npm specifiers grouped by package unordered, so now fill
    // in the npm packages in resolution order ensuring no duplicates are added
    let mut seen_npm_package_ids =
      HashSet::with_capacity(graph.npm_packages.len() + npm_specifiers.len());
    seen_npm_package_ids.extend(graph.npm_packages.iter().cloned());
    for npm_specifier in npm_specifiers {
      if let Some(package_id) =
        pending_info.specifier_resolutions.get(&npm_specifier)
      {
        if seen_npm_package_ids.insert(package_id.clone()) {
          graph.npm_packages.push(package_id.clone());
        }
      }
    }
  }
}

fn new_source_with_text(
  specifier: &ModuleSpecifier,
  text: Arc<[u8]>,
  maybe_charset: Option<&str>,
) -> Result<Arc<str>, Box<ModuleError>> {
  crate::source::decode_source(specifier, text, maybe_charset).map_err(|err| {
    Box::new(ModuleError::LoadingErr(
      specifier.clone(),
      None,
      Arc::new(err.into()),
    ))
  })
}

impl Serialize for Resolution {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    match self {
      Resolution::Ok(resolved) => {
        let mut state = serializer.serialize_struct("ResolvedSpecifier", 2)?;
        state.serialize_field("specifier", &resolved.specifier)?;
        state.serialize_field("span", &resolved.range)?;
        state.end()
      }
      Resolution::Err(err) => {
        let mut state = serializer.serialize_struct("ResolvedError", 2)?;
        state.serialize_field("error", &err.to_string())?;
        state.serialize_field("span", err.range())?;
        state.end()
      }
      Resolution::None => {
        Serialize::serialize(&serde_json::Value::Null, serializer)
      }
    }
  }
}

fn serialize_dependencies<S>(
  dependencies: &IndexMap<String, Dependency>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  #[derive(Serialize)]
  struct DependencyWithSpecifier<'a> {
    specifier: &'a str,
    #[serde(flatten)]
    dependency: &'a Dependency,
  }
  let mut seq = serializer.serialize_seq(Some(dependencies.len()))?;
  for (specifier, dependency) in dependencies {
    seq.serialize_element(&DependencyWithSpecifier {
      specifier,
      dependency,
    })?
  }
  seq.end()
}

fn serialize_source<S>(
  source: &Arc<str>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  serializer.serialize_u32(source.len() as u32)
}

#[cfg(test)]
mod tests {
  use crate::packages::JsrPackageInfoVersion;
  use crate::DefaultModuleAnalyzer;
  use deno_ast::dep::ImportAttribute;
  use pretty_assertions::assert_eq;
  use serde_json::json;

  use super::*;
  use url::Url;

  #[test]
  fn test_range_includes() {
    let range = Range {
      specifier: ModuleSpecifier::parse("file:///a.ts").unwrap(),
      start: Position {
        line: 1,
        character: 20,
      },
      end: Position {
        line: 1,
        character: 30,
      },
    };
    assert!(range.includes(&Position {
      line: 1,
      character: 20
    }));
    assert!(range.includes(&Position {
      line: 1,
      character: 25
    }));
    assert!(range.includes(&Position {
      line: 1,
      character: 30
    }));
    assert!(!range.includes(&Position {
      line: 0,
      character: 25
    }));
    assert!(!range.includes(&Position {
      line: 2,
      character: 25
    }));
  }

  #[test]
  fn test_jsr_import_format() {
    assert!(
      validate_jsr_specifier(&Url::parse("jsr:@scope/mod@tag").unwrap())
        .is_err(),
      "jsr import specifier with tag should be an error"
    );

    assert!(
      validate_jsr_specifier(&Url::parse("jsr:@scope/mod@").unwrap()).is_err()
    );

    assert!(validate_jsr_specifier(
      &Url::parse("jsr:@scope/mod@1.2.3").unwrap()
    )
    .is_ok());

    assert!(
      validate_jsr_specifier(&Url::parse("jsr:@scope/mod").unwrap()).is_ok()
    );
  }

  #[test]
  fn test_module_dependency_includes() {
    let specifier = ModuleSpecifier::parse("file:///a.ts").unwrap();
    let module_analyzer = DefaultModuleAnalyzer::default();
    let content: Arc<[u8]> = Arc::from(
      b"import * as b from \"./b.ts\";\nimport * as c from \"./b.ts\"".to_vec(),
    );
    let module = parse_module(
      GraphKind::All,
      &specifier,
      None,
      content,
      None,
      None,
      &NullFileSystem,
      None,
      &module_analyzer,
      true,
      false,
      None,
    )
    .unwrap();
    let module = module.js().unwrap();
    assert_eq!(module.dependencies.len(), 1);
    let dependency = module.dependencies.first().unwrap().1;
    assert_eq!(
      dependency.maybe_code,
      Resolution::Ok(Box::new(ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///b.ts").unwrap(),
        range: Range {
          specifier: specifier.clone(),
          start: Position {
            line: 0,
            character: 19
          },
          end: Position {
            line: 0,
            character: 27
          },
        },
      }))
    );
    assert_eq!(
      dependency.includes(&Position {
        line: 0,
        character: 21,
      }),
      Some(&Range {
        specifier: specifier.clone(),
        start: Position {
          line: 0,
          character: 19
        },
        end: Position {
          line: 0,
          character: 27
        },
      })
    );
    assert_eq!(
      dependency.includes(&Position {
        line: 1,
        character: 21,
      }),
      Some(&Range {
        specifier,
        start: Position {
          line: 1,
          character: 19
        },
        end: Position {
          line: 1,
          character: 27
        },
      })
    );
    assert_eq!(
      dependency.includes(&Position {
        line: 0,
        character: 18,
      }),
      None,
    );
  }

  #[tokio::test]
  async fn static_dep_of_dynamic_dep_is_dynamic() {
    struct TestLoader {
      loaded_foo: bool,
      loaded_bar: bool,
      loaded_baz: bool,
    }
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => {
            assert!(!options.is_dynamic);
            self.loaded_foo = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"await import('file:///bar.js')".to_vec().into(),
              }))
            })
          }
          "file:///bar.js" => {
            assert!(options.is_dynamic);
            self.loaded_bar = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"import 'file:///baz.js'".to_vec().into(),
              }))
            })
          }
          "file:///baz.js" => {
            assert!(options.is_dynamic);
            self.loaded_baz = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"console.log('Hello, world!')".to_vec().into(),
              }))
            })
          }
          _ => unreachable!(),
        }
      }
    }

    let mut loader = TestLoader {
      loaded_foo: false,
      loaded_bar: false,
      loaded_baz: false,
    };
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &mut loader,
        Default::default(),
      )
      .await;
    assert!(loader.loaded_foo);
    assert!(loader.loaded_bar);
    assert!(loader.loaded_baz);
    assert_eq!(graph.specifiers_count(), 3);
  }

  #[tokio::test]
  async fn missing_module_is_error() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        _options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"await import('file:///bar.js')".to_vec().into(),
            }))
          }),
          "file:///bar.js" => Box::pin(async move { Ok(None) }),
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    let roots = vec![Url::parse("file:///foo.js").unwrap()];
    graph
      .build(roots.clone(), &mut loader, Default::default())
      .await;
    assert!(graph
      .try_get(&Url::parse("file:///foo.js").unwrap())
      .is_ok());
    assert!(matches!(
      graph
        .try_get(&Url::parse("file:///bar.js").unwrap())
        .unwrap_err(),
      ModuleError::Missing(..)
    ));
    let specifiers = graph.specifiers().collect::<HashMap<_, _>>();
    assert_eq!(specifiers.len(), 2);
    assert!(specifiers
      .get(&Url::parse("file:///foo.js").unwrap())
      .unwrap()
      .is_ok());
    assert!(matches!(
      specifiers
        .get(&Url::parse("file:///bar.js").unwrap())
        .unwrap()
        .as_ref()
        .unwrap_err(),
      ModuleError::Missing(..)
    ));

    // should not follow the dynamic import error when walking and not following
    let error_count = graph
      .walk(
        &roots,
        WalkOptions {
          follow_dynamic: false,
          follow_type_only: true,
          check_js: true,
        },
      )
      .errors()
      .count();
    assert_eq!(error_count, 0);

    // should return as dynamic import missing when walking
    let errors = graph
      .walk(
        &roots,
        WalkOptions {
          follow_dynamic: true,
          follow_type_only: true,
          check_js: true,
        },
      )
      .errors()
      .collect::<Vec<_>>();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
      errors[0],
      ModuleGraphError::ModuleError(ModuleError::MissingDynamic(..))
    ));
  }

  #[tokio::test]
  async fn redirected_specifiers() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        _options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: Url::parse("file:///foo_actual.js").unwrap(),
              maybe_headers: None,
              content: b"import 'file:///bar.js'".to_vec().into(),
            }))
          }),
          "file:///bar.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: Url::parse("file:///bar_actual.js").unwrap(),
              maybe_headers: None,
              content: b"(".to_vec().into(),
            }))
          }),
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &mut loader,
        Default::default(),
      )
      .await;
    let specifiers = graph.specifiers().collect::<HashMap<_, _>>();
    dbg!(&specifiers);
    assert_eq!(specifiers.len(), 4);
    assert!(specifiers
      .get(&Url::parse("file:///foo.js").unwrap())
      .unwrap()
      .is_ok());
    assert!(specifiers
      .get(&Url::parse("file:///foo_actual.js").unwrap())
      .unwrap()
      .is_ok());
    assert!(matches!(
      specifiers
        .get(&Url::parse("file:///bar.js").unwrap())
        .unwrap()
        .as_ref()
        .unwrap_err(),
      ModuleError::ParseErr(..)
    ));
    assert!(matches!(
      specifiers
        .get(&Url::parse("file:///bar_actual.js").unwrap())
        .unwrap()
        .as_ref()
        .unwrap_err(),
      ModuleError::ParseErr(..)
    ));
  }

  #[tokio::test]
  async fn local_import_remote_module() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        _options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "https://deno.land/foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content:
                b"import 'FILE:///baz.js'; import 'file:///bar.js'; import 'http://deno.land/foo.js';"
                  .to_vec().into(),
            }))
          }),
          "http://deno.land/foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"export {}".to_vec().into(),
            }))
          }),
          "file:///bar.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"console.log('Hello, world!')".to_vec().into(),
            }))
          }),
          "file:///baz.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"console.log('Hello, world 2!')".to_vec().into(),
            }))
          }),
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    let roots = vec![Url::parse("https://deno.land/foo.js").unwrap()];
    graph
      .build(roots.clone(), &mut loader, Default::default())
      .await;
    assert_eq!(graph.specifiers_count(), 4);
    let errors = graph
      .walk(&roots, Default::default())
      .errors()
      .collect::<Vec<_>>();
    assert_eq!(errors.len(), 3);
    let errors = errors
      .into_iter()
      .map(|err| match err {
        ModuleGraphError::ResolutionError(err) => err,
        _ => unreachable!(),
      })
      .collect::<Vec<_>>();

    assert_eq!(
      errors[0],
      ResolutionError::InvalidDowngrade {
        range: Range {
          specifier: ModuleSpecifier::parse("https://deno.land/foo.js")
            .unwrap(),
          start: Position {
            line: 0,
            character: 57,
          },
          end: Position {
            line: 0,
            character: 82,
          },
        },
        specifier: ModuleSpecifier::parse("http://deno.land/foo.js").unwrap(),
      },
    );
    assert_eq!(
      errors[1],
      ResolutionError::InvalidLocalImport {
        range: Range {
          specifier: ModuleSpecifier::parse("https://deno.land/foo.js")
            .unwrap(),
          start: Position {
            line: 0,
            character: 32,
          },
          end: Position {
            line: 0,
            character: 48,
          },
        },
        specifier: ModuleSpecifier::parse("file:///bar.js").unwrap(),
      },
    );

    assert_eq!(
      errors[2],
      ResolutionError::InvalidLocalImport {
        range: Range {
          specifier: ModuleSpecifier::parse("https://deno.land/foo.js")
            .unwrap(),
          start: Position {
            line: 0,
            character: 7,
          },
          end: Position {
            line: 0,
            character: 23,
          },
        },
        specifier: ModuleSpecifier::parse("file:///baz.js").unwrap(),
      },
    );
  }

  #[tokio::test]
  async fn static_and_dynamic_dep_is_static() {
    struct TestLoader {
      loaded_bar: bool,
    }
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content:
                b"import 'file:///bar.js'; await import('file:///bar.js')"
                  .to_vec()
                  .into(),
            }))
          }),
          "file:///bar.js" => {
            assert!(!options.is_dynamic);
            self.loaded_bar = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"console.log('Hello, world!')".to_vec().into(),
              }))
            })
          }
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader { loaded_bar: false };
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &mut loader,
        Default::default(),
      )
      .await;
    assert!(loader.loaded_bar);
  }

  #[tokio::test]
  async fn dependency_imports() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.ts" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"
                /// <reference path='file:///bar.ts' />
                /// <reference types='file:///bar.ts' />
                /* @jsxImportSource file:///bar.ts */
                import 'file:///bar.ts';
                await import('file:///bar.ts');
                await import('file:///bar.ts', { assert: eval('') });
                import 'file:///baz.json' assert { type: 'json' };
                import type {} from 'file:///bar.ts';
                /** @typedef { import('file:///bar.ts') } bar */
              "
              .to_vec()
              .into(),
            }))
          }),
          "file:///bar.ts" => {
            assert!(!options.is_dynamic);
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"".to_vec().into(),
              }))
            })
          }
          "file:///baz.json" => {
            assert!(!options.is_dynamic);
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"{}".to_vec().into(),
              }))
            })
          }
          _ => unreachable!(),
        }
      }
    }
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.ts").unwrap()],
        &mut TestLoader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    let module = graph.get(&Url::parse("file:///foo.ts").unwrap()).unwrap();
    let module = module.js().unwrap();
    let dependency_a = module.dependencies.get("file:///bar.ts").unwrap();
    let dependency_b = module.dependencies.get("file:///baz.json").unwrap();
    assert_eq!(
      dependency_a.imports,
      vec![
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::TsReferencePath,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 1,
              character: 36
            },
            end: Position {
              line: 1,
              character: 52,
            },
          },
          is_dynamic: false,
          attributes: ImportAttributes::None,
        },
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::TsReferenceTypes,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 2,
              character: 37,
            },
            end: Position {
              line: 2,
              character: 53,
            },
          },
          is_dynamic: false,
          attributes: ImportAttributes::None,
        },
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::Es,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 4,
              character: 23,
            },
            end: Position {
              line: 4,
              character: 39,
            },
          },
          is_dynamic: false,
          attributes: ImportAttributes::None,
        },
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::Es,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 5,
              character: 29,
            },
            end: Position {
              line: 5,
              character: 45,
            },
          },
          is_dynamic: true,
          attributes: ImportAttributes::None,
        },
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::Es,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 6,
              character: 29,
            },
            end: Position {
              line: 6,
              character: 45,
            },
          },
          is_dynamic: true,
          attributes: ImportAttributes::Unknown,
        },
        Import {
          specifier: "file:///bar.ts".to_string(),
          kind: ImportKind::TsType,
          range: Range {
            specifier: Url::parse("file:///foo.ts").unwrap(),
            start: Position {
              line: 8,
              character: 36,
            },
            end: Position {
              line: 8,
              character: 52,
            },
          },
          is_dynamic: false,
          attributes: ImportAttributes::None,
        },
      ]
    );
    assert_eq!(
      dependency_b.imports,
      vec![Import {
        specifier: "file:///baz.json".to_string(),
        kind: ImportKind::Es,
        range: Range {
          specifier: Url::parse("file:///foo.ts").unwrap(),
          start: Position {
            line: 7,
            character: 23,
          },
          end: Position {
            line: 7,
            character: 41,
          },
        },
        is_dynamic: false,
        attributes: ImportAttributes::Known(HashMap::from_iter(vec![(
          "type".to_string(),
          ImportAttribute::Known("json".to_string())
        )])),
      },]
    );
  }

  #[tokio::test]
  async fn fast_check_dts() {
    let mut exports = IndexMap::new();
    exports.insert(".".to_string(), "./foo.ts".to_string());

    let workspace_members = vec![WorkspaceMember {
      base: Url::parse("file:///").unwrap(),
      exports: exports.clone(),
      nv: PackageNv::from_str("@foo/bar@1.0.0").unwrap(),
    }];
    let mut test_loader = MemoryLoader::default();
    test_loader.add_source_with_text(
      "file:///foo.ts",
      "
    export function add(a: number, b: number): number {
      return a + b;
    }
  ",
    );
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.ts").unwrap()],
        &mut test_loader,
        BuildOptions {
          workspace_members: &workspace_members,
          ..Default::default()
        },
      )
      .await;
    graph.build_fast_check_type_graph(BuildFastCheckTypeGraphOptions {
      fast_check_cache: None,
      fast_check_dts: true,
      workspace_fast_check: WorkspaceFastCheckOption::Enabled(
        &workspace_members,
      ),
      ..Default::default()
    });
    graph.valid().unwrap();
    let module = graph.get(&Url::parse("file:///foo.ts").unwrap()).unwrap();
    let module = module.js().unwrap();
    let FastCheckTypeModuleSlot::Module(fsm) =
      module.fast_check.clone().unwrap()
    else {
      unreachable!();
    };
    let dts = fsm.dts.unwrap();
    assert_eq!(
      dts.text.to_string().trim(),
      "export function add(a: number, b: number): number;"
    );
    assert!(dts.diagnostics.is_empty());
  }

  #[tokio::test]
  async fn fast_check_external() {
    let mut exports = IndexMap::new();
    exports.insert(".".to_string(), "./foo.ts".to_string());

    let workspace_members = vec![WorkspaceMember {
      base: Url::parse("file:///").unwrap(),
      exports: exports.clone(),
      nv: PackageNv::from_str("@foo/bar@1.0.0").unwrap(),
    }];
    let mut test_loader = MemoryLoader::default();
    test_loader.add_source_with_text(
      "file:///foo.ts",
      "export * from 'jsr:@package/foo';",
    );
    test_loader.add_jsr_package_info(
      "@package/foo",
      &JsrPackageInfo {
        versions: HashMap::from([(
          Version::parse_standard("1.0.0").unwrap(),
          JsrPackageInfoVersion::default(),
        )]),
      },
    );
    test_loader.add_jsr_version_info(
      "@package/foo",
      "1.0.0",
      &JsrPackageVersionInfo {
        exports: json!({ ".": "./mod.ts" }),
        module_graph: None,
        manifest: Default::default(),
      },
    );
    test_loader.add_external_source("https://jsr.io/@package/foo/1.0.0/mod.ts");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.ts").unwrap()],
        &mut test_loader,
        BuildOptions {
          workspace_members: &workspace_members,
          ..Default::default()
        },
      )
      .await;
    graph.build_fast_check_type_graph(BuildFastCheckTypeGraphOptions {
      fast_check_cache: None,
      fast_check_dts: true,
      workspace_fast_check: WorkspaceFastCheckOption::Enabled(
        &workspace_members,
      ),
      ..Default::default()
    });
    graph.valid().unwrap();
    {
      let module = graph.get(&Url::parse("file:///foo.ts").unwrap()).unwrap();
      let FastCheckTypeModuleSlot::Module(fsm) =
        module.js().unwrap().fast_check.clone().unwrap()
      else {
        unreachable!();
      };
      let dts = fsm.dts.unwrap();
      assert_eq!(
        dts.text.to_string().trim(),
        "export * from 'jsr:@package/foo';"
      );
      assert!(dts.diagnostics.is_empty());
    }

    let module = graph
      .get(&Url::parse("https://jsr.io/@package/foo/1.0.0/mod.ts").unwrap())
      .unwrap();
    assert!(module.external().is_some());
  }
}
