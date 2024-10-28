// Copyright 2018-2024 the Deno authors. MIT license.

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
use crate::jsr::JsrMetadataStore;
use crate::jsr::JsrMetadataStoreServices;
use crate::jsr::PendingJsrPackageVersionInfoLoadItem;
use crate::jsr::PendingResult;
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
use crate::rt::Executor;

use crate::source::*;

use deno_ast::dep::ImportAttributes;
use deno_ast::dep::StaticDependencyKind;
use deno_ast::LineAndColumnIndex;
use deno_ast::MediaType;
use deno_ast::ParseDiagnostic;
use deno_ast::SourcePos;
use deno_ast::SourceTextInfo;
use deno_semver::jsr::JsrDepPackageReq;
use deno_semver::jsr::JsrPackageNvReference;
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
use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::FutureExt;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::sync::Arc;
use thiserror::Error;
use url::Url;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
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
  pub fn new(line: usize, character: usize) -> Self {
    Self { line, character }
  }

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

#[derive(Debug, Clone, Error)]
pub enum JsrLoadError {
  #[error(
    "Unsupported checksum in JSR package manifest. Maybe try upgrading deno?"
  )]
  UnsupportedManifestChecksum,
  #[error(transparent)]
  ContentChecksumIntegrity(ChecksumIntegrityError),
  #[error("Loader should never return an external specifier for a jsr: specifier content load.")]
  ContentLoadExternalSpecifier,
  #[error(transparent)]
  ContentLoad(Arc<anyhow::Error>),
  #[error("JSR package manifest for '{}' failed to load. {:#}", .0, .1)]
  PackageManifestLoad(String, Arc<anyhow::Error>),
  #[error("JSR package not found: {}", .0)]
  PackageNotFound(String),
  #[error("JSR package version not found: {}", .0)]
  PackageVersionNotFound(PackageNv),
  #[error("JSR package version manifest for '{}' failed to load: {:#}", .0, .1)]
  PackageVersionManifestLoad(PackageNv, Arc<anyhow::Error>),
  #[error("JSR package version manifest for '{}' failed to load: {:#}", .0, .1)]
  PackageVersionManifestChecksumIntegrity(PackageNv, ChecksumIntegrityError),
  #[error(transparent)]
  PackageFormat(JsrPackageFormatError),
  #[error("Could not find version of '{}' that matches specified version constraint '{}'", .0.name, .0.version_req)]
  PackageReqNotFound(PackageReq),
  #[error("Redirects in the JSR registry are not supported (redirected to '{}')", .0)]
  RedirectInPackage(ModuleSpecifier),
  #[error("Unknown export '{}' for '{}'.\n  Package exports:\n{}", export_name, .nv, .exports.iter().map(|e| format!(" * {}", e)).collect::<Vec<_>>().join("\n"))]
  UnknownExport {
    nv: PackageNv,
    export_name: String,
    exports: Vec<String>,
  },
}

#[derive(Error, Debug, Clone)]
pub enum JsrPackageFormatError {
  #[error(transparent)]
  JsrPackageParseError(PackageReqReferenceParseError),
  #[error("Version tag not supported in jsr specifiers.")]
  VersionTagNotSupported,
}

#[derive(Debug, Clone, Error)]
pub enum NpmLoadError {
  #[error("npm specifiers are not supported in this environment")]
  NotSupportedEnvironment,
  #[error(transparent)]
  PackageReqResolution(Arc<anyhow::Error>),
  #[error(transparent)]
  PackageReqReferenceParse(PackageReqReferenceParseError),
  #[error(transparent)]
  RegistryInfo(Arc<anyhow::Error>),
}

#[derive(Debug, Error, Clone)]
pub enum ModuleLoadError {
  #[error(transparent)]
  HttpsChecksumIntegrity(ChecksumIntegrityError),
  #[error(transparent)]
  Decode(Arc<std::io::Error>),
  #[error(transparent)]
  Loader(Arc<anyhow::Error>),
  #[error(transparent)]
  Jsr(#[from] JsrLoadError),
  #[error(transparent)]
  NodeUnknownBuiltinModule(#[from] UnknownBuiltInNodeModuleError),
  #[error(transparent)]
  Npm(#[from] NpmLoadError),
  #[error("Too many redirects.")]
  TooManyRedirects,
}

#[derive(Debug, Clone)]
pub enum ModuleError {
  LoadingErr(ModuleSpecifier, Option<Range>, ModuleLoadError),
  Missing(ModuleSpecifier, Option<Range>),
  MissingDynamic(ModuleSpecifier, Range),
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
      | Self::InvalidTypeAssertion { specifier: s, .. }
      | Self::UnsupportedImportAttributeType { specifier: s, .. } => s,
    }
  }

  pub fn maybe_referrer(&self) -> Option<&Range> {
    match self {
      Self::LoadingErr(_, maybe_referrer, _) => maybe_referrer.as_ref(),
      Self::Missing(_, maybe_referrer) => maybe_referrer.as_ref(),
      Self::MissingDynamic(_, range) => Some(range),
      Self::UnsupportedMediaType(_, _, maybe_referrer) => {
        maybe_referrer.as_ref()
      }
      Self::ParseErr { .. } => None,
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
      Self::LoadingErr(_, _, err) => Some(err),
      Self::Missing { .. }
      | Self::MissingDynamic { .. }
      | Self::ParseErr { .. }
      | Self::UnsupportedMediaType { .. }
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
      Self::UnsupportedMediaType(specifier, MediaType::Json, ..) => write!(f, "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import attribute with the type of \"json\".\n  Specifier: {specifier}"),
      Self::UnsupportedMediaType(specifier, MediaType::Cjs | MediaType::Cts, ..) if specifier.scheme() != "file" => write!(f, "Remote CJS modules are not supported.\n  Specifier: {specifier}"),
      Self::UnsupportedMediaType(specifier, media_type, ..) => write!(f, "Expected a JavaScript or TypeScript module, but identified a {media_type} module. Importing these types of modules is currently not supported.\n  Specifier: {specifier}"),
      Self::Missing(specifier, _) => write!(f, "Module not found \"{specifier}\"."),
      Self::MissingDynamic(specifier, _) => write!(f, "Dynamic import not found \"{specifier}\"."),
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
  InvalidJsrHttpsTypesImport {
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
      | Self::InvalidJsrHttpsTypesImport { range, .. }
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
      Self::InvalidDowngrade { .. }
      | Self::InvalidJsrHttpsTypesImport { .. }
      | Self::InvalidLocalImport { .. } => None,
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
        Self::InvalidJsrHttpsTypesImport {
          specifier: a,
          range: a_range,
          ..
        },
        Self::InvalidJsrHttpsTypesImport {
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
      Self::InvalidJsrHttpsTypesImport { specifier, .. } => write!(f, "Importing JSR packages via HTTPS specifiers for type checking is not supported for performance reasons. If you would like types, import via a `jsr:` specifier instead or else use a non-statically analyzable dynamic import.\n  Importing: {specifier}"),
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Import {
  pub specifier: String,
  #[serde(skip_serializing_if = "ImportKind::is_es")]
  pub kind: ImportKind,
  #[serde(rename = "range")]
  pub specifier_range: Range,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  // Don't include attributes in `deno info --json` until someone has a need.
  // Attribute error strings eventually will be included in a separate `Import::errors`, however.
  #[serde(skip_serializing)]
  pub attributes: ImportAttributes,
}

#[derive(Debug, Default, Clone, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
  #[serde(rename = "code", skip_serializing_if = "Resolution::is_none")]
  pub maybe_code: Resolution,
  #[serde(rename = "type", skip_serializing_if = "Resolution::is_none")]
  pub maybe_type: Resolution,
  #[serde(skip_serializing)]
  pub maybe_deno_types_specifier: Option<String>,
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
      if import.specifier_range.includes(position) {
        return Some(&import.specifier_range);
      }
    }
    // `@deno-types` directives won't be associated with an import.
    if let Some(range) = self.maybe_type.includes(position) {
      return Some(range);
    }
    None
  }

  pub fn with_new_resolver(
    &self,
    specifier: &str,
    jsr_url_provider: &dyn JsrUrlProvider,
    maybe_resolver: Option<&dyn Resolver>,
    maybe_npm_resolver: Option<&dyn NpmResolver>,
  ) -> Self {
    let maybe_code = self
      .maybe_code
      .maybe_range()
      .map(|r| {
        resolve(
          specifier,
          r.clone(),
          ResolutionMode::Execution,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        )
      })
      .unwrap_or_default();
    let maybe_type = self
      .maybe_type
      .maybe_range()
      .map(|r| {
        resolve(
          self
            .maybe_deno_types_specifier
            .as_deref()
            .unwrap_or(specifier),
          r.clone(),
          ResolutionMode::Types,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        )
      })
      .unwrap_or_default();
    Self {
      maybe_code,
      maybe_type,
      maybe_deno_types_specifier: self.maybe_deno_types_specifier.clone(),
      is_dynamic: self.is_dynamic,
      maybe_attribute_type: self.maybe_attribute_type.clone(),
      imports: self.imports.clone(),
    }
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TypesDependency {
  pub specifier: String,
  pub dependency: Resolution,
}

impl TypesDependency {
  pub fn with_new_resolver(
    &self,
    jsr_url_provider: &dyn JsrUrlProvider,
    maybe_resolver: Option<&dyn Resolver>,
    maybe_npm_resolver: Option<&dyn NpmResolver>,
  ) -> Self {
    let dependency = self
      .dependency
      .maybe_range()
      .map(|r| {
        resolve(
          &self.specifier,
          r.clone(),
          ResolutionMode::Types,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        )
      })
      .unwrap_or_default();
    Self {
      specifier: self.specifier.clone(),
      dependency,
    }
  }
}

fn is_media_type_unknown(media_type: &MediaType) -> bool {
  matches!(media_type, MediaType::Unknown)
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceMember {
  pub base: Url,
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub version: Option<Version>,
  pub exports: IndexMap<String, String>,
}

impl WorkspaceMember {
  pub fn as_nv(&self) -> PackageNv {
    PackageNv {
      name: self.name.clone(),
      version: self
        .version
        .clone()
        // use a dummy version
        .unwrap_or_else(|| Version::parse_standard("0.0.0").unwrap()),
    }
  }
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
  pub source_map: Arc<str>,
  #[cfg(feature = "fast_check")]
  pub dts: Option<FastCheckDtsModule>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JsModule {
  #[serde(skip_serializing)]
  pub is_script: bool,
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
      is_script: false,
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
    jsr_url_provider: &dyn JsrUrlProvider,
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
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        );
        (
          import,
          Dependency {
            is_dynamic: false,
            maybe_code: Resolution::None,
            maybe_type,
            maybe_deno_types_specifier: None,
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
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
  pub module_parser: Option<&'a dyn crate::EsParser>,
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
  pub executor: &'a dyn Executor,
  pub locker: Option<&'a mut dyn Locker>,
  pub file_system: &'a dyn FileSystem,
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
  /// Whether to pass through JSR specifiers to the resolver instead of
  /// resolving them. This is useful in cases where you want to mark JSR
  /// specifiers as external.
  pub passthrough_jsr_specifiers: bool,
  pub module_analyzer: &'a dyn ModuleAnalyzer,
  pub npm_resolver: Option<&'a dyn NpmResolver>,
  pub reporter: Option<&'a dyn Reporter>,
  pub resolver: Option<&'a dyn Resolver>,
}

#[derive(Debug, Copy, Clone)]
pub enum ModuleEntryRef<'a> {
  Module(&'a Module),
  Err(&'a ModuleError),
  Redirect(&'a ModuleSpecifier),
}

#[derive(Debug, Clone)]
pub struct WalkOptions {
  /// Whether to walk js modules when `kind` is `GraphKind::TypesOnly`.
  pub check_js: bool,
  pub follow_dynamic: bool,
  /// Part of the graph to walk.
  pub kind: GraphKind,
  /// If the fast check module graph should be preferred
  /// to walk over walking all modules.
  ///
  /// For example, when this encounters a package with fast
  /// check modules, then it will only walk the fast checked
  /// modules and not the rest of the graph.
  pub prefer_fast_check_graph: bool,
}

pub struct FillFromLockfileOptions<
  'a,
  TRedirectIter: Iterator<Item = (&'a str, &'a str)>,
  TPackageSpecifiersIter: Iterator<Item = (&'a JsrDepPackageReq, &'a str)>,
> {
  pub redirects: TRedirectIter,
  pub package_specifiers: TPackageSpecifiersIter,
}

pub struct ModuleEntryIterator<'a> {
  graph: &'a ModuleGraph,
  seen: HashSet<&'a ModuleSpecifier>,
  visiting: VecDeque<&'a ModuleSpecifier>,
  follow_dynamic: bool,
  kind: GraphKind,
  check_js: bool,
  prefer_fast_check_graph: bool,
  previous_module: Option<ModuleEntryRef<'a>>,
}

impl<'a> ModuleEntryIterator<'a> {
  fn new(
    graph: &'a ModuleGraph,
    roots: impl Iterator<Item = &'a ModuleSpecifier>,
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
      if options.kind.include_types() {
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
      kind: options.kind,
      check_js: options.check_js,
      prefer_fast_check_graph: options.prefer_fast_check_graph,
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

  /// Gets if the specified media type can be type checked.
  fn is_checkable(&self, media_type: MediaType) -> bool {
    self.check_js
      || !matches!(
        media_type,
        MediaType::JavaScript
          | MediaType::Mjs
          | MediaType::Cjs
          | MediaType::Jsx
      )
  }
}

impl<'a> Iterator for ModuleEntryIterator<'a> {
  type Item = (&'a ModuleSpecifier, ModuleEntryRef<'a>);

  fn next(&mut self) -> Option<Self::Item> {
    match self.previous_module.take() {
      Some(ModuleEntryRef::Module(module)) => match module {
        Module::Js(module) => {
          let check_types =
            self.kind.include_types() && self.is_checkable(module.media_type);
          let module_deps = if check_types && self.prefer_fast_check_graph {
            module.dependencies_prefer_fast_check()
          } else {
            &module.dependencies
          };
          for dep in module_deps.values().rev() {
            if !dep.is_dynamic || self.follow_dynamic {
              let mut resolutions = Vec::with_capacity(2);
              resolutions.push(&dep.maybe_code);
              if self.kind.include_types() {
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
              if let Module::Js(module) = &module {
                if self.kind.include_types() {
                  if let Some(Resolution::Ok(resolved)) = module
                    .maybe_types_dependency
                    .as_ref()
                    .map(|d| &d.dependency)
                  {
                    let specifier = &resolved.specifier;
                    if self.seen.insert(specifier) {
                      self.visiting.push_front(specifier);
                    }
                    if self.kind == GraphKind::TypesOnly {
                      continue; // skip visiting the code module
                    }
                  } else if self.kind == GraphKind::TypesOnly
                    && !self.is_checkable(module.media_type)
                  {
                    continue; // skip visiting
                  }
                }
              }
              break (specifier, ModuleEntryRef::Module(module));
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
            self.iterator.graph.module_slots.get(resolved_specifier);
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
      let kind = self.iterator.kind;
      let follow_dynamic = self.iterator.follow_dynamic;
      let prefer_fast_check_graph = self.iterator.prefer_fast_check_graph;

      if let Some((_, module_entry)) = self.iterator.next() {
        match module_entry {
          ModuleEntryRef::Module(Module::Js(module)) => {
            if kind.include_types() {
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

            let check_types = kind.include_types()
              && self.iterator.is_checkable(module.media_type);
            let module_deps = if check_types && prefer_fast_check_graph {
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
  pub roots: IndexSet<ModuleSpecifier>,
  #[serde(rename = "modules")]
  #[serde(serialize_with = "serialize_module_slots")]
  pub(crate) module_slots: BTreeMap<ModuleSpecifier, ModuleSlot>,
  #[serde(skip_serializing_if = "IndexMap::is_empty")]
  #[serde(serialize_with = "serialize_graph_imports")]
  pub imports: IndexMap<ModuleSpecifier, GraphImport>,
  pub redirects: BTreeMap<ModuleSpecifier, ModuleSpecifier>,
  #[serde(skip_serializing)]
  pub npm_packages: IndexSet<PackageNv>,
  #[serde(skip_serializing)]
  pub has_node_specifier: bool,
  #[serde(rename = "packages")]
  #[serde(skip_serializing_if = "PackageSpecifiers::is_empty")]
  pub packages: PackageSpecifiers,
  /// The result of resolving all npm dependencies of non-dynamic
  /// npm specifiers in the graph.
  #[serde(skip_serializing)]
  pub npm_dep_graph_result: Result<(), Arc<anyhow::Error>>,
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
      npm_dep_graph_result: Ok(()),
    }
  }

  pub fn graph_kind(&self) -> GraphKind {
    self.graph_kind
  }

  /// Fills the upfront information (redirects and JSR specifiers) from
  /// the lockfile into the graph.
  pub fn fill_from_lockfile<
    'a,
    TRedirectIter: Iterator<Item = (&'a str, &'a str)>,
    TPackageSpecifiersIter: Iterator<Item = (&'a JsrDepPackageReq, &'a str)>,
  >(
    &mut self,
    options: FillFromLockfileOptions<'a, TRedirectIter, TPackageSpecifiersIter>,
  ) {
    for (from, to) in options.redirects {
      if let Ok(from) = ModuleSpecifier::parse(from) {
        if let Ok(to) = ModuleSpecifier::parse(to) {
          if !matches!(from.scheme(), "file" | "npm" | "jsr") {
            self.redirects.insert(from, to);
          }
        }
      }
    }
    for (req_dep, value) in options.package_specifiers {
      match req_dep.kind {
        deno_semver::package::PackageKind::Jsr => {
          if let Ok(version) = Version::parse_standard(value) {
            self.packages.add_nv(
              req_dep.req.clone(),
              PackageNv {
                name: req_dep.req.name.clone(),
                version,
              },
            );
          }
        }
        deno_semver::package::PackageKind::Npm => {
          // ignore
        }
      }
    }
  }

  pub async fn build<'a>(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    loader: &'a dyn Loader,
    options: BuildOptions<'a>,
  ) {
    Builder::build(self, roots, loader, options).await
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
      pending_nvs.extend(workspace_members.iter().map(|n| n.as_nv()));
    }
    if pending_nvs.is_empty() {
      return;
    }

    let default_module_parser = crate::CapturingModuleAnalyzer::default();
    let root_symbol = crate::symbols::RootSymbol::new(
      self,
      options.module_parser.unwrap_or(&default_module_parser),
    );

    let modules = crate::fast_check::build_fast_check_type_graph(
      options.fast_check_cache,
      options.jsr_url_provider,
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
            options.jsr_url_provider,
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
    let roots = roots.iter().collect::<IndexSet<_>>();
    if roots.iter().all(|r| self.roots.contains(*r)) {
      // perf - do a straight clone since the roots are the same
      return self.clone();
    }

    let mut new_graph = ModuleGraph::new(self.graph_kind);
    let entries = self.walk(
      roots.iter().copied(),
      WalkOptions {
        follow_dynamic: true,
        kind: self.graph_kind,
        check_js: true,
        prefer_fast_check_graph: false,
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
    new_graph.imports.clone_from(&self.imports);
    new_graph.roots = roots.iter().map(|r| (*r).to_owned()).collect();
    new_graph.npm_packages.clone_from(&self.npm_packages);
    // todo(dsherret): it should be a bit smarter about this, but this is not terrible
    new_graph.packages.clone_from(&self.packages);
    new_graph.has_node_specifier = self.has_node_specifier;

    new_graph
  }

  /// Iterates over all the module entries in the module graph searching from the provided roots.
  pub fn walk<'a>(
    &'a self,
    roots: impl Iterator<Item = &'a ModuleSpecifier>,
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
      .get(specifier)
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
    match self.module_slots.get(specifier) {
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
  pub fn resolve<'a>(
    &'a self,
    specifier: &'a ModuleSpecifier,
  ) -> &'a ModuleSpecifier {
    const MAX_REDIRECTS: usize = 10;
    let mut redirected_specifier = specifier;
    if let Some(specifier) = self.redirects.get(specifier) {
      // only allocate if there's a redirect
      let mut seen = HashSet::with_capacity(MAX_REDIRECTS);
      seen.insert(redirected_specifier);
      seen.insert(specifier);
      redirected_specifier = specifier;
      while let Some(specifier) = self.redirects.get(redirected_specifier) {
        if !seen.insert(specifier) {
          log::warn!("An infinite loop of redirections detected.\n  Original specifier: {specifier}");
          break;
        }
        redirected_specifier = specifier;
        if seen.len() >= MAX_REDIRECTS {
          log::warn!("An excessive number of redirections detected.\n  Original specifier: {specifier}");
          break;
        }
      }
    }
    redirected_specifier
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
  pub fn resolve_dependency<'a>(
    &'a self,
    specifier: &str,
    referrer: &ModuleSpecifier,
    prefer_types: bool,
  ) -> Option<&'a ModuleSpecifier> {
    let referrer = self.resolve(referrer);
    if let Some(ModuleSlot::Module(referring_module)) =
      self.module_slots.get(referrer)
    {
      self.resolve_dependency_from_module(
        specifier,
        referring_module,
        prefer_types,
      )
    } else if let Some(graph_import) = self.imports.get(referrer) {
      let dependency = graph_import.dependencies.get(specifier)?;
      self.resolve_dependency_from_dep(dependency, prefer_types)
    } else {
      None
    }
  }

  pub fn resolve_dependency_from_module<'a>(
    &'a self,
    specifier: &str,
    referring_module: &'a Module,
    prefer_types: bool,
  ) -> Option<&'a ModuleSpecifier> {
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

  pub fn resolve_dependency_from_dep<'a>(
    &'a self,
    dependency: &'a Dependency,
    prefer_types: bool,
  ) -> Option<&'a ModuleSpecifier> {
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
    match self.module_slots.get(resolved_specifier) {
      Some(ModuleSlot::Module(Module::Js(module))) if prefer_types => {
        // check for if this module has a types dependency
        if let Some(Resolution::Ok(resolved)) = module
          .maybe_types_dependency
          .as_ref()
          .map(|d| &d.dependency)
        {
          let resolved_specifier = self.resolve(&resolved.specifier);
          if matches!(
            self.module_slots.get(resolved_specifier),
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
    match self.module_slots.get(specifier) {
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
        self.roots.iter(),
        WalkOptions {
          check_js: true,
          kind: GraphKind::CodeOnly,
          follow_dynamic: false,
          prefer_fast_check_graph: false,
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
  jsr_url_provider: &dyn JsrUrlProvider,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) -> Resolution {
  let response = if let Some(resolver) = maybe_resolver {
    resolver.resolve(specifier_text, &referrer_range, mode)
  } else {
    resolve_import(specifier_text, &referrer_range.specifier)
      .map_err(|err| err.into())
  };
  if mode.is_types() {
    if let Ok(resolved_url) = &response {
      if let Some(package_nv) = jsr_url_provider.package_url_to_nv(resolved_url)
      {
        if Some(package_nv)
          != jsr_url_provider.package_url_to_nv(&referrer_range.specifier)
        {
          return Resolution::Err(Box::new(
            ResolutionError::InvalidJsrHttpsTypesImport {
              specifier: resolved_url.clone(),
              range: referrer_range.clone(),
            },
          ));
        }
      }
    }
  }
  if let Some(npm_resolver) = maybe_npm_resolver {
    if npm_resolver.enables_bare_builtin_node_module() {
      use import_map::ImportMapError;
      use ResolveError::*;
      use SpecifierError::*;
      let res_ref = response.as_ref();
      if matches!(res_ref, Err(Specifier(ImportPrefixMissing { .. })))
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

#[derive(Clone)]
pub(crate) enum ModuleSourceAndInfo {
  Json {
    specifier: ModuleSpecifier,
    source: Arc<str>,
  },
  Js {
    specifier: ModuleSpecifier,
    media_type: MediaType,
    source: Arc<str>,
    maybe_headers: Option<HashMap<String, String>>,
    module_info: Box<ModuleInfo>,
  },
}

impl ModuleSourceAndInfo {
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::Json { specifier, .. } => specifier,
      Self::Js { specifier, .. } => specifier,
    }
  }

  pub fn media_type(&self) -> MediaType {
    match self {
      Self::Json { .. } => MediaType::Json,
      Self::Js { media_type, .. } => *media_type,
    }
  }

  pub fn source(&self) -> &str {
    match self {
      Self::Json { source, .. } => source,
      Self::Js { source, .. } => source,
    }
  }
}

pub(crate) struct ParseModuleAndSourceInfoOptions<'a> {
  pub specifier: ModuleSpecifier,
  pub maybe_headers: Option<HashMap<String, String>>,
  pub content: Arc<[u8]>,
  pub maybe_attribute_type: Option<&'a AttributeTypeWithRange>,
  pub maybe_referrer: Option<&'a Range>,
  pub is_root: bool,
  pub is_dynamic_branch: bool,
}

/// With the provided information, parse a module and return its "module slot"
#[allow(clippy::too_many_arguments)]
#[allow(clippy::result_large_err)]
pub(crate) async fn parse_module_source_and_info(
  module_analyzer: &dyn ModuleAnalyzer,
  opts: ParseModuleAndSourceInfoOptions<'_>,
) -> Result<ModuleSourceAndInfo, ModuleError> {
  let (mut media_type, maybe_charset) =
    resolve_media_type_and_charset_from_headers(
      &opts.specifier,
      opts.maybe_headers.as_ref(),
    );

  if opts.is_root && media_type == MediaType::Unknown {
    // assume javascript
    media_type = MediaType::JavaScript;
  }

  // here we check any media types that should have assertions made against them
  // if they aren't the root and add them to the graph, otherwise we continue
  if media_type == MediaType::Json
    && (opts.is_root
      || opts.is_dynamic_branch
      || matches!(
        opts.maybe_attribute_type.map(|t| t.kind.as_str()),
        Some("json")
      ))
  {
    return match crate::source::decode_source(
      &opts.specifier,
      opts.content,
      maybe_charset,
    ) {
      Ok(text) => Ok(ModuleSourceAndInfo::Json {
        specifier: opts.specifier,
        source: text,
      }),
      Err(err) => Err(ModuleError::LoadingErr(
        opts.specifier,
        None,
        ModuleLoadError::Decode(Arc::new(err)),
      )),
    };
  }

  if let Some(attribute_type) = opts.maybe_attribute_type {
    if attribute_type.kind == "json" {
      return Err(ModuleError::InvalidTypeAssertion {
        specifier: opts.specifier.clone(),
        range: attribute_type.range.clone(),
        actual_media_type: media_type,
        expected_media_type: MediaType::Json,
      });
    } else {
      return Err(ModuleError::UnsupportedImportAttributeType {
        specifier: opts.specifier,
        range: attribute_type.range.clone(),
        kind: attribute_type.kind.clone(),
      });
    }
  }

  if matches!(media_type, MediaType::Cjs | MediaType::Cts)
    && opts.specifier.scheme() != "file"
  {
    return Err(ModuleError::UnsupportedMediaType(
      opts.specifier,
      media_type,
      opts.maybe_referrer.map(|r| r.to_owned()),
    ));
  }

  // Here we check for known ES Modules that we will analyze the dependencies of
  match media_type {
    MediaType::JavaScript
    | MediaType::Mjs
    | MediaType::Jsx
    | MediaType::TypeScript
    | MediaType::Mts
    | MediaType::Tsx
    | MediaType::Cjs
    | MediaType::Cts
    | MediaType::Dts
    | MediaType::Dmts
    | MediaType::Dcts => {
      let source =
        new_source_with_text(&opts.specifier, opts.content, maybe_charset)
          .map_err(|err| *err)?;
      match module_analyzer
        .analyze(&opts.specifier, source.clone(), media_type)
        .await
      {
        Ok(module_info) => {
          // Return the module as a valid module
          Ok(ModuleSourceAndInfo::Js {
            specifier: opts.specifier,
            media_type,
            source,
            maybe_headers: opts.maybe_headers,
            module_info: Box::new(module_info),
          })
        }
        Err(diagnostic) => {
          Err(ModuleError::ParseErr(opts.specifier, diagnostic))
        }
      }
    }
    MediaType::Css
    | MediaType::Json
    | MediaType::Wasm
    | MediaType::SourceMap
    | MediaType::Unknown => Err(ModuleError::UnsupportedMediaType(
      opts.specifier,
      media_type,
      opts.maybe_referrer.map(|r| r.to_owned()),
    )),
  }
}

pub(crate) struct ParseModuleOptions {
  pub graph_kind: GraphKind,
  pub module_source_and_info: ModuleSourceAndInfo,
}

/// With the provided information, parse a module and return its "module slot"
#[allow(clippy::result_large_err)]
pub(crate) fn parse_module(
  file_system: &dyn FileSystem,
  jsr_url_provider: &dyn JsrUrlProvider,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
  options: ParseModuleOptions,
) -> Result<Module, ModuleError> {
  match options.module_source_and_info {
    ModuleSourceAndInfo::Json { specifier, source } => {
      Ok(Module::Json(JsonModule {
        maybe_cache_info: None,
        source,
        media_type: MediaType::Json,
        specifier,
      }))
    }
    ModuleSourceAndInfo::Js {
      specifier,
      media_type,
      source,
      maybe_headers,
      module_info,
    } => Ok(Module::Js(parse_js_module_from_module_info(
      options.graph_kind,
      specifier,
      media_type,
      maybe_headers.as_ref(),
      *module_info,
      source,
      file_system,
      jsr_url_provider,
      maybe_resolver,
      maybe_npm_resolver,
    ))),
  }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn parse_js_module_from_module_info(
  graph_kind: GraphKind,
  specifier: ModuleSpecifier,
  media_type: MediaType,
  maybe_headers: Option<&HashMap<String, String>>,
  module_info: ModuleInfo,
  source: Arc<str>,
  file_system: &dyn FileSystem,
  jsr_url_provider: &dyn JsrUrlProvider,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) -> JsModule {
  let mut module = JsModule::new(specifier, source);
  module.is_script = module_info.is_script;
  module.media_type = media_type;

  // Analyze the TypeScript triple-slash references and self types specifier
  if graph_kind.include_types() {
    if let Some(specifier) = module_info.self_types_specifier.as_ref() {
      let range =
        Range::from_position_range(module.specifier.clone(), specifier.range);
      module.maybe_types_dependency = Some(TypesDependency {
        specifier: specifier.text.clone(),
        dependency: resolve(
          &specifier.text,
          range.clone(),
          ResolutionMode::Types,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        ),
      });
    }

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
              jsr_url_provider,
              maybe_resolver,
              maybe_npm_resolver,
            );
          }
          dep.imports.push(Import {
            specifier: specifier.text,
            kind: ImportKind::TsReferencePath,
            specifier_range: range,
            is_dynamic: false,
            attributes: Default::default(),
          });
        }
        TypeScriptReference::Types(specifier) => {
          let is_untyped = !module.media_type.is_typed();
          if is_untyped && module.maybe_types_dependency.is_some() {
            continue; // early exit if we already have a types dependency
          }
          let range = Range::from_position_range(
            module.specifier.clone(),
            specifier.range,
          );
          let dep_resolution = resolve(
            &specifier.text,
            range.clone(),
            ResolutionMode::Types,
            jsr_url_provider,
            maybe_resolver,
            maybe_npm_resolver,
          );
          if is_untyped {
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
              specifier_range: range,
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
  //
  // Additionally we may augment the JSX import source dependency with a type
  // dependency. This happens as follows:
  // 1. Check if a JSX import source dependency was injected and it is untyped.
  // 2. If the file has a @jsxImportSourceTypes pragma, use that as the import
  //    source types.
  // 3. If the JSX import source was not set through the @jsxImportSource
  //    pragma and the resolver has a default JSX import source types, use
  //    that as the import source types.
  // 4. If none of the above are true, do not inject a type dependency.
  //
  // This means that a default JSX import source types will not be used if the
  // import source was set by the @jsxImportSource pragma. This is done to
  // prevent a default import source types from being injected when the user
  // has explicitly overridden the import source in the file.
  if matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    let has_jsx_import_source_pragma = module_info.jsx_import_source.is_some();
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
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        );
      }
      if graph_kind.include_types() && dep.maybe_type.is_none() {
        let mut types_res = module_info.jsx_import_source_types;
        if types_res.is_none() && !has_jsx_import_source_pragma {
          types_res = maybe_resolver.and_then(|r| {
            r.default_jsx_import_source_types().map(|import_source| {
              SpecifierWithRange {
                text: import_source,
                range: PositionRange {
                  start: Position::zeroed(),
                  end: Position::zeroed(),
                },
              }
            })
          });
        }
        if let Some(import_source_types) = types_res {
          let specifier_text = format!(
            "{}/{}",
            import_source_types.text, jsx_import_source_module
          );
          let range = Range::from_position_range(
            module.specifier.clone(),
            import_source_types.range,
          );
          dep.maybe_type = resolve(
            &specifier_text,
            range,
            ResolutionMode::Types,
            jsr_url_provider,
            maybe_resolver,
            maybe_npm_resolver,
          );
          dep.maybe_deno_types_specifier = Some(specifier_text);
        } else {
          let types_resolution = resolve(
            &specifier_text,
            range.clone(),
            ResolutionMode::Types,
            jsr_url_provider,
            maybe_resolver,
            maybe_npm_resolver,
          );
          if types_resolution.maybe_specifier()
            != dep.maybe_code.maybe_specifier()
          {
            dep.maybe_type = types_resolution;
          }
        }
      }
      dep.imports.push(Import {
        specifier: specifier_text,
        kind: ImportKind::JsxImportSource,
        specifier_range: range,
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
      let specifier_range =
        Range::from_position_range(module.specifier.clone(), specifier.range);
      if dep.maybe_type.is_none() {
        dep.maybe_type = resolve(
          &specifier.text,
          specifier_range.clone(),
          ResolutionMode::Types,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        );
      }
      dep.imports.push(Import {
        specifier: specifier.text,
        kind: ImportKind::JsDoc,
        specifier_range,
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
            jsr_url_provider,
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
      && !module.media_type.is_typed()
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
    jsr_url_provider,
    maybe_resolver,
    maybe_npm_resolver,
  );

  // Return the module as a valid module
  module
}

#[allow(clippy::too_many_arguments)]
fn fill_module_dependencies(
  graph_kind: GraphKind,
  dependencies: Vec<DependencyDescriptor>,
  module_specifier: &ModuleSpecifier,
  module_dependencies: &mut IndexMap<String, Dependency>,
  file_system: &dyn FileSystem,
  jsr_url_provider: &dyn JsrUrlProvider,
  maybe_resolver: Option<&dyn Resolver>,
  maybe_npm_resolver: Option<&dyn NpmResolver>,
) {
  for desc in dependencies {
    let (imports, types_specifier) = match desc {
      DependencyDescriptor::Static(desc) => {
        let is_import_or_export_type = matches!(
          desc.kind,
          StaticDependencyKind::ImportType | StaticDependencyKind::ExportType
        );
        if is_import_or_export_type && !graph_kind.include_types() {
          continue; // skip
        }
        let specifier_range = Range::from_position_range(
          module_specifier.clone(),
          desc.specifier_range,
        );
        (
          vec![Import {
            specifier: desc.specifier,
            kind: match is_import_or_export_type {
              true => ImportKind::TsType,
              false => ImportKind::Es,
            },
            specifier_range,
            is_dynamic: false,
            attributes: desc.import_attributes,
          }],
          desc.types_specifier,
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
        let specifier_range = Range::from_position_range(
          module_specifier.clone(),
          desc.argument_range,
        );
        (
          specifiers
            .into_iter()
            .map(|specifier| Import {
              specifier,
              kind: ImportKind::Es,
              specifier_range: specifier_range.clone(),
              is_dynamic: true,
              attributes: import_attributes.clone(),
            })
            .collect::<Vec<_>>(),
          desc.types_specifier,
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

      if let Some(types_specifier) = &types_specifier {
        if graph_kind.include_types() && dep.maybe_type.is_none() {
          dep.maybe_deno_types_specifier = Some(types_specifier.text.clone());
          dep.maybe_type = resolve(
            &types_specifier.text,
            Range::from_position_range(
              module_specifier.clone(),
              types_specifier.range,
            ),
            ResolutionMode::Types,
            jsr_url_provider,
            maybe_resolver,
            maybe_npm_resolver,
          );
        }
      }
      if import.kind == ImportKind::TsType {
        if dep.maybe_type.is_none() {
          dep.maybe_type = resolve(
            &import.specifier,
            import.specifier_range.clone(),
            ResolutionMode::Types,
            jsr_url_provider,
            maybe_resolver,
            maybe_npm_resolver,
          );
        }
      } else if dep.maybe_code.is_none() {
        // This is a code import, the first one of that specifier in this module.
        // Resolve and determine the initial `is_dynamic` value from it.
        dep.maybe_code = resolve(
          &import.specifier,
          import.specifier_range.clone(),
          ResolutionMode::Execution,
          jsr_url_provider,
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
        let maybe_type = resolve(
          &import.specifier,
          import.specifier_range.clone(),
          ResolutionMode::Types,
          jsr_url_provider,
          maybe_resolver,
          maybe_npm_resolver,
        );
        // only bother setting if the resolved specifier
        // does not match the code specifier
        if maybe_type.maybe_specifier() != dep.maybe_code.maybe_specifier() {
          dep.maybe_type = maybe_type
        }
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
    is_root: bool,
  },
  Module {
    specifier: ModuleSpecifier,
    module_source_and_info: ModuleSourceAndInfo,
    pending_load: Option<Box<(LoaderChecksum, ModuleInfo)>>,
    is_root: bool,
  },
  Redirect {
    count: usize,
    specifier: ModuleSpecifier,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    is_dynamic: bool,
    is_root: bool,
  },
}

impl PendingInfoResponse {
  fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::External { specifier, .. } => specifier,
      Self::Module {
        module_source_and_info,
        ..
      } => module_source_and_info.specifier(),
      Self::Redirect { specifier, .. } => specifier,
    }
  }
}

#[derive(Debug, Clone)]
struct JsrPackageVersionInfoExt {
  base_url: Url,
  inner: Arc<JsrPackageVersionInfo>,
}

impl JsrPackageVersionInfoExt {
  pub fn get_subpath<'a>(&self, specifier: &'a Url) -> Option<&'a str> {
    let base_url = self.base_url.as_str();
    let base_url = base_url.strip_suffix('/').unwrap_or(base_url);
    specifier.as_str().strip_prefix(base_url)
  }

  pub fn get_checksum(&self, sub_path: &str) -> Result<&str, ModuleLoadError> {
    match self.inner.manifest.get(sub_path) {
      Some(manifest_entry) => {
        match manifest_entry.checksum.strip_prefix("sha256-") {
          Some(checksum) => Ok(checksum),
          None => Err(ModuleLoadError::Jsr(
            JsrLoadError::UnsupportedManifestChecksum,
          )),
        }
      }
      // If the checksum is missing then leave it up to the loader to error
      // by providing this special checksum value. For example, someone may
      // be making modifications to their vendor folder in which case this
      // checksum will be ignored and if not, then a loading error will
      // occur about an incorrect checksum.
      None => Ok("package-manifest-missing-checksum"),
    }
  }
}

enum LoadSpecifierKind {
  Jsr(JsrPackageReqReference),
  Npm(NpmPackageReqReference),
  Node(String),
  Url,
}

struct PendingInfo {
  requested_specifier: ModuleSpecifier,
  maybe_range: Option<Range>,
  result: Result<PendingInfoResponse, ModuleError>,
  maybe_version_info: Option<JsrPackageVersionInfoExt>,
  loaded_package_via_https_url: Option<LoadedJsrPackageViaHttpsUrl>,
}

struct PendingModuleLoadItem {
  redirect_count: usize,
  requested_specifier: Url,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_range: Option<Range>,
  load_specifier: Url,
  is_dynamic: bool,
  is_root: bool,
  maybe_checksum: Option<LoaderChecksum>,
  maybe_version_info: Option<JsrPackageVersionInfoExt>,
}

struct LoadedJsrPackageViaHttpsUrl {
  nv: PackageNv,
  manifest_checksum_for_locker: Option<LoaderChecksum>,
}

type PendingInfoFuture<'a> = LocalBoxFuture<'a, PendingInfo>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct AttributeTypeWithRange {
  range: Range,
  /// The kind of attribute (ex. "json").
  kind: String,
}

#[derive(Debug, Default)]
struct PendingNpmState {
  requested_registry_info_loads: HashSet<String>,
  pending_resolutions: Vec<PendingNpmResolutionItem>,
}

#[derive(Debug)]
struct PendingJsrReqResolutionItem {
  specifier: ModuleSpecifier,
  package_ref: JsrPackageReqReference,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_range: Option<Range>,
  is_dynamic: bool,
  is_root: bool,
}

#[derive(Debug)]
struct PendingJsrNvResolutionItem {
  specifier: ModuleSpecifier,
  nv_ref: JsrPackageNvReference,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_range: Option<Range>,
  is_dynamic: bool,
  is_root: bool,
}

#[derive(Debug)]
struct PendingContentLoadItem {
  specifier: ModuleSpecifier,
  maybe_range: Option<Range>,
  result: LoadResult,
  module_info: ModuleInfo,
}

#[derive(Debug, Default)]
struct PendingJsrState {
  pending_resolutions: VecDeque<PendingJsrReqResolutionItem>,
  pending_content_loads:
    FuturesUnordered<LocalBoxFuture<'static, PendingContentLoadItem>>,
  metadata: JsrMetadataStore,
}

#[derive(Debug)]
struct PendingDynamicBranch {
  range: Range,
  maybe_attribute_type: Option<AttributeTypeWithRange>,
  maybe_version_info: Option<JsrPackageVersionInfoExt>,
}

#[derive(Debug, Default)]
struct PendingState<'a> {
  pending: FuturesOrdered<PendingInfoFuture<'a>>,
  jsr: PendingJsrState,
  npm: PendingNpmState,
  dynamic_branches: HashMap<ModuleSpecifier, PendingDynamicBranch>,
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

struct Builder<'a, 'graph> {
  in_dynamic_branch: bool,
  was_dynamic_root: bool,
  file_system: &'a dyn FileSystem,
  jsr_url_provider: &'a dyn JsrUrlProvider,
  passthrough_jsr_specifiers: bool,
  loader: &'a dyn Loader,
  locker: Option<&'a mut dyn Locker>,
  resolver: Option<&'a dyn Resolver>,
  npm_resolver: Option<&'a dyn NpmResolver>,
  module_analyzer: &'a dyn ModuleAnalyzer,
  reporter: Option<&'a dyn Reporter>,
  graph: &'graph mut ModuleGraph,
  state: PendingState<'a>,
  fill_pass_mode: FillPassMode,
  executor: &'a dyn Executor,
  resolved_roots: BTreeSet<ModuleSpecifier>,
}

impl<'a, 'graph> Builder<'a, 'graph> {
  pub async fn build(
    graph: &'graph mut ModuleGraph,
    roots: Vec<ModuleSpecifier>,
    loader: &'a dyn Loader,
    options: BuildOptions<'a>,
  ) {
    let fill_pass_mode = match graph.roots.is_empty() {
      true => FillPassMode::AllowRestart,
      false => FillPassMode::NoRestart,
    };
    let mut builder = Self {
      in_dynamic_branch: options.is_dynamic,
      was_dynamic_root: options.is_dynamic,
      file_system: options.file_system,
      jsr_url_provider: options.jsr_url_provider,
      passthrough_jsr_specifiers: options.passthrough_jsr_specifiers,
      loader,
      locker: options.locker,
      resolver: options.resolver,
      npm_resolver: options.npm_resolver,
      module_analyzer: options.module_analyzer,
      reporter: options.reporter,
      graph,
      state: PendingState::default(),
      fill_pass_mode,
      executor: options.executor,
      resolved_roots: Default::default(),
    };
    builder.fill(roots, options.imports).await;
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
      .filter(|r| !self.graph.roots.contains(*r))
      .cloned()
      .collect::<Vec<_>>();
    let imports = provided_imports
      .iter()
      .filter(|r| !self.graph.imports.contains_key(&r.referrer))
      .cloned()
      .collect::<Vec<_>>();

    self.graph.roots.extend(roots.clone());

    for root in roots {
      self.load(&root, None, self.in_dynamic_branch, true, None, None);
    }

    // process any imports that are being added to the graph.
    self.handle_provided_imports(imports);

    while !(self.state.pending.is_empty()
      && self.state.jsr.pending_resolutions.is_empty()
      && self.state.dynamic_branches.is_empty())
    {
      let specifier = match self.state.pending.next().await {
        Some(PendingInfo {
          requested_specifier,
          maybe_range,
          result,
          maybe_version_info,
          loaded_package_via_https_url,
        }) => {
          if let Some(pkg) = loaded_package_via_https_url {
            if let Some(locker) = &mut self.locker {
              if let Some(checksum) = pkg.manifest_checksum_for_locker {
                locker.set_pkg_manifest_checksum(&pkg.nv, checksum);
              }
            }
            self.graph.packages.ensure_package(pkg.nv);
          }

          match result {
            Ok(response) => {
              self.check_specifier(&requested_specifier, response.specifier());

              self.visit(
                response,
                maybe_range.clone(),
                maybe_version_info.as_ref(),
              );

              Some(requested_specifier)
            }
            Err(err) => {
              self.check_specifier(&requested_specifier, err.specifier());
              self
                .graph
                .module_slots
                .insert(err.specifier().clone(), ModuleSlot::Err(err));
              Some(requested_specifier)
            }
          }
        }
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

        // resolving jsr specifiers will load more specifiers
        if self.state.pending.is_empty() {
          self.resolve_dynamic_branches();
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
      let graph_import = GraphImport::new(
        &referrer,
        imports,
        self.jsr_url_provider,
        self.resolver,
        self.npm_resolver,
      );
      for dep in graph_import.dependencies.values() {
        if let Resolution::Ok(resolved) = &dep.maybe_type {
          self.load(
            &resolved.specifier,
            Some(&resolved.range),
            self.in_dynamic_branch,
            self.resolved_roots.contains(&resolved.specifier),
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
    let mut pending_resolutions =
      std::mem::take(&mut self.state.jsr.pending_resolutions);
    let mut pending_version_resolutions =
      Vec::with_capacity(pending_resolutions.len());
    let should_collect_top_level_nvs =
      self.graph.packages.top_level_packages().is_empty()
        && self.graph.graph_kind.include_types();
    // don't bother pre-allocating because adding to this should be rare
    let mut restarted_pkgs = HashSet::new();
    while let Some(pending_resolution) = pending_resolutions.pop_front() {
      let package_name = &pending_resolution.package_ref.req().name;
      let fut = self
        .state
        .jsr
        .metadata
        .get_package_metadata(package_name)
        .unwrap();
      match fut.await {
        Ok(info) => {
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
              pending_version_resolutions.push(PendingJsrNvResolutionItem {
                specifier: pending_resolution.specifier,
                nv_ref: JsrPackageNvReference::new(PackageNvReference {
                  nv: package_nv,
                  sub_path: pending_resolution
                    .package_ref
                    .into_inner()
                    .sub_path,
                }),
                maybe_attribute_type: pending_resolution.maybe_attribute_type,
                maybe_range: pending_resolution.maybe_range,
                is_dynamic: pending_resolution.is_dynamic,
                is_root: pending_resolution.is_root,
              });
            }
            None => {
              // Generally, prefer a full restart that cache busts if we can
              // because it will cause the meta files for users to be updated
              // more frequently. In cases like non-statically analyzable dynamic
              // branches, FillPassMode will not allow restarting, so just update
              // the single package metadata.
              if self.fill_pass_mode == FillPassMode::AllowRestart {
                return true; // restart
              } else if self.fill_pass_mode != FillPassMode::CacheBusting
                && restarted_pkgs.insert(package_name.clone())
              {
                self
                  .state
                  .jsr
                  .metadata
                  .remove_package_metadata(package_name);
                self.state.jsr.metadata.queue_load_package_info(
                  package_name,
                  CacheSetting::Reload, // force reload this specific package
                  JsrMetadataStoreServices {
                    executor: self.executor,
                    jsr_url_provider: self.jsr_url_provider,
                    loader: self.loader,
                  },
                );
                pending_resolutions.push_front(pending_resolution);
              } else {
                self.graph.module_slots.insert(
                  pending_resolution.specifier.clone(),
                  ModuleSlot::Err(ModuleError::LoadingErr(
                    pending_resolution.specifier.clone(),
                    pending_resolution.maybe_range.clone(),
                    JsrLoadError::PackageReqNotFound(package_req.clone())
                      .into(),
                  )),
                );
              }
            }
          }
        }
        Err(err) => {
          self.graph.module_slots.insert(
            pending_resolution.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              pending_resolution.specifier,
              pending_resolution.maybe_range,
              err.into(),
            )),
          );
        }
      }
    }

    // now resolve the version information
    for resolution_item in pending_version_resolutions {
      let nv = resolution_item.nv_ref.nv();
      let version_info_result = self
        .state
        .jsr
        .metadata
        .get_package_version_metadata(nv)
        .unwrap()
        .await;
      match version_info_result {
        Ok(version_info_load_item) => {
          let version_info = version_info_load_item.info;
          self.graph.packages.ensure_package(nv.clone());
          if let Some(locker) = &mut self.locker {
            if let Some(checksum) = version_info_load_item.checksum_for_locker {
              locker.set_pkg_manifest_checksum(nv, checksum);
            }
          }
          let base_url = self.jsr_url_provider.package_url(nv);
          let export_name = resolution_item.nv_ref.export_name();
          match version_info.export(&export_name) {
            Some(export_value) => {
              self.graph.packages.add_export(
                nv,
                (
                  resolution_item.nv_ref.export_name().into_owned(),
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
              if resolution_item.is_root {
                self.resolved_roots.insert(specifier.clone());
              }
              let version_info = JsrPackageVersionInfoExt {
                base_url,
                inner: version_info,
              };
              self.load(
                &specifier,
                resolution_item.maybe_range.as_ref(),
                resolution_item.is_dynamic,
                resolution_item.is_root,
                resolution_item.maybe_attribute_type,
                Some(&version_info),
              );
            }
            None => {
              self.graph.module_slots.insert(
                resolution_item.specifier.clone(),
                ModuleSlot::Err(ModuleError::LoadingErr(
                  resolution_item.specifier,
                  resolution_item.maybe_range,
                  JsrLoadError::UnknownExport {
                    export_name: export_name.to_string(),
                    nv: resolution_item.nv_ref.into_inner().nv,
                    exports: version_info
                      .exports()
                      .map(|(k, _)| k.to_string())
                      .collect::<Vec<_>>(),
                  }
                  .into(),
                )),
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
              err.into(),
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
            self.resolved_roots.contains(&specifier),
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
                ModuleSlot::Err(ModuleError::LoadingErr(
                  item.specifier,
                  item.maybe_range,
                  JsrLoadError::ContentLoadExternalSpecifier.into(),
                )),
              );
            }
            LoadResponse::Module {
              content,
              specifier,
              maybe_headers: _maybe_headers,
            } if specifier == item.specifier => {
              // fill the existing module slot with the loaded source
              let slot = self.graph.module_slots.get_mut(&specifier).unwrap();
              match slot {
                ModuleSlot::Module(module) => {
                  match module {
                    Module::Js(module) => {
                      self.loader.cache_module_info(
                        &specifier,
                        module.media_type,
                        &content,
                        &item.module_info,
                      );
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
                    Module::Npm(_) | Module::Node(_) | Module::External(_) => {
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
            }
            LoadResponse::Redirect { specifier }
            | LoadResponse::Module { specifier, .. } => {
              // redirects are not supported
              self.graph.module_slots.insert(
                item.specifier.clone(),
                ModuleSlot::Err(ModuleError::LoadingErr(
                  item.specifier,
                  item.maybe_range,
                  JsrLoadError::RedirectInPackage(specifier).into(),
                )),
              );
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
              JsrLoadError::ContentLoad(Arc::new(err)).into(),
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
      self.add_redirect(requested_specifier.clone(), specifier.clone());
    }
  }

  fn add_redirect(
    &mut self,
    requested_specifier: ModuleSpecifier,
    specifier: ModuleSpecifier,
  ) {
    debug_assert_ne!(requested_specifier, specifier);
    // remove a potentially pending redirect that will never resolve
    if let Some(slot) = self.graph.module_slots.get(&requested_specifier) {
      if matches!(slot, ModuleSlot::Pending) {
        self.graph.module_slots.remove(&requested_specifier);
      }
    }

    self
      .graph
      .redirects
      .entry(requested_specifier)
      .or_insert(specifier);
  }

  /// Enqueue a request to load the specifier via the loader.
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    maybe_range: Option<&Range>,
    is_dynamic: bool,
    is_root: bool,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) {
    self.load_with_redirect_count(
      0,
      specifier,
      maybe_range,
      is_dynamic,
      is_root,
      maybe_attribute_type,
      maybe_version_info,
    )
  }

  #[allow(clippy::too_many_arguments)]
  fn load_with_redirect_count(
    &mut self,
    redirect_count: usize,
    specifier: &ModuleSpecifier,
    maybe_range: Option<&Range>,
    is_dynamic: bool,
    is_root: bool,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) {
    struct ProvidedModuleAnalyzer(RefCell<Option<ModuleInfo>>);

    #[async_trait::async_trait(?Send)]
    impl ModuleAnalyzer for ProvidedModuleAnalyzer {
      async fn analyze(
        &self,
        _specifier: &ModuleSpecifier,
        _source: Arc<str>,
        _media_type: MediaType,
      ) -> Result<ModuleInfo, ParseDiagnostic> {
        Ok(self.0.borrow_mut().take().unwrap()) // will only be called once
      }
    }

    let original_specifier = specifier;
    let specifier = self.graph.redirects.get(specifier).unwrap_or(specifier);
    if self.graph.module_slots.contains_key(specifier) {
      // ensure any jsr/npm dependencies that we've already seen are marked
      // as a dependency of the referrer
      if matches!(original_specifier.scheme(), "jsr" | "npm") {
        if let Ok(load_specifier) =
          self.parse_load_specifier_kind(original_specifier, maybe_range)
        {
          self.maybe_mark_dep(&load_specifier, maybe_range);
        }
      }

      return;
    }

    if let Some(version_info) = maybe_version_info {
      if let Some(sub_path) = version_info.get_subpath(specifier) {
        let checksum = match version_info.get_checksum(sub_path) {
          Ok(checksum) => checksum,
          Err(err) => {
            self.graph.module_slots.insert(
              specifier.clone(),
              ModuleSlot::Err(ModuleError::LoadingErr(
                specifier.clone(),
                maybe_range.cloned(),
                err,
              )),
            );
            return;
          }
        };
        let checksum = LoaderChecksum::new(checksum.to_string());
        if let Some(module_info) = version_info.inner.module_info(sub_path) {
          // Check if this specifier is in the cache. If it is, then
          // don't use the module information as it may be out of date
          // with what's in the cache
          let fut = self.loader.load(
            specifier,
            LoadOptions {
              is_dynamic: self.in_dynamic_branch,
              was_dynamic_root: self.was_dynamic_root,
              cache_setting: CacheSetting::Only,
              maybe_checksum: Some(checksum.clone()),
            },
          );
          let is_dynamic_branch = self.in_dynamic_branch;
          let module_analyzer = self.module_analyzer;
          self.state.pending.push_back({
            let requested_specifier = specifier.clone();
            let maybe_range = maybe_range.cloned();
            let version_info = version_info.clone();
            async move {
              let response = fut.await;
              let result = match response {
                Ok(None) => {
                  parse_module_source_and_info(
                    &ProvidedModuleAnalyzer(RefCell::new(Some(
                      module_info.clone(),
                    ))),
                    ParseModuleAndSourceInfoOptions {
                      specifier: requested_specifier.clone(),
                      maybe_headers: Default::default(),
                      content: Arc::new([]) as Arc<[u8]>, // we'll load the content later
                      maybe_attribute_type: maybe_attribute_type.as_ref(),
                      maybe_referrer: maybe_range.as_ref(),
                      is_root,
                      is_dynamic_branch,
                    },
                  )
                  .await
                  .map(|module_source_and_info| {
                    PendingInfoResponse::Module {
                      specifier: requested_specifier.clone(),
                      module_source_and_info,
                      pending_load: Some(Box::new((checksum, module_info))),
                      is_root,
                    }
                  })
                }
                Ok(Some(response)) => match response {
                  LoadResponse::External { specifier } => {
                    Ok(PendingInfoResponse::External { specifier, is_root })
                  }
                  LoadResponse::Redirect { specifier } => {
                    Err(ModuleError::LoadingErr(
                      requested_specifier.clone(),
                      maybe_range.clone(),
                      JsrLoadError::RedirectInPackage(specifier).into(),
                    ))
                  }
                  LoadResponse::Module {
                    content,
                    specifier,
                    maybe_headers,
                  } => parse_module_source_and_info(
                    module_analyzer,
                    ParseModuleAndSourceInfoOptions {
                      specifier: specifier.clone(),
                      maybe_headers,
                      content,
                      maybe_attribute_type: maybe_attribute_type.as_ref(),
                      maybe_referrer: maybe_range.as_ref(),
                      is_root,
                      is_dynamic_branch,
                    },
                  )
                  .await
                  .map(|module_source_and_info| {
                    PendingInfoResponse::Module {
                      specifier: specifier.clone(),
                      module_source_and_info,
                      pending_load: None,
                      is_root,
                    }
                  }),
                },
                Err(err) => Err(ModuleError::LoadingErr(
                  requested_specifier.clone(),
                  maybe_range.clone(),
                  ModuleLoadError::Loader(Arc::new(err)),
                )),
              };
              PendingInfo {
                requested_specifier,
                maybe_range,
                result,
                maybe_version_info: Some(version_info),
                loaded_package_via_https_url: None,
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
          self.load_pending_module(PendingModuleLoadItem {
            redirect_count,
            requested_specifier: specifier.clone(),
            maybe_attribute_type,
            maybe_range: maybe_range.cloned(),
            load_specifier: specifier.clone(),
            is_dynamic,
            is_root,
            maybe_checksum: Some(checksum),
            maybe_version_info: Some(version_info.clone()),
          });
          return;
        }
      }
    }

    let specifier = specifier.clone();
    match self.parse_load_specifier_kind(&specifier, maybe_range) {
      Ok(LoadSpecifierKind::Jsr(package_req_ref)) => {
        self.mark_jsr_dep(&package_req_ref, maybe_range);
        if self.passthrough_jsr_specifiers {
          // mark external
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Module(Module::External(ExternalModule {
              specifier: specifier.clone(),
            })),
          );
        } else {
          self.load_jsr_specifier(
            specifier,
            package_req_ref,
            maybe_attribute_type,
            maybe_range,
            is_dynamic,
            is_root,
          );
        }
      }
      Ok(LoadSpecifierKind::Npm(package_req_ref)) => {
        self.mark_npm_dep(&package_req_ref, maybe_range);
        if let Some(npm_resolver) = self.npm_resolver {
          self.load_npm_specifier(
            npm_resolver,
            specifier.clone(),
            package_req_ref,
            maybe_range,
            is_dynamic,
          );
        } else {
          // mark external
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Module(Module::External(ExternalModule {
              specifier: specifier.clone(),
            })),
          );
        }
      }
      Ok(LoadSpecifierKind::Node(module_name)) => {
        self.graph.has_node_specifier = true;
        self.graph.module_slots.insert(
          specifier.clone(),
          ModuleSlot::Module(Module::Node(BuiltInNodeModule {
            specifier: specifier.clone(),
            module_name,
          })),
        );
      }
      Ok(LoadSpecifierKind::Url) => {
        self.load_pending_module(PendingModuleLoadItem {
          redirect_count,
          requested_specifier: specifier.clone(),
          maybe_attribute_type,
          maybe_range: maybe_range.cloned(),
          load_specifier: specifier.clone(),
          is_dynamic,
          is_root,
          maybe_checksum: None,
          maybe_version_info: None,
        });
      }
      Err(err) => {
        self
          .graph
          .module_slots
          .insert(specifier.clone(), ModuleSlot::Err(*err));
      }
    }
  }

  fn load_jsr_specifier(
    &mut self,
    specifier: Url,
    package_ref: JsrPackageReqReference,
    maybe_attribute_type: Option<AttributeTypeWithRange>,
    maybe_range: Option<&Range>,
    is_dynamic: bool,
    is_root: bool,
  ) {
    let package_name = &package_ref.req().name;
    let specifier = specifier.clone();
    self.queue_load_package_info(package_name);
    self
      .state
      .jsr
      .pending_resolutions
      .push_back(PendingJsrReqResolutionItem {
        specifier,
        package_ref,
        maybe_attribute_type,
        maybe_range: maybe_range.cloned(),
        is_dynamic,
        is_root,
      });
  }

  fn load_npm_specifier(
    &mut self,
    npm_resolver: &dyn NpmResolver,
    specifier: Url,
    package_ref: NpmPackageReqReference,
    maybe_range: Option<&Range>,
    is_dynamic: bool,
  ) {
    if self
      .state
      .npm
      .requested_registry_info_loads
      .insert(package_ref.req().name.clone())
    {
      // request to load
      npm_resolver.load_and_cache_npm_package_info(&package_ref.req().name);
    }

    self
      .state
      .npm
      .pending_resolutions
      .push(PendingNpmResolutionItem {
        specifier,
        package_ref,
        maybe_range: maybe_range.cloned(),
        is_dynamic,
      });
  }

  fn parse_load_specifier_kind(
    &self,
    specifier: &Url,
    maybe_range: Option<&Range>,
  ) -> Result<LoadSpecifierKind, Box<ModuleError>> {
    match specifier.scheme() {
      "jsr" => validate_jsr_specifier(specifier)
        .map(LoadSpecifierKind::Jsr)
        .map_err(|err| {
          Box::new(ModuleError::LoadingErr(
            specifier.clone(),
            maybe_range.cloned(),
            JsrLoadError::PackageFormat(err).into(),
          ))
        }),
      "npm" => NpmPackageReqReference::from_specifier(specifier)
        .map(LoadSpecifierKind::Npm)
        .map_err(|err| {
          Box::new(ModuleError::LoadingErr(
            specifier.clone(),
            maybe_range.cloned(),
            NpmLoadError::PackageReqReferenceParse(err).into(),
          ))
        }),
      _ => {
        if let Some(npm_resolver) = self.npm_resolver {
          match npm_resolver.resolve_builtin_node_module(specifier) {
            Ok(Some(builtin_module)) => {
              return Ok(LoadSpecifierKind::Node(builtin_module))
            }
            Ok(None) => {}
            Err(err) => {
              return Err(Box::new(ModuleError::LoadingErr(
                specifier.clone(),
                maybe_range.cloned(),
                err.into(),
              )))
            }
          }
        }

        Ok(LoadSpecifierKind::Url)
      }
    }
  }

  fn maybe_mark_dep(
    &mut self,
    load_specifier: &LoadSpecifierKind,
    maybe_range: Option<&Range>,
  ) {
    match load_specifier {
      LoadSpecifierKind::Jsr(package_ref) => {
        self.mark_jsr_dep(package_ref, maybe_range);
      }
      LoadSpecifierKind::Npm(package_ref) => {
        self.mark_npm_dep(package_ref, maybe_range);
      }
      LoadSpecifierKind::Node(_) | LoadSpecifierKind::Url => {
        // ignore the rest
      }
    }
  }

  fn mark_jsr_dep(
    &mut self,
    package_ref: &JsrPackageReqReference,
    maybe_range: Option<&Range>,
  ) {
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
  }

  fn mark_npm_dep(
    &mut self,
    package_ref: &NpmPackageReqReference,
    maybe_range: Option<&Range>,
  ) {
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
  }

  #[allow(clippy::too_many_arguments)]
  fn load_pending_module(&mut self, item: PendingModuleLoadItem) {
    let PendingModuleLoadItem {
      redirect_count,
      requested_specifier,
      maybe_attribute_type,
      maybe_range,
      load_specifier,
      is_dynamic,
      is_root,
      mut maybe_checksum,
      mut maybe_version_info,
    } = item;
    self
      .graph
      .module_slots
      .insert(requested_specifier.clone(), ModuleSlot::Pending);
    let loader = self.loader;
    let module_analyzer = self.module_analyzer;
    let jsr_url_provider = self.jsr_url_provider;
    let was_dynamic_root = self.was_dynamic_root;
    let maybe_nv_when_no_version_info = if maybe_version_info.is_none() {
      self
        .jsr_url_provider
        .package_url_to_nv(&requested_specifier)
    } else {
      None
    };
    let maybe_version_load_fut =
      maybe_nv_when_no_version_info.map(|package_nv| {
        self.queue_load_package_version_info(&package_nv);
        let fut = self
          .state
          .jsr
          .metadata
          .get_package_version_metadata(&package_nv)
          .unwrap();
        (package_nv, fut)
      });
    if maybe_checksum.is_none() {
      maybe_checksum = self
        .locker
        .as_ref()
        .and_then(|l| l.get_remote_checksum(&requested_specifier));
    }
    let fut = async move {
      #[allow(clippy::too_many_arguments)]
      async fn try_load(
        is_root: bool,
        redirect_count: usize,
        load_specifier: ModuleSpecifier,
        mut maybe_checksum: Option<LoaderChecksum>,
        maybe_range: Option<&Range>,
        maybe_version_info: &mut Option<JsrPackageVersionInfoExt>,
        maybe_attribute_type: Option<AttributeTypeWithRange>,
        loaded_package_via_https_url: &mut Option<LoadedJsrPackageViaHttpsUrl>,
        maybe_version_load_fut: Option<(
          PackageNv,
          PendingResult<PendingJsrPackageVersionInfoLoadItem>,
        )>,
        is_dynamic: bool,
        was_dynamic_root: bool,
        loader: &dyn Loader,
        jsr_url_provider: &dyn JsrUrlProvider,
        module_analyzer: &dyn ModuleAnalyzer,
      ) -> Result<PendingInfoResponse, ModuleError> {
        if let Some((package_nv, fut)) = maybe_version_load_fut {
          let inner = fut.await.map_err(|err| {
            ModuleError::LoadingErr(
              jsr_url_provider.package_url(&package_nv),
              maybe_range.cloned(),
              err.into(),
            )
          })?;
          let info = JsrPackageVersionInfoExt {
            base_url: jsr_url_provider.package_url(&package_nv),
            inner: inner.info,
          };
          if let Some(sub_path) = info.get_subpath(&load_specifier) {
            maybe_checksum = Some(LoaderChecksum::new(
              info
                .get_checksum(sub_path)
                .map_err(|err| {
                  ModuleError::LoadingErr(
                    load_specifier.clone(),
                    maybe_range.cloned(),
                    err,
                  )
                })?
                .to_string(),
            ));
          }
          maybe_version_info.replace(info);
          loaded_package_via_https_url.replace(LoadedJsrPackageViaHttpsUrl {
            nv: package_nv,
            manifest_checksum_for_locker: inner.checksum_for_locker,
          });
        }

        let result = loader.load(
          &load_specifier,
          LoadOptions {
            is_dynamic,
            was_dynamic_root,
            cache_setting: CacheSetting::Use,
            maybe_checksum: maybe_checksum.clone(),
          },
        );
        match result.await {
          Ok(Some(response)) => match response {
            LoadResponse::Redirect { specifier } => {
              if maybe_version_info.is_some() {
                // This should never happen on the JSR registry. If we ever
                // supported this we'd need a way for the registry to express
                // redirects in the manifest since we don't store checksums
                // or redirect information within the package.
                Err(ModuleError::LoadingErr(
                  load_specifier.clone(),
                  maybe_range.cloned(),
                  JsrLoadError::RedirectInPackage(specifier.clone()).into(),
                ))
              } else if let Some(expected_checksum) = maybe_checksum {
                Err(ModuleError::LoadingErr(
                  load_specifier.clone(),
                  maybe_range.cloned(),
                  ModuleLoadError::HttpsChecksumIntegrity(
                    ChecksumIntegrityError {
                      actual: format!("Redirect to {}", specifier),
                      expected: expected_checksum.into_string(),
                    },
                  ),
                ))
              } else if redirect_count >= loader.max_redirects() {
                Err(ModuleError::LoadingErr(
                  load_specifier.clone(),
                  maybe_range.cloned(),
                  ModuleLoadError::TooManyRedirects,
                ))
              } else {
                Ok(PendingInfoResponse::Redirect {
                  count: redirect_count + 1,
                  specifier,
                  maybe_attribute_type,
                  is_dynamic,
                  is_root,
                })
              }
            }
            LoadResponse::External { specifier } => {
              Ok(PendingInfoResponse::External { specifier, is_root })
            }
            LoadResponse::Module {
              content,
              specifier,
              maybe_headers,
            } => parse_module_source_and_info(
              module_analyzer,
              ParseModuleAndSourceInfoOptions {
                specifier: specifier.clone(),
                maybe_headers,
                content,
                maybe_attribute_type: maybe_attribute_type.as_ref(),
                maybe_referrer: maybe_range,
                is_root,
                is_dynamic_branch: is_dynamic,
              },
            )
            .await
            .map(|module_source_and_info| {
              PendingInfoResponse::Module {
                specifier: specifier.clone(),
                module_source_and_info,
                pending_load: None,
                is_root,
              }
            }),
          },
          Ok(None) => Err(ModuleError::Missing(
            load_specifier.clone(),
            maybe_range.cloned(),
          )),
          Err(err) => match err.downcast::<ChecksumIntegrityError>() {
            // try to return the context of a checksum integrity error
            // so that it can be more easily enhanced
            Ok(err) => Err(ModuleError::LoadingErr(
              load_specifier.clone(),
              maybe_range.cloned(),
              if maybe_version_info.is_some() {
                JsrLoadError::ContentChecksumIntegrity(err).into()
              } else {
                ModuleLoadError::HttpsChecksumIntegrity(err)
              },
            )),
            Err(err) => Err(ModuleError::LoadingErr(
              load_specifier.clone(),
              maybe_range.cloned(),
              ModuleLoadError::Loader(Arc::new(err)),
            )),
          },
        }
      }

      let mut loaded_package_via_https_url = None;
      let result = try_load(
        is_root,
        redirect_count,
        load_specifier,
        maybe_checksum,
        maybe_range.as_ref(),
        &mut maybe_version_info,
        maybe_attribute_type,
        &mut loaded_package_via_https_url,
        maybe_version_load_fut,
        is_dynamic,
        was_dynamic_root,
        loader,
        jsr_url_provider,
        module_analyzer,
      )
      .await;

      PendingInfo {
        result,
        requested_specifier,
        maybe_range,
        maybe_version_info,
        loaded_package_via_https_url,
      }
    }
    .boxed_local();
    self.state.pending.push_back(fut);
  }

  fn queue_load_package_info(&mut self, package_name: &str) {
    self.state.jsr.metadata.queue_load_package_info(
      package_name,
      self.fill_pass_mode.to_cache_setting(),
      JsrMetadataStoreServices {
        executor: self.executor,
        jsr_url_provider: self.jsr_url_provider,
        loader: self.loader,
      },
    );
  }

  fn queue_load_package_version_info(&mut self, package_nv: &PackageNv) {
    self.state.jsr.metadata.queue_load_package_version_info(
      package_nv,
      self.fill_pass_mode.to_cache_setting(),
      self.locker.as_deref(),
      JsrMetadataStoreServices {
        executor: self.executor,
        jsr_url_provider: self.jsr_url_provider,
        loader: self.loader,
      },
    );
  }

  fn visit(
    &mut self,
    response: PendingInfoResponse,
    maybe_referrer: Option<Range>,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) {
    match response {
      PendingInfoResponse::External { specifier, is_root } => {
        if is_root {
          self.resolved_roots.insert(specifier.clone());
        }
        let module_slot =
          ModuleSlot::Module(Module::External(ExternalModule {
            specifier: specifier.clone(),
          }));
        self.graph.module_slots.insert(specifier, module_slot);
      }
      PendingInfoResponse::Module {
        specifier,
        pending_load,
        module_source_and_info,
        is_root,
      } => {
        // this should have been handled by now
        debug_assert_eq!(
          maybe_version_info.is_none(),
          self
            .jsr_url_provider
            .package_url_to_nv(&specifier)
            .is_none(),
          "{}",
          specifier
        );

        if is_root {
          self.resolved_roots.insert(specifier.clone());
        }

        if let Some((checksum, module_info)) = pending_load.map(|v| *v) {
          self.state.jsr.pending_content_loads.push({
            let specifier = specifier.clone();
            let maybe_range = maybe_referrer.clone();
            let fut = self.loader.load(
              &specifier,
              LoadOptions {
                is_dynamic: self.in_dynamic_branch,
                was_dynamic_root: self.was_dynamic_root,
                cache_setting: CacheSetting::Use,
                maybe_checksum: Some(checksum.clone()),
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
        } else if maybe_version_info.is_none()
          // do not insert checksums for declaration files
          && !module_source_and_info.media_type().is_declaration()
          && matches!(specifier.scheme(), "https" | "http")
        {
          if let Some(locker) = &mut self.locker {
            if !locker.has_remote_checksum(&specifier) {
              locker.set_remote_checksum(
                &specifier,
                LoaderChecksum::new(LoaderChecksum::gen(
                  module_source_and_info.source().as_bytes(),
                )),
              );
            }
          }
        }

        let module_slot =
          self.visit_module(module_source_and_info, maybe_version_info);
        self.graph.module_slots.insert(specifier, module_slot);
      }
      PendingInfoResponse::Redirect {
        count,
        specifier,
        is_dynamic,
        is_root,
        maybe_attribute_type,
      } => {
        self.load_with_redirect_count(
          count,
          &specifier,
          maybe_referrer.as_ref(),
          is_dynamic,
          is_root,
          maybe_attribute_type,
          None,
        );
      }
    }
  }

  /// Visit a module, parsing it and resolving any dependencies.
  fn visit_module(
    &mut self,
    module_source_and_info: ModuleSourceAndInfo,
    maybe_version_info: Option<&JsrPackageVersionInfoExt>,
  ) -> ModuleSlot {
    let parse_module_result = parse_module(
      self.file_system,
      self.jsr_url_provider,
      self.resolver,
      self.npm_resolver,
      ParseModuleOptions {
        graph_kind: self.graph.graph_kind,
        module_source_and_info,
      },
    );

    let mut module_slot = match parse_module_result {
      Ok(module) => ModuleSlot::Module(module),
      Err(err) => ModuleSlot::Err(err),
    };

    if let ModuleSlot::Module(Module::Js(module)) = &mut module_slot {
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
                dep.maybe_attribute_type.as_ref().map(|attribute_type| {
                  AttributeTypeWithRange {
                    range: range.clone(),
                    kind: attribute_type.clone(),
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
                  self.resolved_roots.contains(specifier),
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
                  self.resolved_roots.contains(specifier),
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
            self.resolved_roots.contains(&resolved.specifier),
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

/// Pending information to insert into the module graph once
/// npm specifier resolution has been finalized.
struct NpmSpecifierBuildPendingInfo {
  found_pkg_nvs: IndexSet<PackageNv>,
  module_slots: HashMap<ModuleSpecifier, ModuleSlot>,
  dependencies_resolution: Option<Result<(), Arc<anyhow::Error>>>,
  redirects: HashMap<ModuleSpecifier, ModuleSpecifier>,
}

impl NpmSpecifierBuildPendingInfo {
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      found_pkg_nvs: IndexSet::with_capacity(capacity),
      module_slots: HashMap::with_capacity(capacity),
      dependencies_resolution: None,
      redirects: HashMap::with_capacity(capacity),
    }
  }
}

#[derive(Debug)]
struct PendingNpmResolutionItem {
  specifier: ModuleSpecifier,
  package_ref: NpmPackageReqReference,
  maybe_range: Option<Range>,
  is_dynamic: bool,
}

struct NpmSpecifierResolver<'a> {
  npm_resolver: Option<&'a dyn NpmResolver>,
  pending_info: NpmSpecifierBuildPendingInfo,
  pending_npm_specifiers: Vec<PendingNpmResolutionItem>,
}

impl<'a> NpmSpecifierResolver<'a> {
  pub async fn fill_builder(builder: &mut Builder<'a, '_>) {
    let mut npm_specifier_resolver = NpmSpecifierResolver::new(
      builder.npm_resolver,
      std::mem::take(&mut builder.state.npm.pending_resolutions),
    );

    npm_specifier_resolver.resolve().await;

    npm_specifier_resolver.fill_graph(builder.graph);
  }

  fn new(
    npm_resolver: Option<&'a dyn NpmResolver>,
    pending_npm_specifiers: Vec<PendingNpmResolutionItem>,
  ) -> Self {
    Self {
      npm_resolver,
      pending_info: NpmSpecifierBuildPendingInfo::with_capacity(
        pending_npm_specifiers.len(),
      ),
      pending_npm_specifiers,
    }
  }

  async fn resolve(&mut self) {
    if self.pending_npm_specifiers.is_empty() {
      return;
    }

    let Some(npm_resolver) = self.npm_resolver else {
      for item in self.pending_npm_specifiers.drain(..) {
        self.pending_info.module_slots.insert(
          item.specifier.clone(),
          ModuleSlot::Err(ModuleError::LoadingErr(
            item.specifier.clone(),
            item.maybe_range.clone(),
            NpmLoadError::NotSupportedEnvironment.into(),
          )),
        );
      }
      return;
    };

    let (main_items, dynamic_items) = self
      .pending_npm_specifiers
      .drain(..)
      .partition::<Vec<_>, _>(|item| !item.is_dynamic);

    if !main_items.is_empty() {
      let items_by_req: IndexMap<_, Vec<_>> =
        IndexMap::with_capacity(main_items.len());
      let items_by_req =
        main_items
          .into_iter()
          .fold(items_by_req, |mut items_by_req, item| {
            items_by_req
              .entry(item.package_ref.req().clone())
              .or_default()
              .push(item);
            items_by_req
          });
      let all_package_reqs = items_by_req.keys().cloned().collect::<Vec<_>>();
      let result = npm_resolver.resolve_pkg_reqs(&all_package_reqs).await;

      self.pending_info.dependencies_resolution = Some(result.dep_graph_result);

      assert_eq!(all_package_reqs.len(), result.results.len());
      for (req, resolution) in
        all_package_reqs.into_iter().zip(result.results.into_iter())
      {
        let items = items_by_req.get(&req).unwrap();
        for item in items {
          match &resolution {
            Ok(pkg_nv) => {
              self.add_nv_for_item(
                item.specifier.clone(),
                pkg_nv.clone(),
                item.package_ref.sub_path().map(ToOwned::to_owned),
              );
            }
            Err(err) => {
              self.pending_info.module_slots.insert(
                item.specifier.clone(),
                ModuleSlot::Err(ModuleError::LoadingErr(
                  item.specifier.clone(),
                  item.maybe_range.clone(),
                  err.clone().into(),
                )),
              );
            }
          }
        }
      }
    }

    for item in dynamic_items {
      // these need to be resolved one at a time in order to check if the dependency resolution fails
      let mut result = npm_resolver
        .resolve_pkg_reqs(&[item.package_ref.req().clone()])
        .await;
      assert_eq!(result.results.len(), 1);
      match result.results.remove(0) {
        Ok(pkg_nv) => {
          if let Err(err) = result.dep_graph_result {
            self.pending_info.module_slots.insert(
              item.specifier.clone(),
              ModuleSlot::Err(ModuleError::LoadingErr(
                item.specifier,
                item.maybe_range,
                NpmLoadError::PackageReqResolution(err).into(),
              )),
            );
          } else {
            self.add_nv_for_item(
              item.specifier,
              pkg_nv,
              item.package_ref.into_inner().sub_path,
            );
          }
        }
        Err(err) => {
          self.pending_info.module_slots.insert(
            item.specifier.clone(),
            ModuleSlot::Err(ModuleError::LoadingErr(
              item.specifier,
              item.maybe_range,
              err.into(),
            )),
          );
        }
      }
    }
  }

  fn add_nv_for_item(
    &mut self,
    specifier: ModuleSpecifier,
    pkg_nv: PackageNv,
    sub_path: Option<String>,
  ) {
    let pkg_id_ref = NpmPackageNvReference::new(PackageNvReference {
      nv: pkg_nv.clone(),
      sub_path,
    });
    let resolved_specifier = pkg_id_ref.as_specifier();
    if resolved_specifier != specifier {
      self
        .pending_info
        .redirects
        .insert(specifier.clone(), resolved_specifier.clone());
    }
    self.pending_info.found_pkg_nvs.insert(pkg_nv);
    self.pending_info.module_slots.insert(
      resolved_specifier.clone(),
      ModuleSlot::Module(Module::Npm(NpmModule {
        specifier: resolved_specifier,
        nv_reference: pkg_id_ref,
      })),
    );
  }

  fn fill_graph(self, graph: &mut ModuleGraph) {
    let pending_info = self.pending_info;

    // update the graph with the pending information
    for (key, value) in pending_info.module_slots {
      // always keep the existing information in the graph
      // in case it was already used in the runtime
      graph.module_slots.entry(key).or_insert(value);
    }
    for (key, value) in pending_info.redirects {
      graph.redirects.entry(key).or_insert(value);
    }

    graph.npm_packages.extend(pending_info.found_pkg_nvs);
    if let Some(result) = pending_info.dependencies_resolution {
      graph.npm_dep_graph_result = result;
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
      ModuleLoadError::Decode(err.into()),
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
  use deno_ast::dep::ImportAttribute;
  use deno_ast::emit;
  use deno_ast::EmitOptions;
  use deno_ast::SourceMap;
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
    let dependency = Dependency {
      maybe_code: Resolution::Ok(Box::new(ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///b.ts").unwrap(),
        range: Range {
          specifier: specifier.clone(),
          start: Position {
            line: 0,
            character: 19,
          },
          end: Position {
            line: 0,
            character: 27,
          },
        },
      })),
      imports: vec![
        Import {
          specifier: "./b.ts".to_string(),
          kind: ImportKind::Es,
          specifier_range: Range {
            specifier: specifier.clone(),
            start: Position {
              line: 0,
              character: 19,
            },
            end: Position {
              line: 0,
              character: 27,
            },
          },
          is_dynamic: false,
          attributes: Default::default(),
        },
        Import {
          specifier: "./b.ts".to_string(),
          kind: ImportKind::Es,
          specifier_range: Range {
            specifier: specifier.clone(),
            start: Position {
              line: 1,
              character: 19,
            },
            end: Position {
              line: 1,
              character: 27,
            },
          },
          is_dynamic: false,
          attributes: Default::default(),
        },
      ],
      ..Default::default()
    };
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

  #[test]
  fn dependency_with_new_resolver() {
    let referrer = ModuleSpecifier::parse("file:///a/main.ts").unwrap();
    let dependency = Dependency {
      maybe_code: Resolution::Ok(Box::new(ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///wrong.ts").unwrap(),
        range: Range {
          specifier: referrer.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      })),
      maybe_type: Resolution::Ok(Box::new(ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///wrong.ts").unwrap(),
        range: Range {
          specifier: referrer.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      })),
      maybe_deno_types_specifier: Some("./b.d.ts".to_string()),
      ..Default::default()
    };
    let new_dependency =
      dependency.with_new_resolver("./b.ts", Default::default(), None, None);
    assert_eq!(
      new_dependency,
      Dependency {
        maybe_code: Resolution::Ok(Box::new(ResolutionResolved {
          specifier: ModuleSpecifier::parse("file:///a/b.ts").unwrap(),
          range: Range {
            specifier: referrer.clone(),
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        })),
        maybe_type: Resolution::Ok(Box::new(ResolutionResolved {
          specifier: ModuleSpecifier::parse("file:///a/b.d.ts").unwrap(),
          range: Range {
            specifier: referrer.clone(),
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        })),
        maybe_deno_types_specifier: Some("./b.d.ts".to_string()),
        ..Default::default()
      }
    );
  }

  #[test]
  fn types_dependency_with_new_resolver() {
    let referrer = ModuleSpecifier::parse("file:///a/main.ts").unwrap();
    let types_dependency = TypesDependency {
      specifier: "./main.d.ts".to_string(),
      dependency: Resolution::Ok(Box::new(ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///wrong.ts").unwrap(),
        range: Range {
          specifier: referrer.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        },
      })),
    };
    let new_types_dependency =
      types_dependency.with_new_resolver(Default::default(), None, None);
    assert_eq!(
      new_types_dependency,
      TypesDependency {
        specifier: "./main.d.ts".to_string(),
        dependency: Resolution::Ok(Box::new(ResolutionResolved {
          specifier: ModuleSpecifier::parse("file:///a/main.d.ts").unwrap(),
          range: Range {
            specifier: referrer.clone(),
            start: Position::zeroed(),
            end: Position::zeroed(),
          },
        })),
      }
    );
  }

  #[tokio::test]
  async fn static_dep_of_dynamic_dep_is_dynamic() {
    #[derive(Default)]
    struct TestLoader {
      loaded_foo: RefCell<bool>,
      loaded_bar: RefCell<bool>,
      loaded_baz: RefCell<bool>,
      loaded_dynamic_root: RefCell<bool>,
    }
    impl Loader for TestLoader {
      fn load(
        &self,
        specifier: &ModuleSpecifier,
        options: LoadOptions,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => {
            assert!(!options.is_dynamic);
            assert!(!options.was_dynamic_root);
            *self.loaded_foo.borrow_mut() = true;
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
            assert!(!options.was_dynamic_root);
            *self.loaded_bar.borrow_mut() = true;
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
            assert!(!options.was_dynamic_root);
            *self.loaded_baz.borrow_mut() = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"console.log('Hello, world!')".to_vec().into(),
              }))
            })
          }
          "file:///dynamic_root.js" => {
            assert!(options.is_dynamic);
            assert!(options.was_dynamic_root);
            *self.loaded_dynamic_root.borrow_mut() = true;
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

    let loader = TestLoader::default();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    assert!(*loader.loaded_foo.borrow());
    assert!(*loader.loaded_bar.borrow());
    assert!(*loader.loaded_baz.borrow());
    assert!(!*loader.loaded_dynamic_root.borrow());
    assert_eq!(graph.specifiers_count(), 3);

    graph
      .build(
        vec![Url::parse("file:///dynamic_root.js").unwrap()],
        &loader,
        BuildOptions {
          is_dynamic: true,
          ..Default::default()
        },
      )
      .await;
    assert!(*loader.loaded_dynamic_root.borrow());
    assert_eq!(graph.specifiers_count(), 4);
  }

  #[tokio::test]
  async fn missing_module_is_error() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &self,
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
    let loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    let roots = vec![Url::parse("file:///foo.js").unwrap()];
    graph
      .build(roots.clone(), &loader, Default::default())
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
        roots.iter(),
        WalkOptions {
          follow_dynamic: false,
          kind: GraphKind::All,
          check_js: true,
          prefer_fast_check_graph: false,
        },
      )
      .errors()
      .count();
    assert_eq!(error_count, 0);

    // should return as dynamic import missing when walking
    let errors = graph
      .walk(
        roots.iter(),
        WalkOptions {
          follow_dynamic: true,
          kind: GraphKind::All,
          check_js: true,
          prefer_fast_check_graph: false,
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
        &self,
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
    let loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &loader,
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
        &self,
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
    let loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    let roots = vec![Url::parse("https://deno.land/foo.js").unwrap()];
    graph
      .build(roots.clone(), &loader, Default::default())
      .await;
    assert_eq!(graph.specifiers_count(), 4);
    let errors = graph
      .walk(
        roots.iter(),
        WalkOptions {
          check_js: true,
          follow_dynamic: false,
          kind: GraphKind::All,
          prefer_fast_check_graph: false,
        },
      )
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
      loaded_bar: RefCell<bool>,
    }
    impl Loader for TestLoader {
      fn load(
        &self,
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
            *self.loaded_bar.borrow_mut() = true;
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
    let loader = TestLoader {
      loaded_bar: RefCell::new(false),
    };
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.js").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    assert!(*loader.loaded_bar.borrow());
  }

  #[tokio::test]
  async fn dependency_imports() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &self,
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
        &TestLoader,
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
          specifier_range: Range {
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
          specifier_range: Range {
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
          specifier_range: Range {
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
          specifier_range: Range {
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
          specifier_range: Range {
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
          specifier_range: Range {
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
        specifier_range: Range {
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
  async fn dependency_ts_type_import_with_deno_types() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &self,
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
                // @deno-types='file:///bar.d.ts'
                import type { Bar as _ } from 'bar';
              "
              .to_vec()
              .into(),
            }))
          }),
          "file:///bar.d.ts" => {
            assert!(!options.is_dynamic);
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: b"export type Bar = null;\n".to_vec().into(),
              }))
            })
          }
          _ => unreachable!(),
        }
      }
    }
    let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
    graph
      .build(
        vec![Url::parse("file:///foo.ts").unwrap()],
        &TestLoader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    let module = graph.get(&Url::parse("file:///foo.ts").unwrap()).unwrap();
    let module = module.js().unwrap();
    let dependency = module.dependencies.get("bar").unwrap();
    assert_eq!(dependency.maybe_code, Resolution::None);
    assert_eq!(
      dependency.maybe_type.maybe_specifier().unwrap().as_str(),
      "file:///bar.d.ts",
    );
  }

  #[tokio::test]
  async fn dependency_jsx_import_source_types_resolution() {
    let mut mem_loader = MemoryLoader::default();
    mem_loader.add_source_with_text(
      "file:///foo.tsx",
      "
/* @jsxImportSource foo */
",
    );
    mem_loader.add_source(
      "file:///foo/jsx-runtime",
      Source::Module {
        specifier: "file:///foo/jsx-runtime",
        maybe_headers: Some(vec![("content-type", "application/javascript")]),
        content: "",
      },
    );
    mem_loader.add_source(
      "file:///foo/types/jsx-runtime",
      Source::Module {
        specifier: "file:///foo/types/jsx-runtime",
        maybe_headers: Some(vec![("content-type", "application/typescript")]),
        content: "",
      },
    );
    let mut graph = ModuleGraph::new(GraphKind::All);

    #[derive(Debug)]
    struct TestResolver;
    impl Resolver for TestResolver {
      fn resolve(
        &self,
        specifier_text: &str,
        referrer_range: &Range,
        mode: ResolutionMode,
      ) -> Result<ModuleSpecifier, ResolveError> {
        if specifier_text == "foo/jsx-runtime" {
          match mode {
            ResolutionMode::Execution => {
              Ok(ModuleSpecifier::parse("file:///foo/jsx-runtime").unwrap())
            }
            ResolutionMode::Types => Ok(
              ModuleSpecifier::parse("file:///foo/types/jsx-runtime").unwrap(),
            ),
          }
        } else {
          Ok(resolve_import(specifier_text, &referrer_range.specifier)?)
        }
      }
    }

    let resolver = TestResolver;
    graph
      .build(
        vec![Url::parse("file:///foo.tsx").unwrap()],
        &mem_loader,
        BuildOptions {
          resolver: Some(&resolver),
          ..Default::default()
        },
      )
      .await;
    graph.valid().unwrap();
    let module = graph.get(&Url::parse("file:///foo.tsx").unwrap()).unwrap();
    let module = module.js().unwrap();
    let dependency_a = module.dependencies.get("foo/jsx-runtime").unwrap();
    assert_eq!(
      dependency_a.maybe_code.maybe_specifier().unwrap().as_str(),
      "file:///foo/jsx-runtime"
    );
    assert_eq!(
      dependency_a.maybe_type.maybe_specifier().unwrap().as_str(),
      "file:///foo/types/jsx-runtime"
    );
  }

  #[tokio::test]
  async fn dependency_jsx_import_source_types_pragma() {
    let mut mem_loader = MemoryLoader::default();
    mem_loader.add_source_with_text(
      "file:///foo.tsx",
      "
/* @jsxImportSource http://localhost */
/* @jsxImportSourceTypes http://localhost/types */
",
    );
    mem_loader.add_source(
      "http://localhost/jsx-runtime",
      Source::Module {
        specifier: "http://localhost/jsx-runtime",
        maybe_headers: Some(vec![("content-type", "application/javascript")]),
        content: "",
      },
    );
    mem_loader.add_source(
      "http://localhost/types/jsx-runtime",
      Source::Module {
        specifier: "http://localhost/types/jsx-runtime",
        maybe_headers: Some(vec![("content-type", "application/typescript")]),
        content: "",
      },
    );
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.tsx").unwrap()],
        &mem_loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    let module = graph.get(&Url::parse("file:///foo.tsx").unwrap()).unwrap();
    let module = module.js().unwrap();
    let dependency_a = module
      .dependencies
      .get("http://localhost/jsx-runtime")
      .unwrap();
    assert_eq!(
      dependency_a.maybe_type.maybe_specifier().unwrap().as_str(),
      "http://localhost/types/jsx-runtime"
    );
    assert_eq!(
      dependency_a.maybe_deno_types_specifier.as_ref().unwrap(),
      "http://localhost/types/jsx-runtime"
    );
  }

  #[cfg(feature = "fast_check")]
  #[tokio::test]
  async fn fast_check_dts() {
    use deno_ast::EmittedSourceText;

    let mut exports = IndexMap::new();
    exports.insert(".".to_string(), "./foo.ts".to_string());

    let workspace_members = vec![WorkspaceMember {
      base: Url::parse("file:///").unwrap(),
      exports: exports.clone(),
      name: "@foo/bar".to_string(),
      version: Some(Version::parse_standard("1.0.0").unwrap()),
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
        &test_loader,
        BuildOptions {
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
    let source_map =
      SourceMap::single(module.specifier.clone(), module.source.to_string());
    let EmittedSourceText { text, .. } = emit(
      (&dts.program).into(),
      &dts.comments.as_single_threaded(),
      &source_map,
      &EmitOptions {
        remove_comments: false,
        source_map: deno_ast::SourceMapOption::None,
        ..Default::default()
      },
    )
    .unwrap();
    assert_eq!(
      text.trim(),
      "export declare function add(a: number, b: number): number;"
    );
    assert!(dts.diagnostics.is_empty());
  }

  #[cfg(feature = "fast_check")]
  #[tokio::test]
  async fn fast_check_external() {
    use deno_ast::EmittedSourceText;

    let mut exports = IndexMap::new();
    exports.insert(".".to_string(), "./foo.ts".to_string());

    let workspace_members = vec![WorkspaceMember {
      base: Url::parse("file:///").unwrap(),
      exports: exports.clone(),
      name: "@foo/bar".to_string(),
      version: Some(Version::parse_standard("1.0.0").unwrap()),
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
        module_graph_1: None,
        module_graph_2: None,
        manifest: Default::default(),
      },
    );
    test_loader.add_external_source("https://jsr.io/@package/foo/1.0.0/mod.ts");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///foo.ts").unwrap()],
        &test_loader,
        BuildOptions::default(),
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
      let source_map = SourceMap::single(
        module.specifier().clone(),
        module.source().unwrap().to_string(),
      );
      let EmittedSourceText { text, .. } = emit(
        (&dts.program).into(),
        &dts.comments.as_single_threaded(),
        &source_map,
        &EmitOptions {
          remove_comments: false,
          source_map: deno_ast::SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap();
      assert_eq!(text.trim(), "export * from 'jsr:@package/foo';");
      assert!(dts.diagnostics.is_empty());
    }

    let module = graph
      .get(&Url::parse("https://jsr.io/@package/foo/1.0.0/mod.ts").unwrap())
      .unwrap();
    assert!(module.external().is_some());
  }
}
