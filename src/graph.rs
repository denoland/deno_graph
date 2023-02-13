// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::analyzer::analyze_deno_types;
use crate::analyzer::DependencyKind;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleInfo;
use crate::analyzer::PositionRange;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::TypeScriptReference;
use crate::DefaultModuleAnalyzer;
use crate::ImportAssertions;
use crate::ReferrerImports;

use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::module_specifier::SpecifierError;
use crate::source::*;

use anyhow::Error;
use anyhow::Result;
use deno_ast::LineAndColumnIndex;
use deno_ast::MediaType;
use deno_ast::SourcePos;
use deno_ast::SourceTextInfo;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::Future;
use futures::FutureExt;
use once_cell::sync::Lazy;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::pin::Pin;
use std::sync::Arc;

static SUPPORTED_ASSERT_TYPES: Lazy<HashMap<String, MediaType>> =
  Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("json".to_string(), MediaType::Json);
    map
  });

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
    specifier: &ModuleSpecifier,
    range: &PositionRange,
  ) -> Range {
    Range {
      specifier: specifier.clone(),
      start: range.start.clone(),
      end: range.end.clone(),
    }
  }

  /// Determines if a given position is within the range.
  pub fn includes(&self, position: &Position) -> bool {
    (position >= &self.start) && (position <= &self.end)
  }
}

#[derive(Debug, Clone)]
pub enum ModuleGraphError {
  LoadingErr(ModuleSpecifier, Option<Range>, Arc<anyhow::Error>),
  Missing(ModuleSpecifier, Option<Range>),
  ParseErr(ModuleSpecifier, deno_ast::Diagnostic),
  UnsupportedMediaType {
    specifier: ModuleSpecifier,
    media_type: MediaType,
    maybe_referrer: Option<Range>,
  },
  // Note: Resolution and import errors are special in that they don't occupy
  // module slots, whereas the other variants do.
  ResolutionError(ResolutionError),
  ImportError(ImportError),
}

impl ModuleGraphError {
  pub fn specifier(&self) -> Option<&ModuleSpecifier> {
    match self {
      Self::ImportError(_) => None,
      Self::ResolutionError(_) => None,
      Self::LoadingErr(s, _, _)
      | Self::ParseErr(s, _)
      | Self::UnsupportedMediaType { specifier: s, .. }
      | Self::Missing(s, _) => Some(s),
    }
  }

  /// Converts the error into a string along with the range related to the error.
  ///
  /// We don't include the range in the error messages by default because they're
  /// not useful in cases like the LSP where the range is given by the editor itself.
  pub fn to_string_with_range(&self) -> String {
    if let Some(range) = self.maybe_range() {
      format!("{self}\n    at {range}")
    } else {
      format!("{self}")
    }
  }

  pub fn maybe_range(&self) -> Option<&Range> {
    match self {
      Self::LoadingErr(_, maybe_range, _) => maybe_range.as_ref(),
      Self::ImportError(_) => None,
      Self::ResolutionError(err) => Some(err.range()),
      Self::Missing(_, maybe_range) => maybe_range.as_ref(),
      Self::UnsupportedMediaType { maybe_referrer, .. } => {
        maybe_referrer.as_ref()
      }
      Self::ParseErr(_, _) => None,
    }
  }
}

impl std::error::Error for ModuleGraphError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::LoadingErr(_, _, err) => Some(err.as_ref().as_ref()),
      Self::ResolutionError(ref err) => Some(err),
      Self::ImportError(ref err) => Some(err),
      Self::Missing(_, _)
      | Self::ParseErr(_, _)
      | Self::UnsupportedMediaType { .. } => None,
    }
  }
}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LoadingErr(_, _, err) => err.fmt(f),
      Self::ParseErr(_, diagnostic) => write!(f, "The module's source code could not be parsed: {diagnostic}"),
      Self::ResolutionError(err) => err.fmt(f),
      Self::ImportError(err) => err.fmt(f),
      Self::UnsupportedMediaType {
        specifier,
        media_type: MediaType::Json,
        ..
      } => write!(f, "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: {specifier}"),
      Self::UnsupportedMediaType {
        specifier,
        media_type,
        ..
       } => write!(f, "Expected a JavaScript or TypeScript module, but identified a {media_type} module. Importing these types of modules is currently not supported.\n  Specifier: {specifier}"),
      Self::Missing(specifier, _) => {
        write!(f, "Module not found \"{specifier}\".")
      },
    }
  }
}

impl<'a> From<&'a ResolutionError> for ModuleGraphError {
  fn from(err: &'a ResolutionError) -> Self {
    Self::ResolutionError(err.clone())
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
    error: Arc<anyhow::Error>,
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
      Self::ResolverError { error, .. } => Some(error.as_ref().as_ref()),
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
    result: Result<ModuleSpecifier, Error>,
    range: Range,
    specifier: &str,
    remapped: bool,
  ) -> Self {
    match result {
      Ok(specifier) => Self::from_specifier(specifier, range, remapped),
      Err(err) => {
        let resolution_error =
          if let Some(specifier_error) = err.downcast_ref::<SpecifierError>() {
            ResolutionError::InvalidSpecifier {
              error: specifier_error.clone(),
              range,
            }
          } else {
            ResolutionError::ResolverError {
              error: Arc::new(err),
              specifier: specifier.to_string(),
              range,
            }
          };
        Self::Err(Box::new(resolution_error))
      }
    }
  }

  fn from_specifier(
    specifier: ModuleSpecifier,
    range: Range,
    remapped: bool,
  ) -> Self {
    let referrer_scheme = range.specifier.scheme();
    let specifier_scheme = specifier.scheme();
    if referrer_scheme == "https" && specifier_scheme == "http" {
      Resolution::Err(Box::new(ResolutionError::InvalidDowngrade {
        specifier,
        range,
      }))
    } else if matches!(referrer_scheme, "https" | "http")
      && !matches!(specifier_scheme, "https" | "http" | "npm" | "node")
      && !remapped
    {
      Resolution::Err(Box::new(ResolutionError::InvalidLocalImport {
        specifier,
        range,
      }))
    } else {
      Resolution::Ok(Box::new(ResolutionResolved { specifier, range }))
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

fn serialize_resolution<S>(
  resolution: &Resolution,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  match resolution {
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

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportError {
  TypeAssertionFailed {
    specifier: ModuleSpecifier,
    range: Range,
    actual_media_type: MediaType,
    expected_media_type: MediaType,
  },
  TypeAssertionUnsupported {
    specifier: String,
    range: Range,
    assert_type: String,
  },
}

impl std::error::Error for ImportError {}

impl fmt::Display for ImportError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      ImportError::TypeAssertionFailed { specifier, actual_media_type: MediaType::Json, expected_media_type, .. } => write!(f, "Expected a {expected_media_type} module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: {specifier}"),
      ImportError::TypeAssertionFailed { specifier, actual_media_type, expected_media_type, .. } => write!(f, "Expected a {expected_media_type} module, but identified a {actual_media_type} module.\n  Specifier: {specifier}"),
      ImportError::TypeAssertionUnsupported { assert_type, .. } => write!(f, "The import assertion type of \"{assert_type}\" is unsupported."),
    }
  }
}

impl Serialize for ImportError {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serializer.serialize_str(&self.to_string())
  }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Import {
  pub specifier: String,
  pub range: Range,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  #[serde(skip_serializing_if = "ImportAssertions::is_empty")]
  pub assertions: ImportAssertions,
  // Note: This is only populated in a final pass at the end of graph building.
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub errors: Vec<ImportError>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Dependency {
  pub maybe_code: Resolution,
  pub maybe_type: Resolution,
  pub is_dynamic: bool,
  /// List of code imports which refer to this dependency. Only ones which can
  /// populate `Dependency::maybe_code`.
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
    let code = self.maybe_code.includes(position);
    if code.is_none() {
      self.maybe_type.includes(position)
    } else {
      code
    }
  }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TypesDependency {
  pub specifier: String,
  pub dependency: Resolution,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ModuleKind {
  /// An asserted module. The import location is required to determine what the
  /// asserted type is as well as a loader/runtime would want to ensure the
  /// `MediaType` matches the assertion. Dependency analysis does not occur on
  /// asserted modules.
  Asserted,
  /// An ECMAScript Module (JavaScript Module).
  Esm,
  /// Represents a module which is not statically analyzed and is only available
  /// at runtime. It is up to the implementor to ensure that the module is
  /// loaded and available as a dependency. The module does not contain source
  /// code and will have no dependencies.
  External,
  /// A JavaScript script module. A slight misnomer, but it allows "plain"
  /// scripts to be imported into the module graph, but without supporting any
  /// dependency analysis.
  Script,
}

fn is_media_type_unknown(media_type: &MediaType) -> bool {
  matches!(media_type, MediaType::Unknown)
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Module {
  #[serde(
    skip_serializing_if = "BTreeMap::is_empty",
    serialize_with = "serialize_dependencies"
  )]
  pub dependencies: BTreeMap<String, Dependency>,
  pub kind: ModuleKind,
  #[serde(flatten, skip_serializing_if = "Option::is_none")]
  pub maybe_cache_info: Option<CacheInfo>,
  #[serde(
    rename = "size",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_maybe_source"
  )]
  pub maybe_source: Option<Arc<str>>,
  #[serde(rename = "typesDependency", skip_serializing_if = "Option::is_none")]
  pub maybe_types_dependency: Option<TypesDependency>,
  #[serde(skip_serializing_if = "is_media_type_unknown")]
  pub media_type: MediaType,
  pub specifier: ModuleSpecifier,
}

impl Module {
  fn new(
    specifier: ModuleSpecifier,
    kind: ModuleKind,
    source: Arc<str>,
  ) -> Self {
    Self {
      dependencies: Default::default(),
      kind,
      maybe_cache_info: None,
      maybe_source: Some(source),
      maybe_types_dependency: None,
      media_type: MediaType::Unknown,
      specifier,
    }
  }

  pub fn new_without_source(
    specifier: ModuleSpecifier,
    kind: ModuleKind,
  ) -> Self {
    Self {
      dependencies: Default::default(),
      kind,
      maybe_cache_info: None,
      maybe_source: None,
      maybe_types_dependency: None,
      media_type: MediaType::Unknown,
      specifier,
    }
  }

  /// Return the size in bytes of the content of the module.
  pub fn size(&self) -> usize {
    self
      .maybe_source
      .as_ref()
      .map(|s| s.as_bytes().len())
      .unwrap_or(0)
  }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub(crate) enum ModuleSlot {
  /// A module, with source code.
  Module(Module),
  /// When trying to load or parse the module, an error occurred.
  Err(ModuleGraphError),
  /// An internal state set when loading a module asynchronously.
  Pending,
}

type ModuleResult<'a> = (
  &'a ModuleSpecifier,
  Result<&'a Module, &'a ModuleGraphError>,
);

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GraphImport {
  /// A map of resolved dependencies, where the key is the value originally
  /// provided for the import and the value is the resolved dependency.
  pub dependencies: BTreeMap<String, Dependency>,
}

impl GraphImport {
  pub fn new(
    referrer: &ModuleSpecifier,
    imports: Vec<String>,
    maybe_resolver: Option<&dyn Resolver>,
  ) -> Self {
    let referrer_range = Range {
      specifier: referrer.clone(),
      start: Position::zeroed(),
      end: Position::zeroed(),
    };
    let dependencies = imports
      .iter()
      .map(|import| {
        let maybe_type = resolve(import, &referrer_range, maybe_resolver);
        (
          import.clone(),
          Dependency {
            is_dynamic: false,
            maybe_code: Resolution::None,
            maybe_type,
            imports: vec![],
          },
        )
      })
      .collect();
    Self { dependencies }
  }
}

#[derive(Default)]
pub struct BuildOptions<'a> {
  pub is_dynamic: bool,
  /// Additional imports that should be brought into the scope of
  /// the module graph to add to the graph's "imports". This may
  /// be extra modules such as TypeScript's "types" option or JSX
  /// runtime types.
  pub imports: Vec<ReferrerImports>,
  pub resolver: Option<&'a dyn Resolver>,
  pub module_analyzer: Option<&'a dyn ModuleAnalyzer>,
  pub reporter: Option<&'a dyn Reporter>,
}

#[derive(Debug, Copy, Clone)]
pub enum ModuleEntryRef<'a> {
  Module(&'a Module),
  Err(&'a ModuleGraphError),
  Redirect(&'a ModuleSpecifier),
}

#[derive(Debug, Copy, Clone)]
pub struct WalkOptions {
  pub follow_dynamic: bool,
  pub follow_type_only: bool,
  pub check_js: bool,
}

impl Default for WalkOptions {
  fn default() -> Self {
    Self {
      follow_dynamic: true,
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
}

impl<'a> ModuleEntryIterator<'a> {
  fn new(
    graph: &'a ModuleGraph,
    roots: &'a [ModuleSpecifier],
    options: WalkOptions,
  ) -> Self {
    let mut seen = HashSet::<&'a ModuleSpecifier>::with_capacity(
      graph.roots.len() + graph.redirects.len(),
    );
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
          if !seen.contains(specifier) {
            seen.insert(specifier);
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
    }
  }

  /// Consumes the iterator validating all the items for any resolution
  /// or module graph errors.
  ///
  /// This is different than calling `.valid()` on a module graph because
  /// it only applies to the roots filtered by the iterator with the provided
  /// options.
  #[allow(clippy::result_large_err)]
  pub fn validate(self) -> Result<(), ModuleGraphError> {
    let follow_type_only = self.follow_type_only;
    let check_js = self.check_js;
    for (_, module_entry) in self {
      match module_entry {
        ModuleEntryRef::Module(module) => {
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
            if let Some(Resolution::Err(error)) = module
              .maybe_types_dependency
              .as_ref()
              .map(|d| &d.dependency)
            {
              return Err(ModuleGraphError::ResolutionError(*error.clone()));
            }
          }
          for dep in module.dependencies.values() {
            if !dep.is_dynamic {
              let mut resolutions = vec![&dep.maybe_code];
              if check_types {
                resolutions.push(&dep.maybe_type);
              }
              #[allow(clippy::manual_flatten)]
              for resolution in resolutions {
                if let Resolution::Err(error) = resolution {
                  return Err(ModuleGraphError::ResolutionError(
                    *error.clone(),
                  ));
                }
              }
              for import in &dep.imports {
                if !import.is_dynamic {
                  if let Some(error) = import.errors.first() {
                    return Err(ModuleGraphError::ImportError(error.clone()));
                  }
                }
              }
            }
          }
        }
        ModuleEntryRef::Err(error) => {
          return Err(error.clone());
        }
        _ => {}
      }
    }
    Ok(())
  }
}

impl<'a> Iterator for ModuleEntryIterator<'a> {
  type Item = (&'a ModuleSpecifier, ModuleEntryRef<'a>);

  fn next(&mut self) -> Option<Self::Item> {
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

    match &module_entry {
      ModuleEntryRef::Module(module) => {
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
            if !self.seen.contains(specifier) {
              self.seen.insert(specifier);
              self.visiting.push_front(specifier);
            }
          }
        }
        for dep in module.dependencies.values().rev() {
          if !dep.is_dynamic || self.follow_dynamic {
            let mut resolutions = vec![&dep.maybe_code];
            if check_types {
              resolutions.push(&dep.maybe_type);
            }
            #[allow(clippy::manual_flatten)]
            for resolution in resolutions {
              if let Resolution::Ok(resolved) = resolution {
                let specifier = &resolved.specifier;
                if !self.seen.contains(specifier) {
                  self.seen.insert(specifier);
                  self.visiting.push_front(specifier);
                }
              }
            }
          }
        }
      }
      ModuleEntryRef::Err(_) => {}
      ModuleEntryRef::Redirect(specifier) => {
        if !self.seen.contains(specifier) {
          self.seen.insert(specifier);
          self.visiting.push_front(specifier);
        }
      }
    }

    Some((specifier, module_entry))
  }
}

/// The structure which represents a module graph, which can be serialized as
/// well as "printed".  The roots of the graph represent the "starting" point
/// which can be located in the module "slots" in the graph. The graph also
/// contains any redirects where the requested module specifier was redirected
/// to another module specifier when being loaded.
#[derive(Debug, Default, Clone)]
pub struct ModuleGraph {
  graph_kind: GraphKind,
  pub roots: Vec<ModuleSpecifier>,
  pub(crate) module_slots: BTreeMap<ModuleSpecifier, ModuleSlot>,
  pub imports: BTreeMap<ModuleSpecifier, GraphImport>,
  pub redirects: BTreeMap<ModuleSpecifier, ModuleSpecifier>,
}

impl ModuleGraph {
  pub fn new(graph_kind: GraphKind) -> Self {
    Self {
      graph_kind,
      roots: Default::default(),
      module_slots: Default::default(),
      imports: Default::default(),
      redirects: Default::default(),
    }
  }

  pub async fn build<'a>(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    loader: &mut dyn Loader,
    options: BuildOptions<'a>,
  ) {
    let default_analyzer = DefaultModuleAnalyzer::default();
    Builder::build(
      self,
      roots,
      options.imports,
      options.is_dynamic,
      options.resolver,
      loader,
      options.module_analyzer.unwrap_or(&default_analyzer),
      options.reporter,
    )
    .await
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

  /// Returns any errors that are in the module graph.
  pub fn errors(&self) -> impl Iterator<Item = &ModuleGraphError> {
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

  /// Return a vector of references to ES module objects in the graph. Only ES
  /// modules that were fully resolved are present, as "errors" are omitted. If
  /// you need to know what errors are in the graph, use the `.errors()` method,
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
        eprintln!("An infinite loop of redirections detected.\n  Original specifier: {specifier}");
        break;
      }
      redirected_specifier = specifier;
      if seen.len() >= max_redirects {
        eprintln!("An excessive number of redirections detected.\n  Original specifier: {specifier}");
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
  ) -> Option<&ModuleSpecifier> {
    let referrer = self.resolve(referrer);
    let specifier = if let Some(ModuleSlot::Module(referring_module)) =
      self.module_slots.get(&referrer)
    {
      let dependency = referring_module.dependencies.get(specifier)?;
      self.resolve_dependency_specifier(dependency, prefer_types)
    } else if let Some(graph_import) = self.imports.get(&referrer) {
      let dependency = graph_import.dependencies.get(specifier)?;
      self.resolve_dependency_specifier(dependency, prefer_types)
    } else {
      None
    }?;
    // Even if we resolved the specifier, it doesn't mean the module is actually
    // there, and so we will return the final final specifier.
    match self.module_slots.get(&self.resolve(specifier)) {
      Some(ModuleSlot::Module(m)) => Some(&m.specifier),
      _ => None,
    }
  }

  fn resolve_dependency_specifier<'a>(
    &'a self,
    dependency: &'a Dependency,
    prefer_types: bool,
  ) -> Option<&'a ModuleSpecifier> {
    let (maybe_first, maybe_second) = if prefer_types {
      (&dependency.maybe_type, &dependency.maybe_code)
    } else {
      (&dependency.maybe_code, &dependency.maybe_type)
    };
    let specifier = maybe_first
      .maybe_specifier()
      .or_else(|| maybe_second.maybe_specifier());
    match specifier {
      Some(specifier) => {
        if prefer_types {
          return Some(
            self
              .resolve_types_dependency(specifier)
              .unwrap_or(specifier),
          );
        }
        Some(specifier)
      }
      None => None,
    }
  }

  /// For a given specifier, return optionally if it has a types only dependency
  /// assigned on the module. This occurs when there is a header or text in the
  /// module that assigns expresses the types only dependency.
  fn resolve_types_dependency(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&ModuleSpecifier> {
    if let Some(ModuleSlot::Module(module)) = self.module_slots.get(specifier) {
      if let Some(Resolution::Ok(resolved)) = module
        .maybe_types_dependency
        .as_ref()
        .map(|d| &d.dependency)
      {
        return Some(&resolved.specifier);
      }
    }
    None
  }

  /// Return the entries of the specifiers in the graph, where the first value
  /// is a module specifier and the second value is a result that contains a tuple of
  /// the module specifier, module kind, and media type, or the module graph
  /// error.
  pub fn specifiers(
    &self,
  ) -> impl Iterator<Item = (&ModuleSpecifier, Result<&Module, &ModuleGraphError>)>
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
  ) -> Result<Option<&Module>, &ModuleGraphError> {
    let specifier = self.resolve(specifier);
    match self.module_slots.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Ok(Some(module)),
      Some(ModuleSlot::Err(err)) => Err(err),
      _ => Ok(None),
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
}

/// Resolve a string specifier from a referring module, using the resolver if
/// present, returning the resolution result.
fn resolve(
  specifier: &str,
  referrer_range: &Range,
  maybe_resolver: Option<&dyn Resolver>,
) -> Resolution {
  if let Some(resolver) = maybe_resolver {
    let response = resolver.resolve(specifier, &referrer_range.specifier);
    Resolution::from_resolve_result(
      response,
      referrer_range.clone(),
      specifier,
      true,
    )
  } else {
    let response = resolve_import(specifier, &referrer_range.specifier)
      .map_err(|err| err.into());
    Resolution::from_resolve_result(
      response,
      referrer_range.clone(),
      specifier,
      false,
    )
  }
}

impl Serialize for ModuleGraph {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut graph = serializer.serialize_struct("ModuleGraph", 4)?;
    graph.serialize_field("roots", &self.roots)?;
    graph
      .serialize_field("modules", &SerializableModules(&self.module_slots))?;
    if self.imports.is_empty() {
      graph.skip_field("imports")?;
    } else {
      graph
        .serialize_field("imports", &SerializableGraphImports(&self.imports))?;
    }
    graph.serialize_field("redirects", &self.redirects)?;
    graph.end()
  }
}

/// With the provided information, parse a module and return its "module slot"
#[allow(clippy::too_many_arguments)]
#[allow(clippy::result_large_err)]
pub(crate) fn parse_module(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: Arc<str>,
  maybe_referrer: Option<Range>,
  maybe_kind: Option<ModuleKind>,
  maybe_resolver: Option<&dyn Resolver>,
  module_analyzer: &dyn ModuleAnalyzer,
  is_root: bool,
) -> Result<Module, ModuleGraphError> {
  let media_type =
    MediaType::from_specifier_and_headers(specifier, maybe_headers);

  if media_type == MediaType::Json {
    return Ok(Module {
      dependencies: Default::default(),
      kind: ModuleKind::Asserted,
      maybe_cache_info: None,
      maybe_source: Some(content),
      maybe_types_dependency: None,
      media_type: MediaType::Json,
      specifier: specifier.clone(),
    });
  }

  // Here we check for known ES Modules that we will analyze the dependencies of
  match &media_type {
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
      match module_analyzer.analyze(specifier, content.clone(), media_type) {
        Ok(module_info) => {
          // Return the module as a valid module
          Ok(parse_module_from_module_info(
            specifier,
            maybe_kind.unwrap_or(ModuleKind::Esm),
            media_type,
            maybe_headers,
            module_info,
            content,
            maybe_resolver,
          ))
        }
        Err(diagnostic) => {
          Err(ModuleGraphError::ParseErr(specifier.clone(), diagnostic))
        }
      }
    }
    MediaType::Unknown if is_root => {
      match module_analyzer.analyze(
        specifier,
        content.clone(),
        MediaType::JavaScript,
      ) {
        Ok(module_info) => {
          // Return the module as a valid module
          Ok(parse_module_from_module_info(
            specifier,
            maybe_kind.unwrap_or(ModuleKind::Esm),
            media_type,
            maybe_headers,
            module_info,
            content,
            maybe_resolver,
          ))
        }
        Err(diagnostic) => {
          Err(ModuleGraphError::ParseErr(specifier.clone(), diagnostic))
        }
      }
    }
    _ => Err(ModuleGraphError::UnsupportedMediaType {
      specifier: specifier.clone(),
      media_type,
      maybe_referrer,
    }),
  }
}

pub(crate) fn parse_module_from_module_info(
  specifier: &ModuleSpecifier,
  kind: ModuleKind,
  media_type: MediaType,
  maybe_headers: Option<&HashMap<String, String>>,
  module_info: ModuleInfo,
  source: Arc<str>,
  maybe_resolver: Option<&dyn Resolver>,
) -> Module {
  // Init the module and determine its media type
  let mut module = Module::new(specifier.clone(), kind, source);
  module.media_type = media_type;

  // Analyze the TypeScript triple-slash references
  for reference in module_info.ts_references {
    match reference {
      TypeScriptReference::Path(specifier) => {
        let dep = module
          .dependencies
          .entry(specifier.text.clone())
          .or_default();
        let range =
          Range::from_position_range(&module.specifier, &specifier.range);
        if dep.maybe_code.is_none() {
          dep.maybe_code = resolve(&specifier.text, &range, maybe_resolver);
        }
        dep.imports.push(Import {
          specifier: specifier.text.clone(),
          range: range.clone(),
          is_dynamic: false,
          assertions: Default::default(),
          errors: Default::default(),
        });
      }
      TypeScriptReference::Types(specifier) => {
        let range =
          Range::from_position_range(&module.specifier, &specifier.range);
        let dep_resolution = resolve(&specifier.text, &range, maybe_resolver);
        if is_untyped(&module.media_type) {
          module.maybe_types_dependency = Some(TypesDependency {
            specifier: specifier.text,
            dependency: dep_resolution,
          });
        } else {
          let dep = module.dependencies.entry(specifier.text).or_default();
          if dep.maybe_type.is_none() {
            dep.maybe_type = dep_resolution;
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
      let specifier =
        format!("{}/{}", import_source.text, jsx_import_source_module);
      let dep = module.dependencies.entry(specifier.clone()).or_default();
      let range =
        Range::from_position_range(&module.specifier, &import_source.range);
      if dep.maybe_code.is_none() {
        dep.maybe_code = resolve(&specifier, &range, maybe_resolver);
      }
      dep.imports.push(Import {
        specifier,
        range,
        is_dynamic: false,
        assertions: Default::default(),
        errors: Default::default(),
      });
    }
  }

  // Analyze any JSDoc type imports
  for specifier in module_info.jsdoc_imports {
    let dep = module
      .dependencies
      .entry(specifier.text.clone())
      .or_default();
    if dep.maybe_type.is_none() {
      let range =
        Range::from_position_range(&module.specifier, &specifier.range);
      dep.maybe_type = resolve(&specifier.text, &range, maybe_resolver);
    }
  }

  // Analyze the X-TypeScript-Types header
  if module.maybe_types_dependency.is_none() {
    if let Some(headers) = maybe_headers {
      if let Some(types_header) = headers.get("x-typescript-types") {
        let range = Range {
          specifier: module.specifier.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        };
        module.maybe_types_dependency = Some(TypesDependency {
          specifier: types_header.clone(),
          dependency: resolve(types_header, &range, maybe_resolver),
        });
      }
    }
  }

  // Use resolve_types from maybe_resolver
  if let Some(resolver) = maybe_resolver {
    // this will only get called if there is no other types dependency and
    // the media type is untyped.
    if module.maybe_types_dependency.is_none() && is_untyped(&module.media_type)
    {
      module.maybe_types_dependency =
        match resolver.resolve_types(&module.specifier) {
          Ok(Some((specifier, maybe_range))) => Some(TypesDependency {
            specifier: module.specifier.to_string(),
            dependency: Resolution::Ok(Box::new(ResolutionResolved {
              specifier: specifier.clone(),
              range: maybe_range.unwrap_or_else(|| Range {
                specifier,
                start: Position::zeroed(),
                end: Position::zeroed(),
              }),
            })),
          }),
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
  for desc in module_info.dependencies {
    let dep = module
      .dependencies
      .entry(desc.specifier.to_string())
      .or_default();
    let range =
      Range::from_position_range(&module.specifier, &desc.specifier_range);
    let dep_resolution = resolve(&desc.specifier, &range, maybe_resolver);
    if matches!(
      desc.kind,
      DependencyKind::ImportType | DependencyKind::ExportType
    ) {
      if dep.maybe_type.is_none() {
        dep.maybe_type = dep_resolution;
      }
    } else {
      if dep.maybe_code.is_none() {
        // This is a code import, the first one of that specifier in this module.
        // Resolve and determine the initial `is_dynamic` value from it.
        dep.maybe_code = dep_resolution;
        dep.is_dynamic = desc.is_dynamic;
      } else {
        // This is a code import, but not the first one of that specifier in this
        // module. Maybe update the `is_dynamic` value. Static imports take
        // precedence. Note that `@jsxImportSource` and `/// <reference path />`
        // count as static imports for this purpose.
        dep.is_dynamic = dep.is_dynamic && desc.is_dynamic;
      }
      dep.imports.push(Import {
        specifier: desc.specifier.clone(),
        range,
        is_dynamic: desc.is_dynamic,
        assertions: desc.import_assertions.clone(),
        errors: vec![],
      });
    }
    if dep.maybe_type.is_none() {
      let specifier = module.specifier.clone();
      let maybe_type = analyze_deno_types(&desc)
        .map(|pragma| {
          resolve(
            &pragma.specifier,
            &Range::from_position_range(&specifier, &pragma.range),
            maybe_resolver,
          )
        })
        .unwrap_or_else(|| Resolution::None);
      dep.maybe_type = maybe_type
    }
  }

  // Return the module as a valid module
  module
}

/// Determine if a media type is "untyped" and should be checked to see if there
/// are types provided.
fn is_untyped(media_type: &MediaType) -> bool {
  matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  )
}

/// Check import assertions.
fn check_import_assertions(
  import: &Import,
  maybe_module_metadata: Option<&ModuleMetadata>,
) -> Vec<ImportError> {
  let mut errors = vec![];
  if let Some(assert_type) = import.assertions.get("type") {
    match SUPPORTED_ASSERT_TYPES.get(assert_type) {
      Some(&expected_media_type) => {
        if let Some(module_metadata) = maybe_module_metadata {
          if module_metadata.media_type != expected_media_type {
            errors.push(ImportError::TypeAssertionFailed {
              specifier: module_metadata.specifier.clone(),
              range: import.range.clone(),
              actual_media_type: module_metadata.media_type,
              expected_media_type,
            })
          }
        }
      }
      None => errors.push(ImportError::TypeAssertionUnsupported {
        specifier: import.specifier.clone(),
        range: import.range.clone(),
        assert_type: assert_type.clone(),
      }),
    }
  } else if import.assertions.is_known_none("type") {
    if let Some(module_metadata) = maybe_module_metadata {
      if SUPPORTED_ASSERT_TYPES
        .values()
        .any(|t| t == &module_metadata.media_type)
      {
        errors.push(ImportError::TypeAssertionFailed {
          specifier: module_metadata.specifier.clone(),
          range: import.range.clone(),
          actual_media_type: module_metadata.media_type,
          expected_media_type: MediaType::JavaScript,
        })
      }
    }
  }
  errors
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

type LoadWithSpecifierFuture = Pin<
  Box<
    dyn Future<Output = (ModuleSpecifier, Option<Range>, LoadResult)> + 'static,
  >,
>;

#[derive(PartialEq, Eq, Hash)]
pub(crate) struct AssertTypeWithRange {
  range: Range,
  /// The kind of assertion (ex. "json").
  kind: String,
}

/// Metadata for modules which should suffice to check assertions on importers
/// of that module. Needs to be stored separately from `Builder::graph` for
/// concurrent borrows.
#[derive(Clone, Debug)]
struct ModuleMetadata {
  specifier: ModuleSpecifier,
  media_type: MediaType,
}

struct Builder<'a, 'graph> {
  in_dynamic_branch: bool,
  graph: &'graph mut ModuleGraph,
  loader: &'a mut dyn Loader,
  resolver: Option<&'a dyn Resolver>,
  pending: FuturesUnordered<LoadWithSpecifierFuture>,
  dynamic_branches: HashMap<ModuleSpecifier, Range>,
  module_analyzer: &'a dyn ModuleAnalyzer,
  reporter: Option<&'a dyn Reporter>,
  module_metadata: HashMap<ModuleSpecifier, ModuleMetadata>,
}

impl<'a, 'graph> Builder<'a, 'graph> {
  #[allow(clippy::too_many_arguments)]
  pub async fn build(
    graph: &'graph mut ModuleGraph,
    roots: Vec<ModuleSpecifier>,
    imports: Vec<ReferrerImports>,
    is_dynamic: bool,
    resolver: Option<&'a dyn Resolver>,
    loader: &'a mut dyn Loader,
    module_analyzer: &'a dyn ModuleAnalyzer,
    reporter: Option<&'a dyn Reporter>,
  ) {
    Self {
      in_dynamic_branch: is_dynamic,
      graph,
      loader,
      resolver,
      module_analyzer,
      reporter,
      pending: FuturesUnordered::new(),
      dynamic_branches: HashMap::new(),
      module_metadata: HashMap::new(),
    }
    .fill(roots, imports)
    .await
  }

  async fn fill(
    &mut self,
    roots: Vec<ModuleSpecifier>,
    imports: Vec<ReferrerImports>,
  ) {
    let roots = roots
      .into_iter()
      .filter(|r| !self.graph.roots.contains(r))
      .collect::<Vec<_>>();
    let imports = imports
      .into_iter()
      .filter(|r| !self.graph.imports.contains_key(&r.referrer))
      .collect::<Vec<_>>();
    self.graph.roots.extend(roots.clone());
    for root in roots {
      self.load(&root, None, self.in_dynamic_branch);
    }

    // Process any imports that are being added to the graph.
    for referrer_imports in imports {
      let referrer = referrer_imports.referrer;
      let imports = referrer_imports.imports;
      let graph_import = GraphImport::new(&referrer, imports, self.resolver);
      for dep in graph_import.dependencies.values() {
        if let Resolution::Ok(resolved) = &dep.maybe_type {
          self.load(
            &resolved.specifier,
            Some(&resolved.range),
            self.in_dynamic_branch,
          );
        }
      }
      self.graph.imports.insert(referrer, graph_import);
    }

    loop {
      let specifier = match self.pending.next().await {
        Some((specifier, maybe_referrer, Ok(Some(response)))) => {
          self.visit(&specifier, &response, maybe_referrer.clone());
          Some(specifier)
        }
        Some((specifier, maybe_range, Ok(None))) => {
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Err(ModuleGraphError::Missing(
              specifier.clone(),
              maybe_range,
            )),
          );
          Some(specifier)
        }
        Some((specifier, maybe_range, Err(err))) => {
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Err(ModuleGraphError::LoadingErr(
              specifier.clone(),
              maybe_range,
              Arc::new(err),
            )),
          );
          Some(specifier)
        }
        _ => None,
      };
      if let (Some(specifier), Some(reporter)) = (specifier, self.reporter) {
        let modules_total = self.graph.module_slots.len();
        let modules_done = modules_total - self.pending.len();
        reporter.on_load(&specifier, modules_done, modules_total);
      }
      if self.pending.is_empty() {
        // Start visiting queued up dynamic branches. We do this in a separate
        // pass after all static dependencies have been visited because:
        // - If a module is both statically and dynamically imported, we want
        //   the static import to take precedence and only load it with
        //   `is_dynamic: false`.
        // - It's more convenient for tracking whether or not we are currently
        //   visiting a dynamic branch.
        if !self.in_dynamic_branch {
          self.in_dynamic_branch = true;
          for (specifier, range) in std::mem::take(&mut self.dynamic_branches) {
            if !self.graph.module_slots.contains_key(&specifier) {
              self.load(&specifier, Some(&range), true);
            }
          }
        } else {
          break;
        }
      }
    }

    for slot in self.graph.module_slots.values_mut() {
      if let ModuleSlot::Module(ref mut module) = slot {
        // Enrich with cache info from the loader
        module.maybe_cache_info = self.loader.get_cache_info(&module.specifier);

        // Check import assertions
        for dep in module.dependencies.values_mut() {
          let module_metadata = dep
            .maybe_code
            .maybe_specifier()
            .and_then(|s| self.module_metadata.get(s));
          for import in &mut dep.imports {
            import.errors = check_import_assertions(import, module_metadata);
          }
        }
      }
    }
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
  ) {
    let specifier = self.graph.redirects.get(specifier).unwrap_or(specifier);
    if !self.graph.module_slots.contains_key(specifier) {
      self
        .graph
        .module_slots
        .insert(specifier.clone(), ModuleSlot::Pending);
      let specifier = specifier.clone();
      let maybe_range = maybe_range.map(ToOwned::to_owned);
      let fut = self
        .loader
        .load(&specifier, is_dynamic)
        .map(move |res| (specifier, maybe_range, res));
      self.pending.push(Box::pin(fut));
    }
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
    response: &LoadResponse,
    maybe_referrer: Option<Range>,
  ) {
    let (specifier, module_slot) = match response {
      LoadResponse::External { specifier } => {
        self.check_specifier(requested_specifier, specifier);
        let module_slot = ModuleSlot::Module(Module::new_without_source(
          specifier.clone(),
          ModuleKind::External,
        ));
        (specifier, module_slot)
      }
      LoadResponse::Module {
        specifier,
        content,
        maybe_headers,
      } => {
        self.check_specifier(requested_specifier, specifier);
        (
          specifier,
          self.visit_module(
            specifier,
            ModuleKind::Esm,
            maybe_headers.as_ref(),
            content.clone(),
            maybe_referrer,
          ),
        )
      }
    };
    self
      .graph
      .module_slots
      .insert(specifier.clone(), module_slot);
    let media_type = self
      .graph
      .get(specifier)
      .map(|m| m.media_type)
      .unwrap_or(MediaType::Unknown);
    self.module_metadata.insert(
      specifier.clone(),
      ModuleMetadata {
        specifier: specifier.clone(),
        media_type,
      },
    );
  }

  /// Visit a module, parsing it and resolving any dependencies.
  fn visit_module(
    &mut self,
    specifier: &ModuleSpecifier,
    kind: ModuleKind,
    maybe_headers: Option<&HashMap<String, String>>,
    content: Arc<str>,
    maybe_referrer: Option<Range>,
  ) -> ModuleSlot {
    use std::borrow::BorrowMut;
    let is_root = self.roots_contain(specifier);

    let mut module_slot = match parse_module(
      specifier,
      maybe_headers,
      content,
      maybe_referrer,
      Some(kind),
      self.resolver,
      self.module_analyzer,
      is_root,
    ) {
      Ok(module) => ModuleSlot::Module(module),
      Err(err) => ModuleSlot::Err(err),
    };

    if let ModuleSlot::Module(module) = module_slot.borrow_mut() {
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
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), range.clone());
              } else {
                self.load(specifier, Some(range), self.in_dynamic_branch);
              }
            }
          } else {
            dep.maybe_code = Resolution::None;
          }

          if matches!(
            self.graph.graph_kind,
            GraphKind::All | GraphKind::TypesOnly
          ) {
            if let Resolution::Ok(resolved) = &dep.maybe_type {
              let specifier = &resolved.specifier;
              let range = &resolved.range;
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), range.clone());
              } else {
                self.load(specifier, Some(range), self.in_dynamic_branch);
              }
            }
          } else {
            dep.maybe_type = Resolution::None;
          }
        }
      } else {
        module.dependencies.clear();
      }

      if matches!(self.graph.graph_kind, GraphKind::All | GraphKind::TypesOnly)
      {
        if let Some(Resolution::Ok(resolved)) = module
          .maybe_types_dependency
          .as_ref()
          .map(|d| &d.dependency)
        {
          self.load(&resolved.specifier, Some(&resolved.range), false);
        }
      } else {
        module.maybe_types_dependency = None;
      }
    }
    module_slot
  }
}

impl Serialize for Resolution {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serialize_resolution(self, serializer)
  }
}

struct SerializableDependency<'a>(&'a str, &'a Dependency);

impl<'a> Serialize for SerializableDependency<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut map = serializer.serialize_map(None)?;
    map.serialize_entry("specifier", self.0)?;
    if !self.1.maybe_code.is_none() {
      map.serialize_entry("code", &self.1.maybe_code)?;
    }
    if !self.1.maybe_type.is_none() {
      map.serialize_entry("type", &self.1.maybe_type)?;
    }
    if self.1.is_dynamic {
      map.serialize_entry("isDynamic", &self.1.is_dynamic)?;
    }
    if !self.1.imports.is_empty() {
      map.serialize_entry("imports", &self.1.imports)?;
    }
    map.end()
  }
}

struct SerializableDependencies<'a>(&'a BTreeMap<String, Dependency>);

impl<'a> Serialize for SerializableDependencies<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serialize_dependencies(self.0, serializer)
  }
}

fn serialize_dependencies<S>(
  dependencies: &BTreeMap<String, Dependency>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  let mut seq = serializer.serialize_seq(Some(dependencies.iter().count()))?;
  for (specifier_str, dep) in dependencies.iter() {
    let serializeable_dependency = SerializableDependency(specifier_str, dep);
    seq.serialize_element(&serializeable_dependency)?;
  }
  seq.end()
}

struct SerializableModuleSlot<'a>(&'a ModuleSpecifier, &'a ModuleSlot);

impl<'a> Serialize for SerializableModuleSlot<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    match self.1 {
      ModuleSlot::Module(module) => Serialize::serialize(module, serializer),
      ModuleSlot::Err(err) => {
        let mut state = serializer.serialize_struct("ModuleSlot", 2)?;
        state.serialize_field("specifier", self.0)?;
        state.serialize_field("error", &err.to_string())?;
        state.end()
      }
      ModuleSlot::Pending => {
        let mut state = serializer.serialize_struct("ModuleSlot", 2)?;
        state.serialize_field("specifier", self.0)?;
        state.serialize_field(
          "error",
          "[INTERNAL ERROR] A pending module load never completed.",
        )?;
        state.end()
      }
    }
  }
}

struct SerializableModules<'a>(&'a BTreeMap<ModuleSpecifier, ModuleSlot>);

impl<'a> Serialize for SerializableModules<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
    for (specifier, slot) in self.0.iter() {
      let serializeable_module_slot = SerializableModuleSlot(specifier, slot);
      seq.serialize_element(&serializeable_module_slot)?;
    }
    seq.end()
  }
}

struct SerializableGraphImports<'a>(&'a BTreeMap<ModuleSpecifier, GraphImport>);

impl<'a> Serialize for SerializableGraphImports<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
    for (key, value) in self.0.iter() {
      seq.serialize_element(&SerializableGraphImport(key, value))?;
    }
    seq.end()
  }
}

struct SerializableGraphImport<'a>(&'a ModuleSpecifier, &'a GraphImport);

impl<'a> Serialize for SerializableGraphImport<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut val = serializer.serialize_struct("GraphImport", 2)?;
    val.serialize_field("referrer", self.0)?;
    val.serialize_field(
      "dependencies",
      &SerializableDependencies(&self.1.dependencies),
    )?;
    val.end()
  }
}

fn serialize_maybe_source<S>(
  source: &Option<Arc<str>>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  if let Some(source) = source {
    serializer.serialize_u32(source.len() as u32)
  } else {
    serializer.serialize_none()
  }
}

#[cfg(test)]
mod tests {
  use crate::DefaultModuleAnalyzer;

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
  fn test_module_dependency_includes() {
    let specifier = ModuleSpecifier::parse("file:///a.ts").unwrap();
    let module_analyzer = DefaultModuleAnalyzer::default();
    let content = r#"import * as b from "./b.ts";"#;
    let module = parse_module(
      &specifier,
      None,
      content.into(),
      None,
      Some(ModuleKind::Esm),
      None,
      &module_analyzer,
      true,
    )
    .unwrap();
    let maybe_dependency = module.dependencies.values().find_map(|d| {
      d.includes(&Position {
        line: 0,
        character: 21,
      })
      .map(|r| (d, r))
    });
    assert!(maybe_dependency.is_some());
    let (dependency, range) = maybe_dependency.unwrap();
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
      range,
      &Range {
        specifier,
        start: Position {
          line: 0,
          character: 19
        },
        end: Position {
          line: 0,
          character: 27
        },
      }
    );

    let maybe_dependency = module.dependencies.values().find_map(|d| {
      d.includes(&Position {
        line: 0,
        character: 18,
      })
    });
    assert!(maybe_dependency.is_none());
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
        is_dynamic: bool,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => {
            assert!(!is_dynamic);
            self.loaded_foo = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: "await import('file:///bar.js')".into(),
              }))
            })
          }
          "file:///bar.js" => {
            assert!(is_dynamic);
            self.loaded_bar = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: "import 'file:///baz.js'".into(),
              }))
            })
          }
          "file:///baz.js" => {
            assert!(is_dynamic);
            self.loaded_baz = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: "console.log('Hello, world!')".into(),
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
  }

  #[tokio::test]
  async fn missing_module_is_error() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        _is_dynamic: bool,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: "await import('file:///bar.js')".into(),
            }))
          }),
          "file:///bar.js" => Box::pin(async move { Ok(None) }),
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
    assert!(graph
      .try_get(&Url::parse("file:///foo.js").unwrap())
      .is_ok());
    assert!(matches!(
      graph
        .try_get(&Url::parse("file:///bar.js").unwrap())
        .unwrap_err(),
      ModuleGraphError::Missing(..)
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
      ModuleGraphError::Missing(..)
    ));
  }

  #[tokio::test]
  async fn redirected_specifiers() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        _is_dynamic: bool,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: Url::parse("file:///foo_actual.js").unwrap(),
              maybe_headers: None,
              content: "import 'file:///bar.js'".into(),
            }))
          }),
          "file:///bar.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: Url::parse("file:///bar_actual.js").unwrap(),
              maybe_headers: None,
              content: "(".into(),
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
      ModuleGraphError::ParseErr(..)
    ));
    assert!(matches!(
      specifiers
        .get(&Url::parse("file:///bar_actual.js").unwrap())
        .unwrap()
        .as_ref()
        .unwrap_err(),
      ModuleGraphError::ParseErr(..)
    ));
  }

  #[test]
  fn local_import_remote_module() {
    let resolution = Resolution::from_specifier(
      Url::parse("file:///local/mod.ts").unwrap(),
      Range {
        specifier: Url::parse("https://localhost").unwrap(),
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      false,
    );
    assert!(matches!(
      resolution.err().unwrap(),
      ResolutionError::InvalidLocalImport { .. },
    ));
  }

  #[test]
  fn npm_import_remote_module() {
    let resolution = Resolution::from_specifier(
      Url::parse("npm:package").unwrap(),
      Range {
        specifier: Url::parse("https://localhost").unwrap(),
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      false,
    );
    assert!(matches!(resolution, Resolution::Ok { .. }));
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
        is_dynamic: bool,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content:
                "import 'file:///bar.js'; await import('file:///bar.js')".into(),
            }))
          }),
          "file:///bar.js" => {
            assert!(!is_dynamic);
            self.loaded_bar = true;
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: "console.log('Hello, world!')".into(),
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
  async fn import_assertion_errors() {
    struct TestLoader;
    impl Loader for TestLoader {
      fn load(
        &mut self,
        specifier: &ModuleSpecifier,
        is_dynamic: bool,
      ) -> LoadFuture {
        let specifier = specifier.clone();
        match specifier.as_str() {
          "file:///foo1.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: "
                import 'file:///bar.json' assert { type: 'json' };
                import 'file:///bar.json'; // error
              "
              .into(),
            }))
          }),
          "file:///foo2.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: "
                import 'file:///bar.json' assert { type: 'json' };
                import 'file:///bar.json' assert { type: 'invalid' }; // error
              "
              .into(),
            }))
          }),
          "file:///foo3.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: "
                import 'file:///foo2.js' assert { type: 'json' }; // error
                import 'file:///foo2.js';
              "
              .into(),
            }))
          }),
          "file:///foo4.js" => Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: "
                import 'file:///bar.json' assert { type: 'json' };
                await import('file:///bar.json'); // ignored error
              "
              .into(),
            }))
          }),
          "file:///bar.json" => {
            assert!(!is_dynamic);
            Box::pin(async move {
              Ok(Some(LoadResponse::Module {
                specifier: specifier.clone(),
                maybe_headers: Some(HashMap::from_iter(vec![(
                  "Content-Type".to_string(),
                  "application/json".to_string(),
                )])),
                content: "{}".into(),
              }))
            })
          }
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![
          Url::parse("file:///foo1.js").unwrap(),
          Url::parse("file:///foo2.js").unwrap(),
          Url::parse("file:///foo3.js").unwrap(),
          Url::parse("file:///foo4.js").unwrap(),
        ],
        &mut loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      graph
        .walk(
          &[Url::parse("file:///foo1.js").unwrap()],
          WalkOptions::default(),
        )
        .validate()
        .unwrap_err()
        .to_string(),
      "Expected a JavaScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: file:///bar.json"
    );
    assert_eq!(
      graph
        .walk(
          &[Url::parse("file:///foo2.js").unwrap()],
          WalkOptions::default(),
        )
        .validate()
        .unwrap_err()
        .to_string(),
      "The import assertion type of \"invalid\" is unsupported."
    );
    assert_eq!(
      graph
        .walk(
          &[Url::parse("file:///foo3.js").unwrap()],
          WalkOptions::default(),
        )
        .validate()
        .unwrap_err()
        .to_string(),
      "Expected a Json module, but identified a JavaScript module.\n  Specifier: file:///foo2.js"
    );
    graph
      .walk(
        &[Url::parse("file:///foo4.js").unwrap()],
        WalkOptions::default(),
      )
      .validate()
      .unwrap();
  }
}
