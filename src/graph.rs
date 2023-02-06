// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::analyzer::analyze_deno_types;
use crate::analyzer::DependencyKind;
use crate::analyzer::ModuleAnalyzer;
use crate::analyzer::ModuleInfo;
use crate::analyzer::PositionRange;
use crate::analyzer::SpecifierWithRange;
use crate::analyzer::TypeScriptReference;
use crate::DefaultModuleAnalyzer;
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
use std::fmt;
use std::pin::Pin;
use std::sync::Arc;

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
  InvalidTypeAssertion {
    specifier: ModuleSpecifier,
    range: Range,
    actual_media_type: MediaType,
    expected_media_type: MediaType,
  },
  LoadingErr(ModuleSpecifier, Arc<anyhow::Error>),
  Missing(ModuleSpecifier),
  ParseErr(ModuleSpecifier, deno_ast::Diagnostic),
  ResolutionError(ResolutionError),
  UnsupportedImportAssertionType {
    specifier: ModuleSpecifier,
    range: Range,
    kind: String,
  },
  UnsupportedMediaType(ModuleSpecifier, MediaType),
}

impl ModuleGraphError {
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::ResolutionError(err) => &err.range().specifier,
      Self::LoadingErr(s, _)
      | Self::ParseErr(s, _)
      | Self::UnsupportedMediaType(s, _)
      | Self::UnsupportedImportAssertionType { specifier: s, .. }
      | Self::InvalidTypeAssertion { specifier: s, .. }
      | Self::Missing(s) => s,
    }
  }
}

impl std::error::Error for ModuleGraphError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::LoadingErr(_, err) => Some(err.as_ref().as_ref()),
      Self::ResolutionError(ref err) => Some(err),
      Self::InvalidTypeAssertion { .. }
      | Self::Missing(_)
      | Self::ParseErr(_, _)
      | Self::UnsupportedImportAssertionType { .. }
      | Self::UnsupportedMediaType(_, _) => None,
    }
  }
}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LoadingErr(_, err) => err.fmt(f),
      Self::ParseErr(_, diagnostic) => write!(f, "The module's source code could not be parsed: {}", diagnostic),
      Self::ResolutionError(err) => err.fmt(f),
      Self::InvalidTypeAssertion { specifier, range, actual_media_type, expected_media_type } =>
        write!(f, "Expected a {} module, but identified a {} module.\n  Specifier: {}\n    at {}", expected_media_type, actual_media_type, specifier, range),
      Self::UnsupportedMediaType(specifier, MediaType::Json) => write!(f, "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: {}", specifier),
      Self::UnsupportedMediaType(specifier, media_type) => write!(f, "Expected a JavaScript or TypeScript module, but identified a {} module. Importing these types of modules is currently not supported.\n  Specifier: {}", media_type, specifier),
      Self::UnsupportedImportAssertionType { specifier, range, kind } =>
        write!(f, "The import assertion type of \"{}\" is unsupported.\n  Specifier: {}\n    at {}", kind, specifier, range),
      Self::Missing(specifier) => write!(f, "Module not found \"{}\".", specifier),
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
      Self::InvalidDowngrade { specifier, .. } => write!(f, "Modules imported via https are not allowed to import http modules.\n  Importing: {}", specifier),
      Self::InvalidLocalImport { specifier, .. } => write!(f, "Remote modules are not allowed to import local modules. Consider using a dynamic import instead.\n  Importing: {}", specifier),
      Self::ResolverError { error, .. } => error.fmt(f),
      Self::InvalidSpecifier { error, .. } => error.fmt(f),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolved {
  None,
  Ok {
    /// Specifier to.
    specifier: ModuleSpecifier,
    /// Referrer range.
    range: Box<Range>,
  },
  Err(ResolutionError),
}

impl Resolved {
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
        Self::Err(resolution_error)
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
      Resolved::Err(ResolutionError::InvalidDowngrade { specifier, range })
    } else if matches!(referrer_scheme, "https" | "http")
      && !matches!(specifier_scheme, "https" | "http" | "npm" | "node")
      && !remapped
    {
      Resolved::Err(ResolutionError::InvalidLocalImport { specifier, range })
    } else {
      Resolved::Ok {
        specifier,
        range: Box::new(range),
      }
    }
  }

  pub fn includes(&self, position: &Position) -> Option<&Range> {
    match self {
      Self::Ok { range, .. } if range.includes(position) => Some(range),
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
    if let Self::Ok { specifier, .. } = self {
      Some(specifier)
    } else {
      None
    }
  }
}

impl Default for Resolved {
  fn default() -> Self {
    Self::None
  }
}

fn serialize_resolved<S>(
  resolved: &Resolved,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  match resolved {
    Resolved::Ok {
      specifier, range, ..
    } => {
      let mut state = serializer.serialize_struct("ResolvedSpecifier", 2)?;
      state.serialize_field("specifier", specifier)?;
      state.serialize_field("span", range)?;
      state.end()
    }
    Resolved::Err(err) => {
      let mut state = serializer.serialize_struct("ResolvedError", 2)?;
      state.serialize_field("error", &err.to_string())?;
      state.serialize_field("span", err.range())?;
      state.end()
    }
    _ => Serialize::serialize(&serde_json::Value::Null, serializer),
  }
}

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Debug, Default, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
  #[serde(
    rename = "code",
    skip_serializing_if = "Resolved::is_none",
    serialize_with = "serialize_resolved"
  )]
  pub maybe_code: Resolved,
  #[serde(
    rename = "type",
    skip_serializing_if = "Resolved::is_none",
    serialize_with = "serialize_resolved"
  )]
  pub maybe_type: Resolved,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  #[serde(rename = "assertType", skip_serializing_if = "Option::is_none")]
  pub maybe_assert_type: Option<String>,
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
  #[serde(
    rename = "typesDependency",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_type_dependency"
  )]
  pub maybe_types_dependency: Option<(String, Resolved)>,
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
#[derive(Debug)]
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
#[derive(Debug, Clone)]
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
            maybe_code: Resolved::None,
            maybe_type,
            maybe_assert_type: None,
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

/// The structure which represents a module graph, which can be serialized as
/// well as "printed".  The roots of the graph represent the "starting" point
/// which can be located in the module "slots" in the graph. The graph also
/// contains any redirects where the requested module specifier was redirected
/// to another module specifier when being loaded.
#[derive(Debug, Default)]
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
    let mut seen = HashSet::new();
    seen.insert(redirected_specifier);
    while let Some(specifier) = self.redirects.get(redirected_specifier) {
      if !seen.insert(specifier) {
        eprintln!("An infinite loop of redirections detected.\n  Original specifier: {}", specifier);
        break;
      }
      redirected_specifier = specifier;
      if seen.len() > 5 {
        eprintln!("An excessive number of redirections detected.\n  Original specifier: {}", specifier);
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
      if let Some((_, Resolved::Ok { specifier, .. })) =
        &module.maybe_types_dependency
      {
        return Some(specifier);
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
  ) -> Result<Option<&Module>, ModuleGraphError> {
    let specifier = self.resolve(specifier);
    match self.module_slots.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Ok(Some(module)),
      Some(ModuleSlot::Err(err)) => Err(err.clone()),
      _ => Ok(None),
    }
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-type only, non-dynamic imports. The first error is
  /// returned as as error result, otherwise ok if there are no errors.
  pub fn valid(&self) -> Result<(), ModuleGraphError> {
    self.validate(false)
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-dynamic imports that are type only related. The first
  /// error is returned as an error result, otherwise ok if there are no errors.
  ///
  /// This is designed to be used in cases where the graph needs to be validated
  /// from a type checking perspective, prior to type checking the graph.
  pub fn valid_types_only(&self) -> Result<(), ModuleGraphError> {
    self.validate(true)
  }

  fn validate(&self, types_only: bool) -> Result<(), ModuleGraphError> {
    fn validate<F>(
      specifier: &ModuleSpecifier,
      types_only: bool,
      is_type: bool,
      seen: &mut HashSet<ModuleSpecifier>,
      get_module: &F,
    ) -> Result<(), ModuleGraphError>
    where
      F: Fn(&ModuleSpecifier) -> Result<Option<Module>, ModuleGraphError>,
    {
      if seen.contains(specifier) {
        return Ok(());
      }
      seen.insert(specifier.clone());
      let should_error = (is_type && types_only) || (!is_type && !types_only);
      match get_module(specifier) {
        Ok(Some(module)) => {
          if let Some((_, Resolved::Ok { specifier, .. })) =
            &module.maybe_types_dependency
          {
            validate(specifier, types_only, true, seen, get_module)?;
          }
          for dep in module.dependencies.values() {
            if !dep.is_dynamic {
              // TODO(@kitsonk) eliminate duplication with maybe_code below
              match &dep.maybe_type {
                Resolved::Ok { specifier, .. } => {
                  validate(specifier, types_only, true, seen, get_module)?
                }
                Resolved::Err(err) if types_only => {
                  return Err(err.into());
                }
                _ => (),
              }
              match &dep.maybe_code {
                Resolved::Ok { specifier, .. } => {
                  validate(specifier, types_only, false, seen, get_module)?
                }
                Resolved::Err(err) if !types_only => {
                  return Err(err.into());
                }
                _ => (),
              }
            }
          }
          Ok(())
        }
        Ok(None) if should_error => {
          Err(ModuleGraphError::Missing(specifier.clone()))
        }
        Err(err) if should_error => Err(err),
        _ => Ok(()),
      }
    }

    let mut seen = HashSet::new();
    for root in &self.roots {
      validate(root, types_only, false, &mut seen, &|s| {
        self.try_get(s).map(|o| o.cloned())
      })?;
    }
    Ok(())
  }
}

/// Resolve a string specifier from a referring module, using the resolver if
/// present, returning the resolution result.
fn resolve(
  specifier: &str,
  referrer_range: &Range,
  maybe_resolver: Option<&dyn Resolver>,
) -> Resolved {
  if let Some(resolver) = maybe_resolver {
    let response = resolver.resolve(specifier, &referrer_range.specifier);
    Resolved::from_resolve_result(
      response,
      referrer_range.clone(),
      specifier,
      true,
    )
  } else {
    let response = resolve_import(specifier, &referrer_range.specifier)
      .map_err(|err| err.into());
    Resolved::from_resolve_result(
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
pub(crate) fn parse_module(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: Arc<str>,
  maybe_assert_type: Option<AssertTypeWithRange>,
  maybe_kind: Option<ModuleKind>,
  maybe_resolver: Option<&dyn Resolver>,
  module_analyzer: &dyn ModuleAnalyzer,
  is_root: bool,
  is_dynamic_branch: bool,
) -> ModuleSlot {
  let media_type =
    MediaType::from_specifier_and_headers(specifier, maybe_headers);

  // here we check any media types that should have assertions made against them
  // if they aren't the root and add them to the graph, otherwise we continue
  if media_type == MediaType::Json
    && (is_root
      || is_dynamic_branch
      || matches!(
        maybe_assert_type.as_ref().map(|t| t.kind.as_str()),
        Some("json")
      ))
  {
    return ModuleSlot::Module(Module {
      dependencies: Default::default(),
      kind: ModuleKind::Asserted,
      maybe_cache_info: None,
      maybe_source: Some(content),
      maybe_types_dependency: None,
      media_type: MediaType::Json,
      specifier: specifier.clone(),
    });
  }

  if let Some(assert_type) = maybe_assert_type {
    if assert_type.kind == "json" {
      return ModuleSlot::Err(ModuleGraphError::InvalidTypeAssertion {
        specifier: specifier.clone(),
        range: assert_type.range,
        actual_media_type: media_type,
        expected_media_type: MediaType::Json,
      });
    } else {
      return ModuleSlot::Err(
        ModuleGraphError::UnsupportedImportAssertionType {
          specifier: specifier.clone(),
          range: assert_type.range,
          kind: assert_type.kind,
        },
      );
    }
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
          ModuleSlot::Module(parse_module_from_module_info(
            specifier,
            maybe_kind.unwrap_or(ModuleKind::Esm),
            media_type,
            maybe_headers,
            module_info,
            content,
            maybe_resolver,
          ))
        }
        Err(diagnostic) => ModuleSlot::Err(ModuleGraphError::ParseErr(
          specifier.clone(),
          diagnostic,
        )),
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
          ModuleSlot::Module(parse_module_from_module_info(
            specifier,
            maybe_kind.unwrap_or(ModuleKind::Esm),
            media_type,
            maybe_headers,
            module_info,
            content,
            maybe_resolver,
          ))
        }
        Err(diagnostic) => ModuleSlot::Err(ModuleGraphError::ParseErr(
          specifier.clone(),
          diagnostic,
        )),
      }
    }
    _ => ModuleSlot::Err(ModuleGraphError::UnsupportedMediaType(
      specifier.clone(),
      media_type,
    )),
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
        let range =
          Range::from_position_range(&module.specifier, &specifier.range);
        let resolved_dependency =
          resolve(&specifier.text, &range, maybe_resolver);
        let dep = module.dependencies.entry(specifier.text).or_default();
        dep.maybe_code = resolved_dependency;
      }
      TypeScriptReference::Types(specifier) => {
        let range =
          Range::from_position_range(&module.specifier, &specifier.range);
        let resolved_dependency =
          resolve(&specifier.text, &range, maybe_resolver);
        if is_untyped(&module.media_type) {
          module.maybe_types_dependency =
            Some((specifier.text, resolved_dependency));
        } else {
          let dep = module.dependencies.entry(specifier.text).or_default();
          dep.maybe_type = resolved_dependency;
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
      let range =
        Range::from_position_range(&module.specifier, &import_source.range);
      let resolved_dependency = resolve(&specifier, &range, maybe_resolver);
      let dep = module.dependencies.entry(specifier).or_default();
      dep.maybe_code = resolved_dependency;
    }
  }

  // Analyze any JSDoc type imports
  for specifier in module_info.jsdoc_imports {
    let range = Range::from_position_range(&module.specifier, &specifier.range);
    let resolved_dependency = resolve(&specifier.text, &range, maybe_resolver);
    let dep = module.dependencies.entry(specifier.text).or_default();
    dep.maybe_type = resolved_dependency;
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
        let resolved_dependency = resolve(types_header, &range, maybe_resolver);
        module.maybe_types_dependency =
          Some((types_header.clone(), resolved_dependency));
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
          Ok(Some((specifier, maybe_range))) => Some((
            module.specifier.to_string(),
            Resolved::Ok {
              specifier: specifier.clone(),
              range: Box::new(maybe_range.unwrap_or_else(|| Range {
                specifier,
                start: Position::zeroed(),
                end: Position::zeroed(),
              })),
            },
          )),
          Ok(None) => None,
          Err(err) => Some((
            module.specifier.to_string(),
            Resolved::Err(ResolutionError::ResolverError {
              error: Arc::new(err),
              specifier: module.specifier.to_string(),
              range: Range {
                specifier: module.specifier.clone(),
                start: Position::zeroed(),
                end: Position::zeroed(),
              },
            }),
          )),
        };
    }
  }

  // Analyze ES dependencies
  for desc in module_info.dependencies {
    let dep = module
      .dependencies
      .entry(desc.specifier.to_string())
      .or_default();
    dep.is_dynamic = desc.is_dynamic;
    dep.maybe_assert_type = desc.import_assertions.get("type").cloned();
    let resolved_dependency = resolve(
      &desc.specifier,
      &Range::from_position_range(&module.specifier, &desc.specifier_range),
      maybe_resolver,
    );
    if matches!(
      desc.kind,
      DependencyKind::ImportType | DependencyKind::ExportType
    ) {
      dep.maybe_type = resolved_dependency;
    } else {
      dep.maybe_code = resolved_dependency;
    }
    let specifier = module.specifier.clone();
    let maybe_type = analyze_deno_types(&desc)
      .map(|pragma| {
        resolve(
          &pragma.specifier,
          &Range::from_position_range(&specifier, &pragma.range),
          maybe_resolver,
        )
      })
      .unwrap_or_else(|| Resolved::None);
    if dep.maybe_type.is_none() {
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

/// The kind of module graph.
#[derive(Debug)]
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

pub type LoadWithSpecifierFuture =
  Pin<Box<dyn Future<Output = (ModuleSpecifier, LoadResult)> + 'static>>;

#[derive(PartialEq, Eq, Hash)]
pub(crate) struct AssertTypeWithRange {
  range: Range,
  /// The kind of assertion (ex. "json").
  kind: String,
}

struct Builder<'a, 'graph> {
  in_dynamic_branch: bool,
  graph: &'graph mut ModuleGraph,
  loader: &'a mut dyn Loader,
  resolver: Option<&'a dyn Resolver>,
  pending: FuturesUnordered<LoadWithSpecifierFuture>,
  pending_assert_types:
    HashMap<ModuleSpecifier, HashSet<Option<AssertTypeWithRange>>>,
  dynamic_branches: HashMap<ModuleSpecifier, Option<AssertTypeWithRange>>,
  module_analyzer: &'a dyn ModuleAnalyzer,
  reporter: Option<&'a dyn Reporter>,
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
      pending_assert_types: HashMap::new(),
      dynamic_branches: HashMap::new(),
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
      self.load(&root, self.in_dynamic_branch, None);
    }

    // Process any imports that are being added to the graph.
    for referrer_imports in imports {
      let referrer = referrer_imports.referrer;
      let imports = referrer_imports.imports;
      let graph_import = GraphImport::new(&referrer, imports, self.resolver);
      for dep in graph_import.dependencies.values() {
        if let Resolved::Ok { specifier, .. } = &dep.maybe_type {
          self.load(specifier, self.in_dynamic_branch, None);
        }
      }
      self.graph.imports.insert(referrer, graph_import);
    }

    loop {
      let specifier = match self.pending.next().await {
        Some((specifier, Ok(Some(response)))) => {
          let assert_types =
            self.pending_assert_types.remove(&specifier).unwrap();
          for maybe_assert_type in assert_types {
            self.visit(&specifier, &response, maybe_assert_type)
          }
          Some(specifier)
        }
        Some((specifier, Ok(None))) => {
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Err(ModuleGraphError::Missing(specifier.clone())),
          );
          Some(specifier)
        }
        Some((specifier, Err(err))) => {
          self.graph.module_slots.insert(
            specifier.clone(),
            ModuleSlot::Err(ModuleGraphError::LoadingErr(
              specifier.clone(),
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
          for (specifier, maybe_assert_type) in
            std::mem::take(&mut self.dynamic_branches)
          {
            if !self.graph.module_slots.contains_key(&specifier) {
              self.load(&specifier, true, maybe_assert_type);
            }
          }
        } else {
          break;
        }
      }
    }

    // Enrich with cache info from the loader
    for slot in self.graph.module_slots.values_mut() {
      if let ModuleSlot::Module(ref mut module) = slot {
        module.maybe_cache_info = self.loader.get_cache_info(&module.specifier);
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
    is_dynamic: bool,
    maybe_assert_type: Option<AssertTypeWithRange>,
  ) {
    let specifier = self.graph.redirects.get(specifier).unwrap_or(specifier);
    let assert_types = self
      .pending_assert_types
      .entry(specifier.clone())
      .or_default();
    assert_types.insert(maybe_assert_type);
    if !self.graph.module_slots.contains_key(specifier) {
      self
        .graph
        .module_slots
        .insert(specifier.clone(), ModuleSlot::Pending);
      let specifier = specifier.clone();
      let fut = self
        .loader
        .load(&specifier, is_dynamic)
        .map(move |res| (specifier, res));
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
    maybe_assert_type: Option<AssertTypeWithRange>,
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
            maybe_assert_type,
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
    kind: ModuleKind,
    maybe_headers: Option<&HashMap<String, String>>,
    content: Arc<str>,
    maybe_assert_type: Option<AssertTypeWithRange>,
  ) -> ModuleSlot {
    use std::borrow::BorrowMut;
    let is_root = self.roots_contain(specifier);

    let mut module_slot = parse_module(
      specifier,
      maybe_headers,
      content,
      maybe_assert_type,
      Some(kind),
      self.resolver,
      self.module_analyzer,
      is_root,
      self.in_dynamic_branch,
    );

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
            if let Resolved::Ok {
              specifier, range, ..
            } = &dep.maybe_code
            {
              let maybe_assert_type_with_range = dep
                .maybe_assert_type
                .as_ref()
                .map(|assert_type| AssertTypeWithRange {
                  range: *range.clone(),
                  kind: assert_type.clone(),
                });
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), maybe_assert_type_with_range);
              } else {
                self.load(
                  specifier,
                  self.in_dynamic_branch,
                  maybe_assert_type_with_range,
                );
              }
            }
          } else {
            dep.maybe_code = Resolved::None;
          }

          if matches!(
            self.graph.graph_kind,
            GraphKind::All | GraphKind::TypesOnly
          ) {
            if let Resolved::Ok {
              specifier, range, ..
            } = &dep.maybe_type
            {
              let maybe_assert_type_with_range = dep
                .maybe_assert_type
                .as_ref()
                .map(|assert_type| AssertTypeWithRange {
                  range: *range.clone(),
                  kind: assert_type.clone(),
                });
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), maybe_assert_type_with_range);
              } else {
                self.load(
                  specifier,
                  self.in_dynamic_branch,
                  maybe_assert_type_with_range,
                );
              }
            }
          } else {
            dep.maybe_type = Resolved::None;
          }
        }
      } else {
        module.dependencies.clear();
      }

      if matches!(self.graph.graph_kind, GraphKind::All | GraphKind::TypesOnly)
      {
        if let Some((_, Resolved::Ok { specifier, .. })) =
          &module.maybe_types_dependency
        {
          self.load(specifier, false, None);
        }
      } else {
        module.maybe_types_dependency = None;
      }
    }
    module_slot
  }
}

struct SerializableResolved<'a>(&'a Resolved);

impl<'a> Serialize for SerializableResolved<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serialize_resolved(self.0, serializer)
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
      let serializeable_resolved = SerializableResolved(&self.1.maybe_code);
      map.serialize_entry("code", &serializeable_resolved)?;
    }
    if !self.1.maybe_type.is_none() {
      let serializeable_resolved = SerializableResolved(&self.1.maybe_type);
      map.serialize_entry("type", &serializeable_resolved)?;
    }
    if self.1.is_dynamic {
      map.serialize_entry("isDynamic", &self.1.is_dynamic)?;
    }
    if self.1.maybe_assert_type.is_some() {
      map.serialize_entry("assertionType", &self.1.maybe_assert_type)?;
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

fn serialize_type_dependency<S>(
  maybe_types_dependency: &Option<(String, Resolved)>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  match *maybe_types_dependency {
    Some((ref specifier, ref resolved)) => {
      let mut state = serializer.serialize_struct("TypesDependency", 2)?;
      state.serialize_field("specifier", specifier)?;
      let serializeable_resolved = SerializableResolved(resolved);
      state.serialize_field("dependency", &serializeable_resolved)?;
      state.end()
    }
    None => serializer.serialize_none(),
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
    let slot = parse_module(
      &specifier,
      None,
      content.into(),
      None,
      Some(ModuleKind::Esm),
      None,
      &module_analyzer,
      true,
      false,
    );
    if let ModuleSlot::Module(module) = slot {
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
        Resolved::Ok {
          specifier: ModuleSpecifier::parse("file:///b.ts").unwrap(),
          range: Box::new(Range {
            specifier: specifier.clone(),
            start: Position {
              line: 0,
              character: 20,
            },
            end: Position {
              line: 0,
              character: 26,
            },
          }),
        }
      );
      assert_eq!(
        range,
        &Range {
          specifier,
          start: Position {
            line: 0,
            character: 20,
          },
          end: Position {
            line: 0,
            character: 26,
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
    } else {
      panic!("no module returned");
    }
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
    let resolved = Resolved::from_specifier(
      Url::parse("file:///local/mod.ts").unwrap(),
      Range {
        specifier: Url::parse("https://localhost").unwrap(),
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      false,
    );
    assert!(matches!(
      resolved,
      Resolved::Err(ResolutionError::InvalidLocalImport { .. })
    ));
  }

  #[test]
  fn npm_import_remote_module() {
    let resolved = Resolved::from_specifier(
      Url::parse("npm:package").unwrap(),
      Range {
        specifier: Url::parse("https://localhost").unwrap(),
        start: Position::zeroed(),
        end: Position::zeroed(),
      },
      false,
    );
    assert!(matches!(resolved, Resolved::Ok { .. }));
  }
}
