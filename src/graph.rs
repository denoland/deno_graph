// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::ast;
use crate::ast::analyze_deno_types;
use crate::ast::analyze_dependencies;
use crate::ast::analyze_jsdoc_imports;
use crate::ast::analyze_jsx_import_sources;
use crate::ast::analyze_ts_references;
use crate::ast::DependencyKind;
use crate::ast::SourceParser;
use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::module_specifier::SpecifierError;
use crate::source::*;

use anyhow::Result;
use deno_ast::MediaType;
use deno_ast::ParsedSource;
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
use std::cell::RefCell;
#[cfg(feature = "rust")]
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Position {
  /// The 0-indexed line index.
  pub line: usize,
  /// The 0-indexed character index.
  pub character: usize,
}

#[cfg(feature = "rust")]
impl PartialOrd for Position {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[cfg(feature = "rust")]
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
  pub fn zeroed() -> Position {
    Position {
      line: 0,
      character: 0,
    }
  }

  fn from_pos(
    parsed_source: &ParsedSource,
    pos: deno_ast::swc::common::BytePos,
  ) -> Self {
    let line_and_column_index =
      parsed_source.source().line_and_column_index(pos);
    Self {
      line: line_and_column_index.line_index,
      character: line_and_column_index.column_index,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
  pub(crate) fn from_swc_span(
    specifier: &ModuleSpecifier,
    parsed_source: &ParsedSource,
    span: &deno_ast::swc::common::Span,
  ) -> Range {
    Range {
      specifier: specifier.clone(),
      start: Position::from_pos(parsed_source, span.lo),
      end: Position::from_pos(parsed_source, span.hi),
    }
  }

  /// Determines if a given position is within the range.
  #[cfg(feature = "rust")]
  pub fn includes(&self, position: &Position) -> bool {
    (position >= &self.start) && (position <= &self.end)
  }
}

#[derive(Debug)]
pub enum ModuleGraphError {
  InvalidSource(ModuleSpecifier, Option<String>),
  InvalidTypeAssertion {
    specifier: ModuleSpecifier,
    actual_media_type: MediaType,
    expected_media_type: MediaType,
  },
  LoadingErr(ModuleSpecifier, Arc<anyhow::Error>),
  Missing(ModuleSpecifier),
  ParseErr(ModuleSpecifier, deno_ast::Diagnostic),
  ResolutionError(ResolutionError),
  UnsupportedImportAssertionType(ModuleSpecifier, String),
  UnsupportedMediaType(ModuleSpecifier, MediaType),
}

impl Clone for ModuleGraphError {
  fn clone(&self) -> Self {
    match self {
      Self::LoadingErr(specifier, err) => {
        Self::LoadingErr(specifier.clone(), err.clone())
      }
      Self::ParseErr(specifier, err) => {
        Self::ParseErr(specifier.clone(), err.clone())
      }
      Self::ResolutionError(err) => Self::ResolutionError(err.clone()),
      Self::InvalidSource(specifier, maybe_filename) => {
        Self::InvalidSource(specifier.clone(), maybe_filename.clone())
      }
      Self::InvalidTypeAssertion {
        specifier,
        actual_media_type,
        expected_media_type,
      } => Self::InvalidTypeAssertion {
        specifier: specifier.clone(),
        actual_media_type: *actual_media_type,
        expected_media_type: *expected_media_type,
      },
      Self::UnsupportedImportAssertionType(specifier, kind) => {
        Self::UnsupportedImportAssertionType(specifier.clone(), kind.clone())
      }
      Self::UnsupportedMediaType(specifier, media_type) => {
        Self::UnsupportedMediaType(specifier.clone(), *media_type)
      }
      Self::Missing(specifier) => Self::Missing(specifier.clone()),
    }
  }
}

impl ModuleGraphError {
  #[cfg(feature = "rust")]
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::ResolutionError(err) => &err.range().specifier,
      Self::LoadingErr(s, _)
      | Self::ParseErr(s, _)
      | Self::InvalidSource(s, _)
      | Self::UnsupportedMediaType(s, _)
      | Self::UnsupportedImportAssertionType(s, _)
      | Self::Missing(s) => s,
      Self::InvalidTypeAssertion { specifier, .. } => specifier,
    }
  }
}

impl std::error::Error for ModuleGraphError {}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LoadingErr(_, err) => err.fmt(f),
      Self::ParseErr(_, diagnostic) => write!(f, "The module's source code could not be parsed: {}", diagnostic),
      Self::ResolutionError(err) => err.fmt(f),
      Self::InvalidSource(specifier, Some(filename)) => write!(f, "The source code is invalid, as it does not match the expected hash in the lock file.\n  Specifier: {}\n  Lock file: {}", specifier, filename),
      Self::InvalidSource(specifier, None) => write!(f, "The source code is invalid, as it does not match the expected hash in the lock file.\n  Specifier: {}", specifier),
      Self::InvalidTypeAssertion { specifier, actual_media_type, expected_media_type } => write!(f, "Expected a {} module, but identified a {} module.\n  Specifier: {}", expected_media_type, actual_media_type, specifier),
      Self::UnsupportedMediaType(specifier, MediaType::Json) => write!(f, "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: {}", specifier),
      Self::UnsupportedMediaType(specifier, media_type) => write!(f, "Expected a JavaScript or TypeScript module, but identified a {} module. Importing these types of modules is currently not supported.\n  Specifier: {}", media_type, specifier),
      Self::UnsupportedImportAssertionType(_, kind) => write!(f, "The import assertion type of \"{}\" is unsupported.", kind),
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
  InvalidDowngrade(ModuleSpecifier, Range),
  InvalidLocalImport(ModuleSpecifier, Range),
  InvalidSpecifier(SpecifierError, Range),
  ResolverError(Arc<anyhow::Error>, String, Range),
}

impl ResolutionError {
  /// Return a reference to the range that the error applies to.
  pub fn range(&self) -> &Range {
    match self {
      Self::InvalidDowngrade(_, range)
      | Self::InvalidLocalImport(_, range)
      | Self::InvalidSpecifier(_, range)
      | Self::ResolverError(_, _, range) => range,
    }
  }

  /// Converts the error into a string along with the range related to the error.
  #[cfg(feature = "rust")]
  pub fn to_string_with_range(&self) -> String {
    match self {
      Self::InvalidDowngrade(_, range)
      | Self::InvalidLocalImport(_, range)
      | Self::InvalidSpecifier(_, range)
      | Self::ResolverError(_, _, range) => {
        format!("{}\n    at {}", self, range)
      }
    }
  }
}

impl std::error::Error for ResolutionError {}

impl PartialEq for ResolutionError {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (
        Self::ResolverError(_, a, a_range),
        Self::ResolverError(_, b, b_range),
      ) => a == b && a_range == b_range,
      (
        Self::InvalidDowngrade(a, a_range),
        Self::InvalidDowngrade(b, b_range),
      )
      | (
        Self::InvalidLocalImport(a, a_range),
        Self::InvalidLocalImport(b, b_range),
      ) => a == b && a_range == b_range,
      (
        Self::InvalidSpecifier(a, a_range),
        Self::InvalidSpecifier(b, b_range),
      ) => a == b && a_range == b_range,
      _ => false,
    }
  }
}

impl Eq for ResolutionError {}

impl fmt::Display for ResolutionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::InvalidDowngrade(specifier, _) => write!(f, "Modules imported via https are not allowed to import http modules.\n  Importing: {}", specifier),
      Self::InvalidLocalImport(specifier, _) => write!(f, "Remote modules are not allowed to import local modules. Consider using a dynamic import instead.\n  Importing: {}", specifier),
      Self::ResolverError(err, _, _) => err.fmt(f),
      Self::InvalidSpecifier(err, _) => err.fmt(f),
    }
  }
}

pub type Resolved = Option<Result<(ModuleSpecifier, Range), ResolutionError>>;

fn serialize_resolved<S>(
  resolved: &Resolved,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  match resolved {
    Some(Ok((specifier, range))) => {
      let mut state = serializer.serialize_struct("ResolvedSpecifier", 2)?;
      state.serialize_field("specifier", specifier)?;
      state.serialize_field("span", range)?;
      state.end()
    }
    Some(Err(err)) => {
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

fn is_media_type_unknown(media_type: &MediaType) -> bool {
  matches!(media_type, MediaType::Unknown)
}

#[derive(Debug, Default, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
  #[serde(
    rename = "code",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_resolved"
  )]
  pub maybe_code: Resolved,
  #[serde(
    rename = "type",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_resolved"
  )]
  pub maybe_type: Resolved,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
  #[serde(rename = "assertType", skip_serializing_if = "Option::is_none")]
  pub maybe_assert_type: Option<String>,
}

#[cfg(feature = "rust")]
impl Dependency {
  /// Optionally return the module specifier in the module graph that points to
  /// the "code" dependency in the graph.
  pub fn get_code(&self) -> Option<&ModuleSpecifier> {
    match &self.maybe_code {
      Some(Ok((specifier, _))) => Some(specifier),
      _ => None,
    }
  }

  /// Optionally return the module specifier in the module graph that points to
  /// the type only dependency in the graph.
  pub fn get_type(&self) -> Option<&ModuleSpecifier> {
    match &self.maybe_type {
      Some(Ok((specifier, _))) => Some(specifier),
      _ => None,
    }
  }

  /// Check to see if the position falls within the range of the code or types
  /// entry for the dependency, returning a reference to the range if true,
  /// otherwise none.
  pub fn includes(&self, position: &Position) -> Option<&Range> {
    match &self.maybe_code {
      Some(Ok((_, range))) if range.includes(position) => return Some(range),
      Some(Err(err)) if err.range().includes(position) => {
        return Some(err.range())
      }
      _ => (),
    }
    match &self.maybe_type {
      Some(Ok((_, range))) if range.includes(position) => Some(range),
      Some(Err(err)) if err.range().includes(position) => Some(err.range()),
      _ => None,
    }
  }
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Module {
  Es(Box<EsModule>),
  Synthetic(Box<SyntheticModule>),
}

impl Module {
  pub fn maybe_cache_info(&self) -> Option<&CacheInfo> {
    match self {
      Self::Es(m) => m.maybe_cache_info.as_ref(),
      Self::Synthetic(m) => m.maybe_cache_info.as_ref(),
    }
  }

  pub fn maybe_checksum(&self) -> Option<&str> {
    match self {
      Self::Es(m) => m.maybe_checksum.as_deref(),
      Self::Synthetic(m) => m.maybe_checksum.as_deref(),
    }
  }

  pub fn maybe_dependencies(&self) -> Option<&BTreeMap<String, Dependency>> {
    match self {
      Self::Es(m) => Some(&m.dependencies),
      _ => None,
    }
  }

  pub fn maybe_source(&self) -> Option<&str> {
    match self {
      Self::Es(m) => Some(m.source.as_str()),
      Self::Synthetic(m) => m.maybe_source.as_ref().map(|s| s.as_str()),
    }
  }

  pub fn maybe_types_dependency(&self) -> Option<&(String, Resolved)> {
    match self {
      Self::Es(m) => m.maybe_types_dependency.as_ref(),
      _ => None,
    }
  }

  pub fn media_type(&self) -> &MediaType {
    match self {
      Self::Es(m) => &m.media_type,
      Self::Synthetic(m) => &m.media_type,
    }
  }

  pub fn size(&self) -> usize {
    match self {
      Self::Es(m) => m.size(),
      Self::Synthetic(m) => m.size(),
    }
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      Self::Es(m) => &m.specifier,
      Self::Synthetic(m) => &m.specifier,
    }
  }

  #[cfg(feature = "rust")]
  pub fn to_maybe_es_module(self) -> Option<EsModule> {
    match self {
      Self::Es(m) => Some(*m),
      _ => None,
    }
  }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EsModule {
  #[serde(serialize_with = "serialize_dependencies")]
  pub dependencies: BTreeMap<String, Dependency>,
  #[serde(flatten, skip_serializing_if = "Option::is_none")]
  pub maybe_cache_info: Option<CacheInfo>,
  #[serde(rename = "checksum", skip_serializing_if = "Option::is_none")]
  pub maybe_checksum: Option<String>,
  #[serde(
    rename = "typesDependency",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_type_dependency"
  )]
  pub maybe_types_dependency: Option<(String, Resolved)>,
  pub media_type: MediaType,
  #[serde(skip_serializing)]
  pub parsed_source: ParsedSource,
  #[serde(rename = "size", serialize_with = "serialize_source")]
  pub source: Arc<String>,
  pub specifier: ModuleSpecifier,
}

impl EsModule {
  fn new(specifier: ModuleSpecifier, parsed_source: ParsedSource) -> Self {
    let source = parsed_source.source().text();
    Self {
      dependencies: Default::default(),
      maybe_cache_info: None,
      maybe_checksum: None,
      maybe_types_dependency: None,
      media_type: MediaType::Unknown,
      parsed_source,
      source,
      specifier,
    }
  }

  /// Return the size in bytes of the content of the module.
  pub fn size(&self) -> usize {
    self.source.as_bytes().len()
  }
}

/// A synthetic module is a module that is not an ES module. These modules serve
/// two purposes, the ability to inject modules and their dependencies into the
/// graph (like config files and TypeScript "types") as well as provide a way
/// for the graph to contain asserted modules, like JSON modules.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SyntheticModule {
  #[serde(
    skip_serializing_if = "BTreeMap::is_empty",
    serialize_with = "serialize_synthetic_dependencies"
  )]
  pub dependencies: BTreeMap<String, Resolved>,
  #[serde(flatten, skip_serializing_if = "Option::is_none")]
  pub maybe_cache_info: Option<CacheInfo>,
  #[serde(rename = "checksum", skip_serializing_if = "Option::is_none")]
  pub maybe_checksum: Option<String>,
  #[serde(
    rename = "size",
    skip_serializing_if = "Option::is_none",
    serialize_with = "serialize_maybe_source"
  )]
  pub maybe_source: Option<Arc<String>>,
  #[serde(skip_serializing_if = "is_media_type_unknown")]
  pub media_type: MediaType,
  pub specifier: ModuleSpecifier,
}

impl SyntheticModule {
  pub fn new(
    specifier: ModuleSpecifier,
    media_type: MediaType,
    maybe_dependencies: Option<Vec<String>>,
    maybe_source: Option<Arc<String>>,
    maybe_resolver: Option<&dyn Resolver>,
  ) -> Self {
    let dependencies = maybe_dependencies
      .unwrap_or_default()
      .iter()
      .map(|s| {
        let referrer_range = Range {
          specifier: specifier.clone(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        };
        let result = resolve(s, &referrer_range, maybe_resolver);
        (s.clone(), result)
      })
      .collect();
    Self {
      dependencies,
      maybe_cache_info: None,
      maybe_checksum: None,
      maybe_source,
      media_type,
      specifier,
    }
  }

  pub fn size(&self) -> usize {
    self
      .maybe_source
      .as_ref()
      .map(|s| s.as_bytes().len())
      .unwrap_or(0)
  }
}

struct SerializeableSyntheticDependency<'a>(&'a str, &'a Resolved);

impl Serialize for SerializeableSyntheticDependency<'_> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut dep = serializer.serialize_map(Some(2))?;
    dep.serialize_entry("specifier", self.0)?;
    let serializeable_resolved = SerializeableResolved(self.1);
    dep.serialize_entry("type", &serializeable_resolved)?;
    dep.end()
  }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub(crate) enum ModuleSlot {
  /// A module, with source code.
  Module(Module),
  /// When trying to load or parse the module, an error occurred.
  Err(ModuleGraphError),
  /// The module was requested but could not be loaded.
  Missing,
  /// An internal state set when loading a module asynchronously.
  Pending,
}

impl ModuleSlot {
  pub fn is_module(&self) -> bool {
    matches!(*self, Self::Module(_))
  }
}

#[cfg(feature = "rust")]
type ModuleResult = (
  ModuleSpecifier,
  Result<(ModuleSpecifier, MediaType), ModuleGraphError>,
);

/// Convert a module slot entry into a result which contains the resolved
/// module specifier and media type or the module graph error.
#[cfg(feature = "rust")]
fn to_result(
  (specifier, module_slot): (&ModuleSpecifier, &ModuleSlot),
) -> Option<ModuleResult> {
  match module_slot {
    ModuleSlot::Err(err) => Some((specifier.clone(), Err(err.clone()))),
    ModuleSlot::Missing => Some((
      specifier.clone(),
      Err(ModuleGraphError::Missing(specifier.clone())),
    )),
    ModuleSlot::Module(Module::Es(module)) => Some((
      specifier.clone(),
      Ok((module.specifier.clone(), module.media_type)),
    )),
    ModuleSlot::Module(Module::Synthetic(module)) => Some((
      specifier.clone(),
      Ok((module.specifier.clone(), module.media_type)),
    )),
    _ => None,
  }
}

/// The structure which represents a module graph, which can be serialized as
/// well as "printed".  The roots of the graph represent the "starting" point
/// which can be located in the module "slots" in the graph. The graph also
/// contains any redirects where the requested module specifier was redirected
/// to another module specifier when being loaded.
#[derive(Debug, Serialize)]
pub struct ModuleGraph {
  pub roots: Vec<ModuleSpecifier>,
  #[serde(skip_serializing)]
  maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
  #[serde(serialize_with = "serialize_modules", rename = "modules")]
  pub(crate) module_slots: BTreeMap<ModuleSpecifier, ModuleSlot>,
  pub redirects: BTreeMap<ModuleSpecifier, ModuleSpecifier>,
}

impl ModuleGraph {
  fn new(
    roots: Vec<ModuleSpecifier>,
    maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
  ) -> Self {
    Self {
      roots,
      maybe_locker,
      module_slots: Default::default(),
      redirects: Default::default(),
    }
  }

  /// Returns `true` if the specifier resolves to a module within a graph,
  /// otherwise returns `false`.
  #[cfg(feature = "rust")]
  pub fn contains(&self, specifier: &ModuleSpecifier) -> bool {
    let specifier = self.resolve(specifier);
    self
      .module_slots
      .get(&specifier)
      .map_or(false, |ms| matches!(ms, ModuleSlot::Module(_)))
  }

  /// Returns any errors that are in the module graph.
  #[cfg(feature = "rust")]
  pub fn errors(&self) -> Vec<ModuleGraphError> {
    self
      .module_slots
      .iter()
      .filter_map(|(s, ms)| match ms {
        ModuleSlot::Err(err) => Some(err.clone()),
        ModuleSlot::Missing => Some(ModuleGraphError::Missing(s.clone())),
        _ => None,
      })
      .collect()
  }

  /// Get a module from the module graph, returning `None` if the module is not
  /// part of the graph, or if when loading the module it errored. If any module
  /// resolution error is needed, then use the `try_get()` method which will
  /// return any resolution error as the error in the result.
  #[cfg(feature = "rust")]
  pub fn get(&self, specifier: &ModuleSpecifier) -> Option<&Module> {
    let specifier = self.resolve(specifier);
    match self.module_slots.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Some(module),
      _ => None,
    }
  }

  /// Determine if the graph sources are valid by calling the passed locker. If
  /// the integrity of all the sources passes or if there is no locker supplied
  /// the method results in an ok, otherwise returns an error which indicates
  /// the first specifier that failed the integrity check.
  pub fn lock(&self) -> Result<(), ModuleGraphError> {
    if let Some(locker) = &self.maybe_locker {
      let mut locker = locker.borrow_mut();
      for (_, module_slot) in self.module_slots.iter() {
        if let Some((specifier, source)) = match module_slot {
          ModuleSlot::Module(Module::Es(module)) => {
            Some((&module.specifier, module.source.as_ref()))
          }
          ModuleSlot::Module(Module::Synthetic(module)) => module
            .maybe_source
            .as_ref()
            .map(|source| (&module.specifier, source.as_ref())),
          _ => None,
        } {
          if !locker.check_or_insert(specifier, source) {
            return Err(ModuleGraphError::InvalidSource(
              specifier.clone(),
              locker.get_filename(),
            ));
          }
        }
      }
    }
    Ok(())
  }

  /// Return a vector of references to ES module objects in the graph. Only ES
  /// modules that were fully resolved are present, as "errors" are omitted. If
  /// you need to know what errors are in the graph, use the `.errors()` method,
  /// or if you just need to check if everything is "ok" with the graph, use the
  /// `.valid()` method.
  #[cfg(feature = "rust")]
  pub fn modules(&self) -> Vec<&EsModule> {
    self
      .module_slots
      .iter()
      .filter_map(|(_, ms)| match ms {
        ModuleSlot::Module(Module::Es(m)) => Some(m.as_ref()),
        _ => None,
      })
      .collect()
  }

  #[cfg(feature = "rust")]
  pub fn synthetic_modules(&self) -> Vec<&SyntheticModule> {
    self
      .module_slots
      .iter()
      .filter_map(|(_, ms)| match ms {
        ModuleSlot::Module(Module::Synthetic(m)) => Some(m.as_ref()),
        _ => None,
      })
      .collect()
  }

  /// Returns a map of the fully resolved dependency graph of the module graph.
  #[cfg(feature = "rust")]
  pub fn resolution_map(
    &self,
  ) -> HashMap<ModuleSpecifier, HashMap<String, Resolved>> {
    let mut map = HashMap::new();
    for (referrer, module_slot) in &self.module_slots {
      if let ModuleSlot::Module(Module::Es(module)) = module_slot {
        let deps = module
          .dependencies
          .iter()
          .map(|(s, dep)| (s.clone(), dep.maybe_code.clone()))
          .collect();
        map.insert(referrer.clone(), deps);
      }
    }
    map
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
    let referring_module_slot = self.module_slots.get(&referrer)?;
    let maybe_specifier = match referring_module_slot {
      ModuleSlot::Module(Module::Es(referring_module)) => {
        let dependency = referring_module.dependencies.get(specifier)?;
        let (maybe_first, maybe_second) = if prefer_types {
          (&dependency.maybe_type, &dependency.maybe_code)
        } else {
          (&dependency.maybe_code, &dependency.maybe_type)
        };
        if let Some(Ok((specifier, _))) =
          maybe_first.as_ref().or_else(|| maybe_second.as_ref())
        {
          if prefer_types {
            Some(
              self
                .resolve_types_dependency(specifier)
                .unwrap_or(specifier),
            )
          } else {
            Some(specifier)
          }
        } else {
          None
        }
      }
      ModuleSlot::Module(Module::Synthetic(referring_module)) => {
        match referring_module.dependencies.get(specifier) {
          Some(Some(Ok((specifier, _)))) => {
            if prefer_types {
              Some(
                self
                  .resolve_types_dependency(specifier)
                  .unwrap_or(specifier),
              )
            } else {
              Some(specifier)
            }
          }
          _ => None,
        }
      }
      _ => return None,
    };
    // Even if we resolved the specifier, it doesn't mean the module is actually
    // there, and so we will return the final final specifier.
    let specifier = maybe_specifier?;
    match self.module_slots.get(&self.resolve(specifier)) {
      Some(ModuleSlot::Module(Module::Es(m))) => Some(&m.specifier),
      Some(ModuleSlot::Module(Module::Synthetic(m))) => Some(&m.specifier),
      _ => None,
    }
  }

  /// For a given specifier, return optionally if it has a types only dependency
  /// assigned on the module. This occurs when there is a header or text in the
  /// module that assigns expresses the types only dependency.
  fn resolve_types_dependency(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&ModuleSpecifier> {
    if let Some(ModuleSlot::Module(Module::Es(module))) =
      self.module_slots.get(specifier)
    {
      if let Some((_, Some(Ok((specifier, _))))) =
        &module.maybe_types_dependency
      {
        return Some(specifier);
      }
    }
    None
  }

  fn set_checksums(&mut self) {
    if let Some(locker) = &self.maybe_locker {
      let locker = locker.borrow();
      for (_, module_slot) in self.module_slots.iter_mut() {
        if let ModuleSlot::Module(Module::Es(module)) = module_slot {
          module.maybe_checksum =
            Some(locker.get_checksum(module.source.as_str()));
        }
      }
    }
  }

  /// Return a map representation of the specifiers in the graph, where each key
  /// is a module specifier and each value is a result that contains a tuple of
  /// the module specifier and media type, or the module graph error.
  #[cfg(feature = "rust")]
  pub fn specifiers(
    &self,
  ) -> HashMap<
    ModuleSpecifier,
    Result<(ModuleSpecifier, MediaType), ModuleGraphError>,
  > {
    let mut map: HashMap<
      ModuleSpecifier,
      Result<(ModuleSpecifier, MediaType), ModuleGraphError>,
    > = self.module_slots.iter().filter_map(to_result).collect();
    for (specifier, found) in &self.redirects {
      map.insert(specifier.clone(), map.get(found).unwrap().clone());
    }
    map
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
      Some(ModuleSlot::Missing) => {
        Err(ModuleGraphError::Missing(specifier.clone()))
      }
      _ => Ok(None),
    }
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-type only, non-dynamic imports. The first error is
  /// returned as as error result, otherwise ok if there are no errors.
  #[cfg(feature = "rust")]
  pub fn valid(&self) -> Result<(), ModuleGraphError> {
    self.validate(false)
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-dynamic imports that are type only related. The first
  /// error is returned as an error result, otherwise ok if there are no errors.
  ///
  /// This is designed to be used in cases where the graph needs to be validated
  /// from a type checking perspective, prior to type checking the graph.
  #[cfg(feature = "rust")]
  pub fn valid_types_only(&self) -> Result<(), ModuleGraphError> {
    self.validate(true)
  }

  #[cfg(feature = "rust")]
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
        Ok(Some(Module::Es(module))) => {
          if let Some((_, Some(Ok((specifier, _))))) =
            &module.maybe_types_dependency
          {
            validate(specifier, types_only, true, seen, get_module)?;
          }
          for dep in module.dependencies.values() {
            if !dep.is_dynamic {
              // TODO(@kitsonk) eliminate duplication with maybe_code below
              match &dep.maybe_type {
                Some(Ok((specifier, _))) => {
                  validate(specifier, types_only, true, seen, get_module)?
                }
                Some(Err(err)) if types_only => {
                  return Err(err.into());
                }
                _ => (),
              }
              match &dep.maybe_code {
                Some(Ok((specifier, _))) => {
                  validate(specifier, types_only, false, seen, get_module)?
                }
                Some(Err(err)) if !types_only => {
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
  let mut remapped = false;
  let resolved_specifier = if let Some(resolver) = maybe_resolver {
    remapped = true;
    resolver
      .resolve(specifier, &referrer_range.specifier)
      .map_err(|err| {
        if let Some(specifier_error) = err.downcast_ref::<SpecifierError>() {
          ResolutionError::InvalidSpecifier(
            specifier_error.clone(),
            referrer_range.clone(),
          )
        } else {
          ResolutionError::ResolverError(
            Arc::new(err),
            specifier.to_string(),
            referrer_range.clone(),
          )
        }
      })
  } else {
    resolve_import(specifier, &referrer_range.specifier).map_err(|err| {
      ResolutionError::InvalidSpecifier(err, referrer_range.clone())
    })
  };
  let result = match resolved_specifier {
    Ok(specifier) => {
      let referrer_scheme = referrer_range.specifier.scheme();
      let specifier_scheme = specifier.scheme();
      if referrer_scheme == "https" && specifier_scheme == "http" {
        Err(ResolutionError::InvalidDowngrade(
          specifier,
          referrer_range.clone(),
        ))
      } else if (referrer_scheme == "https" || referrer_scheme == "http")
        && !(specifier_scheme == "https" || specifier_scheme == "http")
        && !remapped
      {
        Err(ResolutionError::InvalidLocalImport(
          specifier,
          referrer_range.clone(),
        ))
      } else {
        Ok((specifier, referrer_range.clone()))
      }
    }
    Err(err) => Err(err),
  };
  Some(result)
}

/// With the provided information, parse a module and return its "module slot"
#[allow(clippy::too_many_arguments)]
pub(crate) fn parse_module(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: Arc<String>,
  maybe_assert_type: Option<&str>,
  maybe_resolver: Option<&dyn Resolver>,
  source_parser: &dyn SourceParser,
  is_root: bool,
  is_dynamic_branch: bool,
) -> ModuleSlot {
  let media_type = get_media_type(specifier, maybe_headers);

  // here we check any media types that should have assertions made against them
  // if they aren't the root and add them to the graph, otherwise we continue
  if media_type == MediaType::Json
    && (is_root
      || is_dynamic_branch
      || matches!(maybe_assert_type, Some("json")))
  {
    return ModuleSlot::Module(Module::Synthetic(Box::new(
      SyntheticModule::new(
        specifier.clone(),
        MediaType::Json,
        None,
        Some(content),
        None,
      ),
    )));
  }

  match maybe_assert_type {
    Some("json") => {
      return ModuleSlot::Err(ModuleGraphError::InvalidTypeAssertion {
        specifier: specifier.clone(),
        actual_media_type: media_type,
        expected_media_type: MediaType::Json,
      })
    }
    Some(assert_type) => {
      return ModuleSlot::Err(ModuleGraphError::UnsupportedImportAssertionType(
        specifier.clone(),
        assert_type.to_string(),
      ))
    }
    _ => (),
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
      // Parse the module and start analyzing the module.
      match source_parser.parse_module(specifier, content, media_type) {
        Ok(parsed_source) => {
          // Return the module as a valid module
          ModuleSlot::Module(parse_module_from_ast(
            specifier,
            maybe_headers,
            &parsed_source,
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
      match source_parser.parse_module(
        specifier,
        content,
        MediaType::JavaScript,
      ) {
        Ok(parsed_source) => {
          // Return the module as a valid module
          ModuleSlot::Module(parse_module_from_ast(
            specifier,
            maybe_headers,
            &parsed_source,
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

pub(crate) fn parse_module_from_ast(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  parsed_source: &ParsedSource,
  maybe_resolver: Option<&dyn Resolver>,
) -> Module {
  // Init the module and determine its media type
  let mut module = EsModule::new(specifier.clone(), parsed_source.clone());
  module.media_type = get_media_type(specifier, maybe_headers);

  // Analyze the TypeScript triple-slash references
  for reference in analyze_ts_references(parsed_source) {
    match reference {
      ast::TypeScriptReference::Path(specifier, span) => {
        let range =
          Range::from_swc_span(&module.specifier, parsed_source, &span);
        let resolved_dependency = resolve(&specifier, &range, maybe_resolver);
        let dep = module.dependencies.entry(specifier).or_default();
        dep.maybe_code = resolved_dependency;
      }
      ast::TypeScriptReference::Types(specifier, span) => {
        let range =
          Range::from_swc_span(&module.specifier, parsed_source, &span);
        let resolved_dependency = resolve(&specifier, &range, maybe_resolver);
        if is_untyped(&module.media_type) {
          module.maybe_types_dependency =
            Some((specifier, resolved_dependency));
        } else {
          let dep = module.dependencies.entry(specifier).or_default();
          dep.maybe_type = resolved_dependency;
        }
      }
    }
  }

  // Analyze any JSX Import Source pragma
  if let Some((import_source, span)) = analyze_jsx_import_sources(parsed_source)
  {
    let jsx_import_source_module = maybe_resolver
      .map(|r| r.jsx_import_source_module())
      .unwrap_or(DEFAULT_JSX_IMPORT_SOURCE_MODULE);
    let specifier = format!("{}/{}", import_source, jsx_import_source_module);
    let range = Range::from_swc_span(&module.specifier, parsed_source, &span);
    let resolved_dependency = resolve(&specifier, &range, maybe_resolver);
    let dep = module.dependencies.entry(specifier).or_default();
    dep.maybe_code = resolved_dependency;
  }

  // Analyze any JSDoc type imports
  for (import_source, span) in analyze_jsdoc_imports(parsed_source) {
    let range = Range::from_swc_span(&module.specifier, parsed_source, &span);
    let resolved_dependency = resolve(&import_source, &range, maybe_resolver);
    let dep = module.dependencies.entry(import_source).or_default();
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
            Some(Ok((
              specifier.clone(),
              maybe_range.unwrap_or_else(|| Range {
                specifier,
                start: Position::zeroed(),
                end: Position::zeroed(),
              }),
            ))),
          )),
          Ok(None) => None,
          Err(err) => Some((
            module.specifier.to_string(),
            Some(Err(ResolutionError::ResolverError(
              Arc::new(err),
              module.specifier.to_string(),
              Range {
                specifier: module.specifier.clone(),
                start: Position::zeroed(),
                end: Position::zeroed(),
              },
            ))),
          )),
        };
    }
  }

  // Analyze ES dependencies
  let descriptors = analyze_dependencies(parsed_source);
  for desc in descriptors {
    let dep = module
      .dependencies
      .entry(desc.specifier.to_string())
      .or_default();
    dep.is_dynamic = desc.is_dynamic;
    dep.maybe_assert_type = desc.import_assertions.get("type").cloned();
    let resolved_dependency = resolve(
      &desc.specifier,
      &Range::from_swc_span(
        &module.specifier,
        parsed_source,
        &desc.specifier_span,
      ),
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
      .map(|(text, span)| {
        resolve(
          &text,
          &Range::from_swc_span(&specifier, parsed_source, &span),
          maybe_resolver,
        )
      })
      .unwrap_or_else(|| Resolved::None);
    if dep.maybe_type.is_none() {
      dep.maybe_type = maybe_type
    }
  }

  // Return the module as a valid module
  Module::Es(Box::new(module))
}

fn get_media_type(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
) -> MediaType {
  if let Some(headers) = maybe_headers {
    if let Some(content_type) = headers.get("content-type") {
      MediaType::from_content_type(specifier, content_type)
    } else {
      MediaType::from(specifier)
    }
  } else {
    MediaType::from(specifier)
  }
}

/// Determine if a media type is "untyped" and should be checked to see if there
/// are types provided.
fn is_untyped(media_type: &MediaType) -> bool {
  matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  )
}

/// The kind of build to perform.
pub(crate) enum BuildKind {
  /// All types of dependencies should be analyzed and included in the graph.
  All,
  /// Only code dependencies should be analyzed and included in the graph. This
  /// is useful when transpiling and running code, but not caring about type
  /// only dependnecies.
  CodeOnly,
  /// Only type dependencies should be analyzed and included in the graph. This
  /// is useful when assessing types, like documentation or type checking, when
  /// the code will not be executed.
  TypesOnly,
}

pub type LoadWithSpecifierFuture =
  Pin<Box<dyn Future<Output = (ModuleSpecifier, LoadResult)> + 'static>>;

pub(crate) struct Builder<'a> {
  in_dynamic_branch: bool,
  graph: ModuleGraph,
  loader: &'a mut dyn Loader,
  maybe_resolver: Option<&'a dyn Resolver>,
  pending: FuturesUnordered<LoadWithSpecifierFuture>,
  pending_assert_types: HashMap<ModuleSpecifier, HashSet<Option<String>>>,
  dynamic_branches: HashMap<ModuleSpecifier, Option<String>>,
  source_parser: &'a dyn SourceParser,
  maybe_reporter: Option<&'a dyn Reporter>,
}

impl<'a> Builder<'a> {
  pub fn new(
    roots: Vec<ModuleSpecifier>,
    is_dynamic_root: bool,
    loader: &'a mut dyn Loader,
    maybe_resolver: Option<&'a dyn Resolver>,
    maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
    source_parser: &'a dyn SourceParser,
    maybe_reporter: Option<&'a dyn Reporter>,
  ) -> Self {
    Self {
      in_dynamic_branch: is_dynamic_root,
      graph: ModuleGraph::new(roots, maybe_locker),
      loader,
      maybe_resolver,
      pending: FuturesUnordered::new(),
      pending_assert_types: HashMap::new(),
      dynamic_branches: HashMap::new(),
      source_parser,
      maybe_reporter,
    }
  }

  pub async fn build(
    mut self,
    kind: BuildKind,
    maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
  ) -> ModuleGraph {
    let roots = self.graph.roots.clone();
    for root in roots {
      self.load(&root, self.in_dynamic_branch, None);
    }

    // Process any imports that are being added to the graph.
    if let Some(imports) = maybe_imports {
      for (referrer, specifiers) in imports {
        let synthetic_module = SyntheticModule::new(
          referrer.clone(),
          MediaType::Unknown,
          Some(specifiers),
          None,
          self.maybe_resolver,
        );
        for (specifier, _) in
          synthetic_module.dependencies.values().flatten().flatten()
        {
          self.load(specifier, self.in_dynamic_branch, None);
        }
        self.graph.module_slots.insert(
          referrer,
          ModuleSlot::Module(Module::Synthetic(Box::new(synthetic_module))),
        );
      }
    }

    loop {
      let specifier = match self.pending.next().await {
        Some((specifier, Ok(Some(response)))) => {
          let assert_types =
            self.pending_assert_types.remove(&specifier).unwrap();
          for maybe_assert_type in assert_types {
            self.visit(&specifier, &response, &kind, maybe_assert_type)
          }
          Some(specifier)
        }
        Some((specifier, Ok(None))) => {
          self
            .graph
            .module_slots
            .insert(specifier.clone(), ModuleSlot::Missing);
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
      if let (Some(specifier), Some(reporter)) =
        (specifier, self.maybe_reporter)
      {
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
              self.load(&specifier, true, maybe_assert_type.as_deref());
            }
          }
        } else {
          break;
        }
      }
    }

    // Enrich with cache info from the loader
    for slot in self.graph.module_slots.values_mut() {
      if let ModuleSlot::Module(Module::Es(ref mut module)) = slot {
        module.maybe_cache_info = self.loader.get_cache_info(&module.specifier);
      }
    }
    // Enrich with checksums from locker
    self.graph.set_checksums();

    self.graph
  }

  /// Enqueue a request to load the specifier via the loader.
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
    maybe_assert_type: Option<&str>,
  ) {
    let specifier = self.graph.redirects.get(specifier).unwrap_or(specifier);
    let assert_types = self
      .pending_assert_types
      .entry(specifier.clone())
      .or_default();
    assert_types.insert(maybe_assert_type.map(String::from));
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

  /// Visit a module, parsing it and resolving any dependencies.
  fn visit(
    &mut self,
    requested_specifier: &ModuleSpecifier,
    response: &LoadResponse,
    kind: &BuildKind,
    maybe_assert_type: Option<String>,
  ) {
    use std::borrow::BorrowMut;

    let maybe_headers = response.maybe_headers.as_ref();
    let specifier = response.specifier.clone();

    // If the response was redirected, then we add the module to the redirects
    if *requested_specifier != specifier {
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

    let is_root = self.graph.roots.contains(&specifier);

    let mut module_slot = parse_module(
      &specifier,
      maybe_headers,
      response.content.clone(),
      maybe_assert_type.as_deref(),
      self.maybe_resolver,
      self.source_parser,
      is_root,
      self.in_dynamic_branch,
    );

    if let ModuleSlot::Module(Module::Es(module)) = module_slot.borrow_mut() {
      if matches!(kind, BuildKind::All | BuildKind::CodeOnly)
        || module.maybe_types_dependency.is_none()
      {
        for dep in module.dependencies.values_mut() {
          if matches!(kind, BuildKind::All | BuildKind::CodeOnly)
            || dep.maybe_type.is_none()
          {
            if let Some(Ok((specifier, _))) = &dep.maybe_code {
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), dep.maybe_assert_type.clone());
              } else {
                self.load(
                  specifier,
                  self.in_dynamic_branch,
                  dep.maybe_assert_type.as_deref(),
                );
              }
            }
          } else {
            dep.maybe_code = None;
          }

          if matches!(kind, BuildKind::All | BuildKind::TypesOnly) {
            if let Some(Ok((specifier, _))) = &dep.maybe_type {
              if dep.is_dynamic && !self.in_dynamic_branch {
                self
                  .dynamic_branches
                  .insert(specifier.clone(), dep.maybe_assert_type.clone());
              } else {
                self.load(
                  specifier,
                  self.in_dynamic_branch,
                  dep.maybe_assert_type.as_deref(),
                );
              }
            }
          } else {
            dep.maybe_type = None;
          }
        }
      } else {
        module.dependencies.clear();
      }

      if matches!(kind, BuildKind::All | BuildKind::TypesOnly) {
        if let Some((_, Some(Ok((specifier, _))))) =
          &module.maybe_types_dependency
        {
          self.load(specifier, false, None);
        }
      } else {
        module.maybe_types_dependency = None;
      }
    }
    self.graph.module_slots.insert(specifier, module_slot);
  }
}

struct SerializeableResolved<'a>(&'a Resolved);

impl<'a> Serialize for SerializeableResolved<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serialize_resolved(self.0, serializer)
  }
}

struct SerializeableDependency<'a>(&'a str, &'a Dependency);

impl<'a> Serialize for SerializeableDependency<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let mut map = serializer.serialize_map(None)?;
    map.serialize_entry("specifier", self.0)?;
    if self.1.maybe_code.is_some() {
      let serializeable_resolved = SerializeableResolved(&self.1.maybe_code);
      map.serialize_entry("code", &serializeable_resolved)?;
    }
    if self.1.maybe_type.is_some() {
      let serializeable_resolved = SerializeableResolved(&self.1.maybe_type);
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

fn serialize_dependencies<S>(
  dependencies: &BTreeMap<String, Dependency>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  let mut seq = serializer.serialize_seq(Some(dependencies.iter().count()))?;
  for (specifier_str, dep) in dependencies.iter() {
    let serializeable_dependency = SerializeableDependency(specifier_str, dep);
    seq.serialize_element(&serializeable_dependency)?;
  }
  seq.end()
}

fn serialize_synthetic_dependencies<S>(
  dependencies: &BTreeMap<String, Resolved>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  let mut seq = serializer.serialize_seq(Some(dependencies.iter().count()))?;
  for (specifier_str, res) in dependencies.iter() {
    let serializeable_dep =
      SerializeableSyntheticDependency(specifier_str, res);
    seq.serialize_element(&serializeable_dep)?;
  }
  seq.end()
}

struct SerializeableModuleSlot<'a>(&'a ModuleSpecifier, &'a ModuleSlot);

impl<'a> Serialize for SerializeableModuleSlot<'a> {
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
      ModuleSlot::Missing => {
        let mut state = serializer.serialize_struct("ModuleSlot", 2)?;
        state.serialize_field("specifier", self.0)?;
        state.serialize_field(
          "error",
          "The module was missing and could not be loaded.",
        )?;
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

fn serialize_modules<S>(
  modules: &BTreeMap<ModuleSpecifier, ModuleSlot>,
  serializer: S,
) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  let mut seq = serializer.serialize_seq(Some(modules.iter().count()))?;
  for (specifier, slot) in modules.iter() {
    let serializeable_module_slot = SerializeableModuleSlot(specifier, slot);
    seq.serialize_element(&serializeable_module_slot)?;
  }
  seq.end()
}

pub(crate) fn serialize_type_dependency<S>(
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
      let serializeable_resolved = SerializeableResolved(resolved);
      state.serialize_field("dependency", &serializeable_resolved)?;
      state.end()
    }
    None => serializer.serialize_none(),
  }
}

fn serialize_source<S>(source: &str, serializer: S) -> Result<S::Ok, S::Error>
where
  S: Serializer,
{
  serializer.serialize_u32(source.len() as u32)
}

fn serialize_maybe_source<S>(
  source: &Option<Arc<String>>,
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
  use super::*;
  use crate::ast::DefaultSourceParser;
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
    let source_parser = ast::DefaultSourceParser::default();
    let content = Arc::new(r#"import * as b from "./b.ts";"#.to_string());
    let slot = parse_module(
      &specifier,
      None,
      content,
      None,
      None,
      &source_parser,
      true,
      false,
    );
    if let ModuleSlot::Module(Module::Es(module)) = slot {
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
        Some(Ok((
          ModuleSpecifier::parse("file:///b.ts").unwrap(),
          Range {
            specifier: specifier.clone(),
            start: Position {
              line: 0,
              character: 19
            },
            end: Position {
              line: 0,
              character: 27
            },
          }
        )))
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
              Ok(Some(LoadResponse {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: Arc::new("await import('file:///bar.js')".to_string()),
              }))
            })
          }
          "file:///bar.js" => {
            assert!(is_dynamic);
            self.loaded_bar = true;
            Box::pin(async move {
              Ok(Some(LoadResponse {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: Arc::new("import 'file:///baz.js'".to_string()),
              }))
            })
          }
          "file:///baz.js" => {
            assert!(is_dynamic);
            self.loaded_baz = true;
            Box::pin(async move {
              Ok(Some(LoadResponse {
                specifier: specifier.clone(),
                maybe_headers: None,
                content: Arc::new("console.log('Hello, world!')".to_string()),
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
    let source_parser = DefaultSourceParser::new();
    let builder = Builder::new(
      vec![Url::parse("file:///foo.js").unwrap()],
      false,
      &mut loader,
      None,
      None,
      &source_parser,
      None,
    );
    builder.build(BuildKind::All, None).await;
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
            Ok(Some(LoadResponse {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: Arc::new("await import('file:///bar.js')".to_string()),
            }))
          }),
          "file:///bar.js" => Box::pin(async move { Ok(None) }),
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let source_parser = DefaultSourceParser::new();
    let builder = Builder::new(
      vec![Url::parse("file:///foo.js").unwrap()],
      false,
      &mut loader,
      None,
      None,
      &source_parser,
      None,
    );
    let graph = builder.build(BuildKind::All, None).await;
    assert!(graph
      .try_get(&Url::parse("file:///foo.js").unwrap())
      .is_ok());
    assert!(matches!(
      graph
        .try_get(&Url::parse("file:///bar.js").unwrap())
        .unwrap_err(),
      ModuleGraphError::Missing(..)
    ));
    let specifiers = graph.specifiers();
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
            Ok(Some(LoadResponse {
              specifier: Url::parse("file:///foo_actual.js").unwrap(),
              maybe_headers: None,
              content: Arc::new("import 'file:///bar.js'".to_string()),
            }))
          }),
          "file:///bar.js" => Box::pin(async move {
            Ok(Some(LoadResponse {
              specifier: Url::parse("file:///bar_actual.js").unwrap(),
              maybe_headers: None,
              content: Arc::new("(".to_string()),
            }))
          }),
          _ => unreachable!(),
        }
      }
    }
    let mut loader = TestLoader;
    let source_parser = DefaultSourceParser::new();
    let builder = Builder::new(
      vec![Url::parse("file:///foo.js").unwrap()],
      false,
      &mut loader,
      None,
      None,
      &source_parser,
      None,
    );
    let graph = builder.build(BuildKind::All, None).await;
    let specifiers = graph.specifiers();
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
}
