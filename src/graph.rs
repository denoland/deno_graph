// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::ast;
use crate::ast::analyze_deno_types;
use crate::ast::analyze_dependencies;
use crate::ast::analyze_ts_references;
use crate::ast::Range;
use crate::ast::SourceParser;
use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::module_specifier::SpecifierError;
use crate::source::*;
use crate::text_encoding::strip_bom_mut;

use anyhow::anyhow;
use anyhow::Result;
use data_url::DataUrl;
use deno_ast::MediaType;
use deno_ast::ParsedSource;
use futures::future;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;
use serde::Serialize;
use serde::Serializer;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug)]
pub enum ModuleGraphError {
  LoadingErr(Arc<anyhow::Error>),
  ParseErr(deno_ast::Diagnostic),
  ResolutionError(ResolutionError, ast::Span),
  InvalidSource(ModuleSpecifier, Option<String>),
  UnsupportedMediaType(ModuleSpecifier, MediaType),
}

impl Clone for ModuleGraphError {
  fn clone(&self) -> Self {
    match self {
      Self::LoadingErr(err) => Self::LoadingErr(err.clone()),
      Self::ParseErr(err) => Self::ParseErr(err.clone()),
      Self::ResolutionError(err, span) => {
        Self::ResolutionError(err.clone(), span.clone())
      }
      Self::InvalidSource(specifier, maybe_filename) => {
        Self::InvalidSource(specifier.clone(), maybe_filename.clone())
      }
      Self::UnsupportedMediaType(specifier, media_type) => {
        Self::UnsupportedMediaType(specifier.clone(), media_type.clone())
      }
    }
  }
}

impl std::error::Error for ModuleGraphError {}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LoadingErr(err) => write!(f, "{}", err),
      Self::ParseErr(diagnostic) => write!(f, "The module's source code would not be parsed: {}", diagnostic),
      Self::ResolutionError(err, _) => write!(f, "{}", err),
      Self::InvalidSource(specifier, maybe_filename) => if let Some(filename) = maybe_filename {
        write!(f, "The source code is invalid, as it does not match the expected hash in the lock file.\n  Specifier: {}\n  Lock file: {}", specifier, filename)
      } else {
        write!(f, "The source code is invalid, as it does not match the expected hash in the lock file.\n  Specifier: {}", specifier)
      },
      Self::UnsupportedMediaType(specifier, media_type) => write!(f, "An unsupported media type was attempted to be imported as a module.\n  Specifier: {}\n  MediaType: {}", specifier, media_type)
    }
  }
}

impl From<deno_ast::Diagnostic> for ModuleGraphError {
  fn from(diagnostic: deno_ast::Diagnostic) -> Self {
    Self::ParseErr(diagnostic)
  }
}

#[derive(Debug)]
pub enum ResolutionError {
  InvalidDowngrade(ModuleSpecifier),
  InvalidLocalImport(ModuleSpecifier),
  ResolverError(Arc<anyhow::Error>),
  InvalidSpecifier(SpecifierError),
}

impl std::error::Error for ResolutionError {}

impl Clone for ResolutionError {
  fn clone(&self) -> Self {
    match self {
      Self::InvalidDowngrade(s) => Self::InvalidDowngrade(s.clone()),
      Self::InvalidLocalImport(s) => Self::InvalidLocalImport(s.clone()),
      Self::ResolverError(err) => Self::ResolverError(err.clone()),
      Self::InvalidSpecifier(err) => Self::InvalidSpecifier(err.clone()),
    }
  }
}

impl PartialEq for ResolutionError {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::ResolverError(_), Self::ResolverError(_)) => true,
      (Self::InvalidDowngrade(a), Self::InvalidDowngrade(b))
      | (Self::InvalidLocalImport(a), Self::InvalidLocalImport(b)) => a == b,
      (Self::InvalidSpecifier(a), Self::InvalidSpecifier(b)) => a == b,
      _ => false,
    }
  }
}

impl Eq for ResolutionError {}

impl fmt::Display for ResolutionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::InvalidDowngrade(specifier) => write!(f, "Modules imported via https are not allowed to import http modules.\n  Importing: {}", specifier),
      Self::InvalidLocalImport(specifier) => write!(f, "Remote modules are not allowed to import local modules. Consider using a dynamic import instead.\n  Importing: {}", specifier),
      Self::ResolverError(err) => write!(f, "{}", err),
      Self::InvalidSpecifier(err) => write!(f, "{}", err),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolved {
  Specifier(ModuleSpecifier, ast::Span),
  Err(ResolutionError, ast::Span),
  None,
}

impl Default for Resolved {
  fn default() -> Self {
    Self::None
  }
}

impl Serialize for Resolved {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    match self {
      Self::Specifier(specifier, span) => {
        let mut state = serializer.serialize_struct("ResolvedSpecifier", 2)?;
        state.serialize_field("specifier", specifier)?;
        state.serialize_field("span", span)?;
        state.end()
      }
      Self::Err(err, span) => {
        let mut state = serializer.serialize_struct("ResolvedError", 2)?;
        state.serialize_field("error", &err.to_string())?;
        state.serialize_field("span", span)?;
        state.end()
      }
      _ => Serialize::serialize(&None::<Resolved>, serializer),
    }
  }
}

impl Resolved {
  pub fn is_none(&self) -> bool {
    self == &Self::None
  }
}

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Debug, Default, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
  #[serde(rename = "code", skip_serializing_if = "Resolved::is_none")]
  pub(crate) maybe_code: Resolved,
  #[serde(rename = "type", skip_serializing_if = "Resolved::is_none")]
  pub(crate) maybe_type: Resolved,
  #[serde(skip_serializing_if = "is_false")]
  pub is_dynamic: bool,
}

#[cfg(feature = "rust")]
impl Dependency {
  /// Optionally return the module specifier in the module graph that points to
  /// the "code" dependency in the graph.
  pub fn get_code(&self) -> Option<&ModuleSpecifier> {
    match &self.maybe_code {
      Resolved::Specifier(specifier, _) => Some(specifier),
      _ => None,
    }
  }

  /// Optionally return the module specifier in the module graph that points to
  /// the type only dependency in the graph.
  pub fn get_type(&self) -> Option<&ModuleSpecifier> {
    match &self.maybe_type {
      Resolved::Specifier(specifier, _) => Some(specifier),
      _ => None,
    }
  }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Module {
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

impl Module {
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

#[derive(Debug)]
pub(crate) enum ModuleSlot {
  Module(Module),
  Err(ModuleGraphError),
  Missing,
  Pending,
}

impl ModuleSlot {
  pub fn is_module(&self) -> bool {
    matches!(*self, Self::Module(_))
  }
}

/// Convert a module slot entry into a result which contains the resolved
/// module specifier and media type or the module graph error.
#[cfg(feature = "rust")]
fn to_result(
  (specifier, module_slot): (&ModuleSpecifier, &ModuleSlot),
) -> Option<(
  ModuleSpecifier,
  Result<(ModuleSpecifier, MediaType), ModuleGraphError>,
)> {
  match module_slot {
    ModuleSlot::Err(err) => Some((specifier.clone(), Err(err.clone()))),
    ModuleSlot::Module(module) => Some((
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

  /// Returns any errors that are in the module graph, along with the associated
  /// specifier.
  #[cfg(feature = "rust")]
  pub fn errors(&self) -> Vec<(&ModuleSpecifier, &ModuleGraphError)> {
    self
      .module_slots
      .iter()
      .filter_map(|(s, ms)| match ms {
        ModuleSlot::Err(err) => Some((s, err)),
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
        if let ModuleSlot::Module(module) = module_slot {
          if !locker.check_or_insert(&module.specifier, &module.source) {
            return Err(ModuleGraphError::InvalidSource(
              module.specifier.clone(),
              locker.get_filename(),
            ));
          }
        }
      }
    }
    Ok(())
  }

  /// Return a vector of references to module objects in the graph. Only modules
  /// that were fully resolved are present, as "errors" are omitted. If you need
  /// to know what errors are in the graph, use the `.errors()` method, or if
  /// you just need to check if everything is "ok" with the graph, use the
  /// `.valid()` method.
  #[cfg(feature = "rust")]
  pub fn modules(&self) -> Vec<&Module> {
    self
      .module_slots
      .iter()
      .filter_map(|(_, ms)| match ms {
        ModuleSlot::Module(m) => Some(m),
        _ => None,
      })
      .collect()
  }

  /// Return all resolution errors from the graph. Used when trying to determine
  /// the reason a module might not be available to be loaded.
  #[cfg(feature = "rust")]
  pub fn resolution_errors(
    &self,
  ) -> HashMap<ModuleSpecifier, HashMap<String, (ResolutionError, ast::Span)>>
  {
    let mut errors = HashMap::new();
    for (specifier, module_slot) in &self.module_slots {
      if let ModuleSlot::Module(module) = module_slot {
        for (specifier_str, dep) in &module.dependencies {
          if let Resolved::Err(err, span) = &dep.maybe_code {
            let dep_errors = errors
              .entry(specifier.clone())
              .or_insert_with(|| HashMap::new());
            dep_errors
              .insert(specifier_str.clone(), (err.clone(), span.clone()));
          }
          if let Resolved::Err(err, span) = &dep.maybe_type {
            let dep_errors = errors
              .entry(specifier.clone())
              .or_insert_with(|| HashMap::new());
            dep_errors
              .insert(specifier_str.clone(), (err.clone(), span.clone()));
          }
        }
        if let Some((specifier_str, Resolved::Err(err, span))) =
          &module.maybe_types_dependency
        {
          let dep_errors = errors
            .entry(specifier.clone())
            .or_insert_with(|| HashMap::new());
          dep_errors.insert(specifier_str.clone(), (err.clone(), span.clone()));
        }
      }
    }
    errors
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
    let maybe_specifier =
      if let ModuleSlot::Module(referring_module) = referring_module_slot {
        let dependency = referring_module.dependencies.get(specifier)?;
        let (maybe_first, maybe_second) = if prefer_types {
          (&dependency.maybe_type, &dependency.maybe_code)
        } else {
          (&dependency.maybe_code, &dependency.maybe_type)
        };
        if let Resolved::Specifier(specifier, _) = maybe_first {
          if prefer_types {
            Some(
              self
                .resolve_types_dependency(specifier)
                .unwrap_or(specifier),
            )
          } else {
            Some(specifier)
          }
        } else if let Resolved::Specifier(specifier, _) = maybe_second {
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
      } else {
        None
      };
    // Even if we resolved the specifier, it doesn't mean the module is actually
    // there, and so we will return the final final specifier.
    if let Some(specifier) = maybe_specifier {
      match self.module_slots.get(&self.resolve(specifier)) {
        Some(ModuleSlot::Module(m)) => Some(&m.specifier),
        _ => None,
      }
    } else {
      None
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
      if let Some((_, Resolved::Specifier(specifier, _))) =
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
        if let ModuleSlot::Module(module) = module_slot {
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
    for specifier in self.redirects.keys() {
      if let Some(module) = self.get(specifier) {
        map.insert(
          specifier.clone(),
          Ok((module.specifier.clone(), module.media_type)),
        );
      }
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
      _ => Ok(None),
    }
  }

  /// Walk the graph from the root, checking to see if there are any module
  /// graph errors on non-dynamic imports. The first error is returned as an
  /// error result, otherwise ok if there are no errors.
  #[cfg(feature = "rust")]
  pub fn valid(&self) -> Result<(), (ModuleSpecifier, ModuleGraphError)> {
    fn validate<F>(
      specifier: &ModuleSpecifier,
      seen: &mut HashSet<ModuleSpecifier>,
      get_module: &F,
    ) -> Result<(), (ModuleSpecifier, ModuleGraphError)>
    where
      F: Fn(&ModuleSpecifier) -> Result<Option<Module>, ModuleGraphError>,
    {
      if seen.contains(specifier) {
        return Ok(());
      }
      seen.insert(specifier.clone());
      match get_module(specifier) {
        Ok(Some(module)) => {
          if let Some((_, Resolved::Specifier(specifier, _))) =
            &module.maybe_types_dependency
          {
            validate(specifier, seen, get_module)?;
          }
          for dep in module.dependencies.values() {
            if !dep.is_dynamic {
              match &dep.maybe_code {
                Resolved::Specifier(specifier, _) => {
                  validate(specifier, seen, get_module)?
                }
                Resolved::Err(err, span) => {
                  return Err((
                    specifier.clone(),
                    ModuleGraphError::ResolutionError(
                      err.clone(),
                      span.clone(),
                    ),
                  ))
                }
                _ => (),
              }
              match &dep.maybe_type {
                Resolved::Specifier(specifier, _) => {
                  validate(specifier, seen, get_module)?
                }
                Resolved::Err(err, span) => {
                  return Err((
                    specifier.clone(),
                    ModuleGraphError::ResolutionError(
                      err.clone(),
                      span.clone(),
                    ),
                  ))
                }
                _ => (),
              }
            }
          }
          Ok(())
        }
        Ok(None) => Err((
          specifier.clone(),
          ModuleGraphError::LoadingErr(Arc::new(anyhow!(
            "Cannot resolve module \"{}\".",
            specifier
          ))),
        )),
        Err(err) => Err((specifier.clone(), err)),
      }
    }

    let mut seen = HashSet::new();
    for root in &self.roots {
      validate(root, &mut seen, &|s| self.try_get(s).map(|o| o.cloned()))?;
    }
    Ok(())
  }
}

fn load_data_url(specifier: &ModuleSpecifier) -> Result<Option<LoadResponse>> {
  let url = DataUrl::process(specifier.as_str())
    .map_err(|_| anyhow!("Unable to decode data url."))?;
  let (bytes, _) = url
    .decode_to_vec()
    .map_err(|_| anyhow!("Unable to decode data url."))?;
  let mut headers: HashMap<String, String> = HashMap::new();
  headers.insert("content-type".to_string(), url.mime_type().to_string());
  let mut content = String::from_utf8(bytes)?;
  strip_bom_mut(&mut content);
  Ok(Some(LoadResponse {
    specifier: specifier.clone(),
    maybe_headers: Some(headers),
    content: Arc::new(content),
  }))
}

/// Resolve a string specifier from a referring module, using the resolver if
/// present, returning the resolution result.
fn resolve(
  specifier: &str,
  referrer: &ModuleSpecifier,
  range: &ast::Range,
  maybe_resolver: Option<&dyn Resolver>,
) -> Resolved {
  let mut remapped = false;
  let resolved_specifier = if let Some(resolver) = maybe_resolver {
    remapped = true;
    resolver
      .resolve(specifier, referrer)
      .map_err(|err| ResolutionError::ResolverError(Arc::new(err)))
  } else {
    resolve_import(specifier, referrer)
      .map_err(ResolutionError::InvalidSpecifier)
  };
  let span = ast::Span {
    specifier: referrer.clone(),
    range: range.clone(),
  };
  match resolved_specifier {
    Ok(specifier) => {
      let referrer_scheme = referrer.scheme();
      let specifier_scheme = specifier.scheme();
      if referrer_scheme == "https" && specifier_scheme == "http" {
        Resolved::Err(ResolutionError::InvalidDowngrade(specifier), span)
      } else if (referrer_scheme == "https" || referrer_scheme == "http")
        && !(specifier_scheme == "https" || specifier_scheme == "http")
        && !remapped
      {
        Resolved::Err(ResolutionError::InvalidLocalImport(specifier), span)
      } else {
        Resolved::Specifier(specifier, span)
      }
    }
    Err(err) => Resolved::Err(err, span),
  }
}

/// With the provided information, parse a module and return its "module slot"
pub(crate) fn parse_module(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: Arc<String>,
  maybe_resolver: Option<&dyn Resolver>,
  source_parser: &dyn SourceParser,
  is_root: bool,
) -> ModuleSlot {
  let media_type = get_media_type(specifier, maybe_headers);
  match &media_type {
    MediaType::JavaScript
    | MediaType::Jsx
    | MediaType::TypeScript
    | MediaType::Tsx
    | MediaType::Dts => {
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
        Err(diagnostic) => ModuleSlot::Err(diagnostic.into()),
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
        Err(diagnostic) => ModuleSlot::Err(diagnostic.into()),
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
  let mut module = Module::new(specifier.clone(), parsed_source.clone());
  module.media_type = get_media_type(specifier, maybe_headers);

  // Analyze the TypeScript triple-slash references
  for reference in analyze_ts_references(parsed_source) {
    match reference {
      ast::TypeScriptReference::Path(specifier, range) => {
        let resolved_dependency =
          resolve(&specifier, &module.specifier, &range, maybe_resolver);
        let dep = module.dependencies.entry(specifier).or_default();
        dep.maybe_code = resolved_dependency;
      }
      ast::TypeScriptReference::Types(specifier, range) => {
        let resolved_dependency =
          resolve(&specifier, &module.specifier, &range, maybe_resolver);
        if module.media_type == MediaType::JavaScript
          || module.media_type == MediaType::Jsx
        {
          module.maybe_types_dependency =
            Some((specifier, resolved_dependency));
        } else {
          let dep = module.dependencies.entry(specifier).or_default();
          dep.maybe_type = resolved_dependency;
        }
      }
    }
  }

  // Analyze the X-TypeScript-Types header
  if module.maybe_types_dependency.is_none() {
    if let Some(headers) = maybe_headers {
      if let Some(types_header) = headers.get("x-typescript-types") {
        let resolved_dependency = resolve(
          types_header,
          &module.specifier,
          &ast::Range::default(),
          maybe_resolver,
        );
        module.maybe_types_dependency =
          Some((types_header.clone(), resolved_dependency));
      }
    }
  }

  // Analyze ES dependencies
  let descriptors = analyze_dependencies(parsed_source);
  for desc in descriptors {
    let dep = module
      .dependencies
      .entry(desc.specifier.to_string())
      .or_default();
    let resolved_dependency = resolve(
      &desc.specifier,
      &module.specifier,
      &Range::from_span(parsed_source, &desc.specifier_span),
      maybe_resolver,
    );
    dep.maybe_code = resolved_dependency;
    let specifier = module.specifier.clone();
    let maybe_type = analyze_deno_types(parsed_source, &desc)
      .map(|(s, r)| resolve(&s, &specifier, &r, maybe_resolver))
      .unwrap_or_else(|| Resolved::None);
    if dep.maybe_type.is_none() {
      dep.maybe_type = maybe_type
    }
  }

  // Return the module as a valid module
  module
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

pub(crate) struct Builder<'a> {
  is_dynamic_root: bool,
  graph: ModuleGraph,
  loader: &'a mut dyn Loader,
  maybe_resolver: Option<&'a dyn Resolver>,
  pending: FuturesUnordered<LoadFuture>,
  source_parser: &'a dyn SourceParser,
}

impl<'a> Builder<'a> {
  pub fn new(
    roots: Vec<ModuleSpecifier>,
    is_dynamic_root: bool,
    loader: &'a mut dyn Loader,
    maybe_resolver: Option<&'a dyn Resolver>,
    maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
    source_parser: &'a dyn SourceParser,
  ) -> Self {
    Self {
      is_dynamic_root,
      graph: ModuleGraph::new(roots, maybe_locker),
      loader,
      maybe_resolver,
      pending: FuturesUnordered::new(),
      source_parser,
    }
  }

  pub async fn build(mut self) -> ModuleGraph {
    let roots = self.graph.roots.clone();
    for root in roots {
      self.load(&root, self.is_dynamic_root);
    }

    loop {
      match self.pending.next().await {
        Some((specifier, Ok(Some(response)))) => {
          self.visit(specifier, response)
        }
        Some((specifier, Ok(None))) => {
          self
            .graph
            .module_slots
            .insert(specifier, ModuleSlot::Missing);
        }
        Some((specifier, Err(err))) => {
          self.graph.module_slots.insert(
            specifier,
            ModuleSlot::Err(ModuleGraphError::LoadingErr(Arc::new(err))),
          );
        }
        _ => {}
      }
      if self.pending.is_empty() {
        break;
      }
    }

    // Enrich with cache info from the loader
    for slot in self.graph.module_slots.values_mut() {
      if let ModuleSlot::Module(ref mut module) = slot {
        module.maybe_cache_info = self.loader.get_cache_info(&module.specifier);
      }
    }
    // Enrich with checksums from locker
    self.graph.set_checksums();

    self.graph
  }

  /// Enqueue a request to load the specifier via the loader.
  fn load(&mut self, specifier: &ModuleSpecifier, is_dynamic: bool) {
    if !self.graph.module_slots.contains_key(specifier) {
      self
        .graph
        .module_slots
        .insert(specifier.clone(), ModuleSlot::Pending);
      let f: LoadFuture = if specifier.scheme() == "data" {
        let load_response = load_data_url(specifier);
        Box::pin(future::ready((specifier.clone(), load_response)))
      } else {
        self.loader.load(specifier, is_dynamic)
      };
      self.pending.push(f);
    }
  }

  /// Visit a module, parsing it and resolving any dependencies.
  fn visit(
    &mut self,
    requested_specifier: ModuleSpecifier,
    response: LoadResponse,
  ) {
    let maybe_headers = response.maybe_headers;
    let specifier = response.specifier.clone();

    // If the response was redirected, then we add the module to the redirects
    if requested_specifier != specifier {
      // remove a potentially pending redirect that will never resolve
      if let Some(slot) = self.graph.module_slots.get(&requested_specifier) {
        if matches!(slot, ModuleSlot::Pending) {
          self.graph.module_slots.remove(&requested_specifier);
        }
      }
      self
        .graph
        .redirects
        .insert(requested_specifier, specifier.clone());
    }

    let is_root = self.graph.roots.contains(&specifier);

    let module_slot = parse_module(
      &specifier,
      maybe_headers.as_ref(),
      response.content,
      self.maybe_resolver,
      self.source_parser,
      is_root,
    );

    if let ModuleSlot::Module(module) = &module_slot {
      for dep in module.dependencies.values() {
        if let Resolved::Specifier(specifier, _) = &dep.maybe_code {
          self.load(specifier, dep.is_dynamic);
        }
        if let Resolved::Specifier(specifier, _) = &dep.maybe_type {
          self.load(specifier, dep.is_dynamic);
        }
      }

      if let Some((_, Resolved::Specifier(specifier, _))) =
        &module.maybe_types_dependency
      {
        self.load(specifier, false);
      }
    }
    self.graph.module_slots.insert(specifier, module_slot);
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
    if !self.1.maybe_code.is_none() {
      map.serialize_entry("code", &self.1.maybe_code)?;
    }
    if !self.1.maybe_type.is_none() {
      map.serialize_entry("type", &self.1.maybe_type)?;
    }
    if self.1.is_dynamic {
      map.serialize_entry("isDynamic", &self.1.is_dynamic)?;
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
      state.serialize_field("dependency", resolved)?;
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
