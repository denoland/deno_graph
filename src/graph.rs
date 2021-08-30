// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::ast;
use crate::ast::AstParser;
use crate::ast::ParsedAst;
use crate::ast::Range;
use crate::media_type::MediaType;
use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::module_specifier::SpecifierError;
use crate::source::*;

use anyhow::anyhow;
use anyhow::Result;
use data_url::DataUrl;
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
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum ModuleGraphError {
  LoadingErr(anyhow::Error),
  ParseErr(ast::Diagnostic),
  InvalidSource(ModuleSpecifier),
}

impl Clone for ModuleGraphError {
  fn clone(&self) -> Self {
    match self {
      Self::LoadingErr(err) => Self::LoadingErr(anyhow!(err.to_string())),
      Self::ParseErr(err) => Self::ParseErr(err.clone()),
      Self::InvalidSource(specifier) => Self::InvalidSource(specifier.clone()),
    }
  }
}

impl std::error::Error for ModuleGraphError {}

impl fmt::Display for ModuleGraphError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let msg = match self {
      Self::LoadingErr(err) => {
        format!("An error was returned from the loader: {}", err)
      }
      Self::ParseErr(diagnostic) => {
        format!(
          "The module's source code would not be parsed: {}",
          diagnostic
        )
      }
      Self::InvalidSource(specifier) => {
        format!("The source code is invalid, as it does not match the expected hash in the lock file.\n  Specifier: {}", specifier)
      }
    };
    write!(f, "{}", msg)
  }
}

impl From<ast::Diagnostic> for ModuleGraphError {
  fn from(diagnostic: ast::Diagnostic) -> Self {
    Self::ParseErr(diagnostic)
  }
}

#[derive(Debug)]
pub enum ResolutionError {
  InvalidDowngrade,
  InvalidLocalImport,
  ResolverError(anyhow::Error),
  InvalidSpecifier(SpecifierError),
}

impl Clone for ResolutionError {
  fn clone(&self) -> Self {
    match self {
      Self::InvalidDowngrade => Self::InvalidDowngrade,
      Self::InvalidLocalImport => Self::InvalidLocalImport,
      Self::ResolverError(err) => Self::ResolverError(anyhow!(err.to_string())),
      Self::InvalidSpecifier(err) => Self::InvalidSpecifier(err.clone()),
    }
  }
}

impl PartialEq for ResolutionError {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::ResolverError(_), Self::ResolverError(_))
      | (Self::InvalidDowngrade, Self::InvalidDowngrade)
      | (Self::InvalidLocalImport, Self::InvalidLocalImport) => true,
      (Self::InvalidSpecifier(s), Self::InvalidSpecifier(o)) => s == o,
      _ => false,
    }
  }
}

impl Eq for ResolutionError {}

impl fmt::Display for ResolutionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::InvalidDowngrade => write!(f, "Modules imported via https are not allowed to import http modules."),
      Self::InvalidLocalImport => write!(f, "Remote modules are not allowed to import local modules. Consider using a dynamic import instead."),
      Self::ResolverError(err) => write!(f, "Error returned from resolved: {}", err),
      Self::InvalidSpecifier(err) => write!(f, "Invalid specifier error: {}", err),
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
  #[serde(rename = "size", serialize_with = "serialize_source")]
  pub source: String,
  pub specifier: ModuleSpecifier,
}

impl Module {
  pub fn new(specifier: ModuleSpecifier, source: String) -> Self {
    Self {
      dependencies: Default::default(),
      maybe_cache_info: None,
      maybe_checksum: None,
      maybe_types_dependency: None,
      media_type: MediaType::Unknown,
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

#[derive(Debug, Serialize)]
pub struct ModuleGraph {
  pub root: ModuleSpecifier,
  #[serde(skip_serializing)]
  maybe_locker: Option<Rc<RefCell<dyn Locker>>>,
  #[serde(serialize_with = "serialize_modules")]
  pub(crate) modules: BTreeMap<ModuleSpecifier, ModuleSlot>,
  redirects: BTreeMap<ModuleSpecifier, ModuleSpecifier>,
}

impl ModuleGraph {
  pub fn new(
    root: ModuleSpecifier,
    maybe_locker: Option<Rc<RefCell<dyn Locker>>>,
  ) -> Self {
    Self {
      root,
      maybe_locker,
      modules: Default::default(),
      redirects: Default::default(),
    }
  }

  /// Get a module from the module graph, returning `None` if the module is not
  /// part of the graph, or if when loading the module it errored. If any module
  /// resolution error is needed, then use the `try_get()` method which will
  /// return any resolution error as the error in the result.
  #[cfg(feature = "rust")]
  pub fn get(&self, specifier: &ModuleSpecifier) -> Option<&Module> {
    let specifier = self.resolve(specifier);
    match self.modules.get(&specifier) {
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
      for (_, module_slot) in self.modules.iter() {
        if let ModuleSlot::Module(module) = module_slot {
          if !locker.check_or_insert(&module.specifier, &module.source) {
            return Err(ModuleGraphError::InvalidSource(
              module.specifier.clone(),
            ));
          }
        }
      }
    }
    Ok(())
  }

  /// Resolve a specifier from the module graph following any possible redirects
  /// returning the "final" module.
  pub fn resolve(&self, specifier: &ModuleSpecifier) -> ModuleSpecifier {
    let mut redirected_specifier = specifier;
    while let Some(specifier) = self.redirects.get(redirected_specifier) {
      redirected_specifier = specifier;
    }
    redirected_specifier.clone()
  }

  /// Resolve a dependency of a referring module providing the string specifier
  /// of the depdency and returning an optional fully qualified module
  /// specifier.
  pub fn resolve_dependency(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Option<&ModuleSpecifier> {
    let referrer = self.resolve(referrer);
    let referring_module_slot = self.modules.get(&referrer)?;
    if let ModuleSlot::Module(referring_module) = referring_module_slot {
      let dependency = referring_module.dependencies.get(specifier)?;
      if let Resolved::Specifier(specifier, _) = &dependency.maybe_type {
        Some(specifier)
      } else if let Resolved::Specifier(specifier, _) = &dependency.maybe_code {
        Some(specifier)
      } else {
        None
      }
    } else {
      None
    }
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
    match self.modules.get(&specifier) {
      Some(ModuleSlot::Module(module)) => Ok(Some(module)),
      Some(ModuleSlot::Err(err)) => Err(err.clone()),
      _ => Ok(None),
    }
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
  let content = String::from_utf8(bytes)?;
  Ok(Some(LoadResponse {
    specifier: specifier.clone(),
    maybe_headers: Some(headers),
    content,
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
      .map_err(ResolutionError::ResolverError)
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
        Resolved::Err(ResolutionError::InvalidDowngrade, span)
      } else if (referrer_scheme == "https" || referrer_scheme == "http")
        && !(specifier_scheme == "https" || specifier_scheme == "http")
        && !remapped
      {
        Resolved::Err(ResolutionError::InvalidLocalImport, span)
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
  content: &str,
  maybe_resolver: Option<&dyn Resolver>,
  ast_parser: &mut dyn AstParser,
) -> ModuleSlot {
  // Parse the module and start analyzing the module.
  match ast_parser.parse(
    specifier,
    content,
    get_media_type(specifier, maybe_headers),
  ) {
    Ok(parsed_ast) => {
      // Return the module as a valid module
      ModuleSlot::Module(parse_module_from_ast(
        specifier,
        maybe_headers,
        content,
        parsed_ast,
        maybe_resolver,
      ))
    }
    Err(diagnostic) => ModuleSlot::Err(diagnostic.into()),
  }
}

pub(crate) fn parse_module_from_ast(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&HashMap<String, String>>,
  content: &str,
  parsed_ast: &dyn ParsedAst,
  maybe_resolver: Option<&dyn Resolver>,
) -> Module {
  // Init the module and determine its media type
  let mut module = Module::new(specifier.clone(), content.to_string());
  module.media_type = get_media_type(specifier, maybe_headers);

  // Analyze the TypeScript triple-slash references
  for reference in parsed_ast.analyze_ts_references() {
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
  let descriptors = parsed_ast.analyze_dependencies();
  for desc in descriptors {
    let dep = module
      .dependencies
      .entry(desc.specifier.to_string())
      .or_default();
    let resolved_dependency = resolve(
      &desc.specifier,
      &module.specifier,
      &Range::from_span(parsed_ast, &desc.specifier_span),
      maybe_resolver,
    );
    dep.maybe_code = resolved_dependency;
    let specifier = module.specifier.clone();
    let maybe_type = parsed_ast
      .analyze_deno_types(&desc)
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
  ast_parser: &'a mut dyn AstParser,
}

impl<'a> Builder<'a> {
  pub fn new(
    root_specifier: ModuleSpecifier,
    is_dynamic_root: bool,
    loader: &'a mut dyn Loader,
    maybe_resolver: Option<&'a dyn Resolver>,
    maybe_locker: Option<Rc<RefCell<dyn Locker>>>,
    ast_parser: &'a mut dyn AstParser,
  ) -> Self {
    Self {
      is_dynamic_root,
      graph: ModuleGraph::new(root_specifier, maybe_locker),
      loader,
      maybe_resolver,
      pending: FuturesUnordered::new(),
      ast_parser,
    }
  }

  pub async fn build(mut self) -> ModuleGraph {
    let root = self.graph.root.clone();
    self.load(&root, self.is_dynamic_root);

    loop {
      match self.pending.next().await {
        Some((specifier, Ok(Some(response)))) => {
          self.visit(specifier, response)
        }
        Some((specifier, Ok(None))) => {
          self.graph.modules.insert(specifier, ModuleSlot::Missing);
        }
        Some((specifier, Err(err))) => {
          self.graph.modules.insert(
            specifier,
            ModuleSlot::Err(ModuleGraphError::LoadingErr(err)),
          );
        }
        _ => {}
      }
      if self.pending.is_empty() {
        break;
      }
    }

    // Enrich with cache info from the loader
    for slot in self.graph.modules.values_mut() {
      if let ModuleSlot::Module(ref mut module) = slot {
        module.maybe_cache_info = self.loader.get_cache_info(&module.specifier);
      }
    }

    self.graph
  }

  /// Enqueue a request to load the specifier via the loader.
  fn load(&mut self, specifier: &ModuleSpecifier, is_dynamic: bool) {
    if !self.graph.modules.contains_key(specifier) {
      self
        .graph
        .modules
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
      if let Some(slot) = self.graph.modules.get(&requested_specifier) {
        if matches!(slot, ModuleSlot::Pending) {
          self.graph.modules.remove(&requested_specifier);
        }
      }
      // if the root has been redirected, update the root
      if self.graph.root == requested_specifier {
        self.graph.root = specifier.clone();
      }
      self
        .graph
        .redirects
        .insert(requested_specifier, specifier.clone());
    }

    let module_slot = parse_module(
      &specifier,
      maybe_headers.as_ref(),
      &response.content,
      self.maybe_resolver,
      self.ast_parser,
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
    self.graph.modules.insert(specifier, module_slot);
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
  serializer.serialize_u32(source.as_bytes().iter().count() as u32)
}
