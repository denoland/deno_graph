// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::graph::Range;
use crate::module_specifier::resolve_import;
use crate::packages::JsrPackageInfo;
use crate::packages::JsrPackageVersionInfo;
use crate::text_encoding::strip_bom_mut;
use crate::ModuleInfo;
use crate::SpecifierError;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;

use anyhow::anyhow;
use anyhow::Error;
use data_url::DataUrl;
use deno_ast::ModuleSpecifier;
use futures::future;
use futures::future::LocalBoxFuture;
use once_cell::sync::Lazy;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use url::Url;

mod file_system;
pub use file_system::*;

pub const DEFAULT_JSX_IMPORT_SOURCE_MODULE: &str = "jsx-runtime";

/// Information that comes from an external source which can be optionally
/// included in the module graph.
#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct CacheInfo {
  /// The path to the local representation of the file. If a local file, the
  /// path to the original file, if a remote file, the path to the file in the
  /// cache.
  pub local: Option<PathBuf>,
  /// If the file has been transpiled, the path to the cached version of the
  /// transpiled JavaScript.
  pub emit: Option<PathBuf>,
  /// If the file has been transpiled and there is a source map separate from
  /// the transpiled JavaScript, the path to this file.
  pub map: Option<PathBuf>,
}

/// The response that is expected from a loader's `.load()` method.
///
/// The returned specifier is the final specifier. This can differ from the
/// requested specifier (e.g. if a redirect was encountered when loading)
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum LoadResponse {
  /// A module where the content is not available when building the graph, but
  /// will be available at runtime. The module will be marked as
  /// `ModuleKind::External` and no dependency analysis will be performed.
  External { specifier: ModuleSpecifier },
  /// A loaded module.
  Module {
    /// The content of the remote module.
    content: Arc<str>,
    /// The final specifier of the module.
    specifier: ModuleSpecifier,
    /// If the module is a remote module, the headers should be returned as a
    /// hashmap of lower-cased string values.
    #[serde(rename = "headers", skip_serializing_if = "Option::is_none")]
    maybe_headers: Option<HashMap<String, String>>,
  },
}

pub type LoadResult = Result<Option<LoadResponse>, anyhow::Error>;
pub type LoadFuture = LocalBoxFuture<'static, LoadResult>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CacheSetting {
  /// Attempts to load a specifier from the cache.
  ///
  /// This is used to see whether the specifier is in the cache for `jsr:` specifiers.
  /// * If it is, then it will use the source provided to get the module information.
  /// * If not, then it will use the manifest information to do resolution and
  ///   issue a separate request to the `load` method in order to get the source.
  Only,
  /// The implementation should prefer using the cache.
  Use,
  /// Loads a specifier where the implementation should not load
  /// from an internal cache. This is only ever done when loading
  /// `jsr:` specifier module information and the version constraint
  /// cannot be resolved.
  Reload,
}

impl CacheSetting {
  /// String representation that can be sent to JS for consumption in deno_cache.
  pub fn as_js_str(&self) -> &'static str {
    // note: keep these values aligned with deno_cache
    match self {
      CacheSetting::Only => "only",
      CacheSetting::Use => "use",
      CacheSetting::Reload => "reload",
    }
  }
}

pub static DEFAULT_DENO_REGISTRY_URL: Lazy<Url> =
  Lazy::new(|| Url::parse("https://registry-staging.deno.com").unwrap());

/// A trait which allows asynchronous loading of source files into a module
/// graph in a thread safe way as well as a way to provide additional meta data
/// about any cached resources.
pub trait Loader {
  fn registry_url(&self) -> &Url {
    &DEFAULT_DENO_REGISTRY_URL
  }

  fn registry_package_url(&self, nv: &PackageNv) -> Url {
    self
      .registry_url()
      .join(&format!("{}/{}", nv.name, nv.version))
      .unwrap()
  }

  fn registry_package_url_to_nv(&self, url: &Url) -> Option<PackageNv> {
    let path = url.as_str().strip_prefix(self.registry_url().as_str())?;
    let path = path.strip_prefix('/').unwrap_or(path);
    let parts = path.split('/').take(3).collect::<Vec<_>>();
    if parts.len() != 3 {
      return None;
    }
    Some(PackageNv {
      name: format!("{}/{}", parts[0], parts[1]),
      version: deno_semver::Version::parse_standard(&parts[2]).ok()?,
    })
  }

  /// An optional method which returns cache info for a module specifier.
  fn get_cache_info(&self, _specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    None
  }

  /// A method that given a specifier that asynchronously returns the
  /// source of the file.
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
    cache_setting: CacheSetting,
  ) -> LoadFuture;

  /// Cache the module info for the provided specifier if the loader
  /// supports caching this information.
  fn cache_module_info(
    &mut self,
    _specifier: &ModuleSpecifier,
    _source: &str,
    _module_info: &ModuleInfo,
  ) {
  }
}

#[derive(Error, Debug)]
pub enum ResolveError {
  #[error(transparent)]
  Specifier(#[from] SpecifierError),
  #[error(transparent)]
  Other(#[from] anyhow::Error),
}

/// The kind of resolution currently being done by deno_graph.
#[derive(Debug, Clone, Copy)]
pub enum ResolutionMode {
  /// Resolving for code that will be executed.
  Execution,
  /// Resolving for code that will be used for type information.
  Types,
}

/// A trait which allows the module graph to resolve specifiers and type only
/// dependencies. This can be use to provide import maps and override other
/// default resolution logic used by `deno_graph`.
pub trait Resolver: fmt::Debug {
  /// An optional method that returns the default JSX import source if one is
  /// configured. If this method returns `Some` and a JSX file is encountered
  /// that does not have an import source specified as a pragma, this import
  /// source will be used instead.
  fn default_jsx_import_source(&self) -> Option<String> {
    None
  }

  /// An optional method which returns the JSX import source module which will
  /// be appended to any JSX import source pragmas identified.
  fn jsx_import_source_module(&self) -> &str {
    DEFAULT_JSX_IMPORT_SOURCE_MODULE
  }

  /// Given a string specifier and a referring module specifier, return a
  /// resolved module specifier.
  fn resolve(
    &self,
    specifier_text: &str,
    referrer_range: &Range,
    _mode: ResolutionMode,
  ) -> Result<ModuleSpecifier, ResolveError> {
    Ok(resolve_import(specifier_text, &referrer_range.specifier)?)
  }

  /// Given a module specifier, return an optional tuple which provides a module
  /// specifier that contains the types for the module and an optional range
  /// which contains information about the source of the dependency. This will
  /// only be called for module specifiers are resolved to a non-typed input
  /// (e.g. JavaScript and JSX) and there is not yet types resolved for this
  /// module. Any result will be set on the modules `maybe_types_dependency`
  /// property.
  fn resolve_types(
    &self,
    _specifier: &ModuleSpecifier,
  ) -> Result<Option<(ModuleSpecifier, Option<Range>)>, ResolveError> {
    Ok(None)
  }
}

#[derive(Error, Debug)]
#[error("Unknown built-in \"node:\" module: {module_name}")]
pub struct UnknownBuiltInNodeModuleError {
  /// Name of the invalid module.
  pub module_name: String,
}

#[derive(Debug)]
pub enum NpmPackageReqResolution {
  Ok(PackageNv),
  Err(anyhow::Error),
  /// Error was encountered, but instruct deno_graph to ask for
  /// the registry information again. This is useful to use when
  /// a user specifies an npm specifier that doesn't match any version
  /// found in a cache and you want to cache bust the registry information.
  ///
  /// When the implementation provides this, it should cache bust its
  /// cached/loaded registry information and deno_graph will request
  /// the registry information for every package again then re-attempt resolution.
  ///
  /// deno_graph will restart only once per build call to prevent accidental,
  /// infinite loops, but the implementation should ensure this is only
  /// returned once per session.
  ReloadRegistryInfo(anyhow::Error),
}

pub trait NpmResolver: fmt::Debug {
  /// Gets the builtin node module name from the specifier (ex. "node:fs" -> "fs").
  fn resolve_builtin_node_module(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<String>, UnknownBuiltInNodeModuleError>;

  /// The callback when a bare specifier is resolved to a builtin node module.
  /// (Note: used for printing warnings to discourage that usage of bare specifiers)
  fn on_resolve_bare_builtin_node_module(
    &self,
    module_name: &str,
    range: &Range,
  );

  /// This tells the implementation to asynchronously load within itself the
  /// npm registry package information so that synchronous resolution can occur
  /// afterwards.
  ///
  /// WARNING: deno_graph will stop executing these futures when a
  /// `NpmPackageReqResolution::ReloadRegistryInfo` is returned from
  /// `resolve_npm`. The implementation should be resilient to this.
  fn load_and_cache_npm_package_info(
    &self,
    package_name: &str,
  ) -> LocalBoxFuture<'static, Result<(), anyhow::Error>>;

  /// Resolves an npm package requirement to a resolved npm package name and version.
  fn resolve_npm(&self, package_req: &PackageReq) -> NpmPackageReqResolution;

  /// Returns true when bare node specifier resoluion is enabled
  fn enables_bare_builtin_node_module(&self) -> bool {
    false
  }
}

pub fn load_data_url(
  specifier: &ModuleSpecifier,
) -> Result<Option<LoadResponse>, anyhow::Error> {
  let url = DataUrl::process(specifier.as_str())
    .map_err(|_| anyhow!("Unable to decode data url."))?;
  let (bytes, _) = url
    .decode_to_vec()
    .map_err(|_| anyhow!("Unable to decode data url."))?;
  let mut headers: HashMap<String, String> = HashMap::with_capacity(1);
  headers.insert("content-type".to_string(), url.mime_type().to_string());
  let mut content = String::from_utf8(bytes)?;
  strip_bom_mut(&mut content);
  Ok(Some(LoadResponse::Module {
    specifier: specifier.clone(),
    maybe_headers: Some(headers),
    content: content.into(),
  }))
}

/// An implementation of the loader attribute where the responses are provided
/// ahead of time. This is useful for testing or
#[derive(Default)]
pub struct MemoryLoader {
  sources: HashMap<ModuleSpecifier, Result<LoadResponse, Error>>,
  cache_info: HashMap<ModuleSpecifier, CacheInfo>,
}

pub enum Source<S> {
  Module {
    specifier: S,
    maybe_headers: Option<Vec<(S, S)>>,
    content: S,
  },
  External(S),
  Err(Error),
}

impl<S: AsRef<str>> Source<S> {
  fn into_result(self) -> Result<LoadResponse, Error> {
    match self {
      Source::Module {
        specifier,
        maybe_headers,
        content,
      } => Ok(LoadResponse::Module {
        specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
        maybe_headers: maybe_headers.map(|h| {
          h.into_iter()
            .map(|(k, v)| (k.as_ref().to_string(), v.as_ref().to_string()))
            .collect()
        }),
        content: content.as_ref().into(),
      }),
      Source::External(specifier) => Ok(LoadResponse::External {
        specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
      }),
      Source::Err(error) => Err(error),
    }
  }
}

pub type MemoryLoaderSources<S> = Vec<(S, Source<S>)>;

impl MemoryLoader {
  pub fn new<S: AsRef<str>>(
    sources: MemoryLoaderSources<S>,
    cache_info: Vec<(S, CacheInfo)>,
  ) -> Self {
    Self {
      sources: sources
        .into_iter()
        .map(|(s, r)| {
          let specifier = ModuleSpecifier::parse(s.as_ref()).unwrap();
          (specifier, r.into_result())
        })
        .collect(),
      cache_info: cache_info
        .into_iter()
        .map(|(s, c)| {
          let specifier = ModuleSpecifier::parse(s.as_ref()).unwrap();
          (specifier, c)
        })
        .collect(),
    }
  }

  pub fn add_source<S: AsRef<str>>(
    &mut self,
    specifier: impl AsRef<str>,
    source: Source<S>,
  ) {
    let specifier = ModuleSpecifier::parse(specifier.as_ref()).unwrap();
    self.sources.insert(specifier, source.into_result());
  }

  pub fn add_external_source(&mut self, specifier: impl AsRef<str>) {
    self.add_source(
      specifier.as_ref(),
      Source::External(specifier.as_ref().to_string()),
    );
  }

  pub fn add_source_with_text(
    &mut self,
    specifier: impl AsRef<str>,
    source: impl AsRef<str>,
  ) {
    self.add_source(
      specifier.as_ref(),
      Source::Module {
        specifier: specifier.as_ref().to_string(),
        maybe_headers: None,
        content: source.as_ref().to_string(),
      },
    );
  }

  pub fn add_deno_package_info(
    &mut self,
    name: &str,
    package_info: &JsrPackageInfo,
  ) {
    let specifier = DEFAULT_DENO_REGISTRY_URL
      .join(&format!("{}/meta.json", name))
      .unwrap();
    let json_text = serde_json::to_string(package_info).unwrap();
    self.add_source_with_text(specifier, json_text);
  }

  pub fn add_deno_version_info(
    &mut self,
    nv: &PackageNv,
    version_info: &JsrPackageVersionInfo,
  ) {
    let specifier = DEFAULT_DENO_REGISTRY_URL
      .join(&format!("{}/{}_meta.json", nv.name, nv.version))
      .unwrap();
    let json_text = serde_json::to_string(version_info).unwrap();
    self.add_source_with_text(specifier, json_text);
  }
}

impl Loader for MemoryLoader {
  fn get_cache_info(&self, specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    self.cache_info.get(specifier).cloned()
  }

  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    _is_dynamic: bool,
    _cache_setting: CacheSetting,
  ) -> LoadFuture {
    let response = match self.sources.get(specifier) {
      Some(Ok(response)) => Ok(Some(response.clone())),
      Some(Err(err)) => Err(anyhow!("{}", err)),
      None if specifier.scheme() == "data" => load_data_url(specifier),
      _ => Ok(None),
    };
    Box::pin(future::ready(response))
  }
}

/// A trait which can be used to allow the module graph to report status events
/// to the user.
pub trait Reporter: fmt::Debug {
  /// A handler that is called after each load of a module. It contains the
  /// module specifier of the module that was loaded, and the number of modules
  /// seen (total number of unique specifiers seen), and the number of modules
  /// loaded (where [Loader::load] has been called, and the returned future is
  /// ready).
  fn on_load(
    &self,
    specifier: &ModuleSpecifier,
    modules_done: usize,
    modules_total: usize,
  );
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::module_specifier::resolve_import;
  use serde_json::json;

  #[derive(Debug)]
  pub(crate) struct MockResolver {
    map: HashMap<ModuleSpecifier, HashMap<String, ModuleSpecifier>>,
    types: HashMap<ModuleSpecifier, (ModuleSpecifier, Option<Range>)>,
  }

  impl MockResolver {
    pub fn new<S: AsRef<str>>(
      map: Vec<(S, Vec<(S, S)>)>,
      types: Vec<(S, (S, Option<Range>))>,
    ) -> Self {
      Self {
        map: map
          .into_iter()
          .map(|(r, m)| {
            let referrer = ModuleSpecifier::parse(r.as_ref()).unwrap();
            let map = m
              .into_iter()
              .map(|(s, ms)| {
                let specifier_str = s.as_ref().to_string();
                let specifier = ModuleSpecifier::parse(ms.as_ref()).unwrap();
                (specifier_str, specifier)
              })
              .collect();
            (referrer, map)
          })
          .collect(),
        types: types
          .into_iter()
          .map(|(s, (t, ms))| {
            let specifier = ModuleSpecifier::parse(s.as_ref()).unwrap();
            let types_specifier = ModuleSpecifier::parse(t.as_ref()).unwrap();
            (specifier, (types_specifier, ms))
          })
          .collect(),
      }
    }
  }

  impl Resolver for MockResolver {
    fn resolve(
      &self,
      specifier: &str,
      referrer_range: &Range,
      _mode: ResolutionMode,
    ) -> Result<ModuleSpecifier, ResolveError> {
      if let Some(map) = self.map.get(&referrer_range.specifier) {
        if let Some(resolved_specifier) = map.get(specifier) {
          return Ok(resolved_specifier.clone());
        }
      }
      Ok(resolve_import(specifier, &referrer_range.specifier)?)
    }

    fn resolve_types(
      &self,
      specifier: &ModuleSpecifier,
    ) -> Result<Option<(ModuleSpecifier, Option<Range>)>, ResolveError> {
      Ok(self.types.get(specifier).cloned())
    }
  }

  #[test]
  fn test_deserialize_load_response() {
    let actual: LoadResponse = serde_json::from_value(
      json!({ "kind": "external", "specifier": "https://example.com/bundle" }),
    )
    .unwrap();
    assert_eq!(
      actual,
      LoadResponse::External {
        specifier: ModuleSpecifier::parse("https://example.com/bundle")
          .unwrap()
      }
    );
  }
}
