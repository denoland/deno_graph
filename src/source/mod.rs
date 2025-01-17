// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;
use deno_ast::data_url::RawDataUrl;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_error::JsErrorClass;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::StackString;
use futures::future;
use futures::future::LocalBoxFuture;
use once_cell::sync::Lazy;
use serde::Deserialize;
use serde::Serialize;
use sys_traits::boxed::BoxedFsDirEntry;
use sys_traits::boxed::FsReadDirBoxed;
use sys_traits::BaseFsReadDir;
use thiserror::Error;
use url::Url;

use crate::graph::Range;
use crate::module_specifier::resolve_import;
use crate::packages::JsrPackageInfo;
use crate::packages::JsrPackageVersionInfo;
use crate::ModuleInfo;
use crate::NpmLoadError;
use crate::SpecifierError;

pub type FileSystem = dyn FsReadDirBoxed;

pub struct NullFileSystem;

impl BaseFsReadDir for NullFileSystem {
  type ReadDirEntry = BoxedFsDirEntry;

  fn base_fs_read_dir(
    &self,
    _path: &Path,
  ) -> std::io::Result<
    Box<dyn Iterator<Item = std::io::Result<Self::ReadDirEntry>>>,
  > {
    Ok(Box::new(std::iter::empty()))
  }
}

pub mod wasm;

pub const DEFAULT_JSX_IMPORT_SOURCE_MODULE: &str = "jsx-runtime";

/// Information that comes from an external source which can be optionally
/// included in the module graph.
#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct CacheInfo {
  /// The path to the local representation of the file. If a local file, the
  /// path to the original file, if a remote file, the path to the file in the
  /// cache.
  pub local: Option<PathBuf>,
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
  /// Specifier redirected to another specifier.
  ///
  /// It's important to return the redirects to deno_graph so it
  /// can track them and also tell whether a checksum should be
  /// sent with the load request for JSR dependencies loaded over
  /// HTTPS via a redirect.
  Redirect {
    /// The final specifier of the module.
    specifier: ModuleSpecifier,
  },
  /// A loaded module.
  Module {
    /// The content of the remote module.
    content: Arc<[u8]>,
    /// The final specifier of the module.
    specifier: ModuleSpecifier,
    /// If the module is a remote module, the headers should be returned as a
    /// hashmap of lower-cased string values.
    #[serde(rename = "headers", skip_serializing_if = "Option::is_none")]
    maybe_headers: Option<HashMap<String, String>>,
  },
}

#[derive(Debug, Error, deno_error::JsError)]
pub enum LoadError {
  #[class(inherit)]
  #[error(transparent)]
  ChecksumIntegrity(#[from] ChecksumIntegrityError),
  #[class(inherit)]
  #[error(transparent)]
  Other(Arc<dyn JsErrorClass>),
}

pub type LoadResult = Result<Option<LoadResponse>, LoadError>;
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

pub static DEFAULT_JSR_URL: Lazy<Url> =
  Lazy::new(|| Url::parse("https://jsr.io").unwrap());

#[derive(Debug, Clone, Error, deno_error::JsError)]
#[class(generic)]
#[error("Integrity check failed.\n\nActual: {}\nExpected: {}", .actual, .expected
)]
pub struct ChecksumIntegrityError {
  pub actual: String,
  pub expected: String,
}

/// A SHA-256 checksum to verify the contents of a module
/// with while loading.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct LoaderChecksum(String);

impl LoaderChecksum {
  pub fn new(checksum: String) -> Self {
    Self(checksum)
  }

  pub fn into_string(self) -> String {
    self.0
  }

  pub fn as_str(&self) -> &str {
    &self.0
  }

  pub fn check_source(
    &self,
    source: &[u8],
  ) -> Result<(), ChecksumIntegrityError> {
    let actual_checksum = Self::gen(source);
    if self.0 == actual_checksum {
      Ok(())
    } else {
      Err(ChecksumIntegrityError {
        actual: actual_checksum,
        expected: self.0.to_string(),
      })
    }
  }

  pub fn gen(source: &[u8]) -> String {
    use sha2::Digest;
    use sha2::Sha256;
    let mut hasher = Sha256::new();
    hasher.update(source);
    format!("{:x}", hasher.finalize())
  }
}

impl fmt::Display for LoaderChecksum {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

pub trait Locker {
  fn get_remote_checksum(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<LoaderChecksum>;
  fn has_remote_checksum(&self, specifier: &ModuleSpecifier) -> bool;
  fn set_remote_checksum(
    &mut self,
    specifier: &ModuleSpecifier,
    checksum: LoaderChecksum,
  );

  fn get_pkg_manifest_checksum(
    &self,
    package_nv: &PackageNv,
  ) -> Option<LoaderChecksum>;
  fn set_pkg_manifest_checksum(
    &mut self,
    package_nv: &PackageNv,
    checksum: LoaderChecksum,
  );
}

#[derive(Debug, Default, Clone)]
pub struct HashMapLocker {
  remote: HashMap<ModuleSpecifier, LoaderChecksum>,
  pkg_manifests: HashMap<PackageNv, LoaderChecksum>,
}

impl HashMapLocker {
  pub fn remote(&self) -> &HashMap<ModuleSpecifier, LoaderChecksum> {
    &self.remote
  }

  pub fn pkg_manifests(&self) -> &HashMap<PackageNv, LoaderChecksum> {
    &self.pkg_manifests
  }
}

impl Locker for HashMapLocker {
  fn get_remote_checksum(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<LoaderChecksum> {
    self.remote.get(specifier).cloned()
  }

  fn has_remote_checksum(&self, specifier: &ModuleSpecifier) -> bool {
    self.remote.contains_key(specifier)
  }

  fn set_remote_checksum(
    &mut self,
    specifier: &ModuleSpecifier,
    checksum: LoaderChecksum,
  ) {
    self.remote.insert(specifier.clone(), checksum);
  }

  fn get_pkg_manifest_checksum(
    &self,
    package_nv: &PackageNv,
  ) -> Option<LoaderChecksum> {
    self.pkg_manifests.get(package_nv).cloned()
  }

  fn set_pkg_manifest_checksum(
    &mut self,
    package_nv: &PackageNv,
    checksum: LoaderChecksum,
  ) {
    self.pkg_manifests.insert(package_nv.clone(), checksum);
  }
}

#[derive(Debug, Clone)]
pub struct LoadOptions {
  pub is_dynamic: bool,
  /// If the root specifier building the graph was in a dynamic branch.
  ///
  /// This can be useful for telling if a dynamic load is statically analyzable
  /// where `is_dynamic` is `true`` and `was_dynamic_root` is `false`.
  pub was_dynamic_root: bool,
  pub cache_setting: CacheSetting,
  /// It is the loader's responsibility to verify the provided checksum if it
  /// exists because in the CLI we only verify the checksum of the source when
  /// it is loaded from the global cache. We don't verify it when loaded from
  /// the vendor folder.
  ///
  /// The source may be verified by running `checksum.check_source(content)?`.
  pub maybe_checksum: Option<LoaderChecksum>,
}

/// A trait which allows asynchronous loading of source files into a module
/// graph in a thread safe way as well as a way to provide additional meta data
/// about any cached resources.
pub trait Loader {
  /// The maximum number of redirects allowed.
  fn max_redirects(&self) -> usize {
    10
  }

  /// An optional method which returns cache info for a module specifier.
  fn get_cache_info(&self, _specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    None
  }

  /// A method that given a specifier that asynchronously returns the
  /// source of the file.
  ///
  /// To ensure errors surfaced in the graph are more specific for checksum
  /// integrity errors, ensure this returns a `ChecksumIntegrityError` when
  /// the checksum on `LoadOptions` does not match the loaded source.
  fn load(
    &self,
    specifier: &ModuleSpecifier,
    options: LoadOptions,
  ) -> LoadFuture;

  /// Cache the module info for the provided specifier if the loader
  /// supports caching this information.
  fn cache_module_info(
    &self,
    _specifier: &ModuleSpecifier,
    _media_type: MediaType,
    _source: &Arc<[u8]>,
    _module_info: &ModuleInfo,
  ) {
  }
}

pub trait JsrUrlProvider {
  fn url(&self) -> &Url {
    &DEFAULT_JSR_URL
  }

  fn package_url(&self, nv: &PackageNv) -> Url {
    recommended_registry_package_url(self.url(), nv)
  }

  fn package_url_to_nv(&self, url: &Url) -> Option<PackageNv> {
    recommended_registry_package_url_to_nv(self.url(), url)
  }
}

impl<'a> Default for &'a dyn JsrUrlProvider {
  fn default() -> &'a dyn JsrUrlProvider {
    &DefaultJsrUrlProvider
  }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct DefaultJsrUrlProvider;

impl JsrUrlProvider for DefaultJsrUrlProvider {}

/// The recommended way for getting the registry URL for a package.
///
/// This will concat the registry URL with the package name, a slash, then the version.
pub fn recommended_registry_package_url(
  registry_url: &Url,
  nv: &PackageNv,
) -> Url {
  registry_url
    .join(&format!("{}/{}/", nv.name, nv.version))
    .unwrap()
}

/// The recommended way to get the package name and version from a URL
/// that is found on the registry.
pub fn recommended_registry_package_url_to_nv(
  registry_url: &Url,
  url: &Url,
) -> Option<PackageNv> {
  let path = url.as_str().strip_prefix(registry_url.as_str())?;
  let path = path.strip_prefix('/').unwrap_or(path);
  let mut parts = path.split('/');
  let scope = parts.next()?;
  let name = parts.next()?;
  let version = parts.next()?;
  Some(PackageNv {
    name: {
      capacity_builder::StringBuilder::<StackString>::build(|builder| {
        builder.append(scope);
        builder.append('/');
        builder.append(name);
      })
      .unwrap()
    },
    version: deno_semver::Version::parse_standard(version).ok()?,
  })
}

#[derive(Error, Debug, deno_error::JsError)]
pub enum ResolveError {
  #[class(type)]
  #[error(transparent)]
  Specifier(#[from] SpecifierError),
  #[class(inherit)]
  #[error(transparent)]
  ImportMap(#[from] import_map::ImportMapError),
  #[class(inherit)]
  #[error(transparent)]
  Other(#[from] deno_error::JsErrorBox),
}

/// The kind of resolution currently being done by deno_graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolutionKind {
  /// Resolving for code that will be executed.
  Execution,
  /// Resolving for code that will be used for type information.
  Types,
}

impl ResolutionKind {
  pub fn is_types(&self) -> bool {
    *self == ResolutionKind::Types
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ResolutionMode {
  /// Resolving as an ES module.
  Import,
  /// Resolving as a CJS module.
  Require,
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

  /// An optional method that returns the default JSX types import source if one
  /// is configured. If this method returns `Some` and a JSX file is encountered
  /// that does not have an types import source specified as a pragma, this
  /// types import source will be used instead.
  fn default_jsx_import_source_types(&self) -> Option<String> {
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
    _kind: ResolutionKind,
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

#[derive(Error, Debug, Clone, deno_error::JsError)]
#[class("NotFound")]
#[error("Unknown built-in \"node:\" module: {module_name}")]
pub struct UnknownBuiltInNodeModuleError {
  /// Name of the invalid module.
  pub module_name: String,
}

#[derive(Debug)]
pub struct NpmResolvePkgReqsResult {
  /// The individual results of resolving the package requirements.
  ///
  /// This MUST correspond to the indexes of the provided package requirements.
  pub results: Vec<Result<PackageNv, NpmLoadError>>,
  /// Result of resolving the entire dependency graph after the initial reqs
  /// were resolved to NVs.
  ///
  /// Don't run dependency graph resolution if there are any individual failures.
  pub dep_graph_result: Result<(), Arc<dyn JsErrorClass>>,
}

#[async_trait(?Send)]
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

  /// This is an optimization for the implementation to start loading and caching
  /// the npm registry package information ahead of time.
  fn load_and_cache_npm_package_info(&self, package_name: &str);

  /// Resolves a the package version requirements.
  ///
  /// The implementation MUST return the same amount of resolutions back as
  /// version reqs provided or else a panic will occur.
  async fn resolve_pkg_reqs(
    &self,
    package_req: &[PackageReq],
  ) -> NpmResolvePkgReqsResult;

  /// Returns true when bare node specifier resoluion is enabled
  fn enables_bare_builtin_node_module(&self) -> bool {
    false
  }
}

pub fn load_data_url(
  specifier: &ModuleSpecifier,
) -> Result<Option<LoadResponse>, std::io::Error> {
  let data_url = RawDataUrl::parse(specifier)?;
  let (bytes, mime_type) = data_url.into_bytes_and_mime_type();
  let headers = HashMap::from([("content-type".to_string(), mime_type)]);
  Ok(Some(LoadResponse::Module {
    specifier: specifier.clone(),
    maybe_headers: Some(headers),
    content: Arc::from(bytes),
  }))
}

/// An implementation of the loader attribute where the responses are provided
/// ahead of time. This is useful for testing or
#[derive(Default)]
pub struct MemoryLoader {
  sources:
    HashMap<ModuleSpecifier, Result<LoadResponse, Arc<dyn JsErrorClass>>>,
  cache_info: HashMap<ModuleSpecifier, CacheInfo>,
}

pub enum Source<S> {
  Module {
    specifier: S,
    maybe_headers: Option<Vec<(S, S)>>,
    content: S,
  },
  Redirect(S),
  External(S),
  Err(Arc<dyn JsErrorClass>),
}

impl<S: AsRef<str>> Source<S> {
  fn into_result(self) -> Result<LoadResponse, Arc<dyn JsErrorClass>> {
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
        content: Arc::from(content.as_ref().to_string().into_bytes()),
      }),
      Source::Redirect(specifier) => Ok(LoadResponse::Redirect {
        specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
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

  pub fn add_bytes_source(
    &mut self,
    specifier: impl AsRef<str>,
    content: Vec<u8>,
  ) {
    self.sources.insert(
      ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
      Ok(LoadResponse::Module {
        specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
        maybe_headers: None,
        content: Arc::from(content),
      }),
    );
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

  pub fn add_jsr_package_info(
    &mut self,
    name: &str,
    package_info: &JsrPackageInfo,
  ) {
    let specifier = DEFAULT_JSR_URL
      .join(&format!("{}/meta.json", name))
      .unwrap();
    let json_text = serde_json::to_string(package_info).unwrap();
    self.add_source_with_text(specifier, json_text);
  }

  pub fn add_jsr_version_info(
    &mut self,
    name: &str,
    version: &str,
    version_info: &JsrPackageVersionInfo,
  ) {
    let specifier = DEFAULT_JSR_URL
      .join(&format!("{}/{}_meta.json", name, version))
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
    &self,
    specifier: &ModuleSpecifier,
    _options: LoadOptions,
  ) -> LoadFuture {
    let response = match self.sources.get(specifier) {
      Some(Ok(response)) => Ok(Some(response.clone())),
      Some(Err(err)) => Err(LoadError::Other(err.clone())),
      None if specifier.scheme() == "data" => {
        load_data_url(specifier).map_err(|e| LoadError::Other(Arc::new(e)))
      }
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

/// Resolve a media type and optionally the charset from a module specifier and
/// the value of a content type header.
pub fn resolve_media_type_and_charset_from_headers<'a>(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&'a HashMap<String, String>>,
) -> (MediaType, Option<&'a str>) {
  deno_media_type::resolve_media_type_and_charset_from_content_type(
    specifier,
    maybe_headers
      .and_then(|h| h.get("content-type"))
      .map(|v| v.as_str()),
  )
}

/// Resolve a media type and optionally the charset from a module specifier and
/// the value of a content type header.
pub fn resolve_media_type_and_charset_from_content_type<'a>(
  specifier: &ModuleSpecifier,
  maybe_content_type: Option<&'a String>,
) -> (MediaType, Option<&'a str>) {
  if let Some(content_type) = maybe_content_type {
    let mut content_types = content_type.split(';');
    let media_type = content_types
      .next()
      .map(|content_type| MediaType::from_content_type(specifier, content_type))
      .unwrap_or(MediaType::Unknown);
    let charset = content_types
      .map(str::trim)
      .find_map(|s| s.strip_prefix("charset="));

    (media_type, charset)
  } else {
    (MediaType::from_specifier(specifier), None)
  }
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
      _resolution_kind: ResolutionKind,
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
