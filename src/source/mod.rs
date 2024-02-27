// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::graph::Range;
use crate::module_specifier::resolve_import;
use crate::packages::JsrPackageInfo;
use crate::packages::JsrPackageVersionInfo;
use crate::text_encoding;
use crate::ModuleInfo;
use crate::SpecifierError;
use deno_ast::MediaType;
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
use std::borrow::Cow;
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
    content: Arc<[u8]>,
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

pub static DEFAULT_JSR_URL: Lazy<Url> =
  Lazy::new(|| Url::parse("https://jsr.io").unwrap());

#[derive(Debug, Error)]
#[error("Integrity check failed.\n\nActual: {}\nExpected: {}", .actual, .expected)]
pub struct ChecksumIntegrityError {
  pub actual: String,
  pub expected: String,
}

/// A SHA-256 checksum to verify the contents of a module
/// with while loading.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone)]
pub struct LoadOptions {
  pub is_dynamic: bool,
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
  /// An optional method which returns cache info for a module specifier.
  fn get_cache_info(&self, _specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    None
  }

  /// A method that given a specifier that asynchronously returns the
  /// source of the file.
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    options: LoadOptions,
  ) -> LoadFuture;

  /// Cache the module info for the provided specifier if the loader
  /// supports caching this information.
  fn cache_module_info(
    &mut self,
    _specifier: &ModuleSpecifier,
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
  let parts = path.split('/').take(3).collect::<Vec<_>>();
  if parts.len() != 3 {
    return None;
  }
  Some(PackageNv {
    name: format!("{}/{}", parts[0], parts[1]),
    version: deno_semver::Version::parse_standard(parts[2]).ok()?,
  })
}

#[derive(Error, Debug)]
pub enum ResolveError {
  #[error(transparent)]
  Specifier(#[from] SpecifierError),
  #[error(transparent)]
  Other(#[from] anyhow::Error),
}

/// The kind of resolution currently being done by deno_graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
  let data_url = RawDataUrl::parse(specifier)?;
  let (bytes, headers) = data_url.into_bytes_and_headers();
  Ok(Some(LoadResponse::Module {
    specifier: specifier.clone(),
    maybe_headers: Some(headers),
    content: Arc::from(bytes),
  }))
}

#[derive(Debug, Clone)]
pub struct RawDataUrl {
  pub mime_type: String,
  pub bytes: Vec<u8>,
}

impl RawDataUrl {
  pub fn parse(specifier: &ModuleSpecifier) -> Result<Self, Error> {
    let url = DataUrl::process(specifier.as_str())
      .map_err(|_| anyhow!("Unable to decode data url."))?;
    let (bytes, _) = url
      .decode_to_vec()
      .map_err(|_| anyhow!("Unable to decode data url."))?;
    Ok(RawDataUrl {
      mime_type: url.mime_type().to_string(),
      bytes,
    })
  }

  pub fn charset(&self) -> Option<&str> {
    get_mime_type_charset(&self.mime_type)
  }

  pub fn media_type(&self) -> MediaType {
    let mut content_types = self.mime_type.split(';');
    let Some(content_type) = content_types.next() else {
      return MediaType::Unknown;
    };
    MediaType::from_content_type(
      // this data url will be ignored when resolving the MediaType
      // as in this rare case the MediaType is determined solely based
      // on the provided content type
      &ModuleSpecifier::parse("data:image/png;base64,").unwrap(),
      content_type,
    )
  }

  pub fn decode(self) -> Result<String, std::io::Error> {
    let charset = get_mime_type_charset(&self.mime_type).unwrap_or("utf-8");
    decode_owned_source_with_charset(self.bytes, charset)
  }

  pub fn into_bytes_and_headers(self) -> (Vec<u8>, HashMap<String, String>) {
    let headers = HashMap::from([("content-type".to_string(), self.mime_type)]);
    (self.bytes, headers)
  }
}

fn get_mime_type_charset(mime_type: &str) -> Option<&str> {
  mime_type
    .split(';')
    .skip(1)
    .map(str::trim)
    .find_map(|s| s.strip_prefix("charset="))
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
        content: Arc::from(content.as_ref().to_string().into_bytes()),
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
    &mut self,
    specifier: &ModuleSpecifier,
    _options: LoadOptions,
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

/// Resolve a media type and optionally the charset from a module specifier and
/// the value of a content type header.
pub fn resolve_media_type_and_charset_from_headers<'a>(
  specifier: &ModuleSpecifier,
  maybe_headers: Option<&'a HashMap<String, String>>,
) -> (MediaType, Option<&'a str>) {
  resolve_media_type_and_charset_from_content_type(
    specifier,
    maybe_headers.and_then(|h| h.get("content-type")),
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

/// Decodes the source bytes into a string handling any encoding rules
/// where the bytes may be from a remote module, file module, or other.
pub fn decode_owned_source(
  specifier: &ModuleSpecifier,
  bytes: Vec<u8>,
  maybe_charset: Option<&str>,
) -> Result<String, std::io::Error> {
  let charset = maybe_charset.unwrap_or_else(|| {
    if specifier.scheme() == "file" {
      text_encoding::detect_charset(&bytes)
    } else {
      "utf-8"
    }
  });
  decode_owned_source_with_charset(bytes, charset)
}

/// Decodes the source bytes into a string handling any encoding rules
/// where the source is a `file:` specifier.
pub fn decode_owned_file_source(
  bytes: Vec<u8>,
) -> Result<String, std::io::Error> {
  let charset = text_encoding::detect_charset(&bytes);
  decode_owned_source_with_charset(bytes, charset)
}

fn decode_owned_source_with_charset(
  bytes: Vec<u8>,
  charset: &str,
) -> Result<String, std::io::Error> {
  match text_encoding::convert_to_utf8(&bytes, charset)? {
    Cow::Borrowed(text) => {
      if text.starts_with(text_encoding::BOM_CHAR) {
        Ok(text[text_encoding::BOM_CHAR.len_utf8()..].to_string())
      } else {
        Ok(
          // SAFETY: we know it's a valid utf-8 string at this point
          unsafe { String::from_utf8_unchecked(bytes) },
        )
      }
    }
    Cow::Owned(mut text) => {
      text_encoding::strip_bom_mut(&mut text);
      Ok(text)
    }
  }
}

/// Decodes the source bytes into a string handling any encoding rules
/// for local vs remote files and dealing with the charset.
pub fn decode_source(
  specifier: &ModuleSpecifier,
  bytes: Arc<[u8]>,
  maybe_charset: Option<&str>,
) -> Result<Arc<str>, std::io::Error> {
  let charset = maybe_charset.unwrap_or_else(|| {
    if specifier.scheme() == "file" {
      text_encoding::detect_charset(bytes.as_ref())
    } else {
      "utf-8"
    }
  });
  decode_with_charset(bytes, charset)
}

fn decode_with_charset(
  bytes: Arc<[u8]>,
  charset: &str,
) -> Result<Arc<str>, std::io::Error> {
  let text = match text_encoding::convert_to_utf8(bytes.as_ref(), charset)? {
    Cow::Borrowed(text) => {
      if text.starts_with(text_encoding::BOM_CHAR) {
        text[text_encoding::BOM_CHAR.len_utf8()..].to_string()
      } else {
        return Ok(
          // SAFETY: we know it's a valid utf-8 string at this point
          unsafe {
            let raw_ptr = Arc::into_raw(bytes);
            Arc::from_raw(std::mem::transmute::<*const [u8], *const str>(
              raw_ptr,
            ))
          },
        );
      }
    }
    Cow::Owned(mut text) => {
      text_encoding::strip_bom_mut(&mut text);
      text
    }
  };
  let text: Arc<str> = Arc::from(text);
  Ok(text)
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

  macro_rules! file_url {
    ($path:expr) => {
      if cfg!(target_os = "windows") {
        concat!("file:///C:", $path)
      } else {
        concat!("file://", $path)
      }
    };
  }

  #[test]
  fn test_resolve_media_type_and_charset_from_content_type() {
    let fixtures = vec![
      // Extension only
      (file_url!("/foo/bar.ts"), None, MediaType::TypeScript, None),
      (file_url!("/foo/bar.tsx"), None, MediaType::Tsx, None),
      (file_url!("/foo/bar.d.cts"), None, MediaType::Dcts, None),
      (file_url!("/foo/bar.d.mts"), None, MediaType::Dmts, None),
      (file_url!("/foo/bar.d.ts"), None, MediaType::Dts, None),
      (file_url!("/foo/bar.js"), None, MediaType::JavaScript, None),
      (file_url!("/foo/bar.jsx"), None, MediaType::Jsx, None),
      (file_url!("/foo/bar.json"), None, MediaType::Json, None),
      (file_url!("/foo/bar.wasm"), None, MediaType::Wasm, None),
      (file_url!("/foo/bar.cjs"), None, MediaType::Cjs, None),
      (file_url!("/foo/bar.mjs"), None, MediaType::Mjs, None),
      (file_url!("/foo/bar.cts"), None, MediaType::Cts, None),
      (file_url!("/foo/bar.mts"), None, MediaType::Mts, None),
      (file_url!("/foo/bar"), None, MediaType::Unknown, None),
      // Media type no extension
      (
        "https://deno.land/x/mod",
        Some("application/typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("video/vnd.dlna.mpeg-tts".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("video/mp2t".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/x-typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/ecmascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/ecmascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/x-javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/node".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/jsx".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/tsx".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/json".to_string()),
        MediaType::Json,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/json; charset=utf-8".to_string()),
        MediaType::Json,
        Some("utf-8".to_string()),
      ),
      // Extension with media type
      (
        "https://deno.land/x/mod.ts",
        Some("text/plain".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod.ts",
        Some("foo/bar".to_string()),
        MediaType::Unknown,
        None,
      ),
      (
        "https://deno.land/x/mod.tsx",
        Some("application/typescript".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod.tsx",
        Some("application/javascript".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod.jsx",
        Some("application/javascript".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod.jsx",
        Some("application/x-typescript".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("application/javascript".to_string()),
        MediaType::Dts,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("text/plain".to_string()),
        MediaType::Dts,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("application/x-typescript".to_string()),
        MediaType::Dts,
        None,
      ),
    ];

    for (specifier, maybe_content_type, media_type, maybe_charset) in fixtures {
      let specifier = ModuleSpecifier::parse(specifier).unwrap();
      assert_eq!(
        resolve_media_type_and_charset_from_content_type(
          &specifier,
          maybe_content_type.as_ref()
        ),
        (media_type, maybe_charset.as_deref())
      );
    }
  }

  #[test]
  fn test_parse_valid_data_url() {
    let valid_data_url = "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==";
    let specifier = ModuleSpecifier::parse(valid_data_url).unwrap();
    let raw_data_url = RawDataUrl::parse(&specifier).unwrap();
    assert_eq!(raw_data_url.mime_type, "text/plain");
    assert_eq!(raw_data_url.bytes, b"Hello, World!");
  }

  #[test]
  fn test_charset_with_valid_mime_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=utf-8".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.charset(), Some("utf-8"));
  }

  #[test]
  fn test_charset_with_no_charset_in_mime_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.charset(), None);
  }

  #[test]
  fn test_media_type_with_known_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "application/javascript;charset=utf-8".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.media_type(), MediaType::JavaScript);
  }

  #[test]
  fn test_media_type_with_unknown_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "unknown/unknown".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.media_type(), MediaType::Unknown);
  }

  #[test]
  fn test_decode_with_valid_charset() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=utf-8".to_string(),
      bytes: "Hello, World!".as_bytes().to_vec(),
    };
    assert_eq!(raw_data_url.decode().unwrap(), "Hello, World!");
  }

  #[test]
  fn test_decode_with_invalid_charset() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=invalid-charset".to_string(),
      bytes: vec![],
    };
    assert!(raw_data_url.decode().is_err());
  }

  #[test]
  fn test_into_bytes_and_headers() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=utf-8".to_string(),
      bytes: "Hello, World!".as_bytes().to_vec(),
    };
    let (bytes, headers) = raw_data_url.into_bytes_and_headers();
    assert_eq!(bytes, "Hello, World!".as_bytes());
    assert_eq!(
      headers.get("content-type").unwrap(),
      "text/plain; charset=utf-8"
    );
  }

  #[test]
  fn test_decode_owned_with_bom() {
    let text = decode_owned_file_source(
      format!("{}{}", text_encoding::BOM_CHAR, "Hello").into_bytes(),
    )
    .unwrap();
    assert_eq!(text, "Hello");
  }

  #[test]
  fn test_decode_with_charset_with_bom() {
    let bytes = format!("{}{}", text_encoding::BOM_CHAR, "Hello").into_bytes();
    let charset = "utf-8";
    let text = decode_with_charset(Arc::from(bytes), charset).unwrap();
    assert_eq!(text.as_ref(), "Hello");
  }
}
