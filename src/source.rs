// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::graph::Range;
use crate::module_specifier::resolve_import;
use crate::npm::NpmPackageId;
use crate::npm::NpmPackageIdReference;
use crate::npm::NpmPackageReq;
use crate::text_encoding::strip_bom_mut;

use anyhow::anyhow;
use anyhow::Error;
use anyhow::Result;
use data_url::DataUrl;
use deno_ast::ModuleSpecifier;
use futures::future;
use futures::future::BoxFuture;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;

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

pub type LoadResult = Result<Option<LoadResponse>>;
pub type LoadFuture = BoxFuture<'static, LoadResult>;

/// A trait which allows asynchronous loading of source files into a module
/// graph in a thread safe way as well as a way to provide additional meta data
/// about any cached resources.
pub trait Loader {
  /// An optional method which returns cache info for a module specifier.
  fn get_cache_info(&self, _specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    None
  }

  /// A method that given a specifier that asynchronously returns a response
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
  ) -> LoadFuture;
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
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> BoxFuture<'static, Result<ModuleSpecifier, Error>> {
    Box::pin(futures::future::ready(
      resolve_import(specifier, referrer).map_err(|err| err.into()),
    ))
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
  ) -> BoxFuture<'static, Result<Option<(ModuleSpecifier, Option<Range>)>>> {
    Box::pin(futures::future::ready(Ok(None)))
  }
}

pub fn load_data_url(
  specifier: &ModuleSpecifier,
) -> Result<Option<LoadResponse>> {
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
          let result = match r {
            Source::Module {
              specifier,
              maybe_headers,
              content,
            } => Ok(LoadResponse::Module {
              specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
              maybe_headers: maybe_headers.map(|h| {
                h.into_iter()
                  .map(|(k, v)| {
                    (k.as_ref().to_string(), v.as_ref().to_string())
                  })
                  .collect()
              }),
              content: content.as_ref().into(),
            }),
            Source::External(specifier) => Ok(LoadResponse::External {
              specifier: ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
            }),
            Source::Err(error) => Err(error),
          };
          (specifier, result)
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
}

impl Loader for MemoryLoader {
  fn get_cache_info(&self, specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    self.cache_info.get(specifier).cloned()
  }

  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    _is_dynamic: bool,
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
      referrer: &ModuleSpecifier,
    ) -> BoxFuture<'static, Result<ModuleSpecifier, Error>> {
      Box::pin(futures::future::ready(
        if let Some(resolved_specifier) =
          self.map.get(referrer).and_then(|map| map.get(specifier))
        {
          Ok(resolved_specifier.clone())
        } else {
          resolve_import(specifier, referrer).map_err(|err| err.into())
        },
      ))
    }

    fn resolve_types(
      &self,
      specifier: &ModuleSpecifier,
    ) -> BoxFuture<'static, Result<Option<(ModuleSpecifier, Option<Range>)>>>
    {
      Box::pin(futures::future::ready(Ok(
        self.types.get(specifier).cloned(),
      )))
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
