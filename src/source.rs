// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

use anyhow::Result;
use futures::future::Future;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::pin::Pin;

/// Information that comes from an external source which can be optionally
/// included in the module graph.
#[derive(Debug, Deserialize, Serialize)]
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
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LoadResponse {
  /// The module specifier of the final module. This can differ from the
  /// requested specifier (e.g. if there was a redirect encountered when
  /// loading)
  pub specifier: ModuleSpecifier,
  /// If the module is a remote module, the headers should be returned as a
  /// hashmap of lower-cased string values.
  pub maybe_headers: Option<HashMap<String, String>>,
  /// The content of the remote module.
  pub content: String,
}

pub type LoadResult = (ModuleSpecifier, Result<Option<LoadResponse>>);
pub type LoadFuture = Pin<Box<dyn Future<Output = LoadResult> + 'static>>;

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

/// A trait which allows the module graph to check if a source is "valid" as
/// well as a way to get the checksum of a source for displaying when printing
/// a module graph.
pub trait Locker: fmt::Debug {
  fn check_or_insert(
    &mut self,
    specifier: &ModuleSpecifier,
    source: &str,
  ) -> bool;
  fn get_checksum(&self, content: &str) -> String;
}

/// A trait which allows the module graph to resolve specifiers is a dynamic
/// way, like with import maps.
pub trait Resolver: fmt::Debug {
  /// Given a string specifier and a referring module specifier, return a
  /// resolved module specifier.
  fn resolve(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Result<ModuleSpecifier>;
}

#[cfg(test)]
pub(crate) mod tests {
  use super::*;
  use crate::module_specifier::resolve_import;
  use anyhow::anyhow;
  use anyhow::Error;
  use futures::future;

  pub(crate) struct MockLoader {
    sources: HashMap<ModuleSpecifier, Result<LoadResponse, Error>>,
  }

  impl MockLoader {
    pub fn new<S: AsRef<str>>(
      sources: Vec<(S, Result<(S, Option<Vec<(S, S)>>, S), Error>)>,
    ) -> Self {
      Self {
        sources: sources
          .into_iter()
          .map(|(s, r)| {
            let specifier = ModuleSpecifier::parse(s.as_ref()).unwrap();
            let result = r.map(|(s, mh, c)| LoadResponse {
              specifier: ModuleSpecifier::parse(s.as_ref()).unwrap(),
              maybe_headers: mh.map(|h| {
                h.into_iter()
                  .map(|(k, v)| {
                    (k.as_ref().to_string(), v.as_ref().to_string())
                  })
                  .collect()
              }),
              content: c.as_ref().to_string(),
            });
            (specifier, result)
          })
          .collect(),
      }
    }
  }

  impl Loader for MockLoader {
    fn load(
      &mut self,
      specifier: &ModuleSpecifier,
      _is_dynamic: bool,
    ) -> LoadFuture {
      let response = match self.sources.get(specifier) {
        Some(Ok(response)) => Ok(Some(response.clone())),
        Some(Err(err)) => Err(anyhow!("{}", err)),
        _ => Ok(None),
      };
      Box::pin(future::ready((specifier.clone(), response)))
    }
  }

  #[derive(Debug)]
  pub(crate) struct MockResolver {
    map: HashMap<ModuleSpecifier, HashMap<String, ModuleSpecifier>>,
  }

  impl MockResolver {
    pub fn new<S: AsRef<str>>(map: Vec<(S, Vec<(S, S)>)>) -> Self {
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
      }
    }
  }

  impl Resolver for MockResolver {
    fn resolve(
      &self,
      specifier: &str,
      referrer: &ModuleSpecifier,
    ) -> Result<ModuleSpecifier> {
      if let Some(map) = self.map.get(referrer) {
        if let Some(resolved_specifier) = map.get(specifier) {
          return Ok(resolved_specifier.clone());
        }
      }
      resolve_import(specifier, referrer).map_err(|err| err.into())
    }
  }
}
