// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

use anyhow::Result;
use futures::future::Future;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::pin::Pin;

/// Information that comes from an external source which can be optionally
/// included in the module graph.
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
#[derive(Debug, Clone)]
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

/// The return future of a load request, which the output should be a tuple
/// of the original requested module specifier and a result of the optional
/// load response. If the requested specifier cannot be found, the result should
/// be `None`. If some other error is encountered, the result should be an
/// error containing the error.
pub type LoadFuture = Pin<
  Box<
    (dyn Future<Output = (ModuleSpecifier, Result<Option<LoadResponse>>)>
       + 'static
       + Send),
  >,
>;

/// A trait which allows asynchronous loading of source files into a module
/// graph in a thread safe way as well as a way to provide additional meta data
/// about any cached resources.
pub trait Loader: Sync + Send {
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
}
