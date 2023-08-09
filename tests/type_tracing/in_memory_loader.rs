// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Result;
use deno_ast::ModuleSpecifier;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;

type RemoteFileText = Arc<str>;
type RemoteFileHeaders = Option<HashMap<String, String>>;
type RemoteFileResult = Result<(RemoteFileText, RemoteFileHeaders), String>;

#[derive(Clone, Default)]
pub struct InMemoryLoader {
  modules: HashMap<ModuleSpecifier, RemoteFileResult>,
}

impl InMemoryLoader {
  pub fn add_file(
    &mut self,
    specifier: impl AsRef<str>,
    text: impl AsRef<str>,
  ) -> &mut Self {
    let specifier = specifier.as_ref();
    let specifier =
      if !specifier.starts_with("http") && !specifier.starts_with("file") {
        ModuleSpecifier::parse(&format!("file:///{}", specifier)).unwrap()
      } else {
        ModuleSpecifier::parse(specifier).unwrap()
      };
    self.modules.insert(
      ModuleSpecifier::parse(specifier.as_ref()).unwrap(),
      Ok((text.as_ref().into(), None)),
    );
    self
  }
}

impl Loader for InMemoryLoader {
  fn load_no_cache(
    &mut self,
    specifier: &ModuleSpecifier,
    _is_dynamic: bool,
  ) -> LoadFuture {
    let specifier = specifier.clone();
    let result = self.modules.get(&specifier).map(|result| match result {
      Ok(result) => Ok(LoadResponse::Module {
        specifier, // todo: test a re-direct
        content: result.0.clone(),
        maybe_headers: result.1.clone(),
      }),
      Err(err) => Err(err),
    });
    let result = match result {
      Some(Ok(result)) => Ok(Some(result)),
      Some(Err(err)) => Err(anyhow!("{}", err)),
      None => Ok(None),
    };
    Box::pin(futures::future::ready(result))
  }

  fn load_from_cache(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
  ) -> LoadFuture {
    self.load(specifier, is_dynamic)
  }
}
