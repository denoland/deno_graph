// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

// remove this after https://github.com/rustwasm/wasm-bindgen/issues/2774 is released
#![allow(clippy::unused_unit)]
#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]

use std::collections::HashMap;

use deno_graph::resolve_import;
use deno_graph::source::load_data_url;
use deno_graph::source::CacheInfo;
use deno_graph::source::CacheSetting;
use deno_graph::source::LoadFuture;
use deno_graph::source::Loader;
use deno_graph::source::NullFileSystem;
use deno_graph::source::ResolutionMode;
use deno_graph::source::ResolveError;
use deno_graph::source::Resolver;
use deno_graph::source::DEFAULT_JSX_IMPORT_SOURCE_MODULE;
use deno_graph::BuildOptions;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use deno_graph::Range;
use deno_graph::ReferrerImports;
use deno_graph::SpecifierError;

use anyhow::anyhow;
use futures::future;
use serde::Deserialize;
use serde::Serialize;
use wasm_bindgen::prelude::*;

pub struct JsLoader {
  load: js_sys::Function,
  maybe_cache_info: Option<js_sys::Function>,
}

impl JsLoader {
  pub fn new(
    load: js_sys::Function,
    maybe_cache_info: Option<js_sys::Function>,
  ) -> Self {
    Self {
      load,
      maybe_cache_info,
    }
  }
}

impl Loader for JsLoader {
  fn get_cache_info(&self, specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    if let Some(cache_info_fn) = &self.maybe_cache_info {
      let this = JsValue::null();
      let arg0 = JsValue::from(specifier.to_string());
      let value = cache_info_fn.call1(&this, &arg0).ok()?;
      let cache_info: CacheInfo = serde_wasm_bindgen::from_value(value).ok()?;
      Some(cache_info)
    } else {
      None
    }
  }

  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
    cache_setting: CacheSetting,
  ) -> LoadFuture {
    if specifier.scheme() == "data" {
      Box::pin(future::ready(load_data_url(specifier)))
    } else {
      let specifier = specifier.clone();
      let context = JsValue::null();
      let arg1 = JsValue::from(specifier.to_string());
      let arg2 = JsValue::from(is_dynamic);
      let arg3 = JsValue::from(cache_setting.as_js_str());
      let result = self.load.call3(&context, &arg1, &arg2, &arg3);
      let f = async move {
        let response = match result {
          Ok(result) => {
            wasm_bindgen_futures::JsFuture::from(js_sys::Promise::resolve(
              &result,
            ))
            .await
          }
          Err(err) => Err(err),
        };
        response
          .map(|value| serde_wasm_bindgen::from_value(value).unwrap())
          .map_err(|_| anyhow!("load rejected or errored"))
      };
      Box::pin(f)
    }
  }
}

#[derive(Debug)]
pub struct JsResolver {
  maybe_default_jsx_import_source: Option<String>,
  maybe_jsx_import_source_module: Option<String>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_resolve_types: Option<js_sys::Function>,
}

impl JsResolver {
  pub fn new(
    maybe_default_jsx_import_source: Option<String>,
    maybe_jsx_import_source_module: Option<String>,
    maybe_resolve: Option<js_sys::Function>,
    maybe_resolve_types: Option<js_sys::Function>,
  ) -> Self {
    Self {
      maybe_default_jsx_import_source,
      maybe_jsx_import_source_module,
      maybe_resolve,
      maybe_resolve_types,
    }
  }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
struct JsResolveTypesResponse {
  types: ModuleSpecifier,
  source: Option<Range>,
}

impl Resolver for JsResolver {
  fn default_jsx_import_source(&self) -> Option<String> {
    self.maybe_default_jsx_import_source.clone()
  }

  fn jsx_import_source_module(&self) -> &str {
    self
      .maybe_jsx_import_source_module
      .as_deref()
      .unwrap_or(DEFAULT_JSX_IMPORT_SOURCE_MODULE)
  }

  fn resolve(
    &self,
    specifier: &str,
    referrer_range: &Range,
    _mode: ResolutionMode,
  ) -> Result<ModuleSpecifier, ResolveError> {
    if let Some(resolve) = &self.maybe_resolve {
      let this = JsValue::null();
      let arg1 = JsValue::from(specifier);
      let arg2 = JsValue::from(referrer_range.specifier.to_string());
      let value = match resolve.call2(&this, &arg1, &arg2) {
        Ok(value) => value,
        Err(_) => return Err(anyhow!("JavaScript resolve threw.").into()),
      };
      let value: String = match serde_wasm_bindgen::from_value(value) {
        Ok(value) => value,
        Err(err) => return Err(anyhow!("{}", err.to_string()).into()),
      };
      ModuleSpecifier::parse(&value)
        .map_err(|err| ResolveError::Specifier(SpecifierError::InvalidUrl(err)))
    } else {
      resolve_import(specifier, &referrer_range.specifier)
        .map_err(|err| err.into())
    }
  }

  fn resolve_types(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<(ModuleSpecifier, Option<Range>)>, ResolveError> {
    if let Some(resolve_types) = &self.maybe_resolve_types {
      let this = JsValue::null();
      let arg1 = JsValue::from(specifier.to_string());
      let value = resolve_types
        .call1(&this, &arg1)
        .map_err(|_| anyhow!("JavaScript resolveTypes() function threw."))?;
      let result: Option<JsResolveTypesResponse> =
        serde_wasm_bindgen::from_value(value)
          .map_err(|err| anyhow!("{}", err.to_string()))?;
      Ok(result.map(|v| (v.types, v.source)))
    } else {
      Ok(None)
    }
  }
}

#[wasm_bindgen(js_name = createGraph)]
#[allow(clippy::too_many_arguments)]
pub async fn js_create_graph(
  roots: JsValue,
  load: js_sys::Function,
  maybe_default_jsx_import_source: Option<String>,
  maybe_jsx_import_source_module: Option<String>,
  maybe_cache_info: Option<js_sys::Function>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_resolve_types: Option<js_sys::Function>,
  maybe_graph_kind: Option<String>,
  maybe_imports: JsValue,
) -> Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  let roots_vec: Vec<String> = serde_wasm_bindgen::from_value(roots)
    .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let maybe_imports_map: Option<HashMap<String, Vec<String>>> =
    serde_wasm_bindgen::from_value(maybe_imports)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let mut loader = JsLoader::new(load, maybe_cache_info);
  let maybe_resolver = if maybe_default_jsx_import_source.is_some()
    || maybe_jsx_import_source_module.is_some()
    || maybe_resolve.is_some()
    || maybe_resolve_types.is_some()
  {
    Some(JsResolver::new(
      maybe_default_jsx_import_source,
      maybe_jsx_import_source_module,
      maybe_resolve,
      maybe_resolve_types,
    ))
  } else {
    None
  };
  let mut roots = Vec::with_capacity(roots_vec.len());
  for root in roots_vec.into_iter() {
    let root = ModuleSpecifier::parse(&root)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
    roots.push(root);
  }
  let imports = if let Some(imports_map) = maybe_imports_map {
    let mut imports = Vec::new();
    for (referrer_str, specifier_vec) in imports_map.into_iter() {
      let referrer = ModuleSpecifier::parse(&referrer_str)
        .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      imports.push(ReferrerImports {
        referrer,
        imports: specifier_vec,
      });
    }
    imports
  } else {
    Vec::new()
  };

  let graph_kind = match maybe_graph_kind.as_deref() {
    Some("typesOnly") => GraphKind::TypesOnly,
    Some("codeOnly") => GraphKind::CodeOnly,
    _ => GraphKind::All,
  };
  let mut graph = ModuleGraph::new(graph_kind);
  graph
    .build(
      roots,
      &mut loader,
      BuildOptions {
        is_dynamic: false,
        resolver: maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        file_system: Some(&NullFileSystem),
        npm_resolver: None,
        module_analyzer: None,
        module_parser: None,
        imports,
        reporter: None,
        workspace_fast_check: false,
        workspace_members: Vec::new(),
        fast_check_dts: false,
      },
    )
    .await;
  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
  Ok(graph.serialize(&serializer).unwrap())
}

#[allow(clippy::too_many_arguments)]
#[wasm_bindgen(js_name = parseModule)]
pub fn js_parse_module(
  specifier: String,
  maybe_headers: JsValue,
  maybe_default_jsx_import_source: Option<String>,
  maybe_jsx_import_source_module: Option<String>,
  content: Vec<u8>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_resolve_types: Option<js_sys::Function>,
) -> Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  let maybe_headers: Option<HashMap<String, String>> =
    serde_wasm_bindgen::from_value(maybe_headers)
      .map_err(|err| js_sys::Error::new(&err.to_string()))?;
  let specifier = ModuleSpecifier::parse(&specifier)
    .map_err(|err| js_sys::Error::new(&err.to_string()))?;
  let maybe_resolver = if maybe_default_jsx_import_source.is_some()
    || maybe_jsx_import_source_module.is_some()
    || maybe_resolve.is_some()
    || maybe_resolve_types.is_some()
  {
    Some(JsResolver::new(
      maybe_default_jsx_import_source,
      maybe_jsx_import_source_module,
      maybe_resolve,
      maybe_resolve_types,
    ))
  } else {
    None
  };
  match deno_graph::parse_module(deno_graph::ParseModuleOptions {
    graph_kind: GraphKind::All,
    specifier: &specifier,
    maybe_headers: maybe_headers.as_ref(),
    content: content.into(),
    file_system: &NullFileSystem,
    maybe_resolver: maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
    maybe_module_analyzer: None,
    maybe_npm_resolver: None,
  }) {
    Ok(module) => {
      let serializer =
        serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
      Ok(module.serialize(&serializer).unwrap())
    }
    Err(err) => Err(js_sys::Error::new(&err.to_string()).into()),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use deno_graph::Position;
  use serde_json::from_value;
  use serde_json::json;

  #[test]
  fn test_deserialize_types_response() {
    let actual: Option<JsResolveTypesResponse> = from_value(json!({
      "types": "https://deno.land/x/mod.d.ts",
      "source": {
        "specifier": "file:///package.json"
      }
    }))
    .unwrap();
    assert_eq!(
      actual,
      Some(JsResolveTypesResponse {
        types: ModuleSpecifier::parse("https://deno.land/x/mod.d.ts").unwrap(),
        source: Some(Range {
          specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
          start: Position::zeroed(),
          end: Position::zeroed(),
        })
      })
    );
    let actual: Option<JsResolveTypesResponse> = from_value(json!({
      "types": "https://deno.land/x/mod.d.ts",
    }))
    .unwrap();
    assert_eq!(
      actual,
      Some(JsResolveTypesResponse {
        types: ModuleSpecifier::parse("https://deno.land/x/mod.d.ts").unwrap(),
        source: None
      })
    );
  }
}
