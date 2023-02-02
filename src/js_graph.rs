// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::graph;
use crate::graph::Range;
use crate::module_specifier::resolve_import;
use crate::module_specifier::ModuleSpecifier;
use crate::source::load_data_url;
use crate::source::CacheInfo;
use crate::source::LoadFuture;
use crate::source::Loader;
use crate::source::Resolver;
use crate::source::DEFAULT_JSX_IMPORT_SOURCE_MODULE;

use anyhow::anyhow;
use anyhow::Error;
use anyhow::Result;
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
  ) -> LoadFuture {
    if specifier.scheme() == "data" {
      Box::pin(future::ready(load_data_url(specifier)))
    } else {
      let specifier = specifier.clone();
      let context = JsValue::null();
      let arg1 = JsValue::from(specifier.to_string());
      let arg2 = JsValue::from(is_dynamic);
      let result = self.load.call2(&context, &arg1, &arg2);
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
    referrer: &ModuleSpecifier,
  ) -> Result<ModuleSpecifier, Error> {
    if let Some(resolve) = &self.maybe_resolve {
      let this = JsValue::null();
      let arg1 = JsValue::from(specifier);
      let arg2 = JsValue::from(referrer.to_string());
      let value = match resolve.call2(&this, &arg1, &arg2) {
        Ok(value) => value,
        Err(_) => return Err(anyhow!("JavaScript resolve threw.")),
      };
      let value: String = match serde_wasm_bindgen::from_value(value) {
        Ok(value) => value,
        Err(err) => return Err(anyhow!("{}", err.to_string())),
      };
      Ok(ModuleSpecifier::parse(&value)?)
    } else {
      resolve_import(specifier, referrer).map_err(|err| err.into())
    }
  }

  fn resolve_types(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<(ModuleSpecifier, Option<Range>)>> {
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

#[wasm_bindgen]
pub struct ModuleGraph(pub(crate) graph::ModuleGraph);

#[wasm_bindgen]
impl ModuleGraph {
  #[wasm_bindgen(getter)]
  pub fn roots(&self) -> js_sys::Array {
    self
      .0
      .roots
      .iter()
      .map(|s| JsValue::from(s.to_string()))
      .collect()
  }

  #[wasm_bindgen]
  pub fn get(&self, specifier: String) -> Result<Option<Module>, JsValue> {
    let specifier = ModuleSpecifier::parse(&specifier)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
    self
      .0
      .try_get(&specifier)
      .map(|mm| mm.map(|m| Module(m.clone())))
      .map_err(|err| js_sys::Error::new(&err.to_string()).into())
  }

  #[wasm_bindgen(getter)]
  pub fn modules(&self) -> js_sys::Array {
    self
      .0
      .module_slots
      .values()
      .filter_map(|ms| {
        if let graph::ModuleSlot::Module(m) = ms {
          Some(Module(m.clone()))
        } else {
          None
        }
      })
      .map(JsValue::from)
      .collect()
  }

  #[wasm_bindgen]
  pub fn resolve(&self, specifier: String) -> String {
    let specifier = ModuleSpecifier::parse(&specifier).unwrap();
    self.0.resolve(&specifier).to_string()
  }

  #[wasm_bindgen(js_name = resolveDependency)]
  pub fn resolve_dependency(
    &self,
    specifier: String,
    referrer: String,
    prefer_types: bool,
  ) -> Option<String> {
    let referrer = ModuleSpecifier::parse(&referrer).unwrap();
    self
      .0
      .resolve_dependency(&specifier, &referrer, prefer_types)
      .map(|s| s.to_string())
  }

  #[wasm_bindgen(js_name = toJSON)]
  pub fn to_json(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.serialize(&serializer).unwrap()
  }
}

#[wasm_bindgen]
pub struct Module(pub(crate) graph::Module);

#[wasm_bindgen]
impl Module {
  #[wasm_bindgen(getter, js_name = cacheInfo)]
  pub fn cache_info(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.maybe_cache_info.serialize(&serializer).unwrap()
  }

  #[wasm_bindgen(getter)]
  pub fn dependencies(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.dependencies.serialize(&serializer).unwrap()
  }

  #[wasm_bindgen(getter)]
  pub fn kind(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.kind.serialize(&serializer).unwrap()
  }

  #[wasm_bindgen(getter, js_name = mediaType)]
  pub fn media_type(&self) -> String {
    self.0.media_type.to_string()
  }

  #[wasm_bindgen(getter)]
  pub fn size(&self) -> usize {
    self.0.size()
  }

  #[wasm_bindgen(getter)]
  pub fn source(&self) -> String {
    self
      .0
      .maybe_source
      .as_ref()
      .map(|s| s.to_string())
      .unwrap_or_else(|| "".to_string())
  }

  #[wasm_bindgen(getter)]
  pub fn specifier(&self) -> String {
    self.0.specifier.to_string()
  }

  #[wasm_bindgen(getter, js_name = typesDependency)]
  pub fn maybe_types_dependency(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    graph::serialize_type_dependency(
      &self.0.maybe_types_dependency,
      &serializer,
    )
    .unwrap()
  }

  #[wasm_bindgen(js_name = toJSON)]
  pub fn to_json(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.serialize(&serializer).unwrap()
  }
}

#[cfg(test)]
mod tests {
  use crate::Position;

  use super::*;
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
