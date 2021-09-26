// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::checksum;
use crate::colors::strip_ansi_codes;
use crate::graph;
use crate::module_specifier::ModuleSpecifier;
use crate::source::CacheInfo;
use crate::source::LoadFuture;
use crate::source::Loader;
use crate::source::Locker;
use crate::source::Resolver;

use anyhow::anyhow;
use anyhow::Result;
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
      let cache_info: CacheInfo = value.into_serde().ok()?;
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
    let specifier = specifier.clone();
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier.to_string());
    let arg1 = JsValue::from(is_dynamic);
    let result = self.load.call2(&this, &arg0, &arg1);
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
      (
        specifier,
        response
          .map(|value| value.into_serde().unwrap())
          .map_err(|_| anyhow!("load rejected or errored")),
      )
    };
    Box::pin(f)
  }
}

#[derive(Debug)]
pub struct JsLocker {
  maybe_check: Option<js_sys::Function>,
  maybe_get_checksum: Option<js_sys::Function>,
}

impl JsLocker {
  pub fn new(
    maybe_check: Option<js_sys::Function>,
    maybe_get_checksum: Option<js_sys::Function>,
  ) -> Self {
    Self {
      maybe_check,
      maybe_get_checksum,
    }
  }
}

impl Locker for JsLocker {
  fn check_or_insert(
    &mut self,
    specifier: &ModuleSpecifier,
    source: &str,
  ) -> bool {
    if let Some(check) = &self.maybe_check {
      let this = JsValue::null();
      let arg0 = JsValue::from(specifier.to_string());
      let arg1 = JsValue::from(source);
      if let Ok(value) = check.call2(&this, &arg0, &arg1) {
        if let Ok(value) = value.into_serde::<bool>() {
          return value;
        }
      }
    }
    true
  }

  fn get_checksum(&self, content: &str) -> String {
    if let Some(get_checksum) = &self.maybe_get_checksum {
      let this = JsValue::null();
      let arg0 = JsValue::from(content);
      if let Ok(value) = get_checksum.call1(&this, &arg0) {
        if let Ok(value) = value.into_serde::<String>() {
          return value;
        }
      }
    }
    checksum::gen(&[content.as_bytes()])
  }
}

#[derive(Debug)]
pub struct JsResolver {
  resolve: js_sys::Function,
}

impl JsResolver {
  pub fn new(resolve: js_sys::Function) -> Self {
    Self { resolve }
  }
}

impl Resolver for JsResolver {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Result<ModuleSpecifier> {
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier);
    let arg1 = JsValue::from(referrer.to_string());
    let value = self
      .resolve
      .call2(&this, &arg0, &arg1)
      .map_err(|_| anyhow!("JavaScript resolve() function threw."))?;
    let value: String = value.into_serde()?;
    let resolved_specifier = ModuleSpecifier::parse(&value)?;
    Ok(resolved_specifier)
  }
}

#[wasm_bindgen(module = "/src/deno_apis.js")]
extern "C" {
  fn get_no_color() -> bool;
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

  #[wasm_bindgen]
  pub fn lock(&self) -> Result<(), JsValue> {
    self
      .0
      .lock()
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

  #[wasm_bindgen(js_name = toString)]
  pub fn to_string(&self, maybe_no_color: Option<bool>) -> String {
    let value = self.0.to_string();
    let no_color = maybe_no_color.unwrap_or_else(get_no_color);
    if no_color {
      strip_ansi_codes(value)
    } else {
      value
    }
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
  pub fn checksum(&self) -> Option<String> {
    self.0.maybe_checksum.clone()
  }

  #[wasm_bindgen(getter)]
  pub fn dependencies(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.dependencies.serialize(&serializer).unwrap()
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
    self.0.source.to_string()
  }

  #[wasm_bindgen(getter)]
  pub fn specifier(&self) -> String {
    self.0.specifier.to_string()
  }

  #[wasm_bindgen(getter, js_name = typesDependency)]
  pub fn maybe_types_dependency(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self
      .0
      .maybe_types_dependency
      .serialize(&serializer)
      .unwrap()
  }

  #[wasm_bindgen(js_name = toJSON)]
  pub fn to_json(&self) -> JsValue {
    let serializer =
      serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    self.0.serialize(&serializer).unwrap()
  }
}
