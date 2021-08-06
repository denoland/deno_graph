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
use wasm_bindgen::prelude::*;

pub(crate) struct JsLoader {
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
    let result = self.load.call2(&this, &arg0, &arg1).unwrap();
    let f = async move {
      let response =
        wasm_bindgen_futures::JsFuture::from(js_sys::Promise::resolve(&result))
          .await;
      (
        specifier,
        response
          .map(|value| value.into_serde().unwrap())
          .map_err(|_| anyhow!("promise rejected")),
      )
    };
    Box::pin(f)
  }
}

#[derive(Debug)]
pub(crate) struct JsLocker {
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
pub(crate) struct JsResolver {
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
  #[wasm_bindgen(js_name = toJSON)]
  pub fn to_json(&self) -> JsValue {
    JsValue::from_serde(&self.0).unwrap()
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
