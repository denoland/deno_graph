// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::BTreeMap;
use std::collections::HashMap;

use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;

use crate::ModuleInfo;

#[derive(Serialize, Deserialize, Clone)]
pub struct JsrPackageInfo {
  pub versions: HashMap<Version, JsrPackageInfoVersion>,
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageInfoVersion {
  // no fields yet
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageVersionInfo {
  // ensure the fields on here are resilient to change
  #[serde(default)]
  pub exports: serde_json::Value,
  #[serde(rename = "moduleGraph1")]
  pub module_graph: Option<serde_json::Value>,
}

impl JsrPackageVersionInfo {
  /// Resolves the provided export key.
  ///
  /// Note: This assumes the provided export name is normalized.
  pub fn export(&self, export_name: &str) -> Option<&str> {
    match &self.exports {
      serde_json::Value::String(value) => {
        if export_name == "." {
          Some(value.as_str())
        } else {
          None
        }
      }
      serde_json::Value::Object(map) => match map.get(export_name) {
        Some(serde_json::Value::String(value)) => Some(value.as_str()),
        _ => None,
      },
      _ => None,
    }
  }

  /// Gets the key and values of the exports map.
  pub fn exports(&self) -> Box<dyn Iterator<Item = (&str, &str)> + '_> {
    match &self.exports {
      serde_json::Value::String(value) => {
        Box::new(std::iter::once((".", value.as_str())))
      }
      serde_json::Value::Object(map) => {
        Box::new(map.iter().filter_map(|(key, value)| match value {
          serde_json::Value::String(value) => {
            Some((key.as_str(), value.as_str()))
          }
          _ => None,
        }))
      }
      _ => Box::new(std::iter::empty()),
    }
  }

  pub fn module_info(&self, specifier: &str) -> Option<ModuleInfo> {
    let module_graph = self.module_graph.as_ref()?.as_object()?;
    let module_info = module_graph.get(specifier)?;
    serde_json::from_value(module_info.clone()).ok()
  }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PackageSpecifiers {
  #[serde(flatten)]
  package_reqs: BTreeMap<PackageReq, PackageNv>,
  #[serde(skip_serializing)]
  packages_by_name: HashMap<String, Vec<PackageNv>>,
}

impl PackageSpecifiers {
  pub fn is_empty(&self) -> bool {
    self.package_reqs.is_empty()
  }

  pub fn add(&mut self, package_req: PackageReq, nv: PackageNv) {
    let nvs = self
      .packages_by_name
      .entry(package_req.name.clone())
      .or_default();
    if !nvs.contains(&nv) {
      nvs.push(nv.clone());
    }
    self.package_reqs.insert(package_req, nv);
  }

  pub fn versions_by_name(&self, name: &str) -> Option<&Vec<PackageNv>> {
    self.packages_by_name.get(name)
  }

  pub fn mappings(&self) -> impl Iterator<Item = (&PackageReq, &PackageNv)> {
    self.package_reqs.iter()
  }
}

pub fn resolve_version<'a>(
  version_req: &VersionReq,
  versions: impl Iterator<Item = &'a Version>,
) -> Option<&'a Version> {
  let mut maybe_best_version: Option<&Version> = None;
  for version in versions {
    if version_req.matches(version) {
      let is_best_version = maybe_best_version
        .as_ref()
        .map(|best_version| (*best_version).cmp(version).is_lt())
        .unwrap_or(true);
      if is_best_version {
        maybe_best_version = Some(version);
      }
    }
  }
  maybe_best_version
}
