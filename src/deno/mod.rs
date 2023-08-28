// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;

use crate::ModuleInfo;

#[derive(Deserialize, Clone)]
pub struct DenoPackageInfo {
  pub versions: HashMap<Version, DenoPackageInfoVersion>,
}

#[derive(Deserialize, Clone)]
pub struct DenoPackageInfoVersion {
  pub main: Option<String>,
}

#[derive(Deserialize, Clone)]
pub struct DenoPackageVersionInfo {
  pub main: Option<String>,
  #[serde(rename = "moduleGraph1")]
  pub module_graph: Option<serde_json::Value>,
}

impl DenoPackageVersionInfo {
  pub fn module_info(&self, specifier: &str) -> Option<ModuleInfo> {
    let module_graph = self.module_graph.as_ref()?.as_object()?;
    let module_info = module_graph.get(specifier)?;
    serde_json::from_value(module_info.clone()).ok()
  }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct DenoSpecifierSnapshot {
  #[serde(flatten)]
  package_reqs: HashMap<PackageReq, PackageNv>,
  #[serde(skip_serializing)]
  packages_by_name: HashMap<String, Vec<PackageNv>>,
}

impl DenoSpecifierSnapshot {
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
    self
      .package_reqs
      .insert(package_req, nv);
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
    if version_req.matches(&version) {
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
