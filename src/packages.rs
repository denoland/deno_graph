// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;

use deno_semver::jsr::JsrDepPackageReq;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;

use crate::ModuleInfo;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct JsrPackageInfo {
  pub versions: HashMap<Version, JsrPackageInfoVersion>,
}

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageInfoVersion {
  #[serde(default, skip_serializing_if = "is_false")]
  pub yanked: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageVersionManifestEntry {
  pub checksum: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageVersionInfo {
  // ensure the fields on here are resilient to change
  #[serde(default)]
  pub exports: serde_json::Value,
  #[serde(rename = "moduleGraph1")]
  pub module_graph: Option<serde_json::Value>,
  pub manifest: HashMap<String, JsrPackageVersionManifestEntry>,
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

#[derive(Debug, Clone)]
struct PackageNvInfo {
  manifest_checksum: String,
  /// Collection of exports used.
  exports: BTreeMap<String, String>,
  found_dependencies: HashSet<JsrDepPackageReq>,
}

#[derive(Debug, Clone)]
pub struct PackageManifestIntegrityError {
  pub nv: PackageNv,
  pub actual: String,
  pub expected: String,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PackageSpecifiers {
  #[serde(flatten)]
  package_reqs: BTreeMap<PackageReq, PackageNv>,
  #[serde(skip_serializing)]
  packages_by_name: HashMap<String, Vec<PackageNv>>,
  #[serde(skip_serializing)]
  packages: BTreeMap<PackageNv, PackageNvInfo>,
  /// Cache for packages that have a referrer outside JSR.
  #[serde(skip_serializing)]
  top_level_packages: BTreeSet<PackageNv>,
  #[serde(skip_serializing)]
  used_yanked_packages: BTreeSet<PackageNv>,
}

impl PackageSpecifiers {
  pub fn is_empty(&self) -> bool {
    self.package_reqs.is_empty()
  }

  pub fn add_nv(&mut self, package_req: PackageReq, nv: PackageNv) {
    let nvs = self
      .packages_by_name
      .entry(package_req.name.clone())
      .or_default();
    if !nvs.contains(&nv) {
      nvs.push(nv.clone());
    }
    self.package_reqs.insert(package_req, nv.clone());
  }

  pub(crate) fn ensure_package(
    &mut self,
    nv: PackageNv,
    manifest_checksum: String,
  ) {
    self.packages.entry(nv).or_insert_with(|| PackageNvInfo {
      manifest_checksum,
      exports: Default::default(),
      found_dependencies: Default::default(),
    });
  }

  pub(crate) fn get_manifest_checksum(&self, nv: &PackageNv) -> Option<&str> {
    self.packages.get(nv).map(|p| p.manifest_checksum.as_str())
  }

  pub fn add_manifest_checksum(
    &mut self,
    nv: PackageNv,
    checksum: String,
  ) -> Result<(), Box<PackageManifestIntegrityError>> {
    let package = self.packages.get(&nv);
    if let Some(package) = package {
      if package.manifest_checksum != checksum {
        Err(Box::new(PackageManifestIntegrityError {
          nv,
          actual: checksum,
          expected: package.manifest_checksum.clone(),
        }))
      } else {
        Ok(())
      }
    } else {
      self.packages.insert(
        nv,
        PackageNvInfo {
          manifest_checksum: checksum,
          exports: Default::default(),
          found_dependencies: Default::default(),
        },
      );
      Ok(())
    }
  }

  /// Gets the dependencies (package constraints) of JSR packages found in the graph.
  pub fn packages_with_checksum_and_deps(
    &self,
  ) -> impl Iterator<
    Item = (&PackageNv, &String, impl Iterator<Item = &JsrDepPackageReq>),
  > {
    self.packages.iter().map(|(nv, info)| {
      let deps = info.found_dependencies.iter();
      (nv, &info.manifest_checksum, deps)
    })
  }

  pub(crate) fn add_dependency(
    &mut self,
    nv: &PackageNv,
    dep: JsrDepPackageReq,
  ) {
    self
      .packages
      .get_mut(nv)
      .unwrap()
      .found_dependencies
      .insert(dep);
  }

  pub(crate) fn add_export(
    &mut self,
    nv: &PackageNv,
    export: (String, String),
  ) {
    self
      .packages
      .get_mut(nv)
      .unwrap()
      .exports
      .insert(export.0, export.1);
  }

  pub(crate) fn add_top_level_package(&mut self, nv: PackageNv) {
    self.top_level_packages.insert(nv);
  }

  pub(crate) fn top_level_packages(&self) -> &BTreeSet<PackageNv> {
    &self.top_level_packages
  }

  pub(crate) fn add_used_yanked_package(&mut self, nv: PackageNv) {
    self.used_yanked_packages.insert(nv);
  }

  pub fn used_yanked_packages(&mut self) -> impl Iterator<Item = &PackageNv> {
    self.used_yanked_packages.iter()
  }

  pub fn package_exports(
    &self,
    nv: &PackageNv,
  ) -> Option<&BTreeMap<String, String>> {
    self.packages.get(nv).map(|p| &p.exports)
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
