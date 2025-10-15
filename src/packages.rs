// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;

use deno_semver::jsr::JsrDepPackageReq;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::StackString;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;

use crate::analysis::module_graph_1_to_2;
use crate::analysis::ModuleInfo;
use crate::graph::JsrPackageReqNotFoundError;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct JsrPackageInfo {
  pub versions: HashMap<Version, JsrPackageInfoVersion>,
}

fn is_false(v: &bool) -> bool {
  !v
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct JsrPackageInfoVersion {
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub created_at: Option<chrono::DateTime<chrono::Utc>>,
  #[serde(default, skip_serializing_if = "is_false")]
  pub yanked: bool,
}

impl JsrPackageInfoVersion {
  pub fn matches_newest_dependency_date(
    &self,
    newest_dependency_date: Option<chrono::DateTime<chrono::Utc>>,
  ) -> bool {
    newest_dependency_date
      .and_then(|cutoff| {
        // assume versions not existing are really old
        self.created_at.map(|package_age| package_age < cutoff)
      })
      .unwrap_or(true)
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct JsrPackageVersionManifestEntry {
  pub checksum: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct JsrPackageVersionInfo {
  // ensure the fields on here are resilient to change
  #[serde(default)]
  pub exports: serde_json::Value,
  #[serde(rename = "moduleGraph1")]
  pub module_graph_1: Option<serde_json::Value>,
  #[serde(rename = "moduleGraph2")]
  pub module_graph_2: Option<serde_json::Value>,
  pub manifest: HashMap<String, JsrPackageVersionManifestEntry>,
  /// This is a property that deno_cache_dir sets when copying from
  /// the global to the local cache. If it's set, put this in the lockfile
  /// instead of computing the checksum from the file bytes. This is necessary
  /// because we store less data in the metadata files found in the vendor
  /// directory than in the global cache and also someone may modify the vendored
  /// files then regenerate the lockfile.
  #[serde(rename = "lockfileChecksum")]
  pub lockfile_checksum: Option<String>,
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
    if let Some(module_graph) = self.module_graph_2.as_ref() {
      let module_graph = module_graph.as_object()?;
      let module_info = module_graph.get(specifier)?;
      serde_json::from_value(module_info.clone()).ok()
    } else if let Some(module_graph) = self.module_graph_1.as_ref() {
      let module_graph = module_graph.as_object()?;
      let mut module_info = module_graph.get(specifier)?.clone();
      module_graph_1_to_2(&mut module_info);
      serde_json::from_value(module_info).ok()
    } else {
      None
    }
  }
}

#[derive(Debug, Clone)]
struct PackageNvInfo {
  /// Collection of exports used.
  exports: BTreeMap<String, String>,
  found_dependencies: HashSet<JsrDepPackageReq>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PackageSpecifiers {
  #[serde(flatten)]
  package_reqs: BTreeMap<PackageReq, PackageNv>,
  #[serde(skip_serializing)]
  packages_by_name: HashMap<StackString, Vec<PackageNv>>,
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

  /// The total number of JSR packages found in the graph.
  pub fn packages_len(&self) -> usize {
    self.packages.len()
  }

  /// The total number of dependencies of jsr packages found in the graph.
  pub fn package_deps_sum(&self) -> usize {
    self
      .packages
      .iter()
      .map(|p| p.1.found_dependencies.len())
      .sum()
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

  pub(crate) fn ensure_package(&mut self, nv: PackageNv) {
    self.packages.entry(nv).or_insert_with(|| PackageNvInfo {
      exports: Default::default(),
      found_dependencies: Default::default(),
    });
  }

  /// Gets the dependencies (package constraints) of JSR packages found in the graph.
  pub fn packages_with_deps(
    &self,
  ) -> impl Iterator<Item = (&PackageNv, impl Iterator<Item = &JsrDepPackageReq>)>
  {
    self.packages.iter().map(|(nv, info)| {
      let deps = info.found_dependencies.iter();
      (nv, deps)
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

  pub fn mappings(&self) -> &BTreeMap<PackageReq, PackageNv> {
    &self.package_reqs
  }
}

pub struct JsrVersionResolverResolvedVersion<'a> {
  pub is_yanked: bool,
  pub version: &'a Version,
}

#[derive(Debug, Default, Clone)]
pub struct JsrVersionResolver {
  /// Minimum age for a dependency to be installed.
  pub newest_dependency_date: Option<chrono::DateTime<chrono::Utc>>,
}

impl JsrVersionResolver {
  pub fn resolve_version<'a>(
    &self,
    package_req: &PackageReq,
    package_info: &'a JsrPackageInfo,
    existing_versions: impl Iterator<Item = &'a Version>,
  ) -> Result<JsrVersionResolverResolvedVersion<'a>, JsrPackageReqNotFoundError>
  {
    // 1. try to resolve with the list of existing versions
    if let ResolveVersionResult::Some(version) = resolve_version(
      ResolveVersionOptions {
        version_req: &package_req.version_req,
        // don't use this here because existing versions are ok to resolve to
        newest_dependency_date: None,
      },
      existing_versions.map(|v| (v, None)),
    ) {
      let is_yanked = package_info
        .versions
        .get(version)
        .map(|i| i.yanked)
        .unwrap_or(false);
      return Ok(JsrVersionResolverResolvedVersion { is_yanked, version });
    }

    // 2. attempt to resolve with the unyanked versions
    let mut any_had_higher_newest_dep_version = false;
    let unyanked_versions =
      package_info.versions.iter().filter_map(|(v, i)| {
        if !i.yanked {
          Some((v, Some(i)))
        } else {
          None
        }
      });
    match resolve_version(
      ResolveVersionOptions {
        version_req: &package_req.version_req,
        newest_dependency_date: self.newest_dependency_date,
      },
      unyanked_versions,
    ) {
      ResolveVersionResult::Some(version) => {
        return Ok(JsrVersionResolverResolvedVersion {
          is_yanked: false,
          version,
        });
      }
      ResolveVersionResult::None {
        had_higher_date_version,
      } => {
        any_had_higher_newest_dep_version |= had_higher_date_version;
      }
    }

    // 3. attempt to resolve with the the yanked versions
    let yanked_versions = package_info.versions.iter().filter_map(|(v, i)| {
      if i.yanked {
        Some((v, Some(i)))
      } else {
        None
      }
    });
    match resolve_version(
      ResolveVersionOptions {
        version_req: &package_req.version_req,
        newest_dependency_date: self.newest_dependency_date,
      },
      yanked_versions,
    ) {
      ResolveVersionResult::Some(version) => {
        return Ok(JsrVersionResolverResolvedVersion {
          is_yanked: true,
          version,
        });
      }
      ResolveVersionResult::None {
        had_higher_date_version,
      } => {
        any_had_higher_newest_dep_version |= had_higher_date_version;
      }
    }

    Err(JsrPackageReqNotFoundError {
      req: package_req.clone(),
      newest_dependency_date: any_had_higher_newest_dep_version
        .then_some(self.newest_dependency_date)
        .flatten(),
    })
  }

  pub fn matches_newest_dependency_date(
    &self,
    version_info: &JsrPackageInfoVersion,
  ) -> bool {
    version_info.matches_newest_dependency_date(self.newest_dependency_date)
  }
}

pub struct ResolveVersionOptions<'a> {
  pub version_req: &'a VersionReq,
  pub newest_dependency_date: Option<chrono::DateTime<chrono::Utc>>,
}

pub enum ResolveVersionResult<'a> {
  Some(&'a Version),
  None { had_higher_date_version: bool },
}

pub fn resolve_version<'a>(
  options: ResolveVersionOptions<'_>,
  versions: impl Iterator<Item = (&'a Version, Option<&'a JsrPackageInfoVersion>)>,
) -> ResolveVersionResult<'a> {
  let mut maybe_best_version: Option<&Version> = None;
  let mut had_higher_date_version = false;
  for (version, version_info) in versions {
    if options.version_req.matches(version) {
      had_higher_date_version = true;
      if matches_newest_dependency_date(
        version_info,
        options.newest_dependency_date,
      ) {
        let is_best_version = maybe_best_version
          .as_ref()
          .map(|best_version| (*best_version).cmp(version).is_lt())
          .unwrap_or(true);
        if is_best_version {
          maybe_best_version = Some(version);
        }
      }
    }
  }
  match maybe_best_version {
    Some(version) => ResolveVersionResult::Some(version),
    None => ResolveVersionResult::None {
      had_higher_date_version,
    },
  }
}

fn matches_newest_dependency_date(
  info: Option<&JsrPackageInfoVersion>,
  newest_dependency_date: Option<chrono::DateTime<chrono::Utc>>,
) -> bool {
  info
    .as_ref()
    .map(|info| info.matches_newest_dependency_date(newest_dependency_date))
    .unwrap_or(true)
}
