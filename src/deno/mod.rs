// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::Version;
use serde::Serialize;

use crate::ModuleInfo;
use crate::ModuleSpecifier;

pub struct DenoPackageInfo {
  pub versions: Vec<Version>,
}

pub struct DenoPackageVersionInfo {
  pub module_graph: HashMap<ModuleSpecifier, ModuleInfo>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct DenoSpecifierSnapshot {
  package_reqs: HashMap<PackageReq, PackageNv>,
  packages_by_name: HashMap<String, Vec<PackageNv>>,
}
