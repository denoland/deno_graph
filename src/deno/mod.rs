// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;

pub struct DenoPackageInfo {}

pub enum DenoRegistryPackageInfoCacheSetting {
  PreferCache,
  ForceReload,
}

#[async_trait::async_trait]
pub trait DenoRegistryApi {
  async fn package_info(
    &self,
    name: &str,
    cache_setting: DenoRegistryPackageInfoCacheSetting,
  ) -> Result<DenoPackageInfo, anyhow::Error>;
}

pub struct DenoSpecifierSnapshot {
  package_reqs: HashMap<PackageReq, PackageNv>,
  packages_by_name: HashMap<String, Vec<PackageNv>>,
}
