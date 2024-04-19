// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_semver::package::PackageNv;
use futures::future::Shared;
use futures::FutureExt;

use crate::packages::JsrPackageInfo;
use crate::packages::JsrPackageVersionInfo;
use crate::rt::spawn;
use crate::rt::JoinHandle;
use crate::source::CacheSetting;
use crate::source::JsrUrlProvider;
use crate::source::LoadOptions;
use crate::source::LoadResponse;
use crate::source::Loader;
use crate::source::LoaderChecksum;
use crate::Executor;

#[derive(Clone)]
pub struct PendingJsrPackageVersionInfoLoadItem {
  pub checksum: String,
  pub info: Arc<JsrPackageVersionInfo>,
}

type PendingResult<T> = Shared<JoinHandle<Result<T, Arc<anyhow::Error>>>>;

#[derive(Clone, Copy)]
pub struct JsrMetadataStoreServices<'a> {
  pub loader: &'a dyn Loader,
  pub executor: &'a dyn Executor,
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
}

#[derive(Debug, Default)]
pub struct JsrMetadataStore {
  pending_package_info_loads:
    HashMap<String, PendingResult<Option<Arc<JsrPackageInfo>>>>,
  pending_package_version_info_loads:
    HashMap<PackageNv, PendingResult<PendingJsrPackageVersionInfoLoadItem>>,
}

impl JsrMetadataStore {
  pub fn get_package_metadata(&self, package_name: &str) -> Option<PendingResult<Option<Arc<JsrPackageInfo>>>> {
    self
      .pending_package_info_loads
      .get(package_name)
      .cloned()
  }

  pub fn get_package_version_metadata(&self, nv: &PackageNv) -> Option<PendingResult<PendingJsrPackageVersionInfoLoadItem>> {
    self
      .pending_package_version_info_loads
      .get(nv)
      .cloned()
  }

  pub fn queue_load_package_info(
    &mut self,
    package_name: &str,
    cache_setting: CacheSetting,
    services: JsrMetadataStoreServices,
  ) {
    if self.pending_package_info_loads.contains_key(package_name) {
      return; // already queued
    }

    // request to load
    let specifier = services
      .jsr_url_provider
      .url()
      .join(&format!("{}/meta.json", package_name))
      .unwrap();
    let fut = services.loader.load(
      &specifier,
      LoadOptions {
        is_dynamic: false,
        cache_setting,
        maybe_checksum: None,
      },
    );
    // todo(THIS PR): consolidate with below
    let fut = spawn(
      services.executor,
      async move {
        let data = fut.await.map_err(Arc::new)?;
        match data {
          Some(LoadResponse::Module { content, .. }) => {
            let package_info: JsrPackageInfo = serde_json::from_slice(&content)
              .map_err(|e| Arc::new(e.into()))?;
            Ok(Some(Arc::new(package_info)))
          }
          Some(LoadResponse::Redirect { specifier }) => {
            Err(Arc::new(anyhow::anyhow!(
              "Redirects in the JSR registry are not supported (redirected to '{}')",
              specifier
            )))
          }
          _ => Ok(None),
        }
      }
      
      .boxed_local(),
    );
    self
      .pending_package_info_loads
      .insert(package_name.to_string(), fut.shared());
  }

  pub fn queue_load_package_version_info(
    &mut self,
    package_nv: &PackageNv,
    cache_setting: CacheSetting,
    maybe_expected_checksum: Option<&str>,
    services: JsrMetadataStoreServices,
  ) {
    if self
      .pending_package_version_info_loads
      .contains_key(package_nv)
    {
      return; // already queued
    }

    let specifier = services
      .jsr_url_provider
      .url()
      .join(&format!(
        "{}/{}_meta.json",
        package_nv.name, package_nv.version
      ))
      .unwrap();
    let maybe_expected_checksum = maybe_expected_checksum.map(|c| LoaderChecksum::new(c.to_string()));
    let fut = services.loader.load(
      &specifier,
      LoadOptions {
        is_dynamic: false,
        cache_setting,
        // we won't have a checksum when loading this the
        // first time or when not using a lockfile
        maybe_checksum: maybe_expected_checksum.clone(),
      },
    );
    let fut = spawn(
      services.executor,
      async move {
        let data = fut.await.map_err(Arc::new)?;
        match data {
          Some(LoadResponse::Module { content, .. }) => {
            // if we have the expected checksum, then we can re-use that here
            let checksum = maybe_expected_checksum
              .map(|c| c.into_string())
              .unwrap_or_else(|| LoaderChecksum::gen(&content));
            let version_info: JsrPackageVersionInfo =
              serde_json::from_slice(&content)
                .map_err(|e| Arc::new(e.into()))?;
            Ok(PendingJsrPackageVersionInfoLoadItem {
              checksum,
              info: Arc::new(version_info),
            })
          }
          Some(LoadResponse::Redirect { specifier }) => {
            Err(Arc::new(anyhow::anyhow!(
              "Redirects in the JSR registry are not supported (redirected to '{}')",
              specifier
            )))
          }
          _ => Err(Arc::new(anyhow::anyhow!("Not found: {}", specifier))),
        }
      }
      .boxed_local(),
    );
    self
      .pending_package_version_info_loads
      .insert(package_nv.clone(), fut.shared());
  }
}
