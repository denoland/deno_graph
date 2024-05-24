// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;
use std::sync::Arc;

use deno_ast::ModuleSpecifier;
use deno_semver::package::PackageNv;
use futures::future::Shared;
use futures::FutureExt;

use crate::graph::JsrLoadError;
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

pub type PendingResult<T> = Shared<JoinHandle<Result<T, JsrLoadError>>>;

#[derive(Clone, Copy)]
pub struct JsrMetadataStoreServices<'a> {
  pub loader: &'a dyn Loader,
  pub executor: &'a dyn Executor,
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
}

#[derive(Debug, Default)]
pub struct JsrMetadataStore {
  pending_package_info_loads:
    HashMap<String, PendingResult<Arc<JsrPackageInfo>>>,
  pending_package_version_info_loads:
    HashMap<PackageNv, PendingResult<PendingJsrPackageVersionInfoLoadItem>>,
}

impl JsrMetadataStore {
  pub fn get_package_metadata(
    &self,
    package_name: &str,
  ) -> Option<PendingResult<Arc<JsrPackageInfo>>> {
    self.pending_package_info_loads.get(package_name).cloned()
  }

  pub fn get_package_version_metadata(
    &self,
    nv: &PackageNv,
  ) -> Option<PendingResult<PendingJsrPackageVersionInfoLoadItem>> {
    self.pending_package_version_info_loads.get(nv).cloned()
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
    let fut = self.load_data(
      specifier,
      services,
      cache_setting,
      /* checksum */ None,
      |content| {
        let package_info: JsrPackageInfo = serde_json::from_slice(content)?;
        Ok(Arc::new(package_info))
      },
      {
        let package_name = package_name.to_string();
        |e| JsrLoadError::PackageManifestLoad(package_name, e)
      },
      {
        let package_name = package_name.to_string();
        || JsrLoadError::PackageNotFound(package_name)
      },
    );
    self
      .pending_package_info_loads
      .insert(package_name.to_string(), fut);
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
    let maybe_expected_checksum =
      maybe_expected_checksum.map(|c| LoaderChecksum::new(c.to_string()));
    let fut = self.load_data(
      specifier,
      services,
      cache_setting,
      // we won't have a checksum when loading this the
      // first time or when not using a lockfile
      maybe_expected_checksum.clone(),
      |content| {
        // if we have the expected checksum, then we can re-use that here
        let checksum = maybe_expected_checksum
          .map(|c| c.into_string())
          .unwrap_or_else(|| LoaderChecksum::gen(content));
        let version_info: JsrPackageVersionInfo =
          serde_json::from_slice(content)?;
        Ok(PendingJsrPackageVersionInfoLoadItem {
          checksum,
          info: Arc::new(version_info),
        })
      },
      {
        let package_nv = package_nv.clone();
        |e| JsrLoadError::PackageVersionManifestLoad(package_nv, e)
      },
      {
        let package_nv = package_nv.clone();
        || JsrLoadError::PackageVersionNotFound(package_nv)
      },
    );
    self
      .pending_package_version_info_loads
      .insert(package_nv.clone(), fut);
  }

  #[allow(clippy::too_many_arguments)]
  fn load_data<T: Clone + 'static>(
    &self,
    specifier: ModuleSpecifier,
    services: JsrMetadataStoreServices,
    cache_setting: CacheSetting,
    maybe_expected_checksum: Option<LoaderChecksum>,
    handle_content: impl FnOnce(&[u8]) -> Result<T, serde_json::Error> + 'static,
    create_failed_load_err: impl FnOnce(Arc<anyhow::Error>) -> JsrLoadError
      + 'static,
    create_not_found_error: impl FnOnce() -> JsrLoadError + 'static,
  ) -> PendingResult<T> {
    let fut = services.loader.load(
      &specifier,
      LoadOptions {
        is_dynamic: false,
        cache_setting,
        maybe_checksum: maybe_expected_checksum,
      },
    );
    let fut = spawn(
      services.executor,
      async move {
        let data = match fut.await {
          Ok(data) => data,
          Err(err) => return Err(create_failed_load_err(Arc::new(err))),
        };
        match data {
          Some(LoadResponse::Module { content, .. }) => {
            handle_content(&content)
              .map_err(|e| create_failed_load_err(Arc::new(e.into())))
          }
          Some(LoadResponse::Redirect { specifier }) => {
            Err(JsrLoadError::RedirectInPackage(specifier))
          }
          _ => Err(create_not_found_error()),
        }
      }
      .boxed_local(),
    );
    fut.shared()
  }
}
