// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::BTreeSet;
use std::sync::Arc;

use deno_semver::package::PackageNv;
use serde::Deserialize;
use serde::Serialize;

use crate::ModuleSpecifier;

/// Cache key that's a hash of the package name, version, and
/// sorted export names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FastCheckCacheKey(u64);

impl FastCheckCacheKey {
  #[cfg(feature = "fast_check")]
  pub fn build(
    package_nv: &PackageNv,
    entrypoints: &BTreeSet<ModuleSpecifier>,
  ) -> Self {
    use std::hash::Hash;
    use std::hash::Hasher;
    let mut hasher = twox_hash::XxHash64::default();
    package_nv.hash(&mut hasher);
    for value in entrypoints {
      value.hash(&mut hasher);
    }
    Self(hasher.finish())
  }

  pub fn as_u64(&self) -> u64 {
    self.0
  }
}

// On successful fast check, The value in the hash is a list of files
// used, these files' hashes, and the package's dependencies that are
// used in fast check (not any deps that aren't). The cache is invalidated
// when any of these files change.
//
// On failure, the value in the hash is the entrypoint files along with
// any imported file until a diagnostic is found. These hashes are stored
// so that the cache can be invalidated when any of them change.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FastCheckCacheItem {
  pub dependencies: BTreeSet<PackageNv>, // ordered for determinism when deserializing
  pub modules: Vec<(ModuleSpecifier, FastCheckCacheModuleItem)>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FastCheckCacheModuleItem {
  Info(FastCheckCacheModuleItemInfo),
  Diagnostic(FastCheckCacheModuleItemDiagnostic),
}

impl FastCheckCacheModuleItem {
  pub fn source_hash(&self) -> u64 {
    match self {
      FastCheckCacheModuleItem::Info(info) => info.source_hash,
      FastCheckCacheModuleItem::Diagnostic(diagnostic) => {
        diagnostic.source_hash
      }
    }
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FastCheckCacheModuleItemInfo {
  pub source_hash: u64,
  /// Serialized module_info as JSON because bincode (used in the CLI's cache)
  /// doesn't work well with the ModuleInfo since it makes heavy use of
  /// skip_serializing_if.
  pub module_info: String,
  pub text: Arc<str>,
  pub source_map: Arc<[u8]>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FastCheckCacheModuleItemDiagnostic {
  pub source_hash: u64,
}

/// Cache for storing the results of fast checks based on a package.
///
/// Note: Implementors should bust their cache when their version changes.
pub trait FastCheckCache {
  fn get(&self, key: FastCheckCacheKey) -> Option<FastCheckCacheItem>;
  fn set(&self, key: FastCheckCacheKey, value: FastCheckCacheItem);
}

#[cfg(feature = "fast_check")]
pub(crate) fn fast_insecure_hash(bytes: &[u8]) -> u64 {
  use std::hash::Hash;
  use std::hash::Hasher;
  let mut hasher = twox_hash::XxHash64::default();
  bytes.hash(&mut hasher);
  hasher.finish()
}

#[cfg(test)]
mod test {
  use std::sync::Arc;

  #[test]
  fn module_item_info_serialization() {
    let item = super::FastCheckCacheModuleItem::Info(
      super::FastCheckCacheModuleItemInfo {
        source_hash: 0,
        module_info: Default::default(),
        text: "test".to_string().into(),
        source_map: Arc::new([0, 1, 2]),
      },
    );

    let data = bincode::serialize(&item).unwrap();
    let result = bincode::deserialize(&data).unwrap();
    assert_eq!(item, result);
  }
}
