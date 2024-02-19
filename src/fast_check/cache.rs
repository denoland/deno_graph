// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::BTreeSet;
use std::sync::Arc;

use deno_semver::package::PackageNv;
use serde::Deserialize;
use serde::Serialize;

use crate::ModuleSpecifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// Implementors should bust their cache when their version changes.
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
