// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::BTreeSet;
use std::collections::HashSet;
use std::sync::Arc;

use deno_semver::package::PackageNv;
use serde::Deserialize;
use serde::Serialize;

use crate::FastCheckModule;
use crate::ModuleInfo;
use crate::ModuleSpecifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FastCheckCacheKey(u64);

impl FastCheckCacheKey {
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
  pub dependencies: HashSet<PackageNv>,
  pub modules: Vec<(ModuleSpecifier, FastCheckCacheModuleItem)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FastCheckCacheModuleItemInfo {
  pub source_hash: u64,
  pub module_info: Arc<ModuleInfo>,
  pub text: Arc<str>,
  pub source_map: Arc<[u8]>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FastCheckCacheModuleItemDiagnostic {
  pub source_hash: u64,
}

/// Implementors should bust their cache when their version changes.
pub trait FastCheckCache {
  fn get(&self, key: FastCheckCacheKey) -> Option<FastCheckCacheItem>;
  fn set(&self, key: FastCheckCacheKey, value: FastCheckCacheItem);
}

pub(crate) fn fast_insecure_hash(bytes: &[u8]) -> u64 {
  use std::hash::Hash;
  use std::hash::Hasher;
  let mut hasher = twox_hash::XxHash64::default();
  bytes.hash(&mut hasher);
  hasher.finish()
}
