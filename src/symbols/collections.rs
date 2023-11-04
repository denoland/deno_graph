// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::cell::UnsafeCell;
use std::collections::HashMap;

/// A hash map that supports inserting data while holding references to
/// the underlying data at another key. Due to this property, the values
/// in the hashmap can never be replaced or removed. Inserting data at a
/// previously inserted to key will cause a panic.
pub struct AdditiveOnlyMap<K, V> {
  // store the values in a box to ensure the references are always stored
  // in the same place. Uses an UnsafeCell for faster performance.
  data: UnsafeCell<HashMap<K, Box<V>>>,
}

impl<K, V> Default for AdditiveOnlyMap<K, V> {
  fn default() -> Self {
    Self {
      data: Default::default(),
    }
  }
}

impl<K, V> AdditiveOnlyMap<K, V> {
  #[cfg(test)]
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      data: UnsafeCell::new(HashMap::with_capacity(capacity)),
    }
  }

  pub fn len(&self) -> usize {
    let data = unsafe { &*self.data.get() };
    data.len()
  }
}

impl<K: Eq + std::hash::Hash, V> AdditiveOnlyMap<K, V> {
  pub fn contains_key(&self, key: &K) -> bool {
    let data = unsafe { &*self.data.get() };
    data.contains_key(&key)
  }

  pub fn insert(&self, key: K, value: V) {
    let data = unsafe { &mut *self.data.get() };
    // assert that we never replace any data
    assert!(data.insert(key, Box::new(value)).is_none());
  }

  pub fn get<'a>(&'a self, key: &K) -> Option<&'a V> {
    unsafe {
      let data = &*self.data.get();
      // this is ok because we never remove from the map
      data
        .get(key)
        .map(|value_box| value_box.as_ref() as *const V)
        .map(|raw| &*raw)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn should_support_adding_while_holding_ref_to_value() {
    struct Value {
      value: usize,
    }

    // use a low capacity which will ensure the map is resized once we exceed it
    let map: AdditiveOnlyMap<usize, Value> = AdditiveOnlyMap::with_capacity(2);
    map.insert(0, Value { value: 987 });
    let data = map.get(&0).unwrap();
    for i in 1..100 {
      map.insert(i, Value { value: i });
    }
    assert_eq!(data.value, 987);
    assert_eq!(map.get(&0).unwrap().value, 987);
    assert_eq!(map.get(&99).unwrap().value, 99);
    assert!(map.contains_key(&99));
    assert!(!map.contains_key(&100));
  }
}
