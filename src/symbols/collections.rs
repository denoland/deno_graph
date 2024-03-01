// Copyright 2018-2024 the Deno authors. MIT license.

#![allow(dead_code)]

use indexmap::IndexMap;
use std::cell::UnsafeCell;
use std::collections::HashMap;

macro_rules! define_map {
  ($name:ident, $kind:ident) => {
    /// A map that supports inserting data while holding references to
    /// the underlying data at another key. Due to this property, the values
    /// in the hashmap can never be replaced or removed. Inserting data at a
    /// previously inserted to key will cause a panic.
    pub struct $name<K, V> {
      // store the values in a box to ensure the references are always stored
      // in the same place. Uses an UnsafeCell for faster performance.
      data: UnsafeCell<$kind<K, Box<V>>>,
    }

    impl<K, V> Default for $name<K, V> {
      fn default() -> Self {
        Self {
          data: Default::default(),
        }
      }
    }

    impl<K, V> $name<K, V> {
      #[cfg(test)]
      pub fn with_capacity(capacity: usize) -> Self {
        Self {
          data: UnsafeCell::new($kind::with_capacity(capacity)),
        }
      }

      pub fn len(&self) -> usize {
        let data = unsafe { &*self.data.get() };
        data.len()
      }
    }

    impl<K: Eq + std::hash::Hash, V> $name<K, V> {
      pub fn take(self) -> $kind<K, Box<V>> {
        self.data.into_inner()
      }

      pub fn contains_key(&self, key: &K) -> bool {
        let data = unsafe { &*self.data.get() };
        data.contains_key(key)
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
  };
}

define_map!(AdditiveOnlyMap, HashMap);
define_map!(AdditiveOnlyIndexMap, IndexMap);

macro_rules! define_map_for_copy_values {
  ($name:ident, $kind:ident) => {
    /// An additive hash map for data that is `Copy`. This is slightly more
    /// optimized than `AdditiveOnlyMap` because it won't copy the value.
    pub struct $name<K, V: Copy> {
      data: UnsafeCell<$kind<K, V>>,
    }

    impl<K, V: Copy> Default for $name<K, V> {
      fn default() -> Self {
        Self {
          data: Default::default(),
        }
      }
    }

    impl<K, V: Copy> $name<K, V> {
      pub fn len(&self) -> usize {
        let data = unsafe { &*self.data.get() };
        data.len()
      }

      pub fn take(self) -> $kind<K, V> {
        self.data.into_inner()
      }
    }

    impl<K: Eq + std::hash::Hash, V: Copy> $name<K, V> {
      pub fn contains_key(&self, key: &K) -> bool {
        let data = unsafe { &*self.data.get() };
        data.contains_key(key)
      }

      pub fn insert(&self, key: K, value: V) {
        let data = unsafe { &mut *self.data.get() };
        data.insert(key, value);
      }

      pub fn get(&self, key: &K) -> Option<V> {
        unsafe {
          let data = &*self.data.get();
          data.get(key).copied()
        }
      }
    }
  };
}

define_map_for_copy_values!(AdditiveOnlyMapForCopyValues, HashMap);
define_map_for_copy_values!(AdditiveOnlyIndexMapForCopyValues, IndexMap);

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

  #[test]
  fn support_copy_map() {
    let map: AdditiveOnlyMapForCopyValues<usize, usize> =
      AdditiveOnlyMapForCopyValues::default();
    map.insert(1, 2);
    assert_eq!(map.get(&1), Some(2));
    assert_eq!(map.get(&0), None);
  }
}
