// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::cell::RefCell;
use std::collections::HashMap;

/// A hash map that supports inserting data while holding references to
/// the underlying data at another key. Due to this property, the values
/// in the hashmap can never be replaced or removed. Inserting data at a
/// previously inserted to key will cause a panic.
pub struct AdditiveOnlyMap<K, V> {
  // store the values in a box to ensure the references are always stored
  // in the same place
  data: RefCell<HashMap<K, Box<V>>>,
}

impl<K, V> AdditiveOnlyMap<K, V> {
  pub fn new() -> Self {
    Self {
      data: Default::default(),
    }
  }

  #[cfg(test)]
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      data: RefCell::new(HashMap::with_capacity(capacity)),
    }
  }

  pub fn len(&self) -> usize {
    self.data.borrow().len()
  }

  pub fn take(self) -> HashMap<K, Box<V>> {
    self.data.into_inner()
  }
}

impl<K: Eq + std::hash::Hash, V> AdditiveOnlyMap<K, V> {
  pub fn insert(&self, key: K, value: V) {
    // assert that we never replace any data
    assert!(self
      .data
      .borrow_mut()
      .insert(key, Box::new(value))
      .is_none());
  }

  pub fn get<'a>(&'a self, key: &K) -> Option<&'a V> {
    // this is ok because we never remove from the map
    unsafe {
      let data = self.data.borrow();
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
  }
}
