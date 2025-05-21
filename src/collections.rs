// Copyright 2018-2024 the Deno authors. MIT license.

use indexmap::IndexSet;

/// Collection useful for a phased pass where the pending items
/// are the same values as the seen items.
pub struct SeenPendingCollection<T: std::hash::Hash + Eq + Clone> {
  inner: IndexSet<T>,
  next_index: usize,
}

impl<T: std::hash::Hash + Eq + Clone> SeenPendingCollection<T> {
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      inner: IndexSet::with_capacity(capacity),
      next_index: 0,
    }
  }

  pub fn has_seen(&self, item: &T) -> bool {
    self.inner.contains(item)
  }

  pub fn add(&mut self, item: T) -> bool {
    self.inner.insert(item)
  }

  pub fn extend(&mut self, items: impl Iterator<Item = T>) {
    self.inner.extend(items)
  }

  pub fn next_pending(&mut self) -> Option<T> {
    let next = self.inner.get_index(self.next_index);
    if next.is_some() {
      self.next_index += 1;
    }
    next.cloned()
  }
}
