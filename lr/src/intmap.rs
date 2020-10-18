use intmap::IntMap;
use std::ops::{Index, Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct MyIntMap<V>(IntMap<V>);

impl<V> MyIntMap<V> {
  pub fn new() -> Self {
    Self(IntMap::new())
  }
}

impl<V> Default for MyIntMap<V> {
  fn default() -> Self {
    Self::new()
  }
}

impl<V> Index<u64> for MyIntMap<V> {
  type Output = V;

  fn index(&self, index: u64) -> &V {
    self.0.get(index).expect("index not found")
  }
}

impl<V> Deref for MyIntMap<V> {
  type Target = IntMap<V>;

  fn deref(&self) -> &IntMap<V> {
    &self.0
  }
}

impl<V> DerefMut for MyIntMap<V> {
  fn deref_mut(&mut self) -> &mut IntMap<V> {
    &mut self.0
  }
}