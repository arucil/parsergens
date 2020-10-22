use std::fmt::{self, Debug, Formatter};

type BitBlock = u64;

const BLOCK_NBITS: usize = std::mem::size_of::<BitBlock>() * 8;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TokenSet {
  slice: Box<[BitBlock]>,
}

impl TokenSet {
  pub fn new(num_tokens: usize) -> Self {
    let len = (num_tokens + BLOCK_NBITS - 1) / BLOCK_NBITS;
    Self {
      slice: vec![0; len].into_boxed_slice(),
    }
  }

  pub fn from_token(num_tokens: usize, token: u32) -> Self {
    let mut s = Self::new(num_tokens);
    s.insert(token);
    s
  }

  pub fn clear(&mut self) {
    for x in self.slice.iter_mut() {
      *x = 0;
    }
  }

  pub fn insert(&mut self, token: u32) {
    self.slice[token as usize / BLOCK_NBITS] |=
      1 << (token as u64 % BLOCK_NBITS as u64);
  }

  /// Returns whether the set has changed.
  pub fn union_with(&mut self, other: &TokenSet) -> bool {
    let mut changed = false;
    for i in 0..self.slice.len() {
      let old = self.slice[i];
      self.slice[i] |= other.slice[i];
      changed |= old != self.slice[i];
    }
    changed
  }

  pub fn iter(&self) -> Iter {
    Iter {
      slice: &*self.slice,
      bit: 0,
      index: 0,
    }
  }
}

pub struct Iter<'a> {
  slice: &'a [BitBlock],
  bit: usize,
  index: usize,
}

impl<'a> Iterator for Iter<'a> {
  type Item = u32;

  fn next(&mut self) -> Option<u32> {
    while self.index < self.slice.len() {
      if self.bit < BLOCK_NBITS {
        let bit = (self.slice[self.index] & !((1 << self.bit) - 1))
          .trailing_zeros() as usize;
        if bit < BLOCK_NBITS {
          self.bit = bit + 1;
          return Some((self.index * BLOCK_NBITS + bit) as u32);
        }
      }

      self.index += 1;
      self.bit = 0;
    }
    None
  }
}

impl Debug for TokenSet {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    f.debug_set().entries(self.iter()).finish()
  }
}

#[cfg(test)]
mod tests {
  use super::TokenSet;
  use pretty_assertions::assert_eq;

  #[test]
  fn insert() {
    let mut set = TokenSet::new(15);

    set.insert(7);
    set.insert(3);
    set.insert(7);
    set.insert(14);

    let vec = set.iter().collect::<Vec<_>>();

    assert_eq!(vec, vec![3, 7, 14]);
  }
}