use grammar::{LoweredGrammar, Symbol, Map};
use indexmap::IndexMap;
use fnv::FnvBuildHasher;
use std::fmt::{self, Write};
use std::hash::Hash;
use crate::token_set::TokenSet;

pub use self::states::gen_states;
pub use self::tables::gen_tables;
pub use self::compress::{compress_tables, CompressedTables};

mod states;
mod tables;
mod compress;

pub struct Builder<'a, T: LrComputation> {
  pub grammar: &'a LoweredGrammar,
  pub states: StateStore<T::StateKey>,
  /// eof is the token with the greatest id
  pub eof: usize,
  /// max number of RHS symbols in productions, plus one.
  pub max_nsym_p1: usize,
}

pub type StateStore<Key> = IndexMap<Key, State, FnvBuildHasher>;

/// sorted.
pub type KernelItemSet = Vec<Item>;

pub type LookaheadSet = TokenSet;

#[derive(Debug)]
pub struct State {
  /// Starts with sorted `kernel_len` kernel items, followed by other non-kernel items.
  pub items: Vec<Item>,
  /// size of kernel item set
  pub kernel_len: usize,
  /// symbol -> index of destination state
  pub transitions: Map<Symbol, u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item {
  /// production and dot
  pub key: u32,
  pub lookaheads: LookaheadSet,
}

pub trait LrComputation {
  type StateKey: Eq + Hash;

  /// Returns the index of the state and whether the state has changed.
  fn store_state(
    states: &mut StateStore<Self::StateKey>,
    kernel_item_set: KernelItemSet,
  ) -> (u32, bool);
}

fn encode_item(
  max_nsym_p1: usize,
  prod_ix: usize,
  dot: usize,
) -> u32 {
  (prod_ix * max_nsym_p1 + dot) as u32
}

fn decode_item(
  max_nsym_p1: usize,
  key: u32,
) -> (usize, usize) {
  let prod = key as usize / max_nsym_p1;
  let dot = key as usize % max_nsym_p1;
  (prod, dot)
}

impl State {
  pub fn new(items: KernelItemSet) -> Self {
    State {
      kernel_len: items.len(),
      items,
      transitions: Map::default(),
    }
  }
}

impl<'a, T: LrComputation> Builder<'a, T> {
  pub fn new(
    grammar: &'a LoweredGrammar
  ) -> Self {
    Self {
      grammar,
      states: StateStore::default(),
      eof: grammar.tokens.len(),
      max_nsym_p1: grammar.prods.iter()
        .map(|prod| prod.symbols.len())
        .max()
        .unwrap()
        + 1,
    }
  }
}

#[cfg(test)]
use grammar::HashMap;

#[cfg(test)]
use crate::EntryPoint;

#[cfg(test)]
impl<'a, T: LrComputation> Builder<'a, T> {
  pub fn states(
    &self,
    entry_points: &HashMap<String, EntryPoint>
  ) -> String {
    let mut output = String::new();
    self.fmt_states(entry_points, &mut output).unwrap();
    output
  }

  fn fmt_states(
    &self,
    entry_points: &HashMap<String, EntryPoint>,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    for i in 0..self.states.len() {
      self.fmt_state(entry_points, i, fmt)?;
    }

    Ok(())
  }

  fn fmt_state(
    &self,
    entry_points: &HashMap<String, EntryPoint>,
    state_ix: usize,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    let state = &self.states[state_ix];
    write!(fmt, "State {}", state_ix)?;
    if entry_points.values().find(|x| x.start_state == state_ix as u32).is_some() {
      write!(fmt, " (start)")?;
    }
    writeln!(fmt)?;

    for item in &state.items {
      self.fmt_item(item, fmt)?;

      writeln!(fmt)?;
    }

    writeln!(fmt)
  }
}

impl<'a, T: LrComputation> Builder<'a, T> {
  fn fmt_item(
    &self,
    item: &Item,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    //let item = self.item_store.items[item_ix];
    let (prod, dot) = decode_item(self.max_nsym_p1, item.key);
    let nt = self.grammar.prods[prod].nt;
    let symbols = &self.grammar.prods[prod].symbols;

    write!(fmt, "{} ->", self.grammar.nts[&nt].name)?;

    for (i, sym) in symbols.iter().enumerate() {
      if i == dot {
        write!(fmt, " .")?;
      }

      match sym {
        Symbol::Token(token) => {
          let name = self.grammar.tokens.get(token).map(|s|s.as_str()).unwrap_or("$");
          write!(fmt, " {}", name)?;
        }
        Symbol::Nonterminal(nt) => {
          let name = &self.grammar.nts[nt].name;
          write!(fmt, " {}", name)?;
        }
      }
    }

    if dot == symbols.len() {
      write!(fmt, " .")?;
    }

    write!(fmt, "      ")?;

    let mut slash = false;
    for lookahead in item.lookaheads.iter() {
      if slash {
        write!(fmt, " / ")?;
      }
      slash = true;

      write!(fmt, "{}",
        self.grammar.tokens.get(&lookahead).map(|s|s.as_str()).unwrap_or("$"))?;
    }

    Ok(())
  }
}