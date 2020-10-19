use bittyset::BitSet;
use grammar::{ LoweredGrammar, Symbol, HashMap };
use crate::intmap::MyIntMap;

pub type ItemSet = Vec<u32>;

pub struct Builder<'a, StateInfo> {
  pub grammar: &'a LoweredGrammar,
  pub state_store: StateStore<StateInfo>,
  pub item_store: ItemStore,
  pub eof: u32,
  /// Max numbers of the RHS symbols of productions, plus one.
  pub max_nsym_p1: u32,
}

#[derive(Default)]
pub struct StateStore<StateInfo> {
  /// state -> (item set, lookahead set)
  pub states: Vec<(ItemSet, StateInfo)>,
  // kernel item set -> state
  pub state_indices: HashMap<ItemSet, u32>,
  /// state -> symbol -> next state
  pub goto: Vec<HashMap<Symbol, u32>>,
}

#[derive(Default)]
pub struct ItemStore {
  pub items: MyIntMap<()>,
}

impl<'a, StateInfo> Builder<'a, StateInfo>
  where
    StateInfo: Default,
{
  pub fn new(
    grammar: &'a LoweredGrammar,
  ) -> Self {
    Self {
      grammar,
      state_store: Default::default(),
      item_store: ItemStore {
        items: MyIntMap::new(),
      },
      eof: grammar.tokens
        .keys()
        .map(|x| x.id())
        .max()
        .unwrap() + 1,
      max_nsym_p1: grammar.prods.iter()
        .map(|prod| prod.symbols.len() as u32)
        .max()
        .unwrap() + 1,
    }
  }
}