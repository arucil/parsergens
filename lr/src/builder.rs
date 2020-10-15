use bit_set::BitSet;
use grammar::{ LoweredGrammar, Symbol, HashMap };

pub struct Builder<'a, StateInfo, LrItem> {
  pub grammar: &'a LoweredGrammar,
  pub state_store: StateStore<StateInfo>,
  pub item_store: ItemStore<LrItem>,
  pub eof: u32,
}

#[derive(Default)]
pub struct StateStore<StateInfo> {
  /// state -> (item set, lookahead set)
  pub states: Vec<(BitSet, StateInfo)>,
  // item set -> state
  pub state_indices: HashMap<BitSet, u32>,
  /// state -> symbol -> next state
  pub goto: Vec<HashMap<Symbol, u32>>,
}

#[derive(Default)]
pub struct ItemStore<LrItem> {
  pub items: Vec<LrItem>,
  pub item_indices: HashMap<LrItem, u32>,
}

impl<'a, StateInfo, LrItem> Builder<'a, StateInfo, LrItem>
  where
    StateInfo: Default,
    LrItem: Default,
{
  pub fn new(grammar: &'a LoweredGrammar) -> Self {
    Self {
      grammar,
      state_store: Default::default(),
      item_store: Default::default(),
      eof: grammar.tokens
        .keys()
        .map(|x| x.id())
        .max()
        .unwrap() + 1,
    }
  }
}