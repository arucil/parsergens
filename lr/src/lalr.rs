use grammar::{ LoweredGrammar, TokenId };
use bit_set::BitSet;
use crate::Error;
use crate::ffn::Ffn;
use crate::builder::LrCalculation;
use crate::clr::{ClrCalc, Lr1Item};
use grammar::{Map, BiMap};

pub enum LalrCalc {}

impl LrCalculation for LalrCalc {
  type Item = Lr1Item;

  fn start_item(
    start_prod_ix: usize,
    eof_token: TokenId,
  ) -> Lr1Item {
    ClrCalc::start_item(start_prod_ix, eof_token)
  }

  fn next_item(
    item: &Lr1Item
  ) -> Lr1Item {
    ClrCalc::next_item(item)
  }

  fn closure_step<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    prev: &Lr1Item,
    action: F
  )
    where F: FnMut(Lr1Item)
  {
    ClrCalc::closure_step(grammar, ffn, prev, action)
  }

  fn reduce_tokens<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    item: &Lr1Item,
    action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>
  {
    ClrCalc::reduce_tokens(grammar, ffn, item, action)
  }

  fn merge_states(
    _grammar: &LoweredGrammar,
    states: &mut BiMap<BitSet, u32>,
    items: &mut BiMap<Lr1Item, usize>,
  ) -> Option<Map<u32, u32>> {
    let mut new_states = BiMap::<BitSet, u32>::new();
    let mut new_items = BiMap::new();
    let mut map = Map::new();

    for (state, state_id) in &*states {
      let new_state = state.iter().map(|item| {
        let item = items.get_by_right(&item).unwrap();
        store_item(&mut new_items, item)
      }).collect::<BitSet>();
      let new_state_id = if let Some(id) = new_states.get_by_left(&new_state) {
        *id
      } else {
        let id = new_states.len() as u32;
        new_states.insert(new_state, id);
        id
      };
      map.insert(*state_id, new_state_id);
    }

    *items = new_items;
    *states = new_states;
    Some(map)
  }
}

fn store_item(
  items: &mut BiMap<Lr1Item, usize>,
  item: &Lr1Item
) -> usize {
  let item = Lr1Item {
    token: std::u32::MAX,
    ..*item
  };
  if let Some(&item) = items.get_by_left(&item) {
    item
  } else {
    let len = items.len();
    items.insert(item, len);
    len
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_debug_snapshot, assert_snapshot};
  use grammar::{ TokenId, LoweredGrammar };
  use crate::ffn::Ffn;
  use crate::ffn;
  use crate::augment;
  use crate::builder::Builder;

  fn prepare(input: &str) -> (LoweredGrammar, TokenId, Ffn) {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, eof_token) = augment::augment(grammar);
    let ffn = ffn::compute(&grammar);

    (grammar, eof_token, ffn)
  }

  static SIMPLE: &str = r#"
%token c "c"
%token d "d"

%start S

S = C C
C = c C
  | d
  "#;

  #[test]
  fn simple_states() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<LalrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<LalrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }
}