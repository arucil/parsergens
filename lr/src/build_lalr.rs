use std::collections::{HashMap, VecDeque};
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId};
use bit_set::BitSet;
use crate::first::FirstAndNullable;
use crate::Error;

#[derive(Default)]
struct Builder {
  states: Vec<BitSet>,
  state_indices: HashMap<BitSet, usize>,
  items: Vec<Lr0Item>,
  item_indices: HashMap<Lr0Item, usize>,
  /// state -> item -> lookahead set
  lookaheads: Vec<Map<usize, BitSet>>,
  /// state -> symbol (token or nonterminal) -> next state
  goto: Vec<Map<usize, usize>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Lr0Item {
  prod_ix: usize,
  dot_ix: usize,
}

pub fn build(
  grammar: &LoweredGrammar,
) -> Result<(), Error> {
  let mut builder = Builder::default();
  let fan = crate::first::compute(grammar);
  let mut start_nts = HashMap::new();

  for &start_nt in &grammar.start_nts {
    let start_state = start(&mut builder, grammar, &fan, start_nt)?;
    let start_prod_ix = grammar.nt_metas[&start_nt].range.start;
    let real_start_nt =
      match grammar.prods[start_prod_ix].symbols[0] {
        Symbol::Nonterminal(nt) => nt,
        _ => unreachable!(),
      };
    let nt_name = grammar.nts.get(&real_start_nt).unwrap().clone();
    start_nts.insert(nt_name, (real_start_nt.id(), start_state));
  }

  self.resolve_conflicts()?;

  Ok(())
}

fn start(
  builder: &mut Builder,
  grammar: &LoweredGrammar,
  fan: &FirstAndNullable,
  nt: NonterminalId
) -> Result<u32, Error> {
  let start_prod = grammar.nt_metas[&nt].range.start;
  let start_item = store_item(&mut builder.items, &mut builder.item_indices,
    Lr0Item {
      prod_ix: start_prod,
      dot_ix: 0,
    });
  let mut start_state_set = {
    let mut set = BitSet::new();
    set.insert(start_item);
    set
  };
  self.closure(&mut start_state_set);
  let start_state = self.state(&start_state_set);

  let mut queue = VecDeque::new();
  queue.push_back(start_state_set);

  while let Some(state) = queue.pop_front() {
    let from_state = self.state(&state);
    let mut to_states = Map::<Symbol, BitSet>::new();

    for item in state.iter() {
      let item = self.items.get_by_right(&item).unwrap().clone();
      let symbols = &self.grammar.prods[item.prod_ix()].symbols;
      if item.dot_ix() == symbols.len() {
        if item.prod_ix() == start_prod  {
          self.accept(from_state);
        } else {
          self.reduce(from_state, &item)?;
        }
        continue;
      }

      /*
      if item.dot_ix() == 1 && item.prod_ix() == start_prod {
        self.accept(from_state);
        continue;
      }
      */

      let new_item = store_item(&mut self.items, T::next_item(&item));
      to_states.entry(symbols[item.dot_ix()].clone()).or_default().insert(new_item);
    }

    for (sym, mut to_state) in to_states {
      self.closure(&mut to_state);
      let is_new_state = !self.states.contains_left(&to_state);
      let to_state_ix = self.state(&to_state);

      match sym {
        Symbol::Token(token) => {
          let entry = self.action.get_mut(&from_state).unwrap()
            .entry(token.id())
            .or_default();
          assert!(entry.shift.is_none());

          entry.shift = Some(to_state_ix);
        }
        Symbol::Nonterminal(nt) => {
          let entry = self.goto.get_mut(&from_state).unwrap()
            .entry(nt.id())
            .or_default();
          assert!(*entry == 0);

          *entry = to_state_ix;
        }
      }

      if is_new_state {
        queue.push_back(to_state);
      }
    }
  }

  Ok(start_state)
}

fn store_item(
  items: &mut Vec<Lr0Item>,
  item_indices: &HashMap<Lr0Item, usize>,
  item: Lr0Item,
) -> usize {
  if let Some(&ix) = item_indices.get(&item) {
    ix
  } else {
    let len = items.len();
    items.push(item);
    item_indices.insert(item, len);
    len
  }
}