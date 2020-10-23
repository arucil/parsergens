use std::collections::VecDeque;
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId, HashMap};
use crate::first::NonterminalFirst;
use crate::EntryPoint;
use crate::token_set::TokenSet;
use super::{Builder, LrComputation, Item, State, encode_item, decode_item};


pub fn gen_states<T: LrComputation>(
  builder: &mut Builder<T>,
) -> HashMap<String, EntryPoint> {
  let nt_firsts = crate::first::compute(builder.grammar);
  let mut entry_points = HashMap::default();

  for &start_nt in &builder.grammar.start_nts {
    let start_state = start(builder, &nt_firsts, start_nt);
    let start_prod_ix = builder.grammar.nts[&start_nt].range.start;
    let real_start_nt =
      match builder.grammar.prods[start_prod_ix].symbols[0] {
        Symbol::Nonterminal(nt) => nt,
        _ => unreachable!(),
      };
    let nt_name = builder.grammar.nts[&real_start_nt].name.clone();
    entry_points.insert(nt_name, EntryPoint {
      real_start_nt: real_start_nt.id(),
      start_state,
      accept_prod: start_prod_ix,
    });
  }

  entry_points
}

fn start<T: LrComputation>(
  builder: &mut Builder<T>,
  nt_firsts: &[NonterminalFirst],
  start_nt: NonterminalId,
) -> u32 {
  let start_prod = builder.grammar.nts[&start_nt].range.start;
  let start_item_set = vec![
    Item {
      key: encode_item(builder.max_nsym_p1, start_prod, 0),
      lookaheads: TokenSet::from_token(builder.eof + 1, builder.eof as u32),
    }
  ];

  let (start_state, _) = T::store_state(&mut builder.states, start_item_set);

  let mut queue = VecDeque::new();
  queue.push_back(start_state);

  while let Some(state_ix) = queue.pop_front() {
    let state = &mut builder.states[state_ix as usize];
    compute_closure(builder.grammar, nt_firsts, builder.max_nsym_p1, state);

    let transitions = compute_transitions(builder.grammar, builder.max_nsym_p1, state);
    for (sym, mut kernel_item_set) in transitions {
      kernel_item_set.sort_by_key(|item| item.key);

      let (next_state, changed) = T::store_state(&mut builder.states, kernel_item_set);
      if changed {
        queue.push_back(next_state);
      }
      builder.states[state_ix as usize].transitions.insert(sym, next_state);
    }
  }

  start_state
}

fn compute_closure(
  grammar: &LoweredGrammar,
  nt_firsts: &[NonterminalFirst],
  max_nsym_p1: usize,
  state: &mut State,
) {
  let items = &mut state.items;
  // nt -> start index of items
  let mut nt_starts = HashMap::default();
  let mut first = TokenSet::new(grammar.tokens.len() + 1);

  for (i, item) in items.iter().enumerate() {
    if let (prod, 0) = decode_item(max_nsym_p1, item.key) {
      let nt = grammar.prods[prod].nt;
      if !nt_starts.contains_key(&nt) {
        nt_starts.insert(nt, i);
      }
    }
  }

  let mut i = 0;
  while i < items.len() {
    let (prod, dot) = decode_item(max_nsym_p1, items[i].key);
    let prod = &grammar.prods[prod];
    if dot == prod.symbols.len() {
      i += 1;
      continue;
    }

    if let Symbol::Nonterminal(nt) = &prod.symbols[dot] {
      first.clear();
      crate::first::compute_symbols_first(
        &mut first,
        nt_firsts,
        &prod.symbols[dot + 1..],
        Some(&items[i].lookaheads));

      if let Some(&nt_start) = nt_starts.get(nt) {
        let mut changed = false;
        for j in nt_start .. nt_start + grammar.nts[nt].range.len() {
          changed |= items[j].lookaheads.union_with(&first);
        }

        if changed {
          if i > nt_start {
            i = nt_start;
          }
        } else {
          i += 1;
        }
      } else {
        nt_starts.insert(*nt, items.len());

        for prod_ix in grammar.nts[nt].range.clone() {
          items.push(Item {
            key: encode_item(max_nsym_p1, prod_ix, 0),
            lookaheads: first.clone(),
          });
        }

        i += 1;
      }
    } else {
      i += 1;
    }
  }
}

fn compute_transitions(
  grammar: &LoweredGrammar,
  max_nsym_p1: usize,
  state: &State,
) -> Map<Symbol, Vec<Item>> {
  let mut transitions = Map::<_, Vec<Item>>::default();

  for item in &state.items {
    let (prod_ix, dot) = decode_item(max_nsym_p1, item.key);
    let prod = &grammar.prods[prod_ix];
    if dot == prod.symbols.len() {
      continue;
    }

    let next_item = encode_item(max_nsym_p1, prod_ix, dot + 1);
    transitions.entry(prod.symbols[dot].clone())
      .or_default()
      .push(Item {
        key: next_item,
        lookaheads: item.lookaheads.clone(),
      });
  }

  transitions
}
