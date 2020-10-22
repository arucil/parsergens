use std::collections::VecDeque;
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId, HashMap, Assoc};
use indexmap::IndexMap;
use fnv::FnvBuildHasher;
use std::fmt::{self, Write};
use std::hash::Hash;
use crate::first::NonterminalFirst;
use crate::{
  EntryPoint, Error, ShiftReduceConflictError, ReduceReduceConflictError
};
use crate::token_set::TokenSet;

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

/// Generates ACTION table and GOTO table.
/// 
/// entry in ACTION table:
/// - positive: shift
/// - negative: reduce
/// - zero: error
///
/// entry in GOTO table:
/// - positive: goto
/// - zero: error
pub fn gen_tables<T: LrComputation>(
  builder: &Builder<T>,
) -> Result<(Vec<Vec<i32>>, Vec<Vec<u32>>), Vec<Error>> {
  let num_states = builder.states.len();
  let mut action = vec![vec![0i32; builder.eof as usize + 1]; num_states];
  let mut goto = vec![vec![0u32; builder.grammar.nts.len()]; num_states];
  let mut errors = vec![];

  for (from_state, (_, state)) in builder.states.iter().enumerate() {
    for item in &state.items {
      //let item = builder.item_store.items[item_ix];
      let (prod, dot) = decode_item(builder.max_nsym_p1, item.key);
      let symbols = &builder.grammar.prods[prod].symbols;
      // shift
      if dot < symbols.len() {
        let sym = &symbols[dot];
        let to_state = state.transitions[sym];
        match sym {
          Symbol::Token(tok) => {
            let old = &mut action[from_state][tok.index()];
            if *old < 0 {
              match resolve_sr_conflict(&builder.grammar, !*old as u32, tok.id()) {
                SrConflictResolution::Shift => *old = to_state as i32 + 1,
                SrConflictResolution::Reduce => {
                  // do nothing
                }
                SrConflictResolution::Error => *old = 0,
                SrConflictResolution::Conflict => {
                  errors.push(make_sr_conflict_error(
                    builder,
                    &state.items,
                    tok.id(),
                    !*old as u32));
                }
              }
            } else {
              assert!(*old == 0 || *old == to_state as i32 + 1);
              *old = to_state as i32 + 1;
            }
          }
          Symbol::Nonterminal(nt) => {
            goto[from_state][nt.index()] = to_state + 1;
          }
        }
      } else {
        // reduce
        for lookahead in item.lookaheads.iter() {
          let old = &mut action[from_state][lookahead as usize];
          if *old > 0 {
            match resolve_sr_conflict(&builder.grammar, prod as u32, lookahead) {
              SrConflictResolution::Shift => {
                // do nothing
              }
              SrConflictResolution::Reduce => {
                *old = !(prod as i32);
              }
              SrConflictResolution::Error => {
                *old = 0;
              }
              SrConflictResolution::Conflict => {
                errors.push(make_sr_conflict_error(
                  builder,
                  &state.items,
                  lookahead as u32,
                  prod as u32));
              }
            }
          } else if *old < 0 {
            errors.push(make_rr_conflict_error(
              builder,
              &state.items,
              lookahead as u32,
              !*old as u32,
              prod as u32));
          } else {
            *old = !(prod as i32);
          }
        }
      }
    }
  }

  if errors.is_empty() {
    Ok((action, goto))
  } else {
    Err(errors)
  }
}

enum SrConflictResolution {
  Shift,
  Reduce,
  Error,
  Conflict,
}

fn resolve_sr_conflict(
  grammar: &LoweredGrammar,
  prod_ix: u32,
  tok: u32,
) -> SrConflictResolution {
  match (&grammar.prods[prod_ix as usize].prec, grammar.token_precs.get(&tok)) {
    (Some(prec1), Some((assoc, prec2))) => {
      if prec1 == prec2 {
        match assoc {
          Assoc::LeftAssoc => SrConflictResolution::Reduce,
          Assoc::RightAssoc => SrConflictResolution::Shift,
          Assoc::NonAssoc => SrConflictResolution::Error,
        }
      } else if prec1 < prec2 {
        SrConflictResolution::Shift
      } else {
        SrConflictResolution::Reduce
      }
    }
    _ => SrConflictResolution::Conflict,
  }
}

fn make_rr_conflict_error<T: LrComputation>(
  builder: &Builder<T>,
  item_set: &[Item],
  lookahead: u32,
  reduce1: u32,
  reduce2: u32,
) -> Error {
  let lookahead = builder.grammar.tokens[&lookahead].clone();
  let reduce1 = builder.grammar.prods[reduce1 as usize].to_string(
    &builder.grammar);
  let reduce2 = builder.grammar.prods[reduce2 as usize].to_string(
    &builder.grammar);

  let state_items = item_set.iter()
    .map(|item| {
      let mut buf = String::new();
      builder.fmt_item(item, &mut buf).unwrap();
      buf
    })
    .collect();

  Error::ReduceReduceConflict(ReduceReduceConflictError {
    lookahead,
    state_items,
    reduce1,
    reduce2,
  })
}

fn make_sr_conflict_error<T: LrComputation>(
  builder: &Builder<T>,
  item_set: &[Item],
  token: u32,
  reduce_prod: u32,
) -> Error {
  let shift = builder.grammar.tokens[&token].clone();

  let state_items = item_set.iter()
    .map(|item| {
      let mut buf = String::new();
      builder.fmt_item(item, &mut buf).unwrap();
      buf
    })
    .collect();

  let reduce = builder.grammar.prods[reduce_prod as usize].to_string(&builder.grammar);

  Error::ShiftReduceConflict(ShiftReduceConflictError {
    state_items,
    shift,
    reduce,
  })
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