use std::collections::VecDeque;
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId, Assoc, HashMap};
use bittyset::{BitSet, bitset};
use std::fmt::{self, Write};
use crate::first::FirstAndNullable;
use crate::{Error, ShiftReduceConflictError, ReduceReduceConflictError, EntryPoint};
use crate::builder::{Builder, StateStore};

type ClrBuilder<'a> = Builder<'a, ()>;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lr1Item {
  prod_ix: usize,
  dot_ix: usize,
  lookahead: u32,
}

pub fn build_states(
  builder: &mut ClrBuilder,
  grammar: &LoweredGrammar,
) -> HashMap<String, EntryPoint> {
  let fan = crate::first::compute(grammar);
  let mut entry_points = HashMap::default();

  for &start_nt in &grammar.start_nts {
    let start_state = start(builder, grammar, &fan, start_nt);
    let start_prod_ix = grammar.nt_metas[&start_nt].range.start;
    let real_start_nt =
      match grammar.prods[start_prod_ix].symbols[0] {
        Symbol::Nonterminal(nt) => nt,
        _ => unreachable!(),
      };
    let nt_name = grammar.nts.get(&real_start_nt).unwrap().clone();
    entry_points.insert(nt_name, EntryPoint {
      real_start_nt: real_start_nt.id(),
      start_state,
      accept_prod: start_prod_ix,
    });
  }

  entry_points
}

/// returns ACTION table and GOTO table.
/// 
/// entry in ACTION table:
/// - positive: shift
/// - negative: reduce
/// - zero: error
///
/// entry in GOTO table:
/// - positive: goto
/// - zero: error
pub fn build_tables(
  builder: &ClrBuilder,
) -> Result<(Vec<Vec<i32>>, Vec<Vec<u32>>), Error> {
  let num_states = builder.state_store.states.len();
  let mut action = vec![vec![0i32; builder.eof as usize + 1]; num_states];
  let mut goto = vec![vec![0u32; builder.grammar.nts.len()]; num_states];

  for (from_state, tx) in builder.state_store.goto.iter().enumerate() {
    let (item_set, _) = &builder.state_store.states[from_state];
    for item_ix in item_set.iter() {
      //let item = builder.item_store.items[item_ix];
      let item = decode_item(builder, item_ix);
      let symbols = &builder.grammar.prods[item.prod_ix].symbols;
      // shift
      if item.dot_ix < symbols.len() {
        let sym = &symbols[item.dot_ix];
        let to_state = tx[sym];
        match sym {
          Symbol::Token(tok) => {
            let old = &mut action[from_state][tok.id() as usize];
            if *old < 0 {
              match resolve_sr_conflict(&builder.grammar, !*old as usize, tok.id()) {
                SrConflictResolution::Shift => *old = tok.id() as i32 + 1,
                SrConflictResolution::Reduce => {
                  // do nothing
                }
                SrConflictResolution::Error => *old = 0,
                SrConflictResolution::Conflict => return Err(make_sr_conflict_error(
                  builder,
                  item_set,
                  tok.id(),
                  !*old as usize))
              }
            } else {
              *old = to_state as i32 + 1;
            }
          }
          Symbol::Nonterminal(nt) => {
            goto[from_state][nt.id() as usize] = to_state + 1;
          }
        }
      } else {
        // reduce
        let old = &mut action[from_state][item.lookahead as usize];
        if *old > 0 {
          match resolve_sr_conflict(&builder.grammar, item.prod_ix, item.lookahead) {
            SrConflictResolution::Shift => {
              // do nothing
            }
            SrConflictResolution::Reduce => {
              *old = !(item.prod_ix as i32);
            }
            SrConflictResolution::Error => {
              *old = 0;
            }
            SrConflictResolution::Conflict => return Err(make_sr_conflict_error(
              builder,
              item_set,
              item.lookahead,
              item.prod_ix))
          }
        } else if *old < 0 {
          return Err(make_rr_conflict_error(
            builder,
            item_set,
            item.lookahead,
            !*old as usize,
            item.prod_ix));
        } else {
          *old = !(item.prod_ix as i32);
        }
      }
    }
  }

  Ok((action, goto))
}

enum SrConflictResolution {
  Shift,
  Reduce,
  Error,
  Conflict,
}

fn resolve_sr_conflict(
  grammar: &LoweredGrammar,
  prod_ix: usize,
  tok: u32,
) -> SrConflictResolution {
  match (&grammar.prods[prod_ix].prec, grammar.token_precs.get(&tok)) {
    (Some((_, prec1)), Some((assoc2, prec2))) => {
      if prec1 == prec2 {
        match assoc2 {
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

fn make_rr_conflict_error(
  builder: &ClrBuilder,
  item_set: &BitSet,
  lookahead: u32,
  reduce1: usize,
  reduce2: usize,
) -> Error {
  let lookahead = builder.grammar.tokens[&lookahead].clone();
  let reduce1 = builder.grammar.prods[reduce1].to_string(
    &builder.grammar);
  let reduce2 = builder.grammar.prods[reduce2].to_string(
    &builder.grammar);

  let state_items = item_set.iter()
    .map(|item_ix| {
      let mut buf = String::new();
      builder.fmt_item(item_ix, &mut buf).unwrap();
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

fn make_sr_conflict_error(
  builder: &ClrBuilder,
  item_set: &BitSet,
  token: u32,
  reduce_prod: usize,
) -> Error {
  let shift = builder.grammar.tokens[&token].clone();

  let state_items = item_set.iter()
    .map(|item_ix| {
      let mut buf = String::new();
      builder.fmt_item(item_ix, &mut buf).unwrap();
      buf
    })
    .collect();

  let reduce = builder.grammar.prods[reduce_prod].to_string(&builder.grammar);

  Error::ShiftReduceConflict(ShiftReduceConflictError {
    state_items,
    shift,
    reduce,
  })
}

fn start(
  builder: &mut ClrBuilder,
  grammar: &LoweredGrammar,
  fan: &FirstAndNullable,
  nt: NonterminalId
) -> u32 {
  let start_prod = grammar.nt_metas[&nt].range.start;
  let start_item = store_item(builder,
    Lr1Item {
      prod_ix: start_prod,
      dot_ix: 0,
      lookahead: builder.eof,
    });
  let mut start_item_set = bitset![start_item as usize];

  closure(builder, grammar, fan, &mut start_item_set);
  let start_state = store_state(&mut builder.state_store, start_item_set);

  let mut queue = VecDeque::new();
  queue.push_back(start_state);

  while let Some(from_state) = queue.pop_front() {
    let (item_set, _) = &builder.state_store.states[from_state as usize];
    let mut to_states = Map::<Symbol, BitSet>::default();

    for item_ix in item_set.iter() {
      //let item = builder.item_store.items[item_ix];
      let item = decode_item(builder, item_ix);
      let symbols = &grammar.prods[item.prod_ix].symbols;
      if item.dot_ix == symbols.len() {
        continue;
      }

      let new_item = store_item(builder, Lr1Item {
        prod_ix: item.prod_ix,
        dot_ix: item.dot_ix + 1,
        lookahead: item.lookahead,
      });
      let to_item_set = to_states.entry(symbols[item.dot_ix].clone())
        .or_default();
      to_item_set.insert(new_item as usize);
    }

    for (sym, mut to_item_set) in to_states {
      closure(builder, grammar, fan, &mut to_item_set);

      if let Some(&to_state) = builder.state_store.state_indices.get(&to_item_set) {
        builder.state_store.goto[from_state as usize].insert(sym, to_state);
        continue;
      }

      let to_state = store_state(&mut builder.state_store, to_item_set);

      queue.push_back(to_state);
      builder.state_store.goto[from_state as usize].insert(sym, to_state);
    }
  }

  start_state
}

fn closure(
  builder: &mut ClrBuilder,
  grammar: &LoweredGrammar,
  fan: &FirstAndNullable,
  item_set: &mut BitSet,
) {
  let mut new = item_set.iter().collect::<Vec<_>>();

  while let Some(i) = new.pop() {
    //let item = &builder.item_store.items[i];
    let item = decode_item(builder, i);
    let symbols = &grammar.prods[item.prod_ix].symbols;
    if item.dot_ix < symbols.len() {
      let nt = match &symbols[item.dot_ix] {
        Symbol::Token(_) => continue,
        Symbol::Nonterminal(nt) => nt,
      };

      let mut first = BitSet::new();
      let mut rest_nullable = true;
      for sym in &symbols[item.dot_ix + 1..] {
        match sym {
          Symbol::Token(tok) => {
            first.insert(tok.id() as usize);
            rest_nullable = false;
            break;
          }
          Symbol::Nonterminal(nt) => {
            first.union_with(&fan.first[nt]);
            if !fan.nullable.contains(nt.id() as usize) {
              rest_nullable = false;
              break;
            }
          }
        }
      }

      if rest_nullable {
        first.insert(item.lookahead as usize);
      }

      for prod_ix in grammar.nt_metas[nt].range.clone() {
        for lookahead in first.iter() {
          let item = store_item(builder, Lr1Item {
            prod_ix,
            dot_ix: 0,
            lookahead: lookahead as u32,
          });

          if item_set.insert(item as usize) {
            new.push(item as usize);
          }
        }
      }
    }
  }
}

/// return state index and if lookahead set of the state has changed.
fn store_state(
  state_store: &mut StateStore<()>,
  item_set: BitSet,
) -> u32 {
  if let Some(&ix) = state_store.state_indices.get(&item_set) {
    ix
  } else {
    let ix = state_store.states.len() as u32;
    state_store.states.push((item_set.clone(), ()));
    state_store.state_indices.insert(item_set, ix);
    state_store.goto.push(HashMap::default());
    ix
  }
}

fn store_item(
  builder: &ClrBuilder,
  item: Lr1Item,
) -> u32 {
  let item_ix = (item.prod_ix * builder.max_nsym_p1 + item.dot_ix)
    * (builder.eof as usize + 1)
    + item.lookahead as usize;
  //item_store.items.insert(item_ix);
  item_ix as u32
}

fn decode_item(
  builder: &ClrBuilder,
  item_ix: usize,
) -> Lr1Item {
  let lookahead = (item_ix % (builder.eof as usize + 1)) as u32;
  let prod_and_dot = item_ix / (builder.eof as usize + 1);
  let dot_ix = prod_and_dot % builder.max_nsym_p1;
  let prod_ix = prod_and_dot / builder.max_nsym_p1;

  Lr1Item {
    prod_ix,
    dot_ix,
    lookahead,
  }
}

#[cfg(test)]
impl<'a> ClrBuilder<'a> {
  fn states(
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
    for (state, (item_set, _)) in self.state_store.states.iter().enumerate() {
      let state = state as u32;
      write!(fmt, "State {}", state)?;
      if entry_points.values().find(|x| x.start_state == state).is_some() {
        write!(fmt, " (start)")?;
      }
      writeln!(fmt)?;

      for item_ix in item_set.iter() {
        self.fmt_item(item_ix, fmt)?;

        writeln!(fmt)?;
      }

      writeln!(fmt)?;
    }

    Ok(())
  }
}

impl<'a> ClrBuilder<'a> {
  fn fmt_item(
    &self,
    item_ix: usize,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    //let item = self.item_store.items[item_ix];
    let item = decode_item(self, item_ix);
    let nt = self.grammar.prods[item.prod_ix].nt;
    let symbols = &self.grammar.prods[item.prod_ix].symbols;

    write!(fmt, "{} ->", self.grammar.nts[&nt])?;

    for (i, sym) in symbols.iter().enumerate() {
      if i == item.dot_ix {
        write!(fmt, " .")?;
      }

      match sym {
        Symbol::Token(token) => {
          let name = self.grammar.tokens.get(token).map(|s|s.as_str()).unwrap_or("$");
          write!(fmt, " {}", name)?;
        }
        Symbol::Nonterminal(nt) => {
          let name = &self.grammar.nts[nt];
          write!(fmt, " {}", name)?;
        }
      }
    }

    if item.dot_ix == symbols.len() {
      write!(fmt, " .")?;
    }

    write!(fmt, "      {}", self.grammar.tokens.get(&item.lookahead).map(|s|s.as_str()).unwrap_or("$"))?;

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_snapshot, assert_debug_snapshot};
  use grammar::LoweredGrammar;
  use crate::augment;

  fn prepare(input: &str) -> LoweredGrammar {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let grammar = augment::augment(grammar);

    grammar
  }

  fn merge_action_goto((action, goto): (Vec<Vec<i32>>, Vec<Vec<u32>>)) -> Vec<Vec<i32>> {
    action.into_iter().zip(goto).map(|(mut row1, row2)| {
      row1.extend(row2.into_iter().map(|x| x as i32));
      row1
    })
    .collect()
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
    let grammar = prepare(SIMPLE);
    let mut builder = ClrBuilder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn simple_action_goto() {
    let grammar = prepare(SIMPLE);
    let mut builder = ClrBuilder::new(&grammar);
    let _start_nts = build_states(&mut builder, &grammar);
    let tables = merge_action_goto(build_tables(&builder).unwrap());

    assert_debug_snapshot!(tables);
  }
}
