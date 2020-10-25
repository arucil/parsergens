use grammar::{Symbol, LoweredGrammar, Assoc};
use crate::{Error, ShiftReduceConflictError, ReduceReduceConflictError};
use super::{Builder, LrComputation, Item, decode_item};

/// Generates ACTION table and GOTO table.
/// 
/// entry in `ACTION[state][token]` table:
/// - positive: shift (shift state is never zero, since the starting state is state 0)
/// - negative: reduce (- reduce production - 1)
/// - zero: error
///
/// entry in `GOTO[nt][state]` table:
/// - positive: goto (goto state is never zero, since the starting state is state 0)
/// - zero: error
pub fn gen_tables<T: LrComputation>(
  builder: &Builder<T>,
) -> Result<(Vec<Vec<i32>>, Vec<Vec<u32>>), Vec<Error>> {
  const ERROR_ACTION: i32 = -1;

  let num_states = builder.states.len();
  let mut action = vec![vec![0i32; builder.eof as usize + 1]; num_states];
  let mut goto = vec![vec![0u32; num_states]; builder.grammar.nts.len()];
  let mut errors = vec![];

  for (from_state, (_, state)) in builder.states.iter().enumerate() {
    'iter_items: for item in &state.items {
      //let item = builder.item_store.items[item_ix];
      let (prod, dot) = decode_item(builder.max_nsym_p1, item.key);
      let symbols = &builder.grammar.prods[prod].symbols;
      // shift
      if dot < symbols.len() {
        let sym = &symbols[dot];
        let to_state = state.transitions[sym];
        assert!(to_state != 0);

        match sym {
          Symbol::Token(tok) => {
            let old = &mut action[from_state][tok.index()];
            if *old == ERROR_ACTION {
              continue 'iter_items;
            }

            if *old < 0 {
              match resolve_sr_conflict(&builder.grammar, (-*old - 2) as u32, tok.id()) {
                SrConflictResolution::Shift => *old = to_state as i32,
                SrConflictResolution::Reduce => {
                  // do nothing
                }
                SrConflictResolution::Error => *old = ERROR_ACTION,
                SrConflictResolution::Conflict => {
                  errors.push(make_sr_conflict_error(
                    builder,
                    &state.items,
                    tok.id(),
                    (-*old - 2) as u32));
                  *old = ERROR_ACTION;
                }
              }
            } else {
              assert!(*old == 0 || *old == to_state as i32);
              *old = to_state as i32;
            }
          }
          Symbol::Nonterminal(nt) => {
            goto[nt.index()][from_state] = to_state;
          }
        }
      } else {
        // reduce
        for lookahead in item.lookaheads.iter() {
          let old = &mut action[from_state][lookahead as usize];
          if *old == ERROR_ACTION {
            continue 'iter_items;
          }

          if *old > 0 {
            match resolve_sr_conflict(&builder.grammar, prod as u32, lookahead) {
              SrConflictResolution::Shift => {
                // do nothing
              }
              SrConflictResolution::Reduce => {
                *old = -(prod as i32 + 2);
              }
              SrConflictResolution::Error => {
                *old = ERROR_ACTION;
              }
              SrConflictResolution::Conflict => {
                errors.push(make_sr_conflict_error(
                  builder,
                  &state.items,
                  lookahead as u32,
                  prod as u32));
                *old = ERROR_ACTION;
              }
            }
          } else if *old < 0 {
            errors.push(make_rr_conflict_error(
              builder,
              &state.items,
              lookahead as u32,
              (-*old - 2) as u32,
              prod as u32));
            *old = ERROR_ACTION;
          } else {
            *old = -(prod as i32 + 2);
          }
        }
      }
    }

    for x in &mut action[from_state] {
      // reset ERROR_ACTION to zero, and change the base of reduce actions from
      // -2 to -1.
      if *x < 0 {
        *x += 1;
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
  let lookahead = builder.grammar.tokens.get(&lookahead)
    .map(|s| s.as_str())
    .unwrap_or("$")
    .to_owned();
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