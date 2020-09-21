use std::hash::Hash;
use std::collections::VecDeque;
use bit_set::BitSet;
use grammar::{ TokenId, NonterminalId, LoweredGrammar, Symbol, Assoc, BiMap, Map };
use std::marker::PhantomData;
use crate::Error;
use crate::ffn::Ffn;

pub trait LrCalculation {
  type Item: LrItem;

  fn start_item(
    start_prod_ix: usize,
    eof_token: TokenId,
  ) -> Self::Item;

  fn next_item(
    item: &Self::Item,
  ) -> Self::Item;

  fn closure_step<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    prev: &Self::Item,
    action: F,
  )
    where F: FnMut(Self::Item);

  fn reduce_tokens<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    item: &Self::Item,
    action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>;

  /// return a Map from old states to new states.
  fn merge_states(
    _grammar: &LoweredGrammar,
    _states: &mut BiMap<BitSet, u32>,
    _items: &mut BiMap<Self::Item, usize>,
  ) -> Option<Map<u32, u32>> {
    None
  }
}

pub trait LrItem: Eq + Hash + Ord + Clone {
  fn prod_ix(&self) -> usize;
  fn dot_ix(&self) -> usize;

  #[cfg(test)]
  fn fmt(
    &self,
    grammar: &LoweredGrammar,
    f: &mut impl std::fmt::Write
  ) -> std::fmt::Result;
}

pub struct Builder<'a, T>
  where T: LrCalculation
{
  grammar: &'a LoweredGrammar,
  ffn: Ffn,
  states: BiMap<BitSet, u32>,
  items: BiMap<T::Item, usize>,
  /// state -> token -> (shift state, reduce production)
  action: Map<u32, Map<u32, ActionEntry>>,
  /// state -> non-terminal -> next state
  goto: Map<u32, Map<u32, u32>>,
  goto_row_len: usize,
  pub start: Map<String, (u32, u32)>,
  eof_token: TokenId,
  _marker: PhantomData<T>,
}

#[derive(Default)]
struct ActionEntry {
  shift: Option<u32>,
  reduce: Option<u32>,
}

impl<'a, T> Builder<'a, T>
  where T: LrCalculation
{
  pub fn new(grammar: &'a LoweredGrammar, eof_token: TokenId, ffn: Ffn) -> Self {
    Builder {
      grammar: &grammar,
      ffn,
      states: BiMap::new(),
      items: BiMap::new(),
      action: Map::new(),
      goto: Map::new(),
      goto_row_len: grammar.nts.len(),
      start: Map::new(),
      eof_token,
      _marker: PhantomData,
    }
  }

  pub fn build(&mut self) -> Result<(), Error> {
    for &start_nt in &self.grammar.start_nts {
      let start_state = self.start(start_nt)?;
      let start_prod_ix = self.grammar.nt_metas[&start_nt].range.start;
      let real_start_nt =
        match self.grammar.prods[start_prod_ix].symbols[0] {
          Symbol::Nonterminal(nt) => nt,
          _ => unreachable!(),
        };
      let nt_name = self.grammar.nts.get(&real_start_nt).unwrap().clone();
      self.start.insert(nt_name, (real_start_nt.id(), start_state));
    }

    if let Some(map) = T::merge_states(
      &self.grammar, &mut self.states, &mut self.items)
    {
      self.map_merged_states(map)?;
    }

    self.resolve_conflicts()?;

    Ok(())
  }

  fn map_merged_states(&mut self, map: Map<u32, u32>) -> Result<(), Error> {
    for (_, start_state) in self.start.values_mut() {
      *start_state = map[&*start_state];
    }

    let mut new_action = Map::<u32, Map<u32, ActionEntry>>::new();

    for (state, tx) in &self.action {
      for (token, entry) in tx {
        let new_state = map[state];
        let new_entry = new_action.entry(new_state)
          .or_default()
          .entry(*token)
          .or_default();

        if let Some(shift) = &entry.shift {
          let new_shift = map[shift];
          if let Some(last_new_shift) = new_entry.shift {
            assert!(new_shift == last_new_shift);
          } else {
            new_entry.shift = Some(new_shift);
          }
        }

        if let Some(reduce) = entry.reduce {
          if let Some(last_new_reduce) = new_entry.reduce {
            if last_new_reduce != reduce {
              return Err(Error::ReduceReduceConflict);
            }
          }
          new_entry.reduce = Some(reduce);
        }
      }
    }

    self.action = new_action;

    let mut new_goto = Map::<u32, Map<u32, u32>>::new();

    for (state, tx) in &self.goto {
      for (nt, next_state) in tx {
        let new_state = map[state];
        let new_next_state = map[next_state];
        let new_entry = new_goto.entry(new_state)
          .or_default()
          .entry(*nt)
          .or_default();
        *new_entry = new_next_state;
      }
    }

    self.goto = new_goto;

    Ok(())
  }

  fn resolve_conflicts(&mut self) -> Result<(), Error> {
    for (_, tx) in &mut self.action {
      for (_, ActionEntry { shift, reduce }) in tx {
        if let (&&mut Some(shift_state), &&mut Some(reduce_prod)) = (&shift, &reduce) {
          let (reduce_assoc, reduce_prec) = self.grammar.prods[reduce_prod as usize]
            .prec
            .ok_or(Error::ShiftReduceConflict)?;

          let mut shift_prec = None;
          for item in self.states.get_by_right(&shift_state).unwrap().iter() {
            let item = self.items.get_by_right(&item).unwrap();
            if item.dot_ix() == 0 {
              continue;
            }
            if let Some((assoc, prec)) = self.grammar.prods[item.prod_ix()].prec {
              if let Some((last_assoc, last_prec)) = shift_prec {
                if last_prec != prec {
                  return Err(Error::PrecConflict);
                } else if last_assoc != assoc {
                  return Err(Error::AssocConflict);
                }
              } else {
                shift_prec = Some((assoc, prec));
              }
            }
          }

          if let Some((shift_assoc, shift_prec)) = shift_prec {
            if shift_prec == reduce_prec {
              if shift_assoc == reduce_assoc {
                match shift_assoc {
                  Assoc::LeftAssoc => *shift = None,
                  Assoc::RightAssoc => *reduce = None,
                  Assoc::NonAssoc => {
                    *shift = None;
                    *reduce = None;
                  }
                }
              } else {
                *shift = None;
                *reduce = None;
              }
            } else if shift_prec > reduce_prec {
              *reduce = None;
            } else {
              *shift = None;
            }
          } else {
            return Err(Error::ShiftReduceConflict);
          }
        }
      }
    }

    Ok(())
  }

  pub fn build_action_table(&self) -> Vec<Vec<i32>> {
    let mut action = vec![vec![0i32; self.eof_token.id() as usize + 1]; self.states.len()];

    for (state, tx) in &self.action {
      let row = &mut action[*state as usize];
      for (token, entry) in tx {
        if let Some(new_state) = entry.shift {
          assert!(entry.reduce.is_none());
          row[*token as usize] = new_state as i32 + 1;
        } else if let Some(prod) = entry.reduce {
          row[*token as usize] = if prod == std::i32::MAX as u32 {
            std::i32::MIN
          } else {
            !(prod as i32)
          };
        }
      }
    }

    action
  }

  pub fn build_goto_table(&self) -> Vec<Vec<u32>> {
    let mut goto = vec![vec![0u32; self.goto_row_len]; self.states.len()];

    for (state, tx) in &self.goto {
      let row = &mut goto[*state as usize];
      for (nt, next_state) in tx {
        row[*nt as usize] = *next_state + 1;
      }
    }

    goto
  }

  fn start(&mut self, nt: NonterminalId) -> Result<u32, Error> {
    let start_prod = self.grammar.nt_metas[&nt].range.start;
    let start_item = store_item(&mut self.items,
      T::start_item(start_prod, self.eof_token));
    let start_state_set = self.closure({
      let mut set = BitSet::new();
      set.insert(start_item);
      set
    });
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
          self.reduce(from_state, &item)?;
          continue;
        }

        if item.dot_ix() == 1 && item.prod_ix() == start_prod {
          self.accept(from_state);
          continue;
        }

        let new_item = store_item(&mut self.items, T::next_item(&item));
        to_states.entry(symbols[item.dot_ix()].clone()).or_default().insert(new_item);
      }

      for (sym, to_state) in to_states {
        let to_state = self.closure(to_state);
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

  fn closure(&mut self, initial: BitSet) -> BitSet {
    let mut result = initial.clone();
    let mut last = initial;

    loop {
      let mut new = BitSet::new();
      for i in last.iter() {
        let item = self.items.get_by_right(&i).unwrap().clone();
        let symbols = &self.grammar.prods[item.prod_ix()].symbols;
        if item.dot_ix() < symbols.len() {
          let items = &mut self.items;

          T::closure_step(&self.grammar, &self.ffn, &item, |new_item| {
            let item = store_item(items, new_item);
            if !result.contains(item) {
              new.insert(item);
            }
          })
        }
      }

      if new.is_empty() {
        break;
      }

      result.union_with(&new);

      last = new;
    }

    result
  }

  fn state(&mut self, set: &BitSet) -> u32 {
    if let Some(&state) = self.states.get_by_left(set) {
      state
    } else {
      let state = self.states.len() as u32;
      self.states.insert(set.clone(), state);

      self.action.insert(state, Map::new());
      self.goto.insert(state, Map::new());

      state
    }
  }

  fn reduce(&mut self, from_state: u32, item: &T::Item) -> Result<(), Error> {
    let action = &mut self.action;

    T::reduce_tokens(&self.grammar, &self.ffn, item, |token| {
      let entry = action.get_mut(&from_state).unwrap()
        .entry(token)
        .or_default();
      if entry.reduce.is_some() {
        return Err(Error::ReduceReduceConflict);
      }
      assert!(entry.shift.is_none());

      entry.reduce = Some(item.prod_ix() as u32);
      Ok(())
    })
  }

  fn accept(&mut self, from_state: u32) {
    let entry = self.action.get_mut(&from_state).unwrap()
      .entry(self.eof_token.id())
      .or_default();
    entry.reduce = Some(std::i32::MAX as u32);
  }
}

fn store_item<I>(
  items: &mut BiMap<I, usize>,
  item: I
) -> usize
  where I: LrItem
{
  if let Some(&item) = items.get_by_left(&item) {
    item
  } else {
    let len = items.len();
    items.insert(item, len);
    len
  }
}

#[cfg(test)]
use std::fmt::{self, Write};

#[cfg(test)]
impl<'a, T> Builder<'a, T>
  where T: LrCalculation
{
  pub fn states(self) -> String {
    let mut buf = String::new();
    self.fmt_states(&mut buf).unwrap();
    buf
  }

  fn fmt_states(self, fmt: &mut impl Write) -> fmt::Result {
    let mut states = self.states.into_iter().collect::<Vec<_>>();
    states.sort_by_key(|(_, x)| *x);

    for (state_set, state) in states {
      write!(fmt, "State {}", state)?;
      if self.start.values().find(|x| x.1 == state).is_some() {
        write!(fmt, " (start)")?;
      }
      writeln!(fmt)?;

      for item in state_set.iter() {
        let item = self.items.get_by_right(&item).unwrap();
        item.fmt(&self.grammar, fmt)?;

        writeln!(fmt)?;
      }

      writeln!(fmt)?;
    }

    Ok(())
  }

  pub fn action_goto(self) -> Vec<Vec<i32>> {
    let action = self.build_action_table();
    let goto = self.build_goto_table();

    action.into_iter().zip(goto).map(|(mut row1, row2)| {
      row1.extend(row2.into_iter().map(|x| x as i32));
      row1
    }).collect()
  }
}