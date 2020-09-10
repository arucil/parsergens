//! DFA table compression schem from
//! https://stackoverflow.com/questions/29139162/dfa-state-transition-table-compression

use bitvec::prelude::*;
use super::dfa::Dfa;
use crate::Map;

pub use super::dfa::State;

#[derive(Debug)]
pub struct TabularDfa<V> {
  start: u32,
  state_base: Vec<usize>,
  transitions: Vec<(u32, u32)>,
  accept_states: Map<State, V>,
}

impl<V> TabularDfa<V> {
  pub fn start(&self) -> State {
    State(self.start)
  }

  pub fn transition(&self, state: State, c: u32) -> Option<State> {
    let tx = self.transitions[self.state_base[state.0 as usize] + c as usize];
    if tx.0 == state.0 && tx.1 != 0 {
      Some(State(tx.1 - 1))
    } else {
      None
    }
  }

  /// if the state is accepting state, return the value.
  pub fn result(&self, state: State) -> Option<&V> {
    self.accept_states.get(&state)
  }
}

impl<V> From<Dfa<u32, V>> for TabularDfa<V> {
  fn from(dfa: Dfa<u32, V>) -> Self {
    let last_letter = dfa.transitions.keys()
      .map(|(_, c)| *c).max().unwrap();
    let last_state = dfa.transitions.iter()
      .map(|((s1, _), s2)| s1.0.max(s2.0))
      .max().unwrap()
      + 1;

    let mut state_base = vec![];
    let mut transitions = vec![];
    let mut row_mask = bitvec![0; last_letter as usize + 1];
    let mut base = 0;

    for state in 0..=last_state {
      let mut row_bits = BitVec::<LocalBits, usize>::with_capacity(
        (last_letter + 1) as usize);
      let mut row = vec![];

      for c in 0..=last_letter {
        if let Some(&State(next)) = dfa.transitions.get(&(State(state), c)) {
          row.push(next + 1);
          row_bits.push(true);
        } else {
          row.push(0);
          row_bits.push(false);
        }
      }

      loop {
        let overlap = row_mask.clone() & row_bits.clone();
        if overlap.not_any() {
          row_mask |= row_bits;
          break;
        }

        base += 1;
        row_mask.set(0, false);
        row_mask.rotate_left(1);
      }

      state_base.push(base);

      transitions.resize(base + last_letter as usize + 1, (state, 0));

      for (i, x) in row.into_iter().enumerate() {
        if x != 0 {
          transitions[base + i] = (state, x);
        }
      }

      while row_mask[0] {
        base += 1;
        row_mask.set(0, false);
        row_mask.rotate_left(1);
      }
    }

    Self {
      start: dfa.start,
      state_base,
      transitions,
      accept_states: dfa.accept_states,
    }
  }
}