use std::hash::Hash;
use super::nfa::*;
use crate::{Map, Set};

pub struct NfaBuilder<A, P, V> {
  counter: u32,
  transitions: Map<(State, Option<A>), Set<State>>,
  state_letters: Map<State, Set<A>>,
  accept_states: Map<State, (P, V)>,
}

impl<A, P, V> NfaBuilder<A, P, V>
  where A: Eq + Hash + Copy,
        P: PartialOrd
{
  pub fn new() -> Self {
    Self {
      counter: 0,
      transitions: Map::new(),
      state_letters: Map::new(),
      accept_states: Map::new(),
    }
  }

  pub fn build(self) -> Nfa<A, P, V> {
    Nfa {
      transitions: self.transitions,
      state_letters: self.state_letters,
      accept_states: self.accept_states,
    }
  }

  pub fn state(&mut self) -> State {
    let i = self.counter;
    self.counter += 1;
    State(i)
  }

  pub fn transition(&mut self, src: State, dest: State, c: Option<A>) {
    self.transitions.entry((src, c)).or_default().insert(dest);
    if let Some(c) = c {
      self.state_letters.entry(src).or_default().insert(c);
    }
  }

  pub fn accept(&mut self, state: State, priority: P, value: V) {
    self.accept_states.insert(state, (priority, value));
  }
}