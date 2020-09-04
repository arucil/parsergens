use std::hash::Hash;
use super::dfa::*;
use crate::Map;

pub struct DfaBuilder<A, V> {
  counter: usize,
  transitions: Map<(State, A), State>,
  accept_states: Map<State, V>,
}

impl<A: Eq + Hash, V> DfaBuilder<A, V> {
  pub fn new() -> Self {
    Self {
      counter: 0,
      transitions: Map::new(),
      accept_states: Map::new(),
    }
  }

  pub fn build(self, start: State) -> Dfa<A, V> {
    Dfa {
      start: start.0,
      transitions: self.transitions,
      accept_states: self.accept_states,
    }
  }

  pub fn state(&mut self) -> State {
    self.counter += 1;
    State(self.counter)
  }

  pub fn transition(&mut self, src: State, dest: State, c: A) {
    self.transitions.insert((src, c), dest);
  }

  pub fn accept(&mut self, state: State, value: V) {
    self.accept_states.insert(state, value);
  }
}