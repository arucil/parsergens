use std::hash::Hash;
use super::dfa::*;
use crate::Map;

pub struct DfaBuilder<A, V> {
  counter: u32,
  transitions: Map<(State, A), State>,
  accept_states: Map<State, V>,
}

impl<A, V> DfaBuilder<A, V>
  where A: Eq + Hash + Clone,
        V: Eq + Hash + Clone,
{
  pub fn new() -> Self {
    Self {
      counter: 0,
      transitions: Map::default(),
      accept_states: Map::default(),
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
    let i = self.counter;
    self.counter += 1;
    State(i)
  }

  pub fn transition(&mut self, src: State, dest: State, c: A) {
    self.transitions.insert((src, c), dest);
  }

  pub fn accept(&mut self, state: State, value: V) {
    self.accept_states.insert(state, value);
  }
}