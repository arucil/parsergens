use std::hash::Hash;
use super::nfa::*;
use super::{Map, Set};

pub struct NfaBuilder<A, P, V> {
  counter: usize,
  transitions: Map<(State, Option<A>), Set<State>>,
  alphabets: Map<State, Set<A>>,
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
      alphabets: Map::new(),
      accept_states: Map::new(),
    }
  }

  pub fn build(self) -> Nfa<A, P, V> {
    Nfa {
      transitions: self.transitions,
      alphabets: self.alphabets,
      accept_states: self.accept_states,
    }
  }

  pub fn state(&mut self) -> State {
    self.counter += 1;
    State(self.counter)
  }

  pub fn transition(&mut self, src: State, dest: State, c: Option<A>) {
    self.transitions.entry((src, c)).or_default().insert(dest);
    if let Some(c) = c {
      self.alphabets.entry(src).or_default().insert(c);
    }
  }

  pub fn accept(&mut self, state: State, priority: P, value: V) {
    self.accept_states.insert(state, (priority, value));
  }
}