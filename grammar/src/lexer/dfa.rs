use std::hash::Hash;
use std::fmt::{self, Debug};
use super::dfa_builder::DfaBuilder;
use crate::Map;

#[derive(Debug)]
pub struct Dfa<A, V> {
  pub start: usize,
  pub transitions: Map<(State, A), State>,
  pub accept_states: Map<State, V>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct State(pub usize);

impl<A: Eq + Hash, V> Dfa<A, V> {
  pub fn start(&self) -> State {
    State(self.start)
  }

  pub fn transition(&self, state: State, c: A) -> Option<State> {
    self.transitions.get(&(state, c)).cloned()
  }

  /// if the state is accepting state, return the value.
  pub fn result(&self, state: State) -> Option<&V> {
    self.accept_states.get(&state)
  }

  pub fn builder() -> DfaBuilder<A, V> {
    DfaBuilder::new()
  }
}

impl Debug for State {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "State({})", self.0)
  }
}