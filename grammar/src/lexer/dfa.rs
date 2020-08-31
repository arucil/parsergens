use std::collections::HashMap;

#[derive(Debug)]
pub struct Dfa<T> {
  start: usize,
  transitions: HashMap<(State, char), State>,
  finals: HashMap<State, T>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct State(usize);

impl<T> Dfa<T> {
  pub fn start(&self) -> State {
    State(self.start)
  }

  pub fn transition(&self, state: State, c: char) -> Option<State> {
    self.transitions.get(&(state, c)).cloned()
  }

  /// if the state is final state, return the value.
  pub fn result(&self, state: State) -> Option<&T> {
    self.finals.get(&state)
  }
}