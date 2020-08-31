use std::collections::HashMap;
use super::dfa::Dfa;

#[derive(Debug)]
pub struct Nfa<P, V> {
  transitions: HashMap<(State, Option<char>), Vec<State>>,
  finals: HashMap<State, (P, V)>,
}

pub struct NfaBuilder<P, V> {
  counter: usize,
  transitions: HashMap<(State, Option<char>), Vec<State>>,
  finals: HashMap<State, (P, V)>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct State(usize);

impl<P: PartialOrd, V> Nfa<P, V> {
  pub fn builder() -> NfaBuilder<P, V> {
    NfaBuilder::new()
  }
}

impl<P: PartialOrd, V> NfaBuilder<P, V> {
  fn new() -> Self {
    Self {
      counter: 0,
      transitions: HashMap::new(),
      finals: HashMap::new(),
    }
  }

  pub fn build(self) -> Nfa<P, V> {
    Nfa {
      transitions: self.transitions,
      finals: self.finals,
    }
  }

  pub fn state(&mut self) -> State {
    self.counter += 1;
    State(self.counter)
  }

  pub fn transition(&mut self, src: State, dest: State, c: Option<char>) {
    self.transitions.entry((src, c)).or_default().push(dest);
  }

  pub fn final_state(&mut self, state: State, priority: P, value: V) {
    self.finals.insert(state, (priority, value));
  }
}

impl<P: PartialOrd, V> Nfa<P, V> {
  fn to_dfa(&self, start: State) -> Dfa<V> {
    super::powerset_cons::powerset(self, start)
  }
}