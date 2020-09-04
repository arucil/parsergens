use std::hash::Hash;
use std::fmt::{self, Debug};
use super::dfa::Dfa;
use super::nfa_builder::NfaBuilder;
use crate::{Map, Set};

#[derive(Debug)]
pub struct Nfa<A, P, V> {
  pub transitions: Map<(State, Option<A>), Set<State>>,
  pub alphabets: Map<State, Set<A>>,
  pub accept_states: Map<State, (P, V)>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct State(pub usize);

impl<A, P, V> Nfa<A, P, V>
  where A: Eq + Hash + Copy,
        P: PartialOrd
{
  pub fn builder() -> NfaBuilder<A, P, V> {
    NfaBuilder::new()
  }
}

impl<A, P, V> Nfa<A, P, V>
  where A: Eq + Hash + Copy + Debug,
        P: PartialOrd,
        V: Clone
{
  pub fn to_dfa(&self, start: State) -> Dfa<A, V> {
    super::powerset_cons::powerset(self, start)
  }
}

impl Debug for State {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "State({})", self.0)
  }
}