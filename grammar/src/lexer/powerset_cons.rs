use super::nfa::{Nfa, State};
use super::dfa::Dfa;

pub fn powerset<P: PartialOrd, V>(nfa: &Nfa<P, V>, start: State) -> Dfa<V> {
}