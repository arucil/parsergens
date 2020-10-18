use std::hash::Hash;
use std::fmt::{self, Debug};
use bittyset::BitSet;
use indexmap::IndexSet;
use super::dfa_builder::DfaBuilder;
use crate::{Map, Set};

#[derive(Debug, Clone)]
pub struct Dfa<A, V> {
  pub start: u32,
  pub transitions: Map<(State, A), State>,
  pub accept_states: Map<State, V>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct State(pub u32);

impl<A: Eq + Hash + Clone, V> Dfa<A, V> {
  #[allow(dead_code)]
  pub fn start(&self) -> State {
    State(self.start)
  }

  #[allow(dead_code)]
  pub fn transition(&self, state: State, c: A) -> Option<State> {
    self.transitions.get(&(state, c)).cloned()
  }

  /// if the state is accepting state, return the value.
  #[allow(dead_code)]
  pub fn result(&self, state: State) -> Option<&V> {
    self.accept_states.get(&state)
  }
}

impl<A, V> Dfa<A, V>
  where A: Eq + Hash + Clone,
        V: Eq + Hash + Clone,
{
  pub fn builder() -> DfaBuilder<A, V> {
    DfaBuilder::new()
  }

  /// Hopcroft's algorithm
  pub fn minimize<'a>(&'a self) -> Self {
    let mut states = self.accept_states.iter()
      .fold(Map::<&'a V, BitSet>::default(), |mut map, (state, value)| {
        map.entry(value).or_default().insert(state.0 as usize);
        map
      })
      .into_iter()
      .map(|(_, v)| v)
      .collect::<IndexSet<_>>();

    let mut non_accept_states = self.transitions.iter()
      .fold(BitSet::new(), |mut set, ((src, _), dest)| {
        set.insert(src.0 as usize);
        set.insert(dest.0 as usize);
        set
      });

    for state in self.accept_states.keys() {
      non_accept_states.remove(state.0 as usize);
    }

    states.insert(non_accept_states);

    let to_states = self.transitions.iter()
      .fold(Map::<(A, u32), BitSet>::default(), |mut to_states, ((src, c), dest)| {
        to_states.entry((c.clone(), dest.0)).or_default().insert(src.0 as usize);
        to_states
      });
    let alphabet = self.transitions.keys().map(|(_, c)| c.clone()).collect::<Set<_>>();
    let mut p = states.clone();
    let mut w = states;

    while !w.is_empty() {
      let a = w.pop().unwrap();
      for c in &alphabet {
        let x = a.iter()
          .fold(BitSet::new(), |mut set, state| {
            to_states.get(&(c.clone(), state as u32)).iter().for_each(|states| {
              set.union_with(*states);
            });
            set
          });
        if x.is_empty() {
          continue;
        }

        p = p.into_iter()
          .flat_map(|y| {
            let x_inter_y = x.intersection(&y);
            let y_diff_x = y.difference(&x);

            if x_inter_y.is_empty() || y_diff_x.is_empty() {
              return vec![y];
            }

            if w.remove(&y) {
              w.insert(x_inter_y.clone());
              w.insert(y_diff_x.clone());
            } else {
              if x_inter_y.len() <= y_diff_x.len() {
                w.insert(x_inter_y.clone());
              } else {
                w.insert(y_diff_x.clone());
              }
            }

            vec![x_inter_y, y_diff_x]
          })
          .collect();
      }
    }

    let state_count = self.transitions.iter()
      .flat_map(|((src, _), dest)| vec![src.0, dest.0])
      .collect::<Set<_>>()
      .len();
    if p.len() == state_count {
      return self.clone();
    }

    let new_states = p.into_iter().enumerate()
      .map(|(i, set)| (set, State(i as u32)))
      .collect::<Map<_, _>>();

    let old_new_states = new_states.iter()
      .fold(Map::<State, State>::default(), |mut map, (set, new_state)| {
        for old_state in set {
          map.insert(State(old_state as u32), *new_state);
        }
        map
      });

    let start = old_new_states[&State(self.start)].0;
    let accept_states = self.accept_states.iter()
      .map(|(old_state, value)| (old_new_states[old_state], value.clone()))
      .collect();
    let transitions = self.transitions.iter()
      .map(|((old_src, c), old_dest)|
        ((old_new_states[old_src], c.clone()), old_new_states[old_dest]))
      .collect();
    
    Self {
      start,
      transitions,
      accept_states,
    }
  }
}

impl Debug for State {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "State({})", self.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn minimization_multi_accept() {
    let mut builder = Dfa::builder();

    let a = builder.state();
    let b = builder.state();
    let c = builder.state();
    let d = builder.state();
    let e = builder.state();
    let f = builder.state();

    builder.transition(a, b, '0');
    builder.transition(a, c, '1');
    builder.transition(b, a, '0');
    builder.transition(b, d, '1');
    builder.transition(c, e, '0');
    builder.transition(c, f, '1');
    builder.transition(d, e, '0');
    builder.transition(d, f, '1');
    builder.transition(e, e, '0');
    builder.transition(e, f, '1');
    builder.transition(f, f, '0');
    builder.transition(f, f, '1');

    builder.accept(c, "end");
    builder.accept(d, "end");
    builder.accept(e, "end");

    let dfa = builder.build(a);
    let dfa = dfa.minimize();

    assert_debug_snapshot!(dfa);
  }

  #[test]
  fn minimization_simple() {
    let mut builder = Dfa::builder();

    let s = builder.state();
    let a = builder.state();
    let b = builder.state();
    let c = builder.state();

    builder.transition(s, c, 'a');
    builder.transition(s, b, 'x');
    builder.transition(a, c, 'a');
    builder.transition(a, b, 'x');
    builder.transition(b, a, 'x');

    builder.accept(c, "end");

    let dfa = builder.build(a);
    let dfa = dfa.minimize();

    assert_debug_snapshot!(dfa);
  }
}