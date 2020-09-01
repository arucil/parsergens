use std::hash::Hash;
use bit_set::BitSet;
use bimap::BiHashMap;
use super::nfa::{Nfa, State as NfaState};
use super::dfa::Dfa;
use super::Map;

pub fn powerset<A, P, V>(
  nfa: &Nfa<A, P, V>,
  start: NfaState
) -> Dfa<A, V>
  where A: Eq + Hash + Copy + std::fmt::Debug,
        P: PartialOrd,
        V: Clone,
{
  let mut builder = Dfa::builder();
  let mut dfa_nfa_state_map = BiHashMap::new();

  let initial = epsilon_closure(vec![start.0].into_iter().collect(), nfa);
  let start = builder.state();

  let accepted = check_accept(&initial, nfa);

  if let Some((_, value)) = accepted {
    builder.accept(start, value.clone());
  }

  //dbg!(&initial);

  dfa_nfa_state_map.insert(start, initial);

  let mut last_dfa_states = vec![start];

  loop {
    let mut new_dfa_states = vec![];

    for dfa_state in last_dfa_states {
      let nfa_states = dfa_nfa_state_map.get_by_left(&dfa_state).unwrap();
      let mut transitions: Map<A, Vec<usize>> = Map::new();

      for nfa_state in nfa_states {
        let nfa_state = NfaState(nfa_state);
        let alphabet = nfa.alphabets.get(&nfa_state)
          .into_iter()
          .flat_map(|x| x);
        for &c in alphabet {
          transitions.entry(c).or_default().extend(
            nfa.transitions.get(&(nfa_state, Some(c)))
              .into_iter()
              .flat_map(|x| x)
              .into_iter()
              .map(|s| s.0));
        }
      }

      for (c, nexts) in transitions {
        let nexts = epsilon_closure(nexts.into_iter().collect(), &nfa);
        //println!("{:?} ({:?}): -- {:?} -> {:?}", dfa_state, dfa_nfa_state_map.get_by_left(&dfa_state).unwrap(), c, nexts);

        let dest_dfa_state = if let Some(&s) = dfa_nfa_state_map.get_by_right(&nexts) {
          s
        } else {
          let dest_dfa_state = builder.state();
          new_dfa_states.push(dest_dfa_state);

          let accepted = check_accept(&nexts, nfa);

          if let Some((_, value)) = accepted {
            builder.accept(dest_dfa_state, value.clone());
          }

          dfa_nfa_state_map.insert(dest_dfa_state, nexts);

          dest_dfa_state
        };

        builder.transition(dfa_state, dest_dfa_state, c);
        //println!("{:?} -- {:?} -> {:?}", dfa_state, c, dest_dfa_state);
      }
    }

    if new_dfa_states.is_empty() {
      break;
    }

    last_dfa_states = new_dfa_states;
  }

  builder.build(start)
}

fn check_accept<'a, A, P, V>(
  nfa_states: &BitSet,
  nfa: &'a Nfa<A, P, V>
) -> Option<(&'a P, &'a V)>
  where A: Eq + Hash + Copy + std::fmt::Debug,
        P: PartialOrd,
        V: Clone,
{
  let mut accepted = None;
  for nfa_state in nfa_states {
    if let Some((p, v)) = nfa.accept_states.get(&NfaState(nfa_state)) {
      accepted = Some(if let Some((p0, v0)) = accepted {
        if p > p0 {
          (p, v)
        } else {
          (p0, v0)
        }
      } else {
        (p, v)
      });
    }
  }
  accepted
}

fn epsilon_closure<A, P, V>(
  initial: BitSet,
  nfa: &Nfa<A, P, V>
) -> BitSet
  where A: Eq + Hash,
        P: PartialOrd,
{
  let mut result = initial.clone();
  let mut last = initial;

  loop {
    let mut new = BitSet::new();
    for i in last.iter() {
      for next in nfa.transitions.get(&(NfaState(i), None))
        .into_iter()
        .flat_map(|x| x)
      {
        if !result.contains(next.0) {
          new.insert(next.0);
        }
      }
    }

    if new.is_empty() {
      break;
    }

    result.union_with(&new);

    last = new;
  }

  result
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn no_epsilon_transitions() {
    let mut nfa_builder = Nfa::builder();
    let q0 = nfa_builder.state();
    let q1 = nfa_builder.state();
    let q2 = nfa_builder.state();
    nfa_builder.transition(q0, q0, Some(0));
    nfa_builder.transition(q0, q1, Some(1));
    nfa_builder.transition(q1, q1, Some(0));
    nfa_builder.transition(q1, q1, Some(1));
    nfa_builder.transition(q1, q2, Some(0));
    nfa_builder.transition(q2, q1, Some(1));
    nfa_builder.transition(q2, q2, Some(0));
    nfa_builder.transition(q2, q2, Some(1));

    nfa_builder.accept(q2, 1, "final");

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(q0);

    assert_debug_snapshot!(( nfa, dfa ));
  }

  #[test]
  fn epsilon_transitions() {
    let mut nfa_builder = Nfa::builder();
    let a = nfa_builder.state();
    let b = nfa_builder.state();
    let c = nfa_builder.state();
    nfa_builder.transition(a, a, Some(1));
    nfa_builder.transition(a, b, Some(0));
    nfa_builder.transition(a, b, None);
    nfa_builder.transition(a, c, Some(0));
    nfa_builder.transition(b, b, Some(1));
    nfa_builder.transition(b, c, None);
    nfa_builder.transition(c, c, Some(0));
    nfa_builder.transition(c, c, Some(1));

    nfa_builder.accept(c, 1, "final");

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(a);

    assert_debug_snapshot!(( nfa, dfa ));
  }

  #[test]
  fn priority() {
    let mut nfa_builder = Nfa::builder();
    let q0 = nfa_builder.state();
    let q1 = nfa_builder.state();
    let q2 = nfa_builder.state();
    nfa_builder.transition(q0, q1, None);
    nfa_builder.transition(q0, q2, None);
    nfa_builder.transition(q1, q1, Some('0'));
    nfa_builder.transition(q1, q1, Some('1'));
    nfa_builder.transition(q2, q2, Some('0'));
    nfa_builder.transition(q2, q2, Some('1'));
    nfa_builder.transition(q2, q2, Some('a'));
    nfa_builder.transition(q2, q2, Some('b'));

    nfa_builder.accept(q1, 2, "number");
    nfa_builder.accept(q2, 1, "symbol");

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(q0);

    assert_debug_snapshot!(( nfa, dfa ));
  }

  #[test]
  fn accept_state_is_not_final_state() {
    let mut nfa_builder = Nfa::builder();
    let q_start = nfa_builder.state();
    let q_if_0 = nfa_builder.state();
    let q_if_1 = nfa_builder.state();
    let q_if_2 = nfa_builder.state();
    let q_in_0 = nfa_builder.state();
    let q_in_1 = nfa_builder.state();
    let q_in_2 = nfa_builder.state();
    let q_int_0 = nfa_builder.state();
    let q_int_1 = nfa_builder.state();
    let q_int_2 = nfa_builder.state();
    let q_int_3 = nfa_builder.state();
    let q_id_0 = nfa_builder.state();
    let q_id_1 = nfa_builder.state();

    nfa_builder.transition(q_start, q_if_0, None);
    nfa_builder.transition(q_if_0, q_if_1, Some('i'));
    nfa_builder.transition(q_if_1, q_if_2, Some('f'));
    nfa_builder.accept(q_if_2, 0, "if");

    nfa_builder.transition(q_start, q_in_0, None);
    nfa_builder.transition(q_in_0, q_in_1, Some('i'));
    nfa_builder.transition(q_in_1, q_in_2, Some('n'));
    nfa_builder.accept(q_in_2, 0, "in");

    nfa_builder.transition(q_start, q_int_0, None);
    nfa_builder.transition(q_int_0, q_int_1, Some('i'));
    nfa_builder.transition(q_int_1, q_int_2, Some('n'));
    nfa_builder.transition(q_int_2, q_int_3, Some('t'));
    nfa_builder.accept(q_int_3, 0, "int");

    nfa_builder.transition(q_start, q_id_0, None);
    nfa_builder.transition(q_id_0, q_id_1, Some('i'));
    nfa_builder.transition(q_id_0, q_id_1, Some('n'));
    nfa_builder.transition(q_id_0, q_id_1, Some('t'));
    nfa_builder.transition(q_id_0, q_id_1, Some('f'));
    nfa_builder.transition(q_id_0, q_id_1, Some('a'));
    nfa_builder.transition(q_id_0, q_id_1, Some('b'));
    nfa_builder.transition(q_id_1, q_id_1, Some('i'));
    nfa_builder.transition(q_id_1, q_id_1, Some('n'));
    nfa_builder.transition(q_id_1, q_id_1, Some('t'));
    nfa_builder.transition(q_id_1, q_id_1, Some('f'));
    nfa_builder.transition(q_id_1, q_id_1, Some('a'));
    nfa_builder.transition(q_id_1, q_id_1, Some('b'));
    nfa_builder.accept(q_id_1, 0, "ID");

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(q_start);

    assert_debug_snapshot!(( nfa, dfa ));
  }
}