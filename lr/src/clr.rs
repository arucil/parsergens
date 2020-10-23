use crate::builder::{LrComputation, StateStore, KernelItemSet, State, Item};

pub struct ClrComputation;

impl LrComputation for ClrComputation {
  type StateKey = Vec<Item>;

  /// `kernel_item_set` is sorted by the field `key`.
  ///
  /// Returns whether the state has changed.
  fn store_state(
    states: &mut StateStore<Self::StateKey>,
    kernel_item_set: KernelItemSet,
  ) -> (u32, bool) {
    if let Some(i) = states.get_index_of(&kernel_item_set) {
      (i as u32, false)
    } else {
      let state_ix = states.insert_full(
        kernel_item_set.clone(),
        State::new(kernel_item_set),
      ).0 as u32;

      (state_ix, true)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::builder::*;
  use insta::{assert_snapshot, assert_debug_snapshot};
  use grammar::LoweredGrammar;
  use crate::augment;

  fn prepare(input: &str) -> LoweredGrammar {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let grammar = augment::augment(grammar);

    grammar
  }

  fn merge_action_goto((action, goto): (Vec<Vec<i32>>, Vec<Vec<u32>>)) -> Vec<Vec<i32>> {
    action.into_iter().enumerate().map(|(state, mut row1)| {
      row1.extend((0..goto.len()).map(|nt| goto[nt][state] as i32));
      row1
    })
    .collect()
  }

  static SIMPLE: &str = r#"
%token c "c"
%token d "d"
%start S
S = C C
C = c C
  | d
  "#;

  #[test]
  fn simple_states() {
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::<ClrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn simple_action_goto() {
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::<ClrComputation>::new(&grammar);
    gen_states(&mut builder);
    let tables = merge_action_goto(gen_tables(&builder).unwrap());

    assert_debug_snapshot!(tables);
  }

  static EPSILON: &str = r#"
%token plus "+"
%token mult "*"
%token num "1"
%token lparen "("
%token rparen ")"

%start E

E = T E'
E' = plus T E'
   | ()
T = F T'
T' = mult F T'
   | ()
F = num
  | lparen E rparen
  "#;

  #[test]
  fn epsilon_states() {
    let grammar = prepare(EPSILON);
    let mut builder = Builder::<ClrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn epsilon_action_goto() {
    let grammar = prepare(EPSILON);
    let mut builder = Builder::<ClrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
  }
}