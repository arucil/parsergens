#![feature(proc_macro_hygiene)]

use insta::assert_debug_snapshot;
use pretty_assertions::assert_eq;
use std::collections::BTreeMap;

parsergens::parsergen!(mod parser_with_state: SLR("fixtures/expr_state.pg"));

#[test]
fn state() {
  let mut env = BTreeMap::new();
  let input = r"a1 = (3.2 * 51 + 1) / 20 ; b = a1 + 12; c = (a1 + 1) ^ (b / 13) ";
  let result = parser_with_state::with_input(input)
    .parse_stmt(&mut env);
  assert_eq!(result, Ok(Ok(())));
  assert_debug_snapshot!(env);
}