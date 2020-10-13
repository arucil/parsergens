
use insta::assert_debug_snapshot;
use pretty_assertions::assert_eq;
use std::collections::BTreeMap;

parsergens::parsergen!(mod clr_parser_with_state: LR("fixtures/expr_state.pg"));

#[test]
fn clr() {
  let mut env = BTreeMap::new();
  let input = r"a1 = (3.2 * 51 + 1) / 20 ; b = a1 + 12; c = (a1 + 1) ^ (b / 13) ";
  let result = clr_parser_with_state::with_input(input)
    .parse_stmt(&mut env);
  assert_eq!(result, Ok(Ok(())));
  assert_debug_snapshot!(env);
}

parsergens::parsergen!(mod lalr_parser_with_state: LALR("fixtures/expr_state.pg"));

#[test]
fn lalr() {
  let mut env = BTreeMap::new();
  let input = r"a1 = (3.2 * 51 + 1) / 20 ; b = a1 + 12; c = (a1 + 1) ^ (b / 13) ";
  let result = lalr_parser_with_state::with_input(input)
    .parse_stmt(&mut env);
  assert_eq!(result, Ok(Ok(())));
  assert_debug_snapshot!(env);
}