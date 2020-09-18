#![feature(proc_macro_hygiene)]

use insta::assert_debug_snapshot;

parsergens::parsergen!(mod parser: SLR("fixtures/expr_eval.pg"));

#[test]
fn simple() {
  let tokens = parser::with_input(r"(3.2 * 51 + 1) / 20     ")
    .parse_expr();
  assert_debug_snapshot!(tokens);
}

parsergens::parsergen!(mod parser_with_state: SLR("fixtures/expr_state.pg"));

#[test]
fn state() {
  let tokens = parser_with_state::with_input(r"(3.2 * 51 + 1) / 20     ")
    .parse_expr();
  assert_debug_snapshot!(tokens);
}