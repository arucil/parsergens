
use insta::assert_debug_snapshot;

parsergens::parsergen!(mod clr_parser: LR("fixtures/expr_eval.pg"));

#[test]
fn clr() {
  let result = clr_parser::with_input(r"(3.2 * 51 + 1) / 20     ")
    .parse_expr();
  assert_debug_snapshot!(result);
}

parsergens::parsergen!(mod lalr_parser: LALR("fixtures/expr_eval.pg"));

#[test]
fn lalr() {
  let result = lalr_parser::with_input(r"(3.2 * 51 + 1) / 20     ")
    .parse_expr();
  assert_debug_snapshot!(result);
}
