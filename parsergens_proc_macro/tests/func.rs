
use insta::assert_debug_snapshot;

parsergens_proc_macro::parsergen!(mod clr_parser_with_func: LR("fixtures/expr_func.pg"));

#[test]
fn clr() {
  let input = r"factorial(7) * 1000 + gcd(12, 39)";
  let result = clr_parser_with_func::with_input(input)
    .parse_expr();
  assert_debug_snapshot!(result);
}

parsergens_proc_macro::parsergen!(mod lalr_parser_with_func: LALR("fixtures/expr_func.pg"));

#[test]
fn lalr() {
  let input = r"factorial(7) * 1000 + gcd(12, 39)";
  let result = lalr_parser_with_func::with_input(input)
    .parse_expr();
  assert_debug_snapshot!(result);
}