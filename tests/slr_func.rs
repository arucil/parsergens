#![feature(proc_macro_hygiene)]

use insta::assert_debug_snapshot;

parsergens::parsergen!(mod parser_with_func: SLR("fixtures/expr_func.pg"));

#[test]
fn state() {
  let input = r"factorial(7) * 1000 + gcd(12, 39)";
  let result = parser_with_func::with_input(input)
    .parse_expr();
  assert_debug_snapshot!(result);
}