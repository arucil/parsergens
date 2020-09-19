#![feature(proc_macro_hygiene)]

use insta::assert_debug_snapshot;

parsergens::parsergen!(mod parser: SLR("fixtures/expr_eval.pg"));

#[test]
fn simple() {
  let result = parser::with_input(r"(3.2 * 51 + 1) / 20     ")
    .parse_expr();
  assert_debug_snapshot!(result);
}
