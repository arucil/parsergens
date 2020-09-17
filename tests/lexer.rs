#![feature(proc_macro_hygiene)]

use insta::assert_debug_snapshot;

parsergens::parsergen!(mod parser: SLR("fixtures/expr_tokens.pg"));

#[test]
fn simple() {
  let tokens = parser::lex(r"(3.2 * 51 + Foo_1) / 20. -5  ,    ").collect::<Vec<_>>();
  assert_debug_snapshot!(tokens);
}