
use insta::assert_debug_snapshot;

parsergens_proc_macro::parsergen!(mod parser: LALR("fixtures/expr_tokens.pg"));

#[test]
fn simple() {
  let tokens = parser::lex(r"(3.2 * 51 + Foo_1) / 20. -5  ,    ").collect::<Vec<_>>();
  assert_debug_snapshot!(tokens);
}