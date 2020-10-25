
use insta::assert_debug_snapshot;

parsergens::parsergen!(mod prog_lalr_parser: LR("fixtures/prog.pg"));

#[test]
fn lalr() {
  let result = prog_lalr_parser::with_input(r"foo(1+2, 45*-2); bar(); baz(--((2)),1)")
    .parse_prog();
  assert_debug_snapshot!(result);

  let result = prog_lalr_parser::with_input(r"45*-2")
    .parse_expr();
  assert_debug_snapshot!(result);

  let result = prog_lalr_parser::with_input(r"foo((-2),45*-2)")
    .parse_proc_call();
  assert_debug_snapshot!(result);
}
