
fn main() {
  let input = include_str!("../lr/benches/fixtures/ocaml.pg");
  let _parser = lr::build(input, lr::ParserKind::Lalr).unwrap();
}