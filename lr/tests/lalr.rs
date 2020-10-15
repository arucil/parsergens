use insta::assert_snapshot;

mod parse;

#[test]
fn precedence() {
  let parser = lr::build(r#"
%token plus "+"
%token minus "-"
%token mult "*"
%token div "/"
%token pow "^"
%token equal "=="
%token lparen "("
%token rparen ")"
%token comma ","
%token num /\d+/
%token id /[a-zA-Z]\w*/

%skip /[ \t\n]+/

%non-assoc equal
%left-assoc plus minus
%left-assoc mult div
%right-assoc pow
%right-assoc NEG

%start E

E = E plus E
  | E minus E
  | E mult E
  | E div E
  | E pow E
  | E equal E
  | minus E     %prec NEG
  | lparen E rparen
  | id lparen ARGS rparen
  | num
  | id

ARGS = ()
  | E (comma E)*
  "#, lr::ParserKind::Lalr).unwrap();

  let input = "12 + 3 * x / 5 - 50 - foo ^ -y ^ (-1 == f() + g(3*-4, (p)))";

  assert_snapshot!(parse::parse(&parser, input, "E").join("\n"));
}
