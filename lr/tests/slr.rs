use lr::Parser;
use lr::slr;
use insta::assert_snapshot;

mod parse;

#[test]
fn precedence() {
  let parser = slr::build(r#"
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

%right-assoc NEG
%right-assoc POW
%left-assoc MUL
%left-assoc ADD
%non-assoc REL

%start E

E = E plus E    %prec ADD
  | E minus E   %prec ADD
  | E mult E    %prec MUL
  | E div E     %prec MUL
  | E pow E     %prec POW
  | E equal E   %prec REL
  | minus E     %prec NEG
  | lparen E rparen
  | id lparen ARGS rparen
  | num
  | id

ARGS = ()
  | E (comma E)*
  "#).unwrap();

  let input = "12 + 3 * x / 5 - 50 - foo ^ -y ^ (-1 == f() + g(3*-4, (p)))";

  assert_snapshot!(parse::parse(&parser, input, "E").join("\n"));
}
