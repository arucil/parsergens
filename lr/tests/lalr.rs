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

#[test]
fn simple() {
  let parser = lr::build(r#"
%token c "c"
%token d "d"

%start S

S = C C
C = c C
  | d
  "#, lr::ParserKind::Lalr).unwrap();

  let input = "cdccd";

  assert_snapshot!(parse::parse(&parser, input, "S").join("\n"));
}

#[test]
fn nonassoc() {
  let parser = lr::build(r#"
%token AND "&&"
%token OR "||"
%token GT ">"
%token num /\d+/

%skip /[ \t\n]+/

%right-assoc OR
%right-assoc AND
%non-assoc GT

%start expr

expr = expr AND expr
  | expr OR expr
  | expr GT expr
  | num
  "#, lr::ParserKind::Lalr).unwrap();

  let input = "1 > 2 > 3";

  assert_snapshot!(parse::parse(&parser, input, "expr").join("\n"));
}

#[test]
fn repetition_succeed() {
  let parser = lr::build(r#"
%token a "a"
%token b "b"

%start S

S = S b | a
  "#, lr::ParserKind::Lalr).unwrap();

  let input = "abbbb";

  assert_snapshot!(parse::parse(&parser, input, "S").join("\n"));

}

#[test]
fn repetition_failed() {
  let parser = lr::build(r#"
%token a "a"
%token b "b"

%start S

S = S b | a
  "#, lr::ParserKind::Lalr).unwrap();

  let input2 = "aaab";

  assert_snapshot!(parse::parse(&parser, input2, "S").join("\n"));
}