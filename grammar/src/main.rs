use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

mod lex;
mod ast;

fn main() {
  let input = r"
%start expr

%token PLUS /\+/
%token MINUS /-/
%token MUL /\*/
%token DIV /\//
%token LPAREN /\(/
%token RPAREN /)/
%token COMMA /,/
%token NUMBER /\d+(\.\d*)?/
%token IDENT /[a-zA-Z][\w_]*/

expr =
    expr PLUS expr
  | expr MINUS expr
  | expr MUL expr
  | expr DIV expr
  | factor

factor =
    NUMBER
  | LPAREN expr RPAREN
  | MINUS NUMBER
  | IDENT
  | IDENT LPAREN param_list RPAREN
  | IDENT LPAREN RPAREN

param_list =
    expr
  | param_list COMMA expr
    ";
  let lexer = lex::Lexer::new(input);
  println!("{:#?}", grammar::documentParser::new().parse(input, lexer));
}
