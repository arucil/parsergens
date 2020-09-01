
fn main() {
  let _input = r#"
%start expr

%token PLUS "+"
%token MINUS "-"
%token MUL "\"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token NUMBER /\d+(\.\d*)?/
%token IDENT /[a-zA-Z][\w_]*/

%skip /[ \n]/

expr = expr PLUS expr
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
    "#;
}
