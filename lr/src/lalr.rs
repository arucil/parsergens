use std::collections::HashMap;
use grammar::{ LoweredGrammar, TokenId, BiMap };
use bit_set::BitSet;
use crate::Error;
use crate::ffn::Ffn;
use crate::builder::LrCalculation;
use crate::clr::{ClrCalc, Lr1Item};

pub enum LalrCalc {}

impl LrCalculation for LalrCalc {
  type Item = Lr1Item;

  fn start_item(
    start_prod_ix: usize,
    eof_token: TokenId,
  ) -> Lr1Item {
    ClrCalc::start_item(start_prod_ix, eof_token)
  }

  fn next_item(
    item: &Lr1Item
  ) -> Lr1Item {
    ClrCalc::next_item(item)
  }

  #[inline(always)]
  fn closure_step<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    prev: &Lr1Item,
    action: F
  )
    where F: FnMut(Lr1Item)
  {
    ClrCalc::closure_step(grammar, ffn, prev, action)
  }

  #[inline(always)]
  fn reduce_tokens<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    item: &Lr1Item,
    action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>
  {
    ClrCalc::reduce_tokens(grammar, ffn, item, action)
  }

  fn merge_states(
    _grammar: &LoweredGrammar,
    states: &mut BiMap<BitSet, u32>,
    items: &mut BiMap<Lr1Item, usize>,
  ) -> Option<HashMap<u32, u32>> {
    let mut new_states = BiMap::<BitSet, u32>::new();
    let mut new_items = BiMap::new();
    let mut map = HashMap::new();

    for (state, state_id) in &*states {
      let new_state = state.iter().map(|item| {
        let item = items.get_by_right(&item).unwrap();
        store_item(&mut new_items, item)
      }).collect::<BitSet>();
      let new_state_id = if let Some(id) = new_states.get_by_left(&new_state) {
        *id
      } else {
        let id = new_states.len() as u32;
        new_states.insert(new_state, id);
        id
      };
      map.insert(*state_id, new_state_id);
    }

    *items = new_items;
    *states = new_states;
    Some(map)
  }
}

fn store_item(
  items: &mut BiMap<Lr1Item, usize>,
  item: &Lr1Item
) -> usize {
  let item = Lr1Item {
    token: std::u32::MAX,
    ..*item
  };
  if let Some(&item) = items.get_by_left(&item) {
    item
  } else {
    let len = items.len();
    items.insert(item, len);
    len
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_debug_snapshot, assert_snapshot};
  use grammar::{ TokenId, LoweredGrammar };
  use crate::ffn::Ffn;
  use crate::ffn;
  use crate::augment;
  use crate::builder::Builder;

  fn prepare(input: &str) -> (LoweredGrammar, TokenId, Ffn) {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, eof_token) = augment::augment(grammar);
    let ffn = ffn::compute(&grammar);

    (grammar, eof_token, ffn)
  }

  static SIMPLE: &str = r#"
%token c "c"
%token d "d"

%start S

S = C C
C = c C
  | d
  "#;

  #[test]
  fn simple_states() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<LalrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<LalrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }

  static TIGER: &str = r##"
%token EQ          "="
%token NEQ         "<>"
%token LT          "<"
%token GT          ">"
%token LE          "<="
%token GE          ">="
%token ASSIGN      ":="
%token COLON       ":"
%token COMMA       ","
%token DOT         "."
%token SEMI        ";"
%token PLUS        "+"
%token MINUS       "-"
%token TIMES       "*"
%token DIV         "/"
%token MOD         "%"
%token AND         "&"
%token OR          "|"
%token LBRACE      "{"
%token RBRACE      "}"
%token LPAREN      "("
%token RPAREN      ")"
%token LBRACK      "["
%token RBRACK      "]"
%token TYPE        "type"
%token INT         "int"
%token STRING      "string"
%token ARRAY       "array"
%token OF          "of"
%token VAR         "var"
%token NIL         "nil"
%token FUNCTION    "function"
%token IF          "if"
%token THEN        "then"
%token ELSE        "else"
%token FOR         "for"
%token TO          "to"
%token DO          "do"
%token WHILE       "while"
%token BREAK       "break"
%token LET         "let"
%token IN          "in"
%token END         "end"
%token IDENT       /[a-zA-Z][\w_]*/
%token LIT_INT     /\d+/
%token LIT_STR     /"([^\\\n]|\\([nt"\\]|\d\d\d))*"/

%skip /[ \n]+/

%non-assoc    ELSE
%right-assoc  NEG
%left-assoc   MULT
%left-assoc   ADD
%non-assoc    REL
%non-assoc    EQ
%right-assoc  AND
%right-assoc  OR
%non-assoc    ASSIGN
%non-assoc    ARRAY
%non-assoc    CONTROL

%start program

program = expr

decls = (decl)*

decl = type-decl | var-decl | fun-decl

type-decl = TYPE type-id EQ type

type =
    IDENT
  | LBRACE type-fields RBRACE

type-fields =
    ()
  | IDENT COLON type-id (COMMA IDENT COLON type-id)*

type-id = predef-type | IDENT

predef-type = STRING | INT

var-decl = VAR IDENT (COLON type-id)? ASSIGN expr

fun-decl = FUNCTION IDENT LPAREN type-fields RPAREN (COLON type-id)? EQ expr

lvalue = IDENT lvalue-suffix

lvalue-suffix =
    ()
  | DOT IDENT lvalue-suffix
  | LBRACK expr RBRACK lvalue-suffix

expr =
    lvalue
  | lvalue ASSIGN expr %prec ASSIGN
  | NIL
  | LPAREN seq-exprs RPAREN
  | LIT_INT
  | LIT_STR
  | MINUS expr %prec NEG
  | IDENT LPAREN arg-exprs RPAREN
  | expr MOD expr %prec MULT
  | expr DIV expr %prec MULT
  | expr TIMES expr %prec MULT
  | expr PLUS expr %prec ADD
  | expr MINUS expr %prec ADD
  | expr GT expr %prec REL
  | expr LT expr %prec REL
  | expr GE expr %prec REL
  | expr LE expr %prec REL
  | expr EQ expr %prec EQ
  | expr NEQ expr %prec EQ
  | expr AND expr %prec AND
  | expr OR expr %prec OR
  | type-id LBRACE field-init-exprs RBRACE
  | IDENT LBRACK expr RBRACK OF expr %prec ARRAY
  | predef-type LBRACK expr RBRACK OF expr %prec ARRAY
  | IF expr THEN expr ELSE expr %prec ELSE
  | IF expr THEN expr %prec CONTROL
  | WHILE expr DO expr %prec CONTROL
  | FOR IDENT ASSIGN expr TO expr DO expr %prec CONTROL
  | BREAK
  | LET decls IN seq-exprs END

seq-exprs =
    ()
  | expr (SEMI expr)*

arg-exprs =
    ()
  | expr (COMMA expr)*

field-init-exprs =
    ()
  | IDENT EQ expr (COMMA IDENT EQ expr)*
  "##;

  #[test]
  #[ignore]
  fn tiger_states() {
    let (grammar, eof_token, ffn) = prepare(TIGER);
    let mut builder = Builder::<LalrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

  }
}