use crate::builder::{LrComputation, StateStore, KernelItemSet, State};

pub struct LalrComputation;

impl LrComputation for LalrComputation {
  type StateKey = Vec<u32>;

  /// `kernel_item_set` is sorted by the field `key`.
  ///
  /// Returns whether the state has changed.
  fn store_state(
    states: &mut StateStore<Self::StateKey>,
    kernel_item_set: KernelItemSet,
  ) -> (u32, bool) {
    let kernel_key_set = kernel_item_set.iter()
      .map(|item| item.key)
      .collect::<Vec<_>>();

    if let Some(i) = states.get_index_of(&kernel_key_set) {
      let mut changed = false;
      for (item, new_item) in states[i].items.iter_mut().zip(kernel_item_set) {
        changed |= item.lookaheads.union_with(&new_item.lookaheads);
      }
      (i as u32, changed)
    } else {
      let state_ix = states.insert_full(
        kernel_key_set,
        State::new(kernel_item_set),
      ).0 as u32;

      (state_ix, true)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::builder::*;
  use insta::{assert_snapshot, assert_debug_snapshot};
  use grammar::LoweredGrammar;
  use crate::augment;

  fn prepare(input: &str) -> LoweredGrammar {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let grammar = augment::augment(grammar);

    grammar
  }

  fn merge_action_goto((action, goto): (Vec<Vec<i32>>, Vec<Vec<u32>>)) -> Vec<Vec<i32>> {
    action.into_iter().enumerate().map(|(state, mut row1)| {
      row1.extend((0..goto.len()).map(|nt| goto[nt][state] as i32));
      row1
    })
    .collect()
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
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn simple_action_goto() {
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
  }

  #[test]
  fn simple_tables() {
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let (action, goto) = gen_tables(&builder).unwrap();
    let tables = compress_tables(&grammar, action, goto);

    assert_debug_snapshot!(tables);
  }

  static EPSILON: &str = r#"
%token plus "+"
%token mult "*"
%token num "1"
%token lparen "("
%token rparen ")"

%start E

E = T E'
E' = plus T E'
   | ()
T = F T'
T' = mult F T'
   | ()
F = num
  | lparen E rparen
  "#;

  #[test]
  fn epsilon_states() {
    let grammar = prepare(EPSILON);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn epsilon_action_goto() {
    let grammar = prepare(EPSILON);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
  }

  static PRECEDENCE: &str = r#"
%token minus "-"
%token mult "*"
%token pow "^"
%token equal "=="
%token lparen "("
%token rparen ")"
%token num "1"

%non-assoc equal
%left-assoc minus
%left-assoc mult
%right-assoc pow
%right-assoc NEG

%start E

E = E minus E
  | E mult E
  | E pow E
  | E equal E
  | minus E       %prec NEG
  | lparen E rparen
  | num
  "#;

  #[test]
  fn precedence_states() {
    let grammar = prepare(PRECEDENCE);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn precedence_action_goto() {
    let grammar = prepare(PRECEDENCE);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
  }

  static START_REPETITION: &str = r#"
%token x "-"

%start S

S = S x | ()
  "#;

  #[test]
  fn start_repetition_states() {
    let grammar = prepare(START_REPETITION);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn start_repetition_action_goto() {
    let grammar = prepare(START_REPETITION);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
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

%non-assoc    CONTROL
%non-assoc    NEW_ARRAY
%non-assoc    ASSIGN
%right-assoc  OR
%right-assoc  AND
%non-assoc    EQ NEQ
%non-assoc    LT GT LE GE
%left-assoc   MINUS PLUS
%left-assoc   TIMES DIV MOD
%right-assoc  NEG
%non-assoc    ELSE

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
  | lvalue ASSIGN expr
  | NIL
  | LPAREN seq-exprs RPAREN
  | LIT_INT
  | LIT_STR
  | MINUS expr %prec NEG
  | IDENT LPAREN arg-exprs RPAREN
  | expr MOD expr
  | expr DIV expr
  | expr TIMES expr
  | expr PLUS expr
  | expr MINUS expr
  | expr GT expr
  | expr LT expr
  | expr GE expr
  | expr LE expr
  | expr EQ expr
  | expr NEQ expr
  | expr AND expr
  | expr OR expr
  | type-id LBRACE field-init-exprs RBRACE
  | IDENT LBRACK expr RBRACK OF expr %prec NEW_ARRAY
  | predef-type LBRACK expr RBRACK OF expr %prec NEW_ARRAY
  | IF expr THEN expr ELSE expr
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
  fn tiger_states() {
    let grammar = prepare(TIGER);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let start_nts = gen_states(&mut builder);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn tiger_action_goto() {
    let grammar = prepare(TIGER);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let tables = gen_tables(&builder).unwrap();
    let tables = merge_action_goto(tables);

    assert_debug_snapshot!(tables);
  }

  #[test]
  fn tiger_tables() {
    let grammar = prepare(TIGER);
    let mut builder = Builder::<LalrComputation>::new(&grammar);
    let _start_nts = gen_states(&mut builder);
    let (action, goto) = gen_tables(&builder).unwrap();
    let tables = compress_tables(&grammar, action, goto);

    assert_debug_snapshot!(tables);
  }
}
