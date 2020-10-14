use std::collections::{HashMap, VecDeque};
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId};
use bit_set::BitSet;
use crate::first::FirstAndNullable;
use crate::Error;

/// Lookahead set of an item.
type LookaheadSet = Map<u32, BitSet>;

struct Builder<'a> {
  grammar: &'a LoweredGrammar,
  state_store: StateStore,
  item_store: ItemStore,
  eof: usize,
}

#[derive(Default)]
struct StateStore {
  /// state -> (item set, lookahead set)
  states: Vec<(BitSet, LookaheadSet)>,
  // item set -> state
  state_indices: HashMap<BitSet, u32>,
  /// state -> symbol -> next state
  goto: Vec<Map<Symbol, u32>>,
}

#[derive(Default)]
struct ItemStore {
  items: Vec<Lr0Item>,
  item_indices: HashMap<Lr0Item, u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Lr0Item {
  prod_ix: usize,
  dot_ix: usize,
}

impl<'a> Builder<'a> {
  fn new(grammar: &'a LoweredGrammar) -> Self {
    Self {
      grammar,
      state_store: Default::default(),
      item_store: Default::default(),
      eof: grammar.tokens
        .keys()
        .map(|x| x.id())
        .max()
        .unwrap() as usize + 1,
    }
  }
}

pub fn build(
  grammar: &LoweredGrammar
) -> Result<(), Error> {
  let mut builder = Builder::new(grammar);
  let start_nts = build_states(&mut builder, grammar);

  Ok(())
}

fn build_states(
  builder: &mut Builder,
  grammar: &LoweredGrammar,
) -> HashMap<String, (u32, u32)> {
  let fan = crate::first::compute(grammar);
  let mut start_nts = HashMap::new();

  for &start_nt in &grammar.start_nts {
    let start_state = start(builder, grammar, &fan, start_nt);
    let start_prod_ix = grammar.nt_metas[&start_nt].range.start;
    let real_start_nt =
      match grammar.prods[start_prod_ix].symbols[0] {
        Symbol::Nonterminal(nt) => nt,
        _ => unreachable!(),
      };
    let nt_name = grammar.nts.get(&real_start_nt).unwrap().clone();
    start_nts.insert(nt_name, (real_start_nt.id(), start_state));
  }

  start_nts
}

fn start(
  builder: &mut Builder,
  grammar: &LoweredGrammar,
  fan: &FirstAndNullable,
  nt: NonterminalId
) -> u32 {
  let start_prod = grammar.nt_metas[&nt].range.start;
  let start_item = store_item(&mut builder.item_store,
    Lr0Item {
      prod_ix: start_prod,
      dot_ix: 0,
    });
  let mut start_item_set = {
    let mut set = BitSet::new();
    set.insert(start_item as usize);
    set
  };
  let mut start_lookaheads = {
    let mut m = Map::new();
    let mut set = BitSet::new();
    set.insert(builder.eof);
    m.insert(start_item, set);
    m
  };

  closure(builder, grammar, fan, &mut start_item_set, &mut start_lookaheads);
  let (start_state, _) = store_state(&mut builder.state_store, &start_item_set,
    start_lookaheads);

  let mut queue = VecDeque::new();
  queue.push_back(start_state);

  while let Some(from_state) = queue.pop_front() {
    let (item_set, lookaheads) = &builder.state_store.states[from_state as usize];
    let mut to_states = Map::<Symbol, (BitSet, LookaheadSet)>::new();

    for item_ix in item_set.iter() {
      let item = builder.item_store.items[item_ix];
      let symbols = &grammar.prods[item.prod_ix].symbols;
      if item.dot_ix == symbols.len() {
        continue;
      }

      let new_item = store_item(&mut builder.item_store, Lr0Item {
        prod_ix: item.prod_ix,
        dot_ix: item.dot_ix + 1,
      });
      let (to_item_set, to_lookaheads) = to_states.entry(symbols[item.dot_ix].clone())
        .or_default();
      to_item_set.insert(new_item as usize);
      to_lookaheads.entry(new_item).or_default().union_with(
        &lookaheads[&(item_ix as u32)]);
    }

    for (sym, (mut to_item_set, mut lookaheads)) in to_states {
      closure(builder, grammar, fan, &mut to_item_set, &mut lookaheads);
      let (to_state, changed) = store_state(&mut builder.state_store, &to_item_set,
        lookaheads);

      if changed {
        queue.push_back(to_state);
        builder.state_store.goto[from_state as usize].insert(sym, to_state);
      }
    }
  }

  start_state
}

fn closure(
  builder: &mut Builder,
  grammar: &LoweredGrammar,
  fan: &FirstAndNullable,
  item_set: &mut BitSet,
  lookaheads: &mut LookaheadSet,
) {
  let mut new = item_set.iter().collect::<Vec<_>>();

  while let Some(i) = new.pop() {
    let item = &builder.item_store.items[i];
    let symbols = &grammar.prods[item.prod_ix].symbols;
    if item.dot_ix < symbols.len() {
      let nt = match &symbols[item.dot_ix] {
        Symbol::Token(_) => continue,
        Symbol::Nonterminal(nt) => nt,
      };

      let mut first = BitSet::new();
      let mut rest_nullable = true;
      for sym in &symbols[item.dot_ix + 1..] {
        match sym {
          Symbol::Token(tok) => {
            first.insert(tok.id() as usize);
            rest_nullable = false;
            break;
          }
          Symbol::Nonterminal(nt) => {
            first.union_with(&fan.first[nt]);
            if !fan.nullable.contains(nt.id() as usize) {
              rest_nullable = false;
              break;
            }
          }
        }
      }

      if rest_nullable {
        first.union_with(&lookaheads[&(i as u32)]);
      }

      for prod_ix in grammar.nt_metas[nt].range.clone() {
        let item = store_item(&mut builder.item_store, Lr0Item {
          prod_ix,
          dot_ix: 0,
        });

        if item_set.insert(item as usize) {
          new.push(item as usize);
        }

        lookaheads.entry(item).or_default().union_with(&first);
      }
    }
  }
}

/// return state index and if lookahead set of the state has changed.
fn store_state(
  state_store: &mut StateStore,
  item_set: &BitSet,
  lookaheads: LookaheadSet,
) -> (u32, bool) {
  if let Some(&ix) = state_store.state_indices.get(item_set) {
    let mut changed = false;
    for (item, old_lookaheads) in &mut state_store.states[ix as usize].1 {
      for lookahead in lookaheads[item].iter() {
        if old_lookaheads.insert(lookahead) {
          changed = true;
        }
      }
    }
    (ix, changed)
  } else {
    let ix = state_store.states.len() as u32;
    state_store.states.push((item_set.clone(), lookaheads));
    state_store.state_indices.insert(item_set.clone(), ix);
    state_store.goto.push(Map::new());
    (ix, true)
  }
}

fn store_item(
  item_store: &mut ItemStore,
  item: Lr0Item,
) -> u32 {
  if let Some(&ix) = item_store.item_indices.get(&item) {
    ix
  } else {
    let ix = item_store.items.len() as u32;
    item_store.items.push(item);
    item_store.item_indices.insert(item, ix);
    ix
  }
}

#[cfg(test)]
use std::fmt::{Write, self};

#[cfg(test)]
impl<'a> Builder<'a> {
  fn states(
    &self,
    start_nts: &HashMap<String, (u32, u32)>
  ) -> String {
    let mut output = String::new();
    self.fmt_states(start_nts, &mut output).unwrap();
    output
  }

  fn fmt_states(
    &self,
    start_nts: &HashMap<String, (u32, u32)>,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    for (state, (item_set, lookaheads)) in self.state_store.states.iter().enumerate() {
      let state = state as u32;
      write!(fmt, "State {}", state)?;
      if start_nts.values().find(|x| x.1 == state).is_some() {
        write!(fmt, " (start)")?;
      }
      writeln!(fmt)?;

      for item_ix in item_set.iter() {
        let item = &self.item_store.items[item_ix];
        let lookaheads = &lookaheads[&(item_ix as u32)];
        self.fmt_item(lookaheads, item, fmt)?;

        writeln!(fmt)?;
      }

      writeln!(fmt)?;
    }

    Ok(())
  }

  fn fmt_item(
    &self,
    lookaheads: &BitSet,
    item: &Lr0Item,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    let nt = self.grammar.prods[item.prod_ix].nt;
    let symbols = &self.grammar.prods[item.prod_ix].symbols;

    write!(fmt, "{} ->", self.grammar.nts[&nt])?;

    for (i, sym) in symbols.iter().enumerate() {
      if i == item.dot_ix {
        write!(fmt, " .")?;
      }

      match sym {
        Symbol::Token(token) => {
          let name = self.grammar.tokens.get(token).map(|s|s.as_str()).unwrap_or("$");
          write!(fmt, " {}", name)?;
        }
        Symbol::Nonterminal(nt) => {
          let name = &self.grammar.nts[nt];
          write!(fmt, " {}", name)?;
        }
      }
    }

    if item.dot_ix == symbols.len() {
      write!(fmt, " .")?;
    }

    write!(fmt, "      ")?;

    let mut slash = false;
    for lookahead in lookaheads.iter() {
      if slash {
        write!(fmt, " / ")?;
      }
      slash = true;

      let lookahead = lookahead as u32;
      write!(fmt, "{}",
        self.grammar.tokens.get(&lookahead).map(|s|s.as_str()).unwrap_or("$"))?;
    }

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_snapshot};
  use grammar::LoweredGrammar;
  use crate::augment;

  fn prepare(input: &str) -> LoweredGrammar {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, _) = augment::augment(grammar);

    grammar
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
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
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
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }

  static PRECEDENCE: &str = r#"
%token minus "-"
%token mult "*"
%token pow "^"
%token equal "=="
%token lparen "("
%token rparen ")"
%token num "1"
%right-assoc NEG
%right-assoc POW
%left-assoc MUL
%left-assoc ADD
%non-assoc REL
%start E
E = E minus E    %prec ADD
  | E mult E    %prec MUL
  | E pow E     %prec POW
  | E equal E     %prec REL
  | minus E       %prec NEG
  | lparen E rparen
  | num
  "#;

  #[test]
  fn precedence_states() {
    let grammar = prepare(PRECEDENCE);
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
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
  fn tiger_states() {
    let grammar = prepare(TIGER);
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }
}