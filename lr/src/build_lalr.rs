use std::collections::VecDeque;
use grammar::{LoweredGrammar, Symbol, Map, NonterminalId, Assoc, HashMap};
use bit_set::BitSet;
use std::fmt::{self, Write};
use crate::first::FirstAndNullable;
use crate::{Error, ShiftReduceConflictError, ReduceReduceConflictError, EntryPoint};

/// Lookahead set of an item.
type LookaheadSet = HashMap<u32, BitSet>;

pub struct Builder<'a> {
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
  goto: Vec<HashMap<Symbol, u32>>,
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
  pub fn new(grammar: &'a LoweredGrammar) -> Self {
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

pub fn build_states(
  builder: &mut Builder,
  grammar: &LoweredGrammar,
) -> HashMap<String, EntryPoint> {
  let fan = crate::first::compute(grammar);
  let mut entry_points = HashMap::default();

  for &start_nt in &grammar.start_nts {
    let start_state = start(builder, grammar, &fan, start_nt);
    let start_prod_ix = grammar.nt_metas[&start_nt].range.start;
    let real_start_nt =
      match grammar.prods[start_prod_ix].symbols[0] {
        Symbol::Nonterminal(nt) => nt,
        _ => unreachable!(),
      };
    let nt_name = grammar.nts.get(&real_start_nt).unwrap().clone();
    entry_points.insert(nt_name, EntryPoint {
      real_start_nt: real_start_nt.id(),
      start_state,
      accept_prod: start_prod_ix,
    });
  }

  entry_points
}

/// returns ACTION table and GOTO table.
/// 
/// entry in ACTION table:
/// - positive: shift
/// - negative: reduce
/// - zero: error
///
/// entry in GOTO table:
/// - positive: goto
/// - zero: error
pub fn build_tables(
  builder: &Builder,
) -> Result<(Vec<Vec<i32>>, Vec<Vec<u32>>), Error> {
  let num_states = builder.state_store.states.len();
  let mut action = vec![vec![0i32; builder.eof as usize + 1]; num_states];
  let mut goto = vec![vec![0u32; builder.grammar.nts.len()]; num_states];

  for (from_state, tx) in builder.state_store.goto.iter().enumerate() {
    let (item_set, lookaheads) = &builder.state_store.states[from_state];
    for item_ix in item_set.iter() {
      let item = builder.item_store.items[item_ix];
      let symbols = &builder.grammar.prods[item.prod_ix].symbols;
      // shift
      if item.dot_ix < symbols.len() {
        let sym = &symbols[item.dot_ix];
        let to_state = tx[sym];
        match sym {
          Symbol::Token(tok) => {
            let old = &mut action[from_state][tok.id() as usize];
            if *old < 0 {
              match resolve_sr_conflict(&builder.grammar, !*old as usize, tok.id()) {
                SrConflictResolution::Shift => *old = tok.id() as i32 + 1,
                SrConflictResolution::Reduce => {
                  // do nothing
                }
                SrConflictResolution::Error => *old = 0,
                SrConflictResolution::Conflict => return Err(make_sr_conflict_error(
                  builder,
                  item_set,
                  lookaheads,
                  tok.id(),
                  !*old as usize))
              }
            } else {
              assert!(*old == 0);
              *old = to_state as i32 + 1;
            }
          }
          Symbol::Nonterminal(nt) => {
            goto[from_state][nt.id() as usize] = to_state + 1;
          }
        }
      } else {
        // reduce
        for lookahead in lookaheads[&(item_ix as u32)].iter() {
          let old = &mut action[from_state][lookahead];
          if *old > 0 {
            match resolve_sr_conflict(&builder.grammar, item.prod_ix, lookahead as u32) {
              SrConflictResolution::Shift => {
                // do nothing
              }
              SrConflictResolution::Reduce => {
                *old = !(item.prod_ix as i32);
              }
              SrConflictResolution::Error => {
                *old = 0;
              }
              SrConflictResolution::Conflict => return Err(make_sr_conflict_error(
                builder,
                item_set,
                lookaheads,
                lookahead as u32,
                item.prod_ix))
            }
          } else if *old < 0 {
            return Err(make_rr_conflict_error(
              builder,
              item_set,
              lookaheads,
              lookahead as u32,
              !*old as usize,
              item.prod_ix));
          } else {
            *old = !(item.prod_ix as i32);
          }
        }
      }
    }
  }

  Ok((action, goto))
}

enum SrConflictResolution {
  Shift,
  Reduce,
  Error,
  Conflict,
}

fn resolve_sr_conflict(
  grammar: &LoweredGrammar,
  prod_ix: usize,
  tok: u32,
) -> SrConflictResolution {
  match (&grammar.prods[prod_ix].prec, grammar.token_precs.get(&tok)) {
    (Some((_, prec1)), Some((assoc2, prec2))) => {
      if prec1 == prec2 {
        match assoc2 {
          Assoc::LeftAssoc => SrConflictResolution::Reduce,
          Assoc::RightAssoc => SrConflictResolution::Shift,
          Assoc::NonAssoc => SrConflictResolution::Error,
        }
      } else if prec1 < prec2 {
        SrConflictResolution::Shift
      } else {
        SrConflictResolution::Reduce
      }
    }
    _ => SrConflictResolution::Conflict,
  }
}

fn make_rr_conflict_error(
  builder: &Builder,
  item_set: &BitSet,
  lookaheads: &LookaheadSet,
  lookahead: u32,
  reduce1: usize,
  reduce2: usize,
) -> Error {
  let lookahead = builder.grammar.tokens[&lookahead].clone();
  let reduce1 = builder.grammar.prods[reduce1].to_string(
    &builder.grammar);
  let reduce2 = builder.grammar.prods[reduce2].to_string(
    &builder.grammar);

  let state_items = item_set.iter()
    .map(|item_ix| {
      let mut buf = String::new();
      builder.fmt_item(&lookaheads[&(item_ix as u32)], item_ix, &mut buf).unwrap();
      buf
    })
    .collect();

  Error::ReduceReduceConflict(ReduceReduceConflictError {
    lookahead,
    state_items,
    reduce1,
    reduce2,
  })
}

fn make_sr_conflict_error(
  builder: &Builder,
  item_set: &BitSet,
  lookaheads: &LookaheadSet,
  token: u32,
  reduce_prod: usize,
) -> Error {
  let shift = builder.grammar.tokens[&token].clone();

  let state_items = item_set.iter()
    .map(|item_ix| {
      let mut buf = String::new();
      builder.fmt_item(&lookaheads[&(item_ix as u32)], item_ix, &mut buf).unwrap();
      buf
    })
    .collect();

  let reduce = builder.grammar.prods[reduce_prod].to_string(&builder.grammar);

  Error::ShiftReduceConflict(ShiftReduceConflictError {
    state_items,
    shift,
    reduce,
  })
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
    let mut m = HashMap::default();
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
    let mut to_states = Map::<Symbol, (BitSet, LookaheadSet)>::default();

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

      builder.state_store.goto[from_state as usize].insert(sym, to_state);

      if changed {
        queue.push_back(to_state);
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
    state_store.goto.push(HashMap::default());
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
impl<'a> Builder<'a> {
  fn states(
    &self,
    entry_points: &HashMap<String, EntryPoint>
  ) -> String {
    let mut output = String::new();
    self.fmt_states(entry_points, &mut output).unwrap();
    output
  }

  fn fmt_states(
    &self,
    entry_points: &HashMap<String, EntryPoint>,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    for (state, (item_set, lookaheads)) in self.state_store.states.iter().enumerate() {
      let state = state as u32;
      write!(fmt, "State {}", state)?;
      if entry_points.values().find(|x| x.start_state == state).is_some() {
        write!(fmt, " (start)")?;
      }
      writeln!(fmt)?;

      for item_ix in item_set.iter() {
        let lookaheads = &lookaheads[&(item_ix as u32)];
        self.fmt_item(lookaheads, item_ix, fmt)?;

        writeln!(fmt)?;
      }

      writeln!(fmt)?;
    }

    Ok(())
  }
}

impl<'a> Builder<'a> {
  fn fmt_item(
    &self,
    lookaheads: &BitSet,
    item_ix: usize,
    fmt: &mut impl Write,
  ) -> fmt::Result {
    let item = self.item_store.items[item_ix];
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
  use insta::{assert_snapshot, assert_debug_snapshot};
  use grammar::LoweredGrammar;
  use crate::augment;

  fn prepare(input: &str) -> LoweredGrammar {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, _) = augment::augment(grammar);

    grammar
  }

  fn merge_action_goto((action, goto): (Vec<Vec<i32>>, Vec<Vec<u32>>)) -> Vec<Vec<i32>> {
    action.into_iter().zip(goto).map(|(mut row1, row2)| {
      row1.extend(row2.into_iter().map(|x| x as i32));
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
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn simple_action_goto() {
    let grammar = prepare(SIMPLE);
    let mut builder = Builder::new(&grammar);
    let _start_nts = build_states(&mut builder, &grammar);
    let tables = merge_action_goto(build_tables(&builder).unwrap());

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
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn epsilon_action_goto() {
    let grammar = prepare(EPSILON);
    let mut builder = Builder::new(&grammar);
    let _start_nts = build_states(&mut builder, &grammar);
    let tables = merge_action_goto(build_tables(&builder).unwrap());

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
    let mut builder = Builder::new(&grammar);
    let start_nts = build_states(&mut builder, &grammar);

    assert_snapshot!(builder.states(&start_nts));
  }

  #[test]
  fn precedence_action_goto() {
    let grammar = prepare(PRECEDENCE);
    let mut builder = Builder::new(&grammar);
    let _start_nts = build_states(&mut builder, &grammar);
    let tables = merge_action_goto(build_tables(&builder).unwrap());

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
%non-assoc    ARRAY
%non-assoc    ASSIGN
%right-assoc  OR
%right-assoc  AND
%non-assoc    EQ
%non-assoc    REL
%left-assoc   ADD
%left-assoc   MULT
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