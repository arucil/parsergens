use std::collections::VecDeque;
use bit_set::BitSet;
use grammar::{
  GrammarError, TokenId, NonterminalId, NonterminalIdGen, LoweredGrammar, Symbol,
  Assoc,
};
use crate::{BiMap, Map, Parser, Production};
use crate::ffn::Ffn;
use crate::ffn;
use crate::augment;

#[derive(Debug)]
pub enum Error {
  GrammarError(GrammarError),
  ShiftReduceConflict,
  ReduceReduceConflict,
  PrecConflict,
  AssocConflict,
}

pub fn build(input: &str) -> Result<Parser, Error> {
  let grammar = grammar::build(input).map_err(Error::GrammarError)?;
  let grammar = grammar.lower();
  let (grammar, eof_token) = augment::augment(grammar);
  let ffn = ffn::compute(&grammar);

  let mut builder = Builder::new(&grammar, eof_token, ffn);

  builder.build()?;

  let action = builder.build_action_table();

  let prods = grammar.prods.iter().map(|prod| {
    let symbols = prod.symbols.iter()
      .map(|sym| {
        match sym {
          Symbol::Token(tok) => crate::Symbol::Token(*tok),
          Symbol::Nonterminal(nt) => crate::Symbol::Nonterminal(nt.id()),
        }
      })
      .collect();

    Production {
      rhs_len: prod.symbols.len(),
      nt: prod.nt.id(),
      symbols,
      kind: prod.kind,
      action: prod.action.clone(),
    }
  }).collect();

  let nts = (0..grammar.nts.len()).scan(
    NonterminalIdGen::default(),
    |gen, _| Some(gen.gen()))
    .map(|nt| {
      let name = grammar.nts.get_by_left(&nt).unwrap().clone();
      let ty = grammar.nt_metas[&nt].ty.clone();
      (name, ty)
    })
    .collect();

  Ok(Parser {
    action,
    goto: builder.goto,
    prods,
    nts,
    start: builder.start,
    eof_index: eof_token.id() as usize,
    lexer: grammar.lexer,
    tokens: grammar.tokens,
    user_code: grammar.user_code,
    user_state: grammar.user_state,
  })
}

struct Builder<'a> {
  grammar: &'a LoweredGrammar,
  ffn: Ffn,
  states: BiMap<BitSet, u32>,
  items: BiMap<(usize, usize), usize>,
  /// state -> token -> (shift state, reduce production)
  action: Map<u32, Map<u32, ActionEntry>>,
  goto: Vec<Vec<u32>>,
  goto_row_len: usize,
  start: Map<String, (u32, u32)>,
  eof_token: TokenId,
}

#[derive(Default)]
struct ActionEntry {
  shift: Option<u32>,
  reduce: Option<u32>,
}

impl<'a> Builder<'a> {
  fn new(grammar: &'a LoweredGrammar, eof_token: TokenId, ffn: Ffn) -> Self {
    Builder {
      grammar: &grammar,
      ffn,
      states: BiMap::new(),
      items: BiMap::new(),
      action: Map::new(),
      goto: vec![],
      goto_row_len: grammar.nts.len(),
      start: Map::new(),
      eof_token,
    }
  }

  fn build(&mut self) -> Result<(), Error> {
    for &start_nt in &self.grammar.start_nts {
      let start_state = self.start(start_nt)?;
      let start_prod_ix = self.grammar.nt_metas[&start_nt].range.start;
      let real_start_nt =
        match self.grammar.prods[start_prod_ix].symbols[0] {
          Symbol::Nonterminal(nt) => nt,
          _ => unreachable!(),
        };
      let nt_name = self.grammar.nts.get_by_left(&real_start_nt).unwrap().clone();
      self.start.insert(nt_name, (real_start_nt.id(), start_state));
    }

    self.resolve_conflicts()?;

    Ok(())
  }

  fn resolve_conflicts(&mut self) -> Result<(), Error> {
    for (_, tx) in &mut self.action {
      for (_, ActionEntry { shift, reduce }) in tx {
        if let (&&mut Some(shift_state), &&mut Some(reduce_prod)) = (&shift, &reduce) {
          let (reduce_assoc, reduce_prec) = self.grammar.prods[reduce_prod as usize]
            .prec
            .ok_or(Error::ShiftReduceConflict)?;

          let mut shift_prec = None;
          for item in self.states.get_by_right(&shift_state).unwrap().iter() {
            let &(prod_ix, prod_dot) = self.items.get_by_right(&item).unwrap();
            if prod_dot == 0 {
              continue;
            }
            if let Some((assoc, prec)) = self.grammar.prods[prod_ix].prec {
              if let Some((last_assoc, last_prec)) = shift_prec {
                if last_prec != prec {
                  return Err(Error::PrecConflict);
                } else if last_assoc != assoc {
                  return Err(Error::AssocConflict);
                }
              } else {
                shift_prec = Some((assoc, prec));
              }
            }
          }

          if let Some((shift_assoc, shift_prec)) = shift_prec {
            if shift_prec == reduce_prec {
              if shift_assoc == reduce_assoc {
                match shift_assoc {
                  Assoc::LeftAssoc => *shift = None,
                  Assoc::RightAssoc => *reduce = None,
                  Assoc::NonAssoc => {
                    *shift = None;
                    *reduce = None;
                  }
                }
              } else {
                *shift = None;
                *reduce = None;
              }
            } else if shift_prec > reduce_prec {
              *reduce = None;
            } else {
              *shift = None;
            }
          } else {
            return Err(Error::ShiftReduceConflict);
          }
        }
      }
    }

    Ok(())
  }

  fn build_action_table(&self) -> Vec<Vec<i32>> {
    let mut action = vec![vec![0i32; self.eof_token.id() as usize + 1]; self.goto.len()];

    for (state, tx) in &self.action {
      let row = &mut action[*state as usize];
      for (token, entry) in tx {
        if let Some(new_state) = entry.shift {
          assert!(entry.reduce.is_none());
          row[*token as usize] = new_state as i32 + 1;
        } else if let Some(prod) = entry.reduce {
          row[*token as usize] = if prod == std::i32::MAX as u32 {
            std::i32::MIN
          } else {
            !(prod as i32)
          };
        }
      }
    }

    action
  }

  fn start(&mut self, nt: NonterminalId) -> Result<u32, Error> {
    let start_prod = self.grammar.nt_metas[&nt].range.start;
    let start_item = self.item(start_prod, 0);
    let start_state_set = self.closure({
      let mut set = BitSet::new();
      set.insert(start_item);
      set
    });
    let start_state = self.state(&start_state_set);

    let mut queue = VecDeque::new();
    queue.push_back(start_state_set);

    while let Some(state) = queue.pop_front() {
      let from_state = self.state(&state) as usize;
      let mut to_states = Map::<Symbol, BitSet>::new();

      for item in state.iter() {
        let &(prod_ix, prod_dot) = self.items.get_by_right(&item).unwrap();
        let symbols = &self.grammar.prods[prod_ix].symbols;
        if prod_dot == symbols.len() {
          self.reduce(from_state, prod_ix)?;
          continue;
        }

        if prod_dot == 1 && prod_ix == start_prod {
          self.accept(from_state);
          continue;
        }

        let new_item = self.item(prod_ix, prod_dot + 1);
        to_states.entry(symbols[prod_dot].clone()).or_default().insert(new_item);
      }

      for (sym, to_state) in to_states {
        let to_state = self.closure(to_state);
        let is_new_state = !self.states.contains_left(&to_state);
        let to_state_ix = self.state(&to_state);

        match sym {
          Symbol::Token(token) => {
            let entry = self.action[from_state].entry(token.id()).or_default();
            assert!(entry.shift.is_none());

            entry.shift = Some(to_state_ix);
          }
          Symbol::Nonterminal(nt) => {
            assert!(self.goto[from_state][nt.id() as usize] == 0);

            self.goto[from_state][nt.id() as usize] = to_state_ix + 1;
          }
        }

        if is_new_state {
          queue.push_back(to_state);
        }
      }
    }

    Ok(start_state)
  }

  fn closure(&mut self, initial: BitSet) -> BitSet {
    let mut result = initial.clone();
    let mut last = initial;

    loop {
      let mut new = BitSet::new();
      for i in last.iter() {
        let &(prod_ix, prod_dot) = self.items.get_by_right(&i).unwrap();
        let symbols = &self.grammar.prods[prod_ix].symbols;
        if prod_dot < symbols.len() {
          match &symbols[prod_dot] {
            Symbol::Token(_) => {}
            Symbol::Nonterminal(nt) => {
              for prod_ix in self.grammar.nt_metas[nt].range.clone() {
                let item = self.item(prod_ix, 0);
                if !result.contains(item) {
                  new.insert(item);
                }
              }
            }
          }
        }
      }

      if new.is_empty() {
        break;
      }

      result.union_with(&new);

      last = new;
    }

    result
  }

  fn item(&mut self, prod_ix: usize, prod_dot: usize) -> usize {
    if let Some(&item) = self.items.get_by_left(&(prod_ix, prod_dot)) {
      item
    } else {
      let len = self.items.len();
      self.items.insert((prod_ix, prod_dot), len);
      len
    }
  }

  fn state(&mut self, set: &BitSet) -> u32 {
    if let Some(&state) = self.states.get_by_left(set) {
      state
    } else {
      let state = self.states.len() as u32;
      self.states.insert(set.clone(), state);

      self.action.insert(state, Map::new());
      self.goto.push(vec![0; self.goto_row_len]);

      state
    }
  }

  fn reduce(&mut self, from_state: usize, prod_ix: usize) -> Result<(), Error> {
    let nt = self.grammar.prods[prod_ix].nt;

    for token in self.ffn.follow[&nt].iter() {
      let entry = self.action[from_state].entry(token as u32).or_default();
      if entry.reduce.is_some() {
        return Err(Error::ReduceReduceConflict);
      }
      assert!(entry.shift.is_none());

      entry.reduce = Some(prod_ix as u32);
    }

    Ok(())
  }

  fn accept(&mut self, from_state: usize) {
    let entry = self.action[from_state].entry(self.eof_token.id()).or_default();
    entry.reduce = Some(std::i32::MAX as u32);
  }
}

#[cfg(test)]
use std::fmt::{self, Write};

#[cfg(test)]
impl<'a> Builder<'a> {
  fn states(self) -> String {
    let mut buf = String::new();
    self.fmt_states(&mut buf).unwrap();
    buf
  }

  fn fmt_states(self, fmt: &mut impl Write) -> fmt::Result {
    let mut states = self.states.into_iter().collect::<Vec<_>>();
    states.sort_by_key(|(_, x)| *x);

    for (state_set, state) in states {
      write!(fmt, "State {}", state)?;
      if self.start.values().find(|x| x.1 == state).is_some() {
        write!(fmt, " (start)")?;
      }
      writeln!(fmt)?;

      for item in state_set.iter() {
        let &(prod_ix, prod_dot) = self.items.get_by_right(&item).unwrap();
        let nt = self.grammar.prods[prod_ix].nt;
        let symbols = &self.grammar.prods[prod_ix].symbols;

        write!(fmt, "{} ->", self.grammar.nts.get_by_left(&nt).unwrap())?;

        for (i, sym) in symbols.iter().enumerate() {
          if i == prod_dot {
            write!(fmt, " .")?;
          }

          match sym {
            Symbol::Token(token) => {
              let name = self.grammar.tokens.get_by_left(token).map(|s|s.as_str()).unwrap_or("$");
              write!(fmt, " {}", name)?;
            }
            Symbol::Nonterminal(nt) => {
              let name = self.grammar.nts.get_by_left(nt).unwrap();
              write!(fmt, " {}", name)?;
            }
          }
        } 

        if prod_dot == symbols.len() {
          write!(fmt, " .")?;
        }

        writeln!(fmt)?;
      }

      writeln!(fmt)?;
    }

    Ok(())
  }

  fn action_goto(self) -> Vec<Vec<i32>> {
    let action = self.build_action_table();
    action.into_iter().zip(self.goto).map(|(mut row1, row2)| {
      row1.extend(row2.into_iter().map(|x| x as i32));
      row1
    }).collect()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_debug_snapshot, assert_snapshot};

  fn prepare(input: &str) -> (LoweredGrammar, TokenId, Ffn) {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, eof_token) = augment::augment(grammar);
    let ffn = ffn::compute(&grammar);

    (grammar, eof_token, ffn)
  }

  static SIMPLE: &str = r#"
%token x "x"
%token plus "+"

%start E

E = T plus E
  | T

T = x
  "#;

  #[test]
  fn simple_states() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
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
    let (grammar, eof_token, ffn) = prepare(EPSILON);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn epsilon_action_goto() {
    let (grammar, eof_token, ffn) = prepare(EPSILON);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
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
    let (grammar, eof_token, ffn) = prepare(PRECEDENCE);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn precedence_action_goto() {
    let (grammar, eof_token, ffn) = prepare(PRECEDENCE);
    let mut builder = Builder::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }

}