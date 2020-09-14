use std::collections::VecDeque;
use bit_set::BitSet;
use grammar::{GrammarError, TokenId, NonterminalId, NonterminalIdGen};
use crate::{SlrParser, BiMap, Map};
use lower::{LoweredGrammar, Symbol};
use ffn::Ffn;

mod ffn;
mod lower;
mod augment;

#[derive(Debug)]
pub enum Error {
  GrammarError(GrammarError),
  ShiftReduceConflict,
  ReduceReduceConflict,
}

pub(crate) fn build(input: &str) -> Result<SlrParser, Error> {
  let grammar = grammar::build(input).map_err(Error::GrammarError)?;
  let grammar = lower::lower(grammar);
  let (grammar, eof_token) = augment::augment(grammar);
  let ffn = ffn::compute(&grammar);

  let mut builder = Builder::new(&grammar, eof_token, ffn);

  builder.build()?;

  let prods = grammar.prods.iter().map(|prod| {
    (prod.symbols.len(), prod.nt.id())
  }).collect();

  let nt_names = (0..grammar.nts.len()).scan(
    NonterminalIdGen::default(),
    |gen, _| Some(gen.gen()))
    .map(|nt| grammar.nts.get_by_left(&nt).unwrap().clone())
    .collect();

  Ok(SlrParser {
    action: builder.action,
    goto: builder.goto,
    prods,
    nt_names,
    start: builder.start,
    eof_index: eof_token.id() as usize,
    lexer: grammar.lexer
  })
}

struct Builder<'a> {
  grammar: &'a LoweredGrammar,
  ffn: Ffn,
  states: Map<BitSet, u32>,
  items: BiMap<(usize, usize), usize>,
  action: Vec<Vec<i32>>,
  action_row_len: usize,
  goto: Vec<Vec<u32>>,
  goto_row_len: usize,
  start: Map<String, u32>,
  eof_token: TokenId,
}

impl<'a> Builder<'a> {
  fn new(grammar: &'a LoweredGrammar, eof_token: TokenId, ffn: Ffn) -> Self {
    Builder {
      grammar: &grammar,
      ffn,
      states: Map::new(),
      items: BiMap::new(),
      action: vec![],
      action_row_len: eof_token.id() as usize + 1,
      goto: vec![],
      goto_row_len: grammar.nts.len(),
      start: Map::new(),
      eof_token,
    }
  }

  fn build(&mut self) -> Result<(), Error> {
    for &start_nt in &self.grammar.start_nts {
      let start_state = self.start(start_nt)?;
      let nt_name = self.grammar.nts.get_by_left(&start_nt).unwrap().clone();
      self.start.insert(nt_name, start_state);
    }

    Ok(())
  }

  fn start(&mut self, nt: NonterminalId) -> Result<u32, Error> {
    let start_prod = self.grammar.nt_prods[&nt].start;
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
        let is_new_state = !self.states.contains_key(&to_state);
        let to_state_ix = self.state(&to_state);

        match sym {
          Symbol::Token(token) => {
            let old = self.action[from_state][token.id() as usize];
            if old < 0 {
              return Err(Error::ShiftReduceConflict);
            }
            assert!(old == 0);

            self.action[from_state][token.id() as usize] = (to_state_ix + 1) as i32;
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
              for prod_ix in self.grammar.nt_prods[nt].clone() {
                let item = self.item(prod_ix, 0);
                if !result.contains(item) {
                  new.insert(item);
                }
              }

              /*
              if self.ffn.nullable.contains(nt.id() as usize) {
                let item = self.item(prod_ix, prod_dot + 1);
                if !result.contains(item) {
                  new.insert(item);
                }
              }
              */
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
    if let Some(&state) = self.states.get(set) {
      state
    } else {
      let len = self.states.len() as u32;
      self.states.insert(set.clone(), len);

      self.action.push(vec![0; self.action_row_len]);
      self.goto.push(vec![0; self.goto_row_len]);

      len
    }
  }

  fn reduce(&mut self, from_state: usize, prod_ix: usize) -> Result<(), Error> {
    let nt = self.grammar.prods[prod_ix].nt;

    for token in self.ffn.follow[&nt].iter() {
      let old = self.action[from_state][token];
      if old < 0 {
        //return Err(Error::ReduceReduceConflict);
        println!(">>>>>>>>>>>>>>> reduce-reduce");
      }
      //assert!(old == 0);

      self.action[from_state][token] = !(prod_ix as i32);
    }

    Ok(())
  }

  fn accept(&mut self, from_state: usize) {
    self.action[from_state][self.eof_token.id() as usize] = std::i32::MIN;
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
      if self.start.values().find(|&&x| x == state).is_some() {
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
              let name = self.grammar.lexer.tokens.get_by_left(token).map(|s|s.as_str()).unwrap_or("$");
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
    self.action.into_iter().zip(self.goto).map(|(mut row1, row2)| {
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
    let grammar = lower::lower(grammar);
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

E = T E_

E_ = plus T E_
   | ()

T = F T_

T_ = mult F T_
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
}