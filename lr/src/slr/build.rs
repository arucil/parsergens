use grammar::{ NonterminalIdGen, Symbol, LoweredGrammar };
use crate::{Parser, Production, Nonterminal, Error};
use crate::ffn;
use crate::ffn::Ffn;
use crate::augment;
use crate::builder::{Builder, LrStateCalculation, LrItem};

pub fn build(input: &str) -> Result<Parser, Error> {
  let grammar = grammar::build(input).map_err(Error::GrammarError)?;
  let grammar = grammar.lower();
  let (grammar, eof_token) = augment::augment(grammar);
  let ffn = ffn::compute(&grammar);

  let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

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
      let meta = &grammar.nt_metas[&nt];
      Nonterminal {
        name,
        ty: meta.ty.clone(),
        kind: meta.kind,
        range: meta.range.clone(),
      }
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

enum SlrStateCalc {}

impl LrStateCalculation for SlrStateCalc {
  type Item = (usize, usize);

  fn start_item(
    start_prod_ix: usize,
  ) -> Self::Item {
    (start_prod_ix, 0)
  }

  fn next_item(
    item: &Self::Item
  ) -> Self::Item {
    (item.0, item.1 + 1)
  }

  fn closure_step<F>(
    grammar: &LoweredGrammar,
    prev: &Self::Item,
    mut action: F
  )
    where F: FnMut(Self::Item)
  {
    let symbols = &grammar.prods[prev.prod_ix()].symbols;
    match &symbols[prev.dot_ix()] {
      Symbol::Token(_) => {}
      Symbol::Nonterminal(nt) => {
        for prod_ix in grammar.nt_metas[nt].range.clone() {
          action((prod_ix, 0));
        }
      }
    }
  }

  fn reduce_tokens<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    item: &Self::Item,
    action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>
  {
    let nt = grammar.prods[item.prod_ix()].nt;
    ffn.follow[&nt].iter().map(|x| x as u32).try_for_each(action)
  }
}

impl LrItem for (usize, usize) {
  fn prod_ix(&self) -> usize {
    self.0
  }

  fn dot_ix(&self) -> usize {
    self.1
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::{assert_debug_snapshot, assert_snapshot};
  use grammar::{ TokenId, LoweredGrammar };
  use crate::ffn::Ffn;

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
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

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
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn epsilon_action_goto() {
    let (grammar, eof_token, ffn) = prepare(EPSILON);
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

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
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn precedence_action_goto() {
    let (grammar, eof_token, ffn) = prepare(PRECEDENCE);
    let mut builder = Builder::<SlrStateCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }

}