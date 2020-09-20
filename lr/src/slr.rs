use grammar::{ Symbol, LoweredGrammar, TokenId };
use crate::Error;
use crate::ffn::Ffn;
use crate::builder::{LrCalculation, LrItem};

pub enum SlrCalc {}

impl LrCalculation for SlrCalc {
  type Item = SlrItem;

  fn start_item(
    start_prod_ix: usize,
    _eof: TokenId,
  ) -> SlrItem {
    SlrItem {
      prod_ix: start_prod_ix,
      dot_ix: 0,
    }
  }

  fn next_item(
    item: &SlrItem
  ) -> SlrItem {
    SlrItem {
      prod_ix: item.prod_ix,
      dot_ix: item.dot_ix + 1,
    }
  }

  fn closure_step<F>(
    grammar: &LoweredGrammar,
    _ffn: &Ffn,
    prev: &SlrItem,
    mut action: F
  )
    where F: FnMut(SlrItem)
  {
    let symbols = &grammar.prods[prev.prod_ix()].symbols;
    match &symbols[prev.dot_ix()] {
      Symbol::Token(_) => {}
      Symbol::Nonterminal(nt) => {
        for prod_ix in grammar.nt_metas[nt].range.clone() {
          action(SlrItem {
            prod_ix,
            dot_ix: 0
          });
        }
      }
    }
  }

  fn reduce_tokens<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    item: &SlrItem,
    action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>
  {
    let nt = grammar.prods[item.prod_ix()].nt;
    ffn.follow[&nt].iter().map(|x| x as u32).try_for_each(action)
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct SlrItem {
  prod_ix: usize,
  dot_ix: usize,
}

impl LrItem for SlrItem {
  fn prod_ix(&self) -> usize {
    self.prod_ix
  }

  fn dot_ix(&self) -> usize {
    self.dot_ix
  }

  #[cfg(test)]
  fn fmt(
    &self,
    grammar: &LoweredGrammar,
    f: &mut impl std::fmt::Write,
  ) -> std::fmt::Result {
    let nt = grammar.prods[self.prod_ix].nt;
    let symbols = &grammar.prods[self.prod_ix].symbols;

    write!(f, "{} ->", grammar.nts[&nt])?;

    for (i, sym) in symbols.iter().enumerate() {
      if i == self.dot_ix {
        write!(f, " .")?;
      }

      match sym {
        Symbol::Token(token) => {
          let name = grammar.tokens.get(token).map(|s|s.as_str()).unwrap_or("$");
          write!(f, " {}", name)?;
        }
        Symbol::Nonterminal(nt) => {
          let name = grammar.nts.get(nt).unwrap();
          write!(f, " {}", name)?;
        }
      }
    }

    if self.dot_ix == symbols.len() {
      write!(f, " .")?;
    }

    Ok(())
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
  use crate::Error;

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
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

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
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn epsilon_action_goto() {
    let (grammar, eof_token, ffn) = prepare(EPSILON);
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

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
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn precedence_action_goto() {
    let (grammar, eof_token, ffn) = prepare(PRECEDENCE);
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }

  static AMBIGUOUS: &str = r#"
%token a "a"
%token b "b"
%token c "c"
%token d "d"

%start S

// this is LR(1) but not SLR
// from https://stackoverflow.com/questions/10505717/how-is-this-grammar-lr1-but-not-slr1#:~:text=The%20only%20possible%20shift%2Freduce,)%20and%20LR(1).
S = A a | b A c | d c | b d a
A = d
  "#;

  #[test]
  fn ambiguous() {
    let (grammar, eof_token, ffn) = prepare(AMBIGUOUS);
    let mut builder = Builder::<SlrCalc>::new(&grammar, eof_token, ffn);

    let result = builder.build();

    assert_eq!(result, Err(Error::ShiftReduceConflict));
  }
}