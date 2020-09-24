use grammar::{ Symbol, LoweredGrammar, TokenId };
use bit_set::BitSet;
use crate::Error;
use crate::ffn::Ffn;
use crate::builder::{LrCalculation, LrItem};

pub enum ClrCalc {}

impl LrCalculation for ClrCalc {
  type Item = Lr1Item;

  fn start_item(
    start_prod_ix: usize,
    eof_token: TokenId,
  ) -> Lr1Item {
    Lr1Item {
      prod_ix: start_prod_ix,
      dot_ix: 0,
      token: eof_token.id(),
    }
  }

  fn next_item(
    item: &Lr1Item
  ) -> Lr1Item {
    Lr1Item {
      dot_ix: item.dot_ix + 1,
      ..*item
    }
  }

  fn closure_step<F>(
    grammar: &LoweredGrammar,
    ffn: &Ffn,
    prev: &Lr1Item,
    mut action: F
  )
    where F: FnMut(Lr1Item)
  {
    let symbols = &grammar.prods[prev.prod_ix].symbols;

    match &symbols[prev.dot_ix] {
      Symbol::Token(_) => {}
      Symbol::Nonterminal(nt) => {
        let mut first = BitSet::new();
        let mut rest_empty = true;
        for sym in &symbols[prev.dot_ix + 1..] {
          match sym {
            Symbol::Token(tok) => {
              first.insert(tok.id() as usize);
              rest_empty = false;
              break;
            }
            Symbol::Nonterminal(nt) => {
              first.union_with(&ffn.first[nt]);
              if !ffn.nullable.contains(nt.id() as usize) {
                rest_empty = false;
                break;
              }
            }
          }
        }

        if rest_empty {
          first.insert(prev.token as usize);
        }

        for prod_ix in grammar.nt_metas[nt].range.clone() {
          for token in first.iter() {
            action(Lr1Item {
              prod_ix,
              dot_ix: 0,
              token: token as u32,
            });
          }
        }
      }
    }
  }

  fn reduce_tokens<F>(
    _grammar: &LoweredGrammar,
    _ffn: &Ffn,
    item: &Lr1Item,
    mut action: F,
  ) -> Result<(), Error>
    where F: FnMut(u32) -> Result<(), Error>
  {
    action(item.token)
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Lr1Item {
  pub prod_ix: usize,
  pub dot_ix: usize,
  // MAX if it's an item of a merged LALR state.
  pub token: u32,
}

impl LrItem for Lr1Item {
  fn prod_ix(&self) -> usize {
    self.prod_ix
  }

  fn dot_ix(&self) -> usize {
    self.dot_ix
  }

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
          let name = &grammar.nts[nt];
          write!(f, " {}", name)?;
        }
      }
    }

    if self.dot_ix == symbols.len() {
      write!(f, " .")?;
    }

    if self.token != std::u32::MAX {
      write!(f, "      {}",
        grammar.tokens.get(&self.token).map(|s|s.as_str()).unwrap_or("$"))?;
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

  fn prepare(input: &str) -> (LoweredGrammar, TokenId, Ffn) {
    let grammar = grammar::build(input).unwrap();
    let grammar = grammar.lower();
    let (grammar, eof_token) = augment::augment(grammar);
    let ffn = ffn::compute(&grammar);

    (grammar, eof_token, ffn)
  }

  static SIMPLE: &str = r#"
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
  fn simple_states() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<ClrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_snapshot!(builder.states());
  }

  #[test]
  fn simple_action_goto() {
    let (grammar, eof_token, ffn) = prepare(SIMPLE);
    let mut builder = Builder::<ClrCalc>::new(&grammar, eof_token, ffn);

    builder.build().unwrap();

    assert_debug_snapshot!(builder.action_goto());
  }
}