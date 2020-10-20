//! compute FIRST, FOLLOW, and NULLABLE sets.

use bittyset::{BitSet, bitset};
use bitvec::prelude::*;
use grammar::{LoweredGrammar, NonterminalId, Symbol};
use crate::Map;

#[derive(Debug, Clone)]
pub struct FirstAndNullable {
  /// indexed by NT
  pub first: Vec<BitSet>,
  /// indexed by NT
  pub nullable: BitSet,
}

pub fn compute(grammar: &LoweredGrammar) -> FirstAndNullable {
  let nullable = bitset![];
  let first = vec![bitset![]; grammar.nts.len()];

  let mut changed = true;
  while changed {
    changed = false;
    for prod in &grammar.prods {
      for sym in &prod.symbols {
      }
    }
  }

  FirstAndNullable {
    first,
    nullable,
  }
}

fn compute_first(
  grammar: &LoweredGrammar,
  symbols: &[Symbol],
  last: Option<&BitSet>,
) -> BitSet {
  let mut first = Map::<NonterminalId, BitSet>::default();

  for &nt in grammar.nts.keys() {
    compute_nonterminal_first(grammar, nullable, &mut first, &mut BitSet::new(), nt);
  }

  first
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn simple() {
    let grammar = grammar::build(r#"
%token a "a"
%token c "c"
%token d "d"

%start Z

Z = d
  | X Y Z

Y = ()
  | c

X = Y
  | a
    "#).unwrap();

    let lowered = grammar.lower();
    let first = compute(&lowered);

    assert_debug_snapshot!(first);
  }

  #[test]
  fn recursive() {
    let grammar = grammar::build(r#"
%token int "1"
%token id "a"
%token lparen "("
%token rparen ")"
%token plus "+"
%token mult "*"
%token comma ","

%start E

E = E plus E
  | E mult E
  | lparen E rparen
  | plus E
  | int
  | id
  | id lparen PARAM-LIST rparen

PARAM-LIST = ()
  | E (comma E)*
    "#).unwrap();

    let lowered = grammar.lower();
    let first = compute(&lowered);

    assert_debug_snapshot!(first);
  }

  #[test]
  fn ll_expr() {
    let grammar = grammar::build(r#"
%token plus "+"
%token mult "*"
%token num "1"
%token lparen "("
%token rparen ")"

%start S

S = E

E = T E'

E' = plus T E'
   | ()

T = F T'

T' = mult F T'
   | ()

F = num
  | lparen E rparen
    "#).unwrap();

    let lowered = grammar.lower();
    let first = compute(&lowered);

    assert_debug_snapshot!(first);
  }
}
