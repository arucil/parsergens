//! compute FIRST, FOLLOW, and NULLABLE sets.

use bittyset::BitSet;
use bitvec::prelude::*;
use grammar::{LoweredGrammar, NonterminalId, Symbol};
use crate::Map;

#[derive(Debug, Clone)]
pub struct FirstAndNullable {
  pub first: Map<NonterminalId, BitSet>,
  pub nullable: BitSet,
}

pub fn compute(grammar: &LoweredGrammar) -> FirstAndNullable {
  let nullable = compute_nullable(grammar);
  let first = compute_first(grammar, &nullable);

  FirstAndNullable {
    first,
    nullable,
  }
}

fn compute_first(
  grammar: &LoweredGrammar,
  nullable: &BitSet
) -> Map<NonterminalId, BitSet> {
  let mut first = Map::<NonterminalId, BitSet>::default();

  for &nt in grammar.nts.keys() {
    compute_nonterminal_first(grammar, nullable, &mut first, &mut BitSet::new(), nt);
  }

  first
}

fn compute_nonterminal_first(
  grammar: &LoweredGrammar,
  nullable: &BitSet,
  first: &mut Map<NonterminalId, BitSet>,
  visiting: &mut BitSet,
  nt: NonterminalId,
) {
  if first.contains_key(&nt) || visiting.contains(nt.id() as usize) {
    return;
  }

  let range = grammar.nt_metas[&nt].range.clone();
  let mut nt_first = BitSet::new();

  for prod in &grammar.prods[range] {
    for symbol in &prod.symbols {
      match symbol {
        Symbol::Token(token) => {
          nt_first.insert(token.id() as usize);
          break;
        }
        Symbol::Nonterminal(nt_sym) => {
          let v = visiting.insert(nt.id() as usize);
          compute_nonterminal_first(grammar, nullable, first, visiting, *nt_sym);
          if !v {
            visiting.remove(nt.id() as usize);
          }

          if let Some(nt_sym_first) = first.get(nt_sym) {
            nt_first.union_with(nt_sym_first);
          }

          if !nullable.contains(nt_sym.id() as usize) {
            break;
          }
        }
      }
    }
  }

  first.insert(nt, nt_first);
}

fn compute_nullable(grammar: &LoweredGrammar) -> BitSet {
  let mut prods_nullable = bitvec![0; grammar.prods.len()];
  let mut prods_completed = bitvec![0; grammar.prods.len()];

  loop {
    let mut changed = false;

    'outer: for (i, prod) in grammar.prods.iter().enumerate() {
      if prods_completed[i] {
        continue;
      }

      let mut prod_nullable = true;
      for sym in &prod.symbols {
        match sym {
          Symbol::Token(_) => {
            prod_nullable = false;
            break;
          }
          Symbol::Nonterminal(nt) => {
            let nt_range = &grammar.nt_metas[nt].range;
            if BitSlice::all(&prods_completed[nt_range.clone()]) {
              if !prods_nullable[nt_range.clone()].some() {
                prod_nullable = false;
                break;
              }
            } else {
              continue 'outer;
            }
          }
        }
      }

      prods_nullable.set(i, prod_nullable);
      prods_completed.set(i, true);
      changed = true;
    }

    if !changed {
      break;
    }
  }

  grammar.nt_metas.iter().filter_map(|(nt, meta)| {
    if prods_nullable[meta.range.clone()].some() {
      Some(nt.id() as usize)
    } else {
      None
    }
  }).collect()
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
