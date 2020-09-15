//! compute FIRST, FOLLOW, and NULLABLE sets.

use bit_set::BitSet;
use bitvec::prelude::*;
use super::lower::{LoweredGrammar, NonterminalId, Symbol};
use crate::Map;

#[derive(Default, Debug)]
pub struct Ffn {
  pub first: Map<NonterminalId, BitSet>,
  pub follow: Map<NonterminalId, BitSet>,
  pub nullable: BitSet,
}

pub fn compute(grammar: &LoweredGrammar) -> Ffn {
  let nullable = compute_nullable(grammar);
  let first = compute_first(grammar, &nullable);
  let follow = compute_follow(grammar, &nullable, &first);

  Ffn {
    nullable,
    first,
    follow
  }
}

fn compute_follow(
  grammar: &LoweredGrammar,
  nullable: &BitSet,
  first: &Map<NonterminalId, BitSet>,
) -> Map<NonterminalId, BitSet> {
  let mut follow = Map::<NonterminalId, BitSet>::new();

  loop {
    let mut changed = false;

    for prod in &grammar.prods {
      let mut sym_follow = None;
      for symbol in prod.symbols.iter().rev() {
        match symbol {
          Symbol::Token(token) => {
            sym_follow = Some({
              let mut set = BitSet::new();
              set.insert(token.id() as usize);
              set
            });
          }
          Symbol::Nonterminal(nt) => {
            if sym_follow.is_none() {
              sym_follow = Some(follow.get(&prod.nt).cloned().unwrap_or_default());
            }

            let nt_follow = follow.entry(*nt).or_default();
            let old = (*nt_follow).clone();

            match sym_follow {
              None => unreachable!(),
              Some(mut sf) => {
                nt_follow.union_with(&sf);
                if *nt_follow != old {
                  changed = true;
                }

                sym_follow = Some(if nullable.contains(nt.id() as usize) {
                  sf.union_with(&first[nt]);
                  sf
                } else {
                  first[nt].clone()
                });
              }
            }
          }
        }
      }
    }

    if !changed {
      break;
    }
  }

  follow
}

fn compute_first(
  grammar: &LoweredGrammar,
  nullable: &BitSet
) -> Map<NonterminalId, BitSet> {
  let mut first = Map::<NonterminalId, BitSet>::new();

  for &nt in grammar.nts.left_values() {
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

  let range = grammar.nt_prods[&nt].clone();
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
            let nt_range = &grammar.nt_prods[nt];
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

  grammar.nt_prods.iter().filter_map(|(nt, range)| {
    if prods_nullable[range.clone()].some() {
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
  fn ffn_simple() {
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

    let lowered = super::super::lower::lower(grammar);
    let ffn = compute(&lowered);

    assert_debug_snapshot!(ffn);
  }

  #[test]
  fn ffn_recursive() {
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

    let lowered = super::super::lower::lower(grammar);
    let ffn = compute(&lowered);

    assert_debug_snapshot!(ffn);
  }

  #[test]
  fn ffn_ll_expr() {
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

    let lowered = super::super::lower::lower(grammar);
    let ffn = compute(&lowered);

    assert_debug_snapshot!(ffn);
  }
}