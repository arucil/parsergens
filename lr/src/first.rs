//! compute FIRST, FOLLOW, and NULLABLE sets.

use grammar::{LoweredGrammar, Symbol};
use crate::token_set::TokenSet;

#[derive(Debug, Clone)]
pub struct NonterminalFirst {
  pub first: TokenSet,
  pub nullable: bool,
}

pub(crate) fn compute(grammar: &LoweredGrammar) -> Vec<NonterminalFirst> {
  let mut first = TokenSet::new(grammar.tokens.len() + 1);
  let mut nt_firsts = vec![
    NonterminalFirst {
      first: first.clone(),
      nullable: false,
    };
    grammar.nts.len()
  ];

  let mut changed = true;
  while changed {
    changed = false;
    for prod in &grammar.prods {
      let nt = prod.nt.index();
      if prod.symbols.iter().all(|sym| {
        match sym {
          Symbol::Token(_) => false,
          Symbol::Nonterminal(nt) => nt_firsts[nt.index()].nullable,
        }
      }) {
        changed |= !nt_firsts[nt].nullable;
        nt_firsts[nt].nullable = true;
      }

      first.clear();
      compute_symbols_first(&mut first, &nt_firsts, &prod.symbols, None);
      changed |= nt_firsts[nt].first.union_with(&first);
    }
  }

  nt_firsts
}

pub(crate) fn compute_symbols_first(
  result: &mut TokenSet,
  nt_firsts: &[NonterminalFirst],
  symbols: &[Symbol],
  last: Option<&TokenSet>,
) {
  for sym in symbols {
    match sym {
      Symbol::Token(tok) => {
        result.insert(tok.id());
        return;
      }
      Symbol::Nonterminal(nt) => {
        result.union_with(&nt_firsts[nt.index()].first);
        if !nt_firsts[nt.index()].nullable {
          return;
        }
      }
    }
  }

  if let Some(last) = last {
    result.union_with(last);
  }
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
