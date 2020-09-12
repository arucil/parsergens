//! compute FIRST, FOLLOW, and NULLABLE sets.

use bit_set::BitSet;
use grammar::{Grammar, NonterminalId, Item};
use crate::{Map, Set};

pub struct FfnResult {
  first: Map<NonterminalId, BitSet>,
  follow: Map<NonterminalId, BitSet>,
  nullable: Set<NonterminalId>,
}

pub fn compute(grammar: &Grammar) -> FfnResult {
  let nullable = compute_nullable(grammar);
  todo!()
}

fn compute_nullable(grammar: &Grammar) -> Set<NonterminalId> {
  let mut nullable = Set::new();
  let mut visited = Set::new();
  for &nonterminal in grammar.nonterminals.left_values() {
    compute_nullable_help(grammar, nonterminal, &mut nullable, &mut visited);
  }
  nullable
}

fn compute_nullable_help(
  grammar: &Grammar,
  nonterminal: NonterminalId,
  nullable: &mut Set<NonterminalId>,
  visited: &mut Set<NonterminalId>,
) -> bool {
  if visited.contains(&nonterminal) {
    return nullable.contains(&nonterminal);
  }

  let range = grammar.nonterminal_productions
    .get(&nonterminal).unwrap().clone();

  for production in &grammar.productions[range] {
    if compute_items_nullable_help(grammar, &production.items, nullable, visited) {
      nullable.insert(nonterminal);
      return true;
    }
  }

  false
}

fn compute_items_nullable_help(
  grammar: &Grammar,
  items: &[Item],
  nullable: &mut Set<NonterminalId>,
  visited: &mut Set<NonterminalId>,
) -> bool {
  items.iter().all(|item| {
    match item {
      Item::Nonterminal(nonterminal) => {
        compute_nullable_help(grammar, *nonterminal, nullable, visited)
      }
      Item::Token(_) => false,
      Item::Optional(_) => true,
      Item::Many(_) => true,
      Item::Many1(items) => {
        compute_items_nullable_help(grammar, items, nullable, visited)
      }
    }
  })
}