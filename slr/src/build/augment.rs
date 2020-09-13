use grammar::NonterminalIdGen;
use super::lower::{LoweredGrammar, Production, ProductionAction, Symbol};

/// Add S' -> S to grammar.
pub fn augment(grammar: LoweredGrammar) -> LoweredGrammar {
  let max_nt_id = grammar.nts
    .left_values()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let mut prods = grammar.prods;
  let mut nt_prods = grammar.nt_prods;

  let start_nts = grammar.start_nts.into_iter().map(|nt| {
    let new_start_nt = nt_id_gen.gen();
    let start = prods.len();

    prods.push(Production {
      nt: new_start_nt,
      action: ProductionAction::None,
      symbols: vec![Symbol::Nonterminal(nt)],
    });

    nt_prods.insert(new_start_nt, start..prods.len());

    new_start_nt
  }).collect();

  LoweredGrammar {
    prods,
    start_nts,
    nt_prods,
    ..grammar
  }
}