use grammar::NonterminalIdGen;
use grammar::{
  LoweredGrammar, Production, ProductionKind, Symbol,
  LoweredNonterminal, NonterminalKind,
};

/// Add `S' -> S $` to grammar, where `$` is a new token representing EOF.
pub fn augment(grammar: LoweredGrammar) -> LoweredGrammar {
  let max_nt_id = grammar.nts
    .keys()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let mut prods = grammar.prods;
  let mut nts = grammar.nts;

  let start_nts = grammar.start_nts.into_iter().map(|nt| {
    let new_start_nt = nt_id_gen.gen();
    let start = prods.len();

    prods.push(Production {
      nt: new_start_nt,
      kind: ProductionKind::Ordinary,
      symbols: vec![
        Symbol::Nonterminal(nt),
        //Symbol::Token(eof_token),
      ],
      prec: None,
      action: None,
    });

    let new_nt_name = format!("_{}", nts[&nt].name);

    nts.insert(new_start_nt, LoweredNonterminal {
      name: new_nt_name,
      range: start..prods.len(),
      ty: None,
      kind: NonterminalKind::User,
    });

    new_start_nt
  }).collect();

  let grammar = LoweredGrammar {
    prods,
    start_nts,
    nts,
    ..grammar
  };

  grammar
}
