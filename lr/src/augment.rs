use grammar::{NonterminalIdGen, TokenIdGen};
use grammar::{
  LoweredGrammar, Production, ProductionKind, Symbol, TokenId,
  LoweredNonterminalMetadata, NonterminalKind,
};

/// Add `S' -> S $` to grammar, where `$` is a new token representing EOF.
pub fn augment(grammar: LoweredGrammar) -> (LoweredGrammar, TokenId) {
  let max_nt_id = grammar.nts
    .keys()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let max_token_id = grammar.tokens
    .keys()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut token_id_gen = TokenIdGen::from(max_token_id);
  let eof_token = token_id_gen.gen();

  let mut prods = grammar.prods;
  let mut nt_metas = grammar.nt_metas;
  let mut nts = grammar.nts;

  let start_nts = grammar.start_nts.into_iter().map(|nt| {
    let new_start_nt = nt_id_gen.gen();
    let start = prods.len();

    prods.push(Production {
      nt: new_start_nt,
      kind: ProductionKind::Ordinary,
      symbols: vec![
        Symbol::Nonterminal(nt),
        Symbol::Token(eof_token),
      ],
      prec: None,
      action: None,
    });

    nt_metas.insert(new_start_nt, LoweredNonterminalMetadata {
      range: start..prods.len(),
      ty: None,
      kind: NonterminalKind::User,
    });

    let new_nt_name = format!("_{}", nts[&nt]);
    nts.insert(new_start_nt, new_nt_name);

    new_start_nt
  }).collect();

  let grammar = LoweredGrammar {
    prods,
    start_nts,
    nt_metas,
    nts,
    ..grammar
  };

  (grammar, eof_token)
}
