use grammar::{NonterminalIdGen, TokenIdGen};
use super::lower::{
  LoweredGrammar, Production, ProductionAction, Symbol, TokenId
};

/// Add `S' -> S $` to grammar, where `$` is a new token representing EOF.
pub fn augment(grammar: LoweredGrammar) -> (LoweredGrammar, TokenId) {
  let max_nt_id = grammar.nts
    .left_values()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let max_token_id = grammar.lexer.tokens
    .left_values()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut token_id_gen = TokenIdGen::from(max_token_id);
  let eof_token = token_id_gen.gen();

  let mut prods = grammar.prods;
  let mut nt_prods = grammar.nt_prods;
  let mut nts = grammar.nts;

  let start_nts = grammar.start_nts.into_iter().map(|nt| {
    let new_start_nt = nt_id_gen.gen();
    let start = prods.len();

    prods.push(Production {
      nt: new_start_nt,
      action: ProductionAction::None,
      symbols: vec![
        Symbol::Nonterminal(nt),
        Symbol::Token(eof_token),
      ],
    });

    nt_prods.insert(new_start_nt, start..prods.len());

    let new_nt_name = format!("_{}", nts.get_by_left(&nt).unwrap());
    nts.insert(new_start_nt, new_nt_name);

    new_start_nt
  }).collect();

  let grammar = LoweredGrammar {
    prods,
    start_nts,
    nt_prods,
    nts,
    ..grammar
  };

  (grammar, eof_token)
}