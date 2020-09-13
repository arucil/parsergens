use std::ops::Range;
use grammar::{Grammar, NonterminalIdGen, Item};
use crate::{Set, BiMap, Map};

pub use grammar::{NonterminalId, Lexer, TokenId};

#[derive(Debug)]
pub struct LoweredGrammar {
  pub prods: Vec<Production>,
  pub start_nts: Set<NonterminalId>,
  pub nts: BiMap<NonterminalId, String>,
  pub nt_prods: Map<NonterminalId, Range<usize>>,
  pub lexer: Lexer,
}

#[derive(Debug)]
pub struct Production {
  pub nt: NonterminalId,
  pub action: ProductionAction,
  pub symbols: Vec<Symbol>,
}

#[derive(Debug)]
pub enum ProductionAction {
  None,
  RepetitionFirst,
  RepetitionRest,
}

#[derive(Clone, Debug)]
pub enum Symbol {
  Nonterminal(NonterminalId),
  Token(TokenId),
}

pub fn lower(grammar: Grammar) -> LoweredGrammar {
  let max_nt_id = grammar.nts
    .left_values()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let mut lowered = LoweredGrammar {
    prods: vec![],
    start_nts: grammar.start_nts,
    nts: grammar.nts,
    nt_prods: Map::new(),
    lexer: grammar.lexer,
  };

  for (nt, range) in grammar.nt_prods {
    let mut prod_symbols = vec![];

    for prod in &grammar.prods[range] {
      prod_symbols.push(
        lower_items(&prod.items, &mut lowered, &mut nt_id_gen));
    }

    let start = lowered.prods.len();

    for symbols in prod_symbols {
      lowered.prods.push(Production {
        nt,
        action: ProductionAction::None,
        symbols,
      });
    }

    let end = lowered.prods.len();

    lowered.nt_prods.insert(nt, start..end);
  }

  lowered
}

fn lower_items(
  items: &[Item],
  lowered: &mut LoweredGrammar,
  nt_id_gen: &mut NonterminalIdGen,
) -> Vec<Symbol> {
  items.iter().map(|item| {
    match item {
      Item::Nonterminal(nt) => Symbol::Nonterminal(*nt),
      Item::Token(token) => Symbol::Token(*token),
      Item::Optional(items) => {
        let symbols = lower_items(items, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '?');
        lowered.nts.insert(nt, name);

        lowered.prods.push(Production {
          nt,
          action: ProductionAction::None,
          symbols: vec![],
        });
        lowered.prods.push(Production {
          nt,
          action: ProductionAction::None,
          symbols,
        });

        let end = lowered.prods.len();

        lowered.nt_prods.insert(nt, start..end);

        Symbol::Nonterminal(nt)
      }
      Item::Many(items) => {
        let mut symbols = lower_items(items, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '*');
        lowered.nts.insert(nt, name);

        lowered.prods.push(Production {
          nt,
          action: ProductionAction::RepetitionFirst,
          symbols: vec![],
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          action: ProductionAction::RepetitionRest,
          symbols,
        });

        let end = lowered.prods.len();

        lowered.nt_prods.insert(nt, start..end);

        Symbol::Nonterminal(nt)
      }
      Item::Many1(items) => {
        let mut symbols = lower_items(items, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '+');
        lowered.nts.insert(nt, name);

        lowered.prods.push(Production {
          nt,
          action: ProductionAction::RepetitionFirst,
          symbols: symbols.clone(),
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          action: ProductionAction::RepetitionRest,
          symbols,
        });

        let end = lowered.prods.len();

        lowered.nt_prods.insert(nt, start..end);

        Symbol::Nonterminal(nt)
      }
    }
  }).collect()
}

fn make_production_name(
  symbols: &[Symbol],
  lowered: &LoweredGrammar,
  suffix: char
) -> String {
  let mut buf = "(".to_owned();
  let mut space = false;

  for symbol in symbols {
    if space {
      buf.push(' ');
    }

    match symbol {
      Symbol::Nonterminal(nt) => {
        buf.push_str(lowered.nts.get_by_left(nt).unwrap());
      }
      Symbol::Token(token) => {
        buf.push_str(lowered.lexer.tokens.get_by_left(token).unwrap());
      }
    }

    space = true;
  }

  buf.push(')');
  buf.push(suffix);

  buf
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn lower_repetition() {
    let grammar = grammar::build(r#"
%token A "a"

%start top

top = A (atom (A)? )* A
  | (A atom)+

atom = A
  | ()
    "#).unwrap();

    let lowered = lower(grammar);

    assert_debug_snapshot!(lowered);
  }
}