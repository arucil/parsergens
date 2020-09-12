use std::ops::Range;
use grammar::{NonterminalId, Grammar, Lexer, TokenId, NonterminalIdGen, Item};
use crate::{Set, BiMap, Map};

#[derive(Debug)]
pub struct LoweredGrammar {
  pub productions: Vec<Production>,
  pub start_nonterminals: Set<NonterminalId>,
  pub nonterminals: BiMap<NonterminalId, String>,
  pub nonterminal_productions: Map<NonterminalId, Range<usize>>,
  pub lexer: Lexer,
}

#[derive(Debug)]
pub struct Production {
  pub nonterminal: NonterminalId,
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

fn lower(grammar: Grammar) -> LoweredGrammar {
  let max_nonterminal_id = grammar.nonterminals
    .left_values()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nonterminal_id_gen = NonterminalIdGen::from(max_nonterminal_id);

  let mut lowered = LoweredGrammar {
    productions: vec![],
    start_nonterminals: grammar.start_nonterminals,
    nonterminals: grammar.nonterminals,
    nonterminal_productions: Map::new(),
    lexer: grammar.lexer,
  };

  for (nonterminal, range) in grammar.nonterminal_productions {
    let mut production_symbols = vec![];

    for production in &grammar.productions[range] {
      production_symbols.push(
        lower_items(&production.items, &mut lowered, &mut nonterminal_id_gen));
    }

    let start = lowered.productions.len();

    for symbols in production_symbols {
      lowered.productions.push(Production {
        nonterminal,
        action: ProductionAction::None,
        symbols,
      });
    }

    let end = lowered.productions.len();

    lowered.nonterminal_productions.insert(nonterminal, start..end);
  }

  lowered
}

fn lower_items(
  items: &[Item],
  lowered: &mut LoweredGrammar,
  nonterminal_id_gen: &mut NonterminalIdGen,
) -> Vec<Symbol> {
  items.iter().map(|item| {
    match item {
      Item::Nonterminal(nonterminal) => Symbol::Nonterminal(*nonterminal),
      Item::Token(token) => Symbol::Token(*token),
      Item::Optional(items) => {
        let symbols = lower_items(items, lowered, nonterminal_id_gen);
        let start = lowered.productions.len();
        let nonterminal = nonterminal_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '?');
        lowered.nonterminals.insert(nonterminal, name);

        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::None,
          symbols: vec![],
        });
        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::None,
          symbols,
        });

        let end = lowered.productions.len();

        lowered.nonterminal_productions.insert(nonterminal, start..end);

        Symbol::Nonterminal(nonterminal)
      }
      Item::Many(items) => {
        let mut symbols = lower_items(items, lowered, nonterminal_id_gen);
        let start = lowered.productions.len();
        let nonterminal = nonterminal_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '*');
        lowered.nonterminals.insert(nonterminal, name);

        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::RepetitionFirst,
          symbols: vec![],
        });

        symbols.insert(0, Symbol::Nonterminal(nonterminal));
        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::RepetitionRest,
          symbols,
        });

        let end = lowered.productions.len();

        lowered.nonterminal_productions.insert(nonterminal, start..end);

        Symbol::Nonterminal(nonterminal)
      }
      Item::Many1(items) => {
        let mut symbols = lower_items(items, lowered, nonterminal_id_gen);
        let start = lowered.productions.len();
        let nonterminal = nonterminal_id_gen.gen();
        let name = make_production_name(&symbols, lowered, '+');
        lowered.nonterminals.insert(nonterminal, name);

        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::RepetitionFirst,
          symbols: symbols.clone(),
        });

        symbols.insert(0, Symbol::Nonterminal(nonterminal));
        lowered.productions.push(Production {
          nonterminal,
          action: ProductionAction::RepetitionRest,
          symbols,
        });

        let end = lowered.productions.len();

        lowered.nonterminal_productions.insert(nonterminal, start..end);

        Symbol::Nonterminal(nonterminal)
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
      Symbol::Nonterminal(nonterminal) => {
        buf.push_str(lowered.nonterminals.get_by_left(nonterminal).unwrap());
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