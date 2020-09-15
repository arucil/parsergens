use std::ops::Range;
use crate::{Grammar, NonterminalIdGen, Item, Lexer, TokenId, NonterminalId};
use crate::{Set, BiMap, Map};

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
  pub kind: ProductionKind,
  pub symbols: Vec<Symbol>,
}

#[derive(Debug, Clone, Copy)]
pub enum ProductionKind {
  Ordinary,
  RepetitionFirst,
  RepetitionRest,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
  Nonterminal(NonterminalId),
  Token(TokenId),
}

pub(super) fn lower(grammar: Grammar) -> LoweredGrammar {
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
    let mut rule_symbols = vec![];

    for rule in &grammar.rules[range] {
      rule_symbols.push(
        lower_items(&rule.items, &mut lowered, &mut nt_id_gen));
    }

    let start = lowered.prods.len();

    for symbols in rule_symbols {
      lowered.prods.push(Production {
        nt,
        kind: ProductionKind::Ordinary,
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
          kind: ProductionKind::Ordinary,
          symbols: vec![],
        });
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::Ordinary,
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
          kind: ProductionKind::RepetitionFirst,
          symbols: vec![],
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
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
          kind: ProductionKind::RepetitionFirst,
          symbols: symbols.clone(),
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
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
    let grammar = crate::build(r#"
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