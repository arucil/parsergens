use std::ops::Range;
use crate::{
  Grammar, NonterminalIdGen, Item, Lexer, TokenId, NonterminalId
};
use crate::{Set, BiMap, Map};

#[derive(Debug)]
pub struct LoweredGrammar {
  pub prods: Vec<Production>,
  pub start_nts: Set<NonterminalId>,
  pub nts: BiMap<NonterminalId, String>,
  pub nt_metas: Map<NonterminalId, LoweredNonterminalMetadata>,
  pub lexer: Option<Lexer>,
  pub tokens: BiMap<TokenId, String>,
  pub user_code: Vec<String>,
}

#[derive(Debug)]
pub struct Production {
  pub nt: NonterminalId,
  pub kind: ProductionKind,
  pub symbols: Vec<Symbol>,
  pub action: Option<String>,
}

#[derive(Debug)]
pub struct LoweredNonterminalMetadata {
  pub range: Range<usize>,
  pub ty: Option<String>,
  pub kind: NonterminalKind,
}

#[derive(Debug, Clone, Copy)]
pub enum NonterminalKind {
  User,
  Repetition,
  Optional,
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
    nt_metas: Map::new(),
    lexer: grammar.lexer,
    tokens: grammar.tokens,
    user_code: grammar.user_code,
  };

  for (nt, meta) in grammar.nt_metas {
    let mut rules = vec![];

    for rule in &grammar.rules[meta.range] {
      rules.push((
        lower_items(&rule.items, &mut lowered, &mut nt_id_gen),
        rule.action.clone()
      ));
    }

    let start = lowered.prods.len();

    for (symbols, action) in rules {
      lowered.prods.push(Production {
        nt,
        kind: ProductionKind::Ordinary,
        symbols,
        action,
      });
    }

    let end = lowered.prods.len();

    let meta = LoweredNonterminalMetadata {
      range: start..end,
      ty: meta.ty,
      kind: NonterminalKind::User,
    };

    lowered.nt_metas.insert(nt, meta);
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
          action: None,
        });
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::Ordinary,
          symbols,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nt_metas.insert(nt, LoweredNonterminalMetadata {
          range: start..end,
          ty: None,
          kind: NonterminalKind::Optional,
        });

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
          action: None,
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
          symbols,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nt_metas.insert(nt, LoweredNonterminalMetadata {
          range: start..end,
          ty: None,
          kind: NonterminalKind::Repetition,
        });

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
          action: None,
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
          symbols,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nt_metas.insert(nt, LoweredNonterminalMetadata {
          range: start..end,
          ty: None,
          kind: NonterminalKind::Repetition,
        });

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
        buf.push_str(lowered.tokens.get_by_left(token).unwrap());
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