use std::ops::Range;
use std::fmt;
use crate::{
  Grammar, NonterminalIdGen, Item, Lexer, TokenId, NonterminalId, Assoc,
  Set, Map, UserState, HashMap,
};

#[derive(Debug)]
pub struct LoweredGrammar {
  pub prods: Vec<Production>,
  pub start_nts: Set<NonterminalId>,
  pub nts: Map<NonterminalId, LoweredNonterminal>,
  pub lexer: Option<Lexer>,
  pub tokens: Map<TokenId, String>,
  pub token_precs: Map<TokenId, (Assoc, u32)>,
  pub user_code: Vec<String>,
  pub user_state: Vec<UserState>,
}

#[derive(Debug)]
pub struct Production {
  pub nt: NonterminalId,
  pub kind: ProductionKind,
  pub symbols: Vec<Symbol>,
  pub prec: Option<(Assoc, u32)>,
  pub action: Option<String>,
}

#[derive(Debug)]
pub struct LoweredNonterminal {
  pub name: String,
  pub range: Range<usize>,
  pub ty: Option<String>,
  pub kind: NonterminalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonterminalKind {
  User,
  Repetition,
  Optional,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductionKind {
  Ordinary,
  RepetitionEpsilon,
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
    .keys()
    .map(|x| x.id())
    .max()
    .unwrap();
  let mut nt_id_gen = NonterminalIdGen::from(max_nt_id);

  let mut lowered = LoweredGrammar {
    prods: vec![],
    start_nts: grammar.start_nts,
    nts: Map::default(),
    lexer: grammar.lexer,
    tokens: grammar.tokens,
    token_precs: grammar.token_precs,
    user_code: grammar.user_code,
    user_state: grammar.user_state,
  };

  let nt_names = grammar.nts.iter()
    .map(|(&id, nt)| (id, nt.name.clone()))
    .collect();

  for (nt_id, nt) in grammar.nts {
    let mut rules = vec![];

    for rule in &grammar.rules[nt.range] {
      rules.push((
        lower_items(&rule.items, &nt_names, &mut lowered, &mut nt_id_gen),
        rule.prec,
        rule.action.clone()
      ));
    }

    let start = lowered.prods.len();

    for (symbols, prec, action) in rules {
      lowered.prods.push(Production {
        nt: nt_id,
        kind: ProductionKind::Ordinary,
        symbols,
        prec,
        action,
      });
    }

    let end = lowered.prods.len();

    let nt = LoweredNonterminal {
      name: nt.name,
      range: start..end,
      ty: nt.ty,
      kind: NonterminalKind::User,
    };

    lowered.nts.insert(nt_id, nt);
  }

  lowered
}

fn lower_items(
  items: &[Item],
  nt_names: &HashMap<NonterminalId, String>,
  lowered: &mut LoweredGrammar,
  nt_id_gen: &mut NonterminalIdGen,
) -> Vec<Symbol> {
  items.iter().map(|item| {
    match item {
      Item::Nonterminal(nt) => Symbol::Nonterminal(*nt),
      Item::Token(token) => Symbol::Token(*token),
      Item::Optional(items) => {
        let symbols = lower_items(items, nt_names, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, nt_names, &lowered.tokens, '?');

        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::Ordinary,
          symbols: vec![],
          prec: None,
          action: None,
        });
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::Ordinary,
          symbols,
          prec: None,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nts.insert(nt, LoweredNonterminal {
          name,
          range: start..end,
          ty: None,
          kind: NonterminalKind::Optional,
        });

        Symbol::Nonterminal(nt)
      }
      Item::Many(items) => {
        let mut symbols = lower_items(items, nt_names, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, nt_names, &lowered.tokens, '*');

        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionEpsilon,
          symbols: vec![],
          prec: None,
          action: None,
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
          symbols,
          prec: None,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nts.insert(nt, LoweredNonterminal {
          name,
          range: start..end,
          ty: None,
          kind: NonterminalKind::Repetition,
        });

        Symbol::Nonterminal(nt)
      }
      Item::Many1(items) => {
        let mut symbols = lower_items(items, nt_names, lowered, nt_id_gen);
        let start = lowered.prods.len();
        let nt = nt_id_gen.gen();
        let name = make_production_name(&symbols, nt_names, &lowered.tokens, '+');

        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionFirst,
          symbols: symbols.clone(),
          prec: None,
          action: None,
        });

        symbols.insert(0, Symbol::Nonterminal(nt));
        lowered.prods.push(Production {
          nt,
          kind: ProductionKind::RepetitionRest,
          symbols,
          prec: None,
          action: None,
        });

        let end = lowered.prods.len();

        lowered.nts.insert(nt, LoweredNonterminal {
          name,
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
  nt_names: &HashMap<NonterminalId, String>,
  tokens: &Map<TokenId, String>,
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
        buf.push_str(&nt_names[nt]);
      }
      Symbol::Token(token) => {
        buf.push_str(&tokens[token]);
      }
    }

    space = true;
  }

  buf.push(')');
  buf.push(suffix);

  buf
}

impl Production {
  pub fn fmt(&self, grammar: &LoweredGrammar, f: &mut impl fmt::Write) -> fmt::Result {
    write!(f, "{} ->", grammar.nts[&self.nt].name)?;
    for sym in &self.symbols {
      match sym {
        Symbol::Token(tok) => write!(f, " {}", grammar.tokens[tok])?,
        Symbol::Nonterminal(nt) => write!(f, " {}", grammar.nts[nt].name)?,
      }
    }
    Ok(())
  }

  pub fn to_string(&self, grammar: &LoweredGrammar) -> String {
    let mut s = String::new();
    self.fmt(grammar, &mut s).unwrap();
    s
  }
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