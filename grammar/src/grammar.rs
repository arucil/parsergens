use super::lexer::{Lexer, TokenId};
use crate::{Set, BiMap};

#[derive(Debug)]
pub struct Grammar {
  /// productions of the same nonterminals are consecutive.
  pub productions: Vec<Production>,
  pub start_nonterminals: Set<NonterminalId>,
  pub nonterminals: BiMap<NonterminalId, String>,
  pub lexer: Lexer,
}

#[derive(Debug)]
pub struct Production {
  pub nonterminal: NonterminalId,
  pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
  Nonterminal(NonterminalId),
  Token(TokenId),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct NonterminalId(u32);

#[derive(Debug)]

#[derive(Default)]
pub(crate) struct NonterminalIdGen(u32);

impl NonterminalIdGen {
  pub fn gen(&mut self) -> NonterminalId {
    let i = self.0;
    self.0 += 1;
    NonterminalId(i)
  }
}