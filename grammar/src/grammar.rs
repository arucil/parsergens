use std::ops::Range;
use super::lexer::{Lexer, TokenId};
use crate::{Set, BiMap, Map};

#[derive(Debug)]
pub struct Grammar {
  /// productions of the same nonterminals are consecutive.
  pub productions: Vec<Production>,
  pub start_nonterminals: Set<NonterminalId>,
  pub nonterminals: BiMap<NonterminalId, String>,
  pub nonterminal_productions: Map<NonterminalId, Range<usize>>,
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
  Optional(Vec<Item>),
  Many(Vec<Item>),
  Many1(Vec<Item>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct NonterminalId(u32);

impl NonterminalId {
  pub fn id(&self) -> u32 {
    self.0
  }
}

#[derive(Default)]
pub struct NonterminalIdGen(u32);

impl NonterminalIdGen {
  pub fn gen(&mut self) -> NonterminalId {
    let i = self.0;
    self.0 += 1;
    NonterminalId(i)
  }

  pub fn from(start: u32) -> Self {
    Self(start + 1)
  }
}