use std::ops::Range;
use std::borrow::Borrow;
use super::lexer::{Lexer, TokenId};
use crate::{Set, BiMap, Map};

pub use lower::*;

pub mod lower;

#[derive(Debug)]
pub struct Grammar {
  /// productions of the same nonterminals are consecutive.
  pub rules: Vec<Rule>,
  /// starting non-terminals
  pub start_nts: Set<NonterminalId>,
  /// non-terminals
  pub nts: BiMap<NonterminalId, String>,
  pub nt_prods: Map<NonterminalId, Range<usize>>,
  pub lexer: Lexer,
}

#[derive(Debug)]
pub struct Rule {
  pub nt: NonterminalId,
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

impl Borrow<u32> for NonterminalId {
  fn borrow(&self) -> &u32 {
    &self.0
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

impl Grammar {
  pub fn lower(self) -> LoweredGrammar {
    lower::lower(self)
  }
}