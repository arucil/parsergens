use std::ops::Range;
use std::borrow::Borrow;
use super::lexer::{Lexer, TokenId};
use super::Assoc;
use crate::{Set, Map};

pub use lower::*;

pub mod lower;

#[derive(Debug)]
pub struct Grammar {
  /// productions of the same nonterminals are consecutive.
  pub rules: Vec<Rule>,
  /// starting non-terminals
  pub start_nts: Set<NonterminalId>,
  /// non-terminals
  pub nts: Map<NonterminalId, String>,
  pub nt_metas: Map<NonterminalId, NonterminalMetadata>,
  pub lexer: Option<Lexer>,
  pub tokens: Map<TokenId, String>,
  pub token_precs: Map<TokenId, (Assoc, u32)>,
  pub user_code: Vec<String>,
  pub user_state: Vec<UserState>,
}

#[derive(Debug)]
pub struct Rule {
  pub nt: NonterminalId,
  pub items: Vec<Item>,
  pub prec: Option<(Assoc, u32)>,
  pub action: Option<String>,
}

#[derive(Debug)]
pub enum Item {
  Nonterminal(NonterminalId),
  Token(TokenId),
  Optional(Vec<Item>),
  Many(Vec<Item>),
  Many1(Vec<Item>),
}

#[derive(Debug)]
pub struct NonterminalMetadata {
  pub range: Range<usize>,
  pub ty: Option<String>,
}

#[derive(Debug)]
pub struct UserState {
  pub state: String,
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