#![feature(type_alias_impl_trait)]

use grammar::{TokenId, Map, BiMap};
use std::ops::Range;

pub use grammar::{UserState, NonterminalKind, ProductionKind, GrammarError};

pub mod slr;
mod ffn;
mod augment;
mod builder;

#[derive(Debug)]
pub struct Parser {
  /// positive: shift (n - 1)  
  /// zero: error
  /// negative: reduce (-n - 1)
  /// MIN: accept
  pub action: Vec<Vec<i32>>,
  /// positive: goto (n - 1)
  /// zero: error
  pub goto: Vec<Vec<u32>>,
  pub prods: Vec<Production>,
  pub nts: Vec<Nonterminal>,
  /// non-terminal name -> (non-terminal id, starting state)
  pub start: Map<String, (u32, u32)>,
  pub eof_index: usize,
  pub lexer: Option<grammar::Lexer>,
  pub tokens: BiMap<TokenId, String>,
  pub user_code: Vec<String>,
  pub user_state: Vec<UserState>,
}

#[derive(Debug)]
pub struct Nonterminal {
  pub name: String,
  pub ty: Option<String>,
  pub kind: NonterminalKind,
  pub range: Range<usize>,
}

#[derive(Debug)]
pub struct Production {
  pub rhs_len: usize,
  pub nt: u32,
  pub symbols: Vec<Symbol>,
  pub kind: ProductionKind,
  pub action: Option<String>,
}

#[derive(Debug)]
pub enum Symbol {
  Token(TokenId),
  Nonterminal(u32),
}

#[derive(Debug)]
pub enum Error {
  GrammarError(GrammarError),
  ShiftReduceConflict,
  ReduceReduceConflict,
  PrecConflict,
  AssocConflict,
}