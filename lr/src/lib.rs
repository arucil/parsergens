use grammar::{ProductionKind, TokenId, Map, BiMap};

pub mod slr;
mod ffn;
mod augment;

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
  /// (length of RHS of the production, non-terminal id)
  pub prods: Vec<(usize, u32, ProductionKind)>,
  /// (name, type)
  pub nts: Vec<(String, Option<String>)>,
  /// (non-terminal name, starting state)
  pub start: Map<String, u32>,
  pub eof_index: usize,
  pub lexer: Option<grammar::Lexer>,
  pub tokens: BiMap<TokenId, String>,
  pub user_code: Vec<String>,
}