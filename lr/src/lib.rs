#![feature(type_alias_impl_trait)]

use grammar::{TokenId, Map, NonterminalIdGen};
use std::ops::Range;
use std::collections::HashMap;
use builder::{Builder, LrCalculation};

pub use grammar::{UserState, NonterminalKind, ProductionKind, GrammarError};

mod slr;
mod clr;
mod lalr;
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
  pub start: HashMap<String, (u32, u32)>,
  pub eof_index: usize,
  pub lexer: Option<grammar::Lexer>,
  pub tokens: Map<TokenId, String>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
  GrammarError(GrammarError),
  ShiftReduceConflict(ShiftReduceConflictError),
  ReduceReduceConflict(ReduceReduceConflictError),
  PrecConflict(PrecConflictError),
  AssocConflict(AssocConflictError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShiftReduceConflictError {
  pub state_items: Vec<String>,
  pub shift: String,
  pub reduce: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReduceReduceConflictError {
  pub state_items: Vec<String>,
  pub lookahead: String,
  pub reduce1: String,
  pub reduce2: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrecConflictError {
  pub state_items: Vec<String>,
  pub prod1: String,
  pub prod2: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssocConflictError {
  pub state_items: Vec<String>,
  pub prod1: String,
  pub prod2: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserKind {
  Slr,
  Clr,
  Lalr,
}

pub fn build(input: &str, kind: ParserKind) -> Result<Parser, Error> {
  match kind {
    ParserKind::Slr => build_parser::<slr::SlrCalc>(input),
    ParserKind::Clr => build_parser::<clr::ClrCalc>(input),
    ParserKind::Lalr => build_parser::<lalr::LalrCalc>(input),
  }
}

fn build_parser<T>(
  input: &str
) -> Result<Parser, Error>
  where T: LrCalculation
{
  let grammar = grammar::build(input).map_err(Error::GrammarError)?;
  let grammar = grammar.lower();
  let (grammar, eof_token) = augment::augment(grammar);
  let ffn = ffn::compute(&grammar);

  let mut builder = Builder::<T>::new(&grammar, eof_token, ffn);

  builder.build()?;

  let action = builder.build_action_table();
  let goto = builder.build_goto_table();

  let prods = grammar.prods.iter().map(|prod| {
    let symbols = prod.symbols.iter()
      .map(|sym| {
        match sym {
          grammar::Symbol::Token(tok) => Symbol::Token(*tok),
          grammar::Symbol::Nonterminal(nt) => Symbol::Nonterminal(nt.id()),
        }
      })
      .collect();

    Production {
      rhs_len: prod.symbols.len(),
      nt: prod.nt.id(),
      symbols,
      kind: prod.kind,
      action: prod.action.clone(),
    }
  }).collect();

  let nts = (0..grammar.nts.len()).scan(
    NonterminalIdGen::default(),
    |gen, _| Some(gen.gen()))
    .map(|nt| {
      let name = grammar.nts[&nt].clone();
      let meta = &grammar.nt_metas[&nt];
      Nonterminal {
        name,
        ty: meta.ty.clone(),
        kind: meta.kind,
        range: meta.range.clone(),
      }
    })
    .collect();

  Ok(Parser {
    action,
    goto,
    prods,
    nts,
    start: builder.start,
    eof_index: eof_token.id() as usize,
    lexer: grammar.lexer,
    tokens: grammar.tokens,
    user_code: grammar.user_code,
    user_state: grammar.user_state,
  })
}