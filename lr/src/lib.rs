#![feature(type_alias_impl_trait)]

use grammar::{TokenId, Map, NonterminalIdGen, HashMap};
use std::ops::Range;
use builder::Builder;

pub use grammar::{
  LoweredGrammar,
  UserState,
  NonterminalKind,
  ProductionKind,
  GrammarError
};

//mod clr;
//mod lalr;
mod first;
mod augment;
mod builder;
mod build_lalr;
mod build_clr;

#[derive(Debug)]
pub struct Parser {
  /// - positive: shift (n - 1)  
  /// - zero: error
  /// - negative: reduce (-n - 1)
  /// - MIN: accept
  pub action: Vec<Vec<i32>>,
  /// - positive: goto (n - 1)
  /// - zero: error
  pub goto: Vec<Vec<u32>>,
  pub prods: Vec<Production>,
  pub nts: Vec<Nonterminal>,
  /// non-terminal name -> (non-terminal id, starting state)
  pub entry_points: HashMap<String, EntryPoint>,
  pub eof_index: usize,
  pub lexer: Option<grammar::Lexer>,
  pub tokens: Map<TokenId, String>,
  pub user_code: Vec<String>,
  pub user_state: Vec<UserState>,
}

#[derive(Debug)]
pub struct EntryPoint {
  pub real_start_nt: u32,
  pub start_state: u32,
  pub accept_prod: usize,
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
  Clr,
  Lalr,
}

pub fn build(input: &str, kind: ParserKind) -> Result<Parser, Error> {
  match kind {
    ParserKind::Clr => build_parser(
      input,
      build_clr::build_states,
      build_clr::build_tables),
    ParserKind::Lalr => build_parser(
      input,
      build_lalr::build_states,
      build_lalr::build_tables),
  }
}

fn build_parser<S: Default, I: Default>(
  input: &str,
  build_states: fn(&mut Builder<S, I>, &LoweredGrammar) -> HashMap<String, EntryPoint>,
  build_tables: fn(&Builder<S, I>) -> Result<(Vec<Vec<i32>>, Vec<Vec<u32>>), Error>,
) -> Result<Parser, Error> {
  let grammar = grammar::build(input).map_err(Error::GrammarError)?;
  let grammar = grammar.lower();
  let grammar = augment::augment(grammar);

  let mut builder = Builder::new(&grammar);
  let entry_points = build_states(&mut builder, &grammar);
  let (action, goto) = build_tables(&builder)?;

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
    entry_points: entry_points,
    eof_index: builder.eof as usize,
    lexer: grammar.lexer,
    tokens: grammar.tokens,
    user_code: grammar.user_code,
    user_state: grammar.user_state,
  })
}