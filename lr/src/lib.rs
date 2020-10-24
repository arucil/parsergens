#![feature(type_alias_impl_trait)]

use grammar::{TokenId, Map, NonterminalIdGen, HashMap};
use std::ops::Range;
use self::builder::{
  Builder, LrComputation, gen_states, gen_tables, compress_tables,
};

pub use self::builder::CompressedTables;

pub use grammar::{
  LoweredGrammar,
  UserState,
  NonterminalKind,
  ProductionKind,
  GrammarError
};

mod clr;
mod lalr;
mod first;
mod augment;
mod builder;
mod token_set;

#[derive(Debug)]
pub struct Parser {
  /// - ACTION table:
  ///     + positive: shift (state index)
  ///     + zero: error
  ///     + negative: reduce (-(state index) - 1)
  /// - GOTO table:
  ///     + positive: goto (state index)
  ///     + zero: error
  pub parse_tables: CompressedTables,
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

pub fn build(input: &str, kind: ParserKind) -> Result<Parser, Vec<Error>> {
  match kind {
    ParserKind::Clr => build_parser::<self::clr::ClrComputation>(input),
    ParserKind::Lalr => build_parser::<self::lalr::LalrComputation>(input),
  }
}

fn build_parser<T: LrComputation>(
  input: &str,
) -> Result<Parser, Vec<Error>> {
  let grammar = grammar::build(input)
    .map_err(|err| vec![
      Error::GrammarError(err)
    ])?;
  let grammar = grammar.lower();
  let grammar = augment::augment(grammar);

  let mut builder = Builder::new(&grammar);
  let entry_points = gen_states::<T>(&mut builder);
  let (action, goto) = gen_tables::<T>(&builder)?;
  let parse_tables = compress_tables(&grammar, action, goto);

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
    .map(|nt_id| {
      let name = grammar.nts[&nt_id].name.clone();
      let nt = &grammar.nts[&nt_id];
      Nonterminal {
        name,
        ty: nt.ty.clone(),
        kind: nt.kind,
        range: nt.range.clone(),
      }
    })
    .collect();

  Ok(Parser {
    parse_tables,
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