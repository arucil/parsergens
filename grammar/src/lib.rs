use lalrpop_util::ParseError;

mod grammar_parser;

pub mod lexer;
pub mod grammar;

pub use grammar::*;
pub use lexer::*;
use grammar_parser::lex::{Token, LexError};

#[derive(Debug)]
pub enum GrammarError {
  ParseError(String, usize, usize),
  TermNotFound(String, usize, usize),
}

pub fn build(grammar: &str) -> Result<Grammar, GrammarError> {
  let ast = grammar_parser::parse(grammar)
    .map_err(|err| err.into())?;

  todo!()
}

impl<'a> Into<GrammarError> for ParseError<usize, Token<'a>, LexError> {
  fn into(self) -> GrammarError {
    match self {
      Self::InvalidToken { location } => {
        GrammarError::ParseError("invalid token or EOF".to_owned(), location, location)
      }
      Self::UnrecognizedEOF { location, expected } => {
        GrammarError::ParseError(
          format!("expected {}, found EOF", expected.join(", ")),
          location,
          location)
      }
      Self::UnrecognizedToken { token, expected } => {
        GrammarError::ParseError(
          format!("expected {}, found {}", expected.join(", "), token.1),
          token.0,
          token.2)
      }
      Self::ExtraToken { token } => {
        GrammarError::ParseError(
          token.1.to_string(),
          token.0,
          token.2
        )
      }
      Self::User { error } => {
        match error {
          LexError::UnclosedRegex(start, end) => {
            GrammarError::ParseError("unclosed regexp".to_owned(), start, end)
          }
          LexError::InvalidChar(start, end) => {
            GrammarError::ParseError("invalid character".to_owned(), start, end)
          }
        }
      }
    }
  }
}