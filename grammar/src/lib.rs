#![feature(or_patterns, never_type)]

use lalrpop_util::ParseError;

mod grammar_parser;

pub mod lexer;
pub mod grammar;

use self::grammar::*;
pub use lexer::Lexer;

use grammar_parser::ast::Decl;
use grammar_parser::lex::{Token, LexErrorKind, LexError};
use grammar_parser::regex::{RegexError, RegexErrorKind};
use grammar_parser::UserParseError;
use lexer::LexerError;

#[derive(Debug)]
pub enum GrammarError {
  ParseError(String, usize, usize),
  TermNotFound(String, usize, usize),
}

pub fn build(grammar: &str) -> Result<Grammar, GrammarError> {
  let ast = grammar_parser::parse(grammar)
    .map_err(|err| err.into())?;

  let decls = ast.iter()
    .filter_map(|decl| match &decl.1 {
      Decl::Token(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let skips = ast.iter()
    .filter_map(|decl| match &decl.1 {
      Decl::Skip(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let lexer = Lexer::new(&decls, &skips).map_err(|err| {
    match err {
      LexerError::NoTokens => {
        GrammarError::ParseError("no token declarations".to_owned(), 0, grammar.len())
      }
      LexerError::RegexError(err) => {
        err.into()
      }
    }
  })?;

  todo!()
}

impl<'a> Into<GrammarError> for ParseError<usize, Token<'a>, UserParseError> {
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
          UserParseError::LexError(error) => error.into(),
          UserParseError::RegexError(error) => error.into(),
        }
      }
    }
  }
}

impl<'a> Into<GrammarError> for LexError {
  fn into(self) -> GrammarError {
    match self.kind {
      LexErrorKind::UnclosedRegex => {
        GrammarError::ParseError("unclosed regexp".to_owned(),
          self.span.0, self.span.1)
      }
      LexErrorKind::UnclosedString => {
        GrammarError::ParseError("unclosed string".to_owned(),
          self.span.0, self.span.1)
      }
      LexErrorKind::InvalidChar => {
        GrammarError::ParseError("invalid character".to_owned(),
          self.span.0, self.span.1)
      }
    }
  }
}

impl<'a> Into<GrammarError> for RegexError {
  fn into(self) -> GrammarError {
    match self.kind {
      RegexErrorKind::SyntaxError => {
        GrammarError::ParseError("regex syntax error".to_owned(),
          self.span.0, self.span.1)
      }
      RegexErrorKind::InvalidCodePoint => {
        GrammarError::ParseError("invalid code point".to_owned(),
          self.span.0, self.span.1)
      }
      RegexErrorKind::Empty => {
        GrammarError::ParseError("regex or string may accept empty string".to_owned(),
          self.span.0, self.span.1)
      }
    }
  }
}