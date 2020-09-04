#![feature(or_patterns, never_type)]

use lalrpop_util::ParseError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::files::SimpleFile;

mod grammar_parser;

pub mod lexer;
pub mod grammar;

use self::grammar::*;
pub use lexer::Lexer;

use grammar_parser::ast;
use grammar_parser::lex::{Token, LexErrorKind, LexError};
use grammar_parser::regex::{RegexError, RegexErrorKind};
use grammar_parser::UserParseError;
use lexer::LexerError;

#[cfg(not(debug_assertions))]
use std::collections::HashMap;
#[cfg(not(debug_assertions))]
use bimap::BiHashMap;

#[cfg(debug_assertions)]
use indexmap::{IndexMap, IndexSet};
#[cfg(debug_assertions)]
use bimap::BiBTreeMap;

#[cfg(not(debug_assertions))]
type Map<K, V> = HashMap<K, V>;

#[cfg(debug_assertions)]
type Map<K, V> = IndexMap<K, V>;

#[cfg(not(debug_assertions))]
type BiMap<K, V> = BiHashMap<K, V>;

#[cfg(debug_assertions)]
type BiMap<K, V> = BiBTreeMap<K, V>;

#[cfg(not(debug_assertions))]
type Set<K> = HashSet<K>;

#[cfg(debug_assertions)]
type Set<K> = IndexSet<K>;


#[derive(Debug)]
pub struct GrammarError {
  kind: GrammarErrorKind,
  message: String,
  span: (usize, usize),
}

#[derive(Debug)]
pub enum GrammarErrorKind {
  ParseError,
  TermNotFound,
  NameConflict,
}

pub fn build(grammar: &str) -> Result<Grammar, GrammarError> {
  let ast = grammar_parser::parse(grammar)
    .map_err(|err| err.into())?;

  let decls = ast.iter()
    .filter_map(|decl| match &decl.1 {
      ast::Decl::Token(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let skips = ast.iter()
    .filter_map(|decl| match &decl.1 {
      ast::Decl::Skip(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let starts = ast.iter()
    .filter_map(|decl| match &decl.1 {
      ast::Decl::Start(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let rules = ast.iter()
    .filter_map(|decl| match &decl.1 {
      ast::Decl::Rule(decl) => Some(decl),
      _ => None,
    })
    .collect::<Vec<_>>();

  let lexer = Lexer::new(&decls, &skips).map_err(|err| {
    match err {
      LexerError::NoTokens => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("no token declarations"),
          span: (0, grammar.len()),
        }
      }
      LexerError::RegexError(err) => {
        err.into()
      }
    }
  })?;

  let mut rule_id_gen = RuleIdGen::default();
  let rule_names = rules.iter()
    .map(|decl| {
      if lexer.token_names.get_by_right(&decl.name.1).is_some() {
        return Err(GrammarError {
          kind: GrammarErrorKind::NameConflict,
          message: format!("rule name collides with token name"),
          span: decl.name.0,
        });
      }

      let id = rule_id_gen.gen();

      Ok((id, decl.name.1.clone()))
    })
    .collect::<Result<BiMap<_, _>, GrammarError>>()?;

  let start_rules = starts.iter()
    .map(|decl| {
      if let Some(&id) = rule_names.get_by_right(&decl.name.1) {
        Ok(id)
      } else {
        Err(GrammarError {
          kind: GrammarErrorKind::TermNotFound,
          message: format!("rule not found"),
          span: decl.name.0,
        })
      }
    })
    .collect::<Result<Set<_>, GrammarError>>()?;

  let rules = rules.iter()
    .map(|decl| {
      Ok((
        *rule_names.get_by_right(&decl.name.1).unwrap(),
        Rule {
          name: decl.name.1.clone(),
          alts: decl.alts.iter().map(|alt| {
            match &alt.1 {
              ast::RuleAlt::Epsilon => Ok(vec![]),
              ast::RuleAlt::Terms(terms) => {
                Ok(terms.iter().map(|term| {
                  if let Some(&id) = rule_names.get_by_right(&term.1) {
                    Ok(Term::Rule(id))
                  } else if let Some(&id) = lexer.token_names.get_by_right(&term.1) {
                    Ok(Term::Token(id))
                  } else {
                    Err(GrammarError {
                      kind: GrammarErrorKind::TermNotFound,
                      message: format!("rule or token not found"),
                      span: term.0,
                    })
                  }
                }).collect::<Result<Vec<_>, GrammarError>>()?)
              }
            }
          }).collect::<Result<Vec<_>, GrammarError>>()?,
        }
      ))
    })
    .collect::<Result<Map<_, _>, GrammarError>>()?;

  Ok(Grammar {
    rules,
    start_rules,
    rule_names,
    lexer,
  })
}

pub fn report_error(input: &str, error: &GrammarError) {
  let writer = StandardStream::stderr(ColorChoice::Always);
  let config = term::Config::default();
  let files = SimpleFile::new("", input);

  let diagnostic = Diagnostic::error();
  let diagnostic = match error.kind {
    GrammarErrorKind::ParseError => {
      diagnostic.with_message("syntax error")
    }
    GrammarErrorKind::NameConflict => {
      diagnostic.with_message("name conflict")
    }
    GrammarErrorKind::TermNotFound => {
      diagnostic.with_message("term not found")
    }
  };
  let diagnostic = diagnostic.with_labels(vec![
    Label::primary((), error.span.0..error.span.1).with_message(&error.message)
  ]);

  term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}

impl<'a> Into<GrammarError> for ParseError<usize, Token<'a>, UserParseError> {
  fn into(self) -> GrammarError {
    match self {
      Self::InvalidToken { location } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid token or EOF"),
          span: (location, location),
        }
      }
      Self::UnrecognizedEOF { location, expected } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("expected {}, found EOF", expected.join(", ")),
          span: (location, location),
        }
      }
      Self::UnrecognizedToken { token, expected } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("expected {}, found {}", expected.join(", "), token.1),
          span: (token.0, token.2),
        }
      }
      Self::ExtraToken { token } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: token.1.to_string(),
          span: (token.0, token.2),
        }
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
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("unclosed regex"),
          span: self.span,
        }
      }
      LexErrorKind::UnclosedString => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("unclosed string"),
          span: self.span,
        }
      }
      LexErrorKind::InvalidChar => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid character"),
          span: self.span,
        }
      }
    }
  }
}

impl<'a> Into<GrammarError> for RegexError {
  fn into(self) -> GrammarError {
    match self.kind {
      RegexErrorKind::SyntaxError => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("regex syntax error"),
          span: self.span,
        }
      }
      RegexErrorKind::InvalidCodePoint => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid code point"),
          span: self.span,
        }
      }
      RegexErrorKind::Empty => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("regex or string may accept empty string"),
          span: self.span,
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn expr() {
    let input = r#"
%start expr

%token PLUS "+"
%token MINUS "-"
%token MUL "*"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token NUMBER /\d+(\.\d*)?/
%token IDENT /[a-zA-Z][\w_]*/

%skip /[ \n]/

expr = expr PLUS expr
  | expr MINUS expr
  | expr MUL expr
  | expr DIV expr
  | factor

factor =
    NUMBER
  | LPAREN expr RPAREN
  | MINUS factor
  | IDENT
  | IDENT LPAREN param_list RPAREN
  | IDENT LPAREN RPAREN

param_list =
    expr
  | param_list COMMA expr
    "#;

    assert_debug_snapshot!(build(input));
  }
}