
pub mod lex;
pub mod ast;
pub mod regex;

use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(pub grammar, "/grammar_parser/grammar.rs");

#[derive(Debug)]
pub enum UserParseError {
  LexError(lex::LexError),
  RegexError(regex::RegexError),
}

pub fn parse(
  input: &str
) -> Result<ast::Grammar, ParseError<usize, lex::Token, UserParseError>> {
  let lexer = lex::Lexer::new(input);
  grammar::DocumentParser::new().parse(input, lexer)
}
