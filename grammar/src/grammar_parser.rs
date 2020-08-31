
pub mod lex;
pub mod ast;

use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(pub grammar, "/grammar_parser/grammar.rs");

pub fn parse(
  input: &str
) -> Result<ast::Grammar, ParseError<usize, lex::Token, lex::LexError>> {
  let lexer = lex::Lexer::new(input);
  grammar::DocumentParser::new().parse(input, lexer)
}
