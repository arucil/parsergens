use std::collections::HashMap;
#[cfg(debug_assertions)]
use indexmap::{IndexMap, IndexSet};
use dfa::Dfa;
use super::grammar_parser::ast::{TokenDecl, SkipDecl};
use super::grammar_parser::regex::RegexError;

pub use tokens::Tokens;

mod nfa;
mod nfa_builder;
mod dfa;
mod dfa_builder;
mod powerset_cons;
mod util;
pub mod build;
pub mod tokens;

#[cfg(not(debug_assertions))]
type Map<K, V> = HashMap<K, V>;

#[cfg(debug_assertions)]
type Map<K, V> = IndexMap<K, V>;

#[cfg(not(debug_assertions))]
type Set<K> = HashSet<K>;

#[cfg(debug_assertions)]
type Set<K> = IndexSet<K>;

pub struct Lexer {
  dfa: Dfa<u32, TokenId>,
  char_intervals: Vec<u32>,
  token_names: HashMap<TokenId, String>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct TokenId(u32);

#[derive(Debug)]
pub enum LexerError {
  NoTokens,
  RegexError(RegexError),
}

impl Lexer {
  pub fn new(decls: &[&TokenDecl], skips: &[&SkipDecl]) -> Result<Self, LexerError> {
    build::build(decls, skips)
  }

  pub fn lex<'lexer, 'input>(
    &'lexer self,
    input: &'input str
  ) -> Tokens<'lexer, 'input> {
    Tokens::new(self, input)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::grammar_parser;
  use super::super::grammar_parser::ast::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn skip() {
    let ast = grammar_parser::parse(r#"
%token INT /\d+/
%skip /[ \n]/
    "#).unwrap();

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

    let lexer = Lexer::new(&decls, &skips).unwrap();
    let tokens = lexer.lex(r"  123  456  
  0127401  
 5768   
    ").collect::<Vec<_>>();

    assert_debug_snapshot!(tokens);
  }

  #[test]
  fn lex_expr() {
    let ast = grammar_parser::parse(r#"
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
    "#).unwrap();

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

    let lexer = Lexer::new(&decls, &skips).unwrap();
    let tokens = lexer.lex(r"(3.2 * 51 + Foo_1) / 20. -5  ,    ").collect::<Vec<_>>();

    assert_debug_snapshot!(tokens);
  }
}