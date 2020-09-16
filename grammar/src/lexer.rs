use std::borrow::Borrow;
use tabular_dfa::TabularDfa;
use super::grammar_parser::ast::{TokenDecl, SkipDecl};
use super::grammar_parser::regex::RegexError;
use super::{BiMap, Set};

pub use tokens::Tokens;

mod nfa;
mod nfa_builder;
mod dfa;
mod dfa_builder;
mod tabular_dfa;
mod powerset_cons;
mod util;
pub mod build;
pub mod tokens;

#[derive(Debug)]
pub struct Lexer {
  pub dfa: TabularDfa<TokenId>,
  pub char_intervals: Vec<u32>,
  pub skip: Set<TokenId>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub struct TokenId(u32);

#[derive(Default)]
pub struct TokenIdGen(u32);

#[derive(Debug)]
pub struct Token<'input> {
  pub kind: TokenId,
  pub text: &'input str,
  pub start: usize,
  pub end: usize,
}

impl TokenIdGen {
  pub fn gen(&mut self) -> TokenId {
    let i = self.0;
    self.0 += 1;
    TokenId(i)
  }

  pub fn from(start: u32) -> Self {
    Self(start + 1)
  }
}

#[derive(Debug)]
pub enum LexerError {
  RegexError(RegexError),
}

impl TokenId {
  pub fn id(&self) -> u32 {
    self.0
  }
}

impl Borrow<u32> for TokenId {
  fn borrow(&self) -> &u32 {
    &self.0
  }
}

impl Lexer {
  pub fn new(
    decls: &[TokenDecl],
    skips: &[SkipDecl]
  ) -> Result<(Self, BiMap<TokenId, String>), LexerError> {
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
%skip /#[^\n]*/
    "#).unwrap();

    let decls = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Token(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let skips = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Skip(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let lexer = Lexer::new(&decls, &skips).unwrap().0;
    let tokens = lexer.lex(r"  123  456  # lorem ipsum
  0127401  #

 5768   ##dolorsitamet##
    #").collect::<Vec<_>>();

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
        Decl::Token(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let skips = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Skip(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let lexer = Lexer::new(&decls, &skips).unwrap().0;
    let tokens = lexer.lex(r"(3.2 * 51 + Foo_1) / 20. -5  ,    ").collect::<Vec<_>>();

    assert_debug_snapshot!(tokens);
  }

  #[test]
  fn match_longest() {
    let ast = grammar_parser::parse(r#"
%token IN "in"
%token INTEGER "integer"
%token TEGE "tege"
    "#).unwrap();

    let decls = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Token(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let skips = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Skip(decl) => Some(decl.clone()),
        _ => None,
      })
      .collect::<Vec<_>>();

    let lexer = Lexer::new(&decls, &skips).unwrap().0;
    let tokens = lexer.lex(r"integeintegerinteg").collect::<Vec<_>>();

    assert_debug_snapshot!(tokens);
  }
}