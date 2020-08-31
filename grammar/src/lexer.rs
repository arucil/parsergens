use super::grammar_parser::ast::TokenDecl;

mod nfa;
mod dfa;
mod powerset_cons;

pub struct Lexer;

pub fn build(decls: &[TokenDecl]) -> Lexer {
}