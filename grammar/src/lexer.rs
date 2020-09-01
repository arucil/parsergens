use std::collections::HashSet;
#[cfg(not(debug_assertions))]
use std::collections::HashMap;
#[cfg(debug_assertions)]
use indexmap::{IndexMap, IndexSet};
use super::grammar_parser::ast::{TokenDecl, SkipDecl};
use dfa::Dfa;

mod nfa;
mod nfa_builder;
mod dfa;
mod dfa_builder;
mod powerset_cons;

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
  skip_set: HashSet<TokenId>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct TokenId(usize);

pub fn build(decls: &[TokenDecl], skips: &[SkipDecl]) -> Lexer {
  todo!()
}