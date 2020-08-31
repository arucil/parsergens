use std::collections::HashMap;
use super::lexer::Lexer;

pub struct Grammar {
  pub rules: HashMap<RuleId, Rule>,
  pub token_names: HashMap<TokenId, String>,
  pub lexer: Lexer,
}

pub struct Rule {
  pub name: String,
  pub alts: Vec<RuleAlt>,
}

pub type RuleAlt = Vec<Term>;

#[derive(PartialEq, Eq, Hash)]
pub enum Term {
  Rule(RuleId),
  Token(TokenId),
}

#[derive(PartialEq, Eq, Hash)]
pub struct RuleId(usize);

#[derive(PartialEq, Eq, Hash)]
pub struct TokenId(usize);