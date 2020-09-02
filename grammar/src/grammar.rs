use std::collections::HashMap;
use super::lexer::{Lexer, TokenId};

pub struct Grammar {
  pub rules: HashMap<RuleId, Rule>,
  pub rule_names: HashMap<RuleId, String>,
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
pub struct RuleId(u32);

#[derive(Default)]
pub(crate) struct RuleIdGen(u32);

impl RuleIdGen {
  pub fn gen(&mut self) -> RuleId {
    let i = self.0;
    self.0 += 1;
    RuleId(i)
  }
}