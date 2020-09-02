use std::collections::{HashMap, HashSet};
use bimap::BiHashMap;
use super::lexer::{Lexer, TokenId};

#[derive(Debug)]
pub struct Grammar {
  pub rules: HashMap<RuleId, Rule>,
  pub start_rules: HashSet<RuleId>,
  pub rule_names: BiHashMap<RuleId, String>,
  pub lexer: Lexer,
}

#[derive(Debug)]
pub struct Rule {
  pub name: String,
  pub alts: Vec<RuleAlt>,
}

pub type RuleAlt = Vec<Term>;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Term {
  Rule(RuleId),
  Token(TokenId),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
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