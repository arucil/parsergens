use std::ops::Range;

pub type Grammar = Vec<Spanned<Decl>>;

#[derive(Debug)]
pub enum Decl {
  Token(TokenDecl),
  Start(StartDecl),
  Rule(RuleDecl),
}

#[derive(Debug)]
pub struct TokenDecl {
  pub name: Spanned<String>,
  pub regex: Spanned<String>,
}

#[derive(Debug)]
pub struct StartDecl {
  pub name: Spanned<String>,
}

#[derive(Debug)]
pub struct RuleDecl {
  pub name: Spanned<String>,
  pub alts: Vec<Spanned<RuleAlt>>,
}

#[derive(Debug)]
pub enum RuleAlt {
  Epsilon,
  Terms(Vec<Spanned<String>>),
}

#[derive(Debug)]
pub struct Spanned<T>(pub Range<usize>, pub T);