use std::ops::Range;

pub type Grammar = Vec<Spanned<Decl>>;

#[derive(Debug)]
pub enum Decl {
  Token(TokenDecl),
  Start(StartDecl),
  Rule(RuleDecl),
  Skip(SkipDecl),
}

#[derive(Debug)]
pub struct TokenDecl {
  pub name: Spanned<String>,
  pub pattern: TokenPattern,
}

#[derive(Debug)]
pub enum TokenPattern {
  Regex(Spanned<(String, Regex)>),
  String(Spanned<String>),
}

#[derive(Debug)]
pub struct Regex(Vec<RegexItem>);

#[derive(Debug)]
pub enum RegexItem {
  Any,
  Char(char),
  CharSet(Vec<Range<char>>),
  CharClass(CharClassKind),
  Or(Regex, Regex),
  Optional(Regex),
  Many(Regex),
  Many1(Regex),
}

#[derive(Debug)]
pub enum CharClassKind {
  Digit,
  Word,
}

#[derive(Debug)]
pub struct SkipDecl {
  pub pattern: TokenPattern,
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