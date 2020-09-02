
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
pub struct TokenPattern {
  pub kind: TokenPatternKind,
  pub source: Spanned<String>,
  pub regex: Regex,
}

#[derive(Debug)]
pub enum TokenPatternKind {
  Regex,
  String,
}

#[derive(Debug)]
pub enum Regex {
  Empty,
  Any,
  Char(char),
  CharSet(Vec<CharSetItem>),
  CharClass(CharClass),
  Alt(Vec<Regex>),
  Concat(Vec<Regex>),
  Optional(Box<Regex>),
  Many(Box<Regex>),
  Many1(Box<Regex>),
}

#[derive(Debug)]
pub enum CharSetItem {
  Range(char, char),
  CharClass(CharClass),
  Char(char),
}

#[derive(Debug, Clone, Copy)]
pub enum CharClass {
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
pub struct Spanned<T>(pub (usize, usize), pub T);