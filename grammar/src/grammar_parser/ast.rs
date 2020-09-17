
pub type Grammar = Vec<Spanned<Decl>>;

#[derive(Debug)]
pub enum Decl {
  Token(TokenDecl),
  ExtToken(ExternalTokenDecl),
  Start(StartDecl),
  Rule(RuleDecl),
  Skip(SkipDecl),
  User(UserDecl),
  Assoc(AssocDecl),
}

#[derive(Debug, Clone)]
pub struct TokenDecl {
  pub name: Spanned<String>,
  pub pattern: TokenPattern,
}

#[derive(Debug, Clone)]
pub struct ExternalTokenDecl {
  pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct TokenPattern {
  pub kind: TokenPatternKind,
  pub source: Spanned<String>,
  pub regex: Regex,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenPatternKind {
  Regex,
  String,
}

#[derive(Debug, Clone)]
pub enum Regex {
  Empty,
  Any,
  Char(char),
  CharSet(Vec<CharSetItem>, bool),
  CharClass(CharClass),
  Alt(Vec<Regex>),
  Concat(Vec<Regex>),
  Optional(Box<Regex>),
  Many(Box<Regex>),
  Many1(Box<Regex>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct SkipDecl {
  pub pattern: TokenPattern,
}

#[derive(Debug, Clone)]
pub struct UserDecl {
  pub code: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct StartDecl {
  pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct RuleDecl {
  pub name: Spanned<String>,
  pub ty: Option<Spanned<String>>,
  pub alts: Vec<Spanned<RuleAlt>>,
}

#[derive(Debug, Clone)]
pub struct RuleAlt {
  pub terms: RuleAltTerms,
  pub prec: Option<Spanned<String>>,
  pub action: Option<Spanned<String>>,
}

#[derive(Debug, Clone)]
pub enum RuleAltTerms {
  Epsilon,
  Terms(Vec<Term>),
}

#[derive(Debug, Clone)]
pub enum Term {
  Symbol(Spanned<String>),
  Optional(Vec<Term>),
  Many(Vec<Term>),
  Many1(Vec<Term>),
}

#[derive(Debug, Clone)]
pub struct AssocDecl {
  pub names: Vec<Spanned<String>>,
  pub assoc: Spanned<Assoc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
  LeftAssoc,
  RightAssoc,
  NonAssoc,
}

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub (usize, usize), pub T);

impl CharClass {
  pub fn ranges(&self) -> impl Iterator<Item=(char, char)> {
    let f = |&(a, b): &'static (char, char)| (a, b);

    match self {
      Self::Digit => {
        [('0', '9')].iter().map(f)
      }
      Self::Word => {
        [('0', '9'), ('a', 'z'), ('A', 'Z')].iter().map(f)
      }
    }
  }
}