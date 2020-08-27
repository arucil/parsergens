use std::str::CharIndices;
use std::iter::Peekable;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum TokenKind {
  Ident,
  Start,
  Token,
  Regex,

  Percent,
  Assign,
  Or,
  Epsilon,
  Comma,

  Indent,
  Newline,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
  pub kind: TokenKind,
  pub text: &'a str
}

#[derive(Debug)]
pub enum LexError {
  UnclosedRegex,
  InvalidChar,
}

pub struct Lexer<'a> {
  input: &'a str,
  chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.char_indices().peekable()
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Spanned<Token<'a>, usize, LexError>;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some((_, c)) = self.chars.peek() {
      self.chars.next();
    }

    None
  }
}