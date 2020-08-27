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
  sol: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.char_indices().peekable(),
      sol: true
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Spanned<Token<'a>, usize, LexError>;

  fn next(&mut self) -> Option<Self::Item> {
    let &(i, _) = self.chars.peek()?;

    while let Some((_, ' ')) = self.chars.peek() {
      self.chars.next();
    }

    let &(j, c) = self.chars.peek()?;
    if j > i && self.sol {
      if c != '\n' && &self.input[j..j + 2] != "//" {
        let tok = Token {
          kind: TokenKind::Indent,
          text: &self.input[i..j],
        };
        return Some(Ok((i, tok, j)));
      }
    }

    self.sol = false;
    self.chars.next();

    match c {
      '\n' => {
        self.sol = true;
        let token = Token {
          kind: TokenKind::Newline,
          text: &self.input[j..j + 1],
        };
        return Some(Ok((j, token, j + 1)));
      }
      '/' => {
        if let Some((_, '/')) = self.chars.peek() {
          self.chars.next();
          loop {
            let (k, c) = self.chars.next()?;
            if c == '\n' {
              self.sol = true;
              let token = Token {
                kind: TokenKind::Newline,
                text: &self.input[k..k + 1],
              };
              return Some(Ok((k, token, k + 1)));
            }
          }
        } else {
          loop {
          }
        }
      }
      _ => {}
    }

    Some(Err(LexError::InvalidChar))
  }
}