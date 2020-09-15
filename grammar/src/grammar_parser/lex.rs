use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::Display;
use std::fmt;
use super::UserParseError;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum TokenKind {
  Start,
  Token,
  Skip,

  Ident,
  Regex,
  String,

  Percent,
  Assign,
  Or,
  LParen,
  RParen,
  Comma,
  Asterisk,
  QuestionMark,
  Plus,

  Indent,
  Newline,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
  pub kind: TokenKind,
  pub text: &'a str
}

#[derive(Debug)]
pub struct LexError {
  pub kind: LexErrorKind,
  pub span: (usize, usize),
}

#[derive(Debug)]
pub enum LexErrorKind {
  UnclosedRegex,
  InvalidChar,
  UnclosedString,
}

pub struct Lexer<'a> {
  input: &'a str,
  chars: Peekable<CharIndices<'a>>,
  bol: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.char_indices().peekable(),
      bol: true
    }
  }

  fn single_char_token(
    &self, kind: TokenKind, index: usize
  ) -> Option<<Self as Iterator>::Item> {
    let token = Token {
      kind,
      text: &self.input[index..index + 1],
    };
    return Some(Ok((index, token, index + 1)));
  }

  fn lex_quoted(
    &mut self,
    start: usize,
    quote: char,
    kind: TokenKind,
    unclosed_err_kind: LexErrorKind,
  ) -> Option<<Self as Iterator>::Item> {
    let mut unescaped = true;
    let mut end = start;
    loop {
      match self.chars.next() {
        Some((t, '\\')) => {
          unescaped = !unescaped;
          end += t + 1;
        }
        Some((k, c)) if c == quote && unescaped => {
          self.chars.next();
          let token = Token {
            kind,
            text: &self.input[start..k + 1],
          };
          return Some(Ok((start, token, k + 1)));
        }
        Some((_, '\n')) | None => {
          return Some(Err(UserParseError::LexError(LexError {
            kind: unclosed_err_kind,
            span: (start, end),
          })))
        }
        Some((t, c)) => {
          unescaped = true;
          end += t + c.len_utf8();
        }
      }
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Spanned<Token<'a>, usize, UserParseError>;

  fn next(&mut self) -> Option<Self::Item> {
    let &(i, _) = self.chars.peek()?;

    while let Some((_, ' ')) = self.chars.peek() {
      self.chars.next();
    }

    let &(j, c) = self.chars.peek()?;
    if j > i && self.bol {
      if c != '\n' && &self.input[j..j + 2] != "//" {
        let tok = Token {
          kind: TokenKind::Indent,
          text: &self.input[i..j],
        };
        return Some(Ok((i, tok, j)));
      }
    }

    self.bol = false;
    self.chars.next();

    match c {
      '\n' => {
        self.bol = true;
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
              self.bol = true;
              let token = Token {
                kind: TokenKind::Newline,
                text: &self.input[k..k + 1],
              };
              return Some(Ok((k, token, k + 1)));
            }
          }
        } else {
          return self.lex_quoted(j, '/', TokenKind::Regex, LexErrorKind::UnclosedRegex);
        }
      }
      '"' => {
        return self.lex_quoted(j, '"', TokenKind::String, LexErrorKind::UnclosedString);
      }
      '%' => return self.single_char_token(TokenKind::Percent, j),
      '=' => return self.single_char_token(TokenKind::Assign, j),
      '|' => return self.single_char_token(TokenKind::Or, j),
      ',' => return self.single_char_token(TokenKind::Comma, j),
      '*' => return self.single_char_token(TokenKind::Asterisk, j),
      '+' => return self.single_char_token(TokenKind::Plus, j),
      '?' => return self.single_char_token(TokenKind::QuestionMark, j),
      '(' => return self.single_char_token(TokenKind::LParen, j),
      ')' => return self.single_char_token(TokenKind::RParen, j),
      _ if c.is_ascii_alphabetic() || c == '\'' => {
        let mut last = j;
        loop {
          match self.chars.peek() {
            Some(&(i, c)) => {
              if !(c.is_alphanumeric() || c == '-' || c == '\'' || c == '_') {
                break;
              }
              last = i;
            }
            None => break,
          }
          self.chars.next();
        }

        let text = &self.input[j..last + 1];
        let kind = match text {
          "start" => TokenKind::Start,
          "token" => TokenKind::Token,
          "skip" => TokenKind::Skip,
          _ => TokenKind::Ident,
        };

        let token = Token {
          kind,
          text,
        };
        return Some(Ok((j, token, last + 1)));
      }
      _ => {}
    }

    Some(Err(UserParseError::LexError(LexError {
      kind: LexErrorKind::InvalidChar,
      span: (j, j + c.len_utf8()),
    })))
  }
}

impl<'a> Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<{:?}: {:?}>", self.kind, self.text)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn tokens() {
    let result = Lexer::new(r#"
%% web /\d+\\[0-9]\//  //12345
    | token,start  

  =  ( Ax3_ ) "abcdef" foo-bar''
   
    "#).collect::<Vec<_>>();

    assert_debug_snapshot!(result);
  }
}