use std::str::CharIndices;
use std::iter::Peekable;
use super::ast::{Regex, CharClassKind};

#[derive(Debug)]
pub struct RegexError {
  pub kind: RegexErrorKind,
  pub span: (usize, usize),
}

#[derive(Debug)]
pub enum RegexErrorKind {
  SyntaxError,
  InvalidCodePoint,
  Empty,
}

pub fn parse_raw_string(
  input: &str,
  start: usize,
) -> Result<Regex, RegexError> {
  let mut chars = input.char_indices().peekable();
  let mut items = vec![];

  while let Some((i0, c)) = chars.next() {
    if c == '\\' {
      let (i, c) = chars.next().unwrap();
      match c {
        'n' => items.push(Regex::Char('\n')),
        'r' => items.push(Regex::Char('\r')),
        't' => items.push(Regex::Char('\t')),
        'U' => {
          let cp_start = i + 1;
          let mut cp_end = cp_start;
          while let &(i, c) = chars.peek().unwrap() {
            if !c.is_ascii_hexdigit() {
              cp_end = i;
              break;
            }
          }

          if cp_start == cp_end {
            return Err(RegexError {
              kind: RegexErrorKind::SyntaxError,
              span: (i0 + start, cp_end + start),
            })
          } else {
            let ch = input[cp_start..cp_end].parse::<u32>()
              .map_err(|_| ())
              .and_then(|cp| std::char::from_u32(cp).ok_or(()))
              .map_err(|_| RegexError {
                kind: RegexErrorKind::InvalidCodePoint,
                span: (cp_start + start, cp_end + start),
              })?;

            items.push(Regex::Char(ch))
          }
        }
        _ => items.push(Regex::Char(c)),
      }
    } else {
      items.push(Regex::Char(c));
    }
  }

  if items.is_empty() {
    Err(RegexError {
      kind: RegexErrorKind::Empty,
      span: (input.len() + start, input.len() + start),
    })
  } else {
    Ok(Regex::Seq(items))
  }
}

pub fn parse_regex(
  input: &str,
  start: usize,
) -> Result<Regex, RegexError> {
  RegexParser::new(input)
    .parse()
    .map_err(|err| {
      RegexError {
        kind: err.kind,
        span: (err.span.0 + start, err.span.1 + start),
      }
    })
}

struct RegexParser<'a> {
  input: &'a str,
  chars: Peekable<CharIndices<'a>>,
}

impl<'a> RegexParser<'a> {
  fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.char_indices().peekable()
    }
  }

  fn parse(&mut self) -> Result<Regex, RegexError> {
    todo!()
  }

  fn parse_regex(&mut self) -> Result<Regex, RegexError> {
  }

  fn parse_factor(&mut self) -> Result<Regex, RegexError> {
    match self.chars.next() {
      Some((_, '.')) => {
        Ok(Regex::Any)
      }
      Some((_, '(')) => {
        let regex = self.parse_regex()?;
        self.match_char(')');
        Ok(regex)
      }
      Some((i, '\\')) => {
        self.parse_escape(i)
      }
    }
  }

  fn parse_escape(&mut self, start: usize) -> Result<Regex, RegexError> {
    match self.chars.next() {
      Some((_, 'n')) => {
        Ok(Regex::Char('\n'))
      }
      Some((_, 'r')) => {
        Ok(Regex::Char('\r'))
      }
      Some((_, 't')) => {
        Ok(Regex::Char('\t'))
      }
      Some((_, 'd')) => {
        Ok(Regex::CharClass(CharClassKind::Digit))
      }
      Some((_, 'w')) => {
        Ok(Regex::CharClass(CharClassKind::Digit))
      }
      Some((i, 'U')) => {
        let cp_start = i + 1;
        let cp_end = cp_start;
        while let Some(&(i, c)) = self.chars.peek() {
          if !c.is_ascii_hexdigit() {
            cp_end = i;
            break;
          }
        }

        if cp_start == cp_end {
          return Err(RegexError {
            kind: RegexErrorKind::SyntaxError,
            span: (start, cp_end),
          })
        } else {
          let ch = self.input[cp_start..cp_end].parse::<u32>()
            .map_err(|_| ())
            .and_then(|cp| std::char::from_u32(cp).ok_or(()))
            .map_err(|_| RegexError {
              kind: RegexErrorKind::InvalidCodePoint,
              span: (cp_start, cp_end),
            })?;

          Ok(Regex::Char(ch))
        }
      }
      Some((_, c)) => {
        Ok(Regex::Char(c))
      }
      None => {
        Err(RegexError {
          kind: RegexErrorKind::SyntaxError,
          span: (self.input.len(), self.input.len()),
        })
      }
    }
  }

  fn match_char(&mut self, c: char) -> Result<char, RegexError> {
    self.match_char_if(|x| x == c)
  }

  fn match_char_if(&mut self, f: impl Fn(char) -> bool) -> Result<char, RegexError> {
    if let Some((i, c)) = self.chars.next() {
      if f(c) {
        Ok(c)
      } else {
        Err(RegexError {
          kind: RegexErrorKind::SyntaxError,
          span: (i, i + c.len_utf8())
        })
      }
    } else {
      Err(RegexError {
        kind: RegexErrorKind::SyntaxError,
        span: (self.input.len(), self.input.len())
      })
    }
  }

  fn advance(&mut self) {
    self.chars.next();
  }
}