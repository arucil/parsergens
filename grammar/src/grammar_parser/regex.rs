use std::str::CharIndices;
use std::iter::Peekable;
use either::Either::{self, *};
use super::ast::{Regex, CharClass, CharSetItem};

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

type Result<T=Regex> = std::result::Result<T, RegexError>;

pub fn parse_string_literal(
  input: &str,
  start: usize,
) -> Result {
  RegexParser::new(input)
    .parse_string_literal()
    .map_err(|err| {
      RegexError {
        kind: err.kind,
        span: (err.span.0 + start, err.span.1 + start),
      }
    })
}

pub fn parse_regex(
  input: &str,
  start: usize,
) -> Result {
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

  fn parse(&mut self) -> Result {
    let regex = self.parse_regex()?;

    if let Some((i, _)) = self.chars.next() {
      syntax_error(i, self.input.len())?
    } else {
      Ok(regex)
    }
  }

  fn parse_string_literal(&mut self) -> Result {
    let mut items = vec![];

    while let Some((i0, c)) = self.chars.next() {
      if c == '\\' {
        match self.parse_escape(i0, false)? {
          Left(c) => items.push(Regex::Char(c)),
          _ => unreachable!(),
        }
      } else {
        items.push(Regex::Char(c));
      }
    }

    if items.is_empty() {
      Ok(Regex::Empty)
    } else if items.len() == 1 {
      Ok(items.pop().unwrap())
    } else {
      Ok(Regex::Concat(items))
    }
  }

  fn parse_regex(&mut self) -> Result {
    self.parse_alt()
  }

  fn parse_alt(&mut self) -> Result {
    let mut items = vec![self.parse_concat()?];

    while let Some((_, '|')) = self.chars.peek() {
      self.advance();
      items.push(self.parse_concat()?);
    }

    if items.len() == 1 {
      Ok(items.pop().unwrap())
    } else {
      Ok(Regex::Alt(items))
    }
  }

  fn parse_concat(&mut self) -> Result {
    let mut items = vec![self.parse_repetition()?];

    loop {
      if let Some((_, '|' | ')')) | None = self.chars.peek() {
        break;
      }

      items.push(self.parse_repetition()?);
    }

    if items.len() == 1 {
      Ok(items.pop().unwrap())
    } else {
      Ok(Regex::Concat(items))
    }
  }

  fn parse_repetition(&mut self) -> Result {
    let mut item = self.parse_factor()?;

    while let Some(&(_, c@('+' | '*' | '?'))) = self.chars.peek() {
      self.advance();
      item = match c {
        '+' => Regex::Many1(Box::new(item)),
        '*' => Regex::Many(Box::new(item)),
        '?' => Regex::Optional(Box::new(item)),
        _ => unreachable!(),
      };
    }

    Ok(item)
  }

  fn parse_factor(&mut self) -> Result {
    match self.chars.peek() {
      Some((_, '.')) => {
        self.advance();
        Ok(Regex::Any)
      }
      Some((_, '(')) => {
        self.advance();
        let regex = self.parse_regex()?;
        self.match_char(')')?;
        Ok(regex)
      }
      Some(&(i, '\\')) => {
        self.advance();
        match self.parse_escape(i, true)? {
          Left(c) => Ok(Regex::Char(c)),
          Right(class) => Ok(Regex::CharClass(class)),
        }
      }
      Some(&(i, '[')) => {
        self.advance();
        self.parse_char_set(i)
      }
      Some(&(i, c@('+' | '*' | '?'))) => {
        syntax_error(i, i + c.len_utf8())?
      }
      Some((_, '|' | ')')) | None => {
        Ok(Regex::Empty)
      }
      Some(&(_, c)) => {
        self.advance();
        Ok(Regex::Char(c))
      }
    }
  }

  fn parse_char_set(&mut self, _start: usize) -> Result {
    let mut pending_range = false;
    let mut items = vec![];

    loop {
      let item = match self.chars.next() {
        Some((_, ']')) => {
          if pending_range {
            items.push(CharSetItem::Char('-'));
          }
          return Ok(Regex::CharSet(items));
        }
        Some((i, '\\')) => {
          match self.parse_escape(i, true)? {
            Left(c) => CharSetItem::Char(c),
            Right(class) => CharSetItem::CharClass(class),
          }
        }
        Some((_, '-')) => {
          if pending_range {
            CharSetItem::Char('-')
          } else if let Some(CharSetItem::Char(_)) = items.last() {
            pending_range = true;
            continue;
          } else {
            CharSetItem::Char('-')
          }
        }
        Some((_, c)) => {
          CharSetItem::Char(c)
        }
        None => {
          self.eof_error()?
        }
      };

      if pending_range {
        match &item {
          CharSetItem::Char(c) => {
            let c0 = match items.pop().unwrap() {
              CharSetItem::Char(c) => c,
              _ => unreachable!(),
            };
            items.push(CharSetItem::Range(c0, *c));
          }
          _ => {
            items.push(CharSetItem::Char('-'));
            items.push(item);
          }
        }
        pending_range = false;
      } else {
        items.push(item);
      }
    }
  }

  fn parse_escape(
    &mut self,
    start: usize,
    enable_class: bool
  ) -> Result<Either<char, CharClass>> {
    match self.chars.next() {
      Some((_, 'n')) => {
        Ok(Left('\n'))
      }
      Some((_, 'r')) => {
        Ok(Left('\r'))
      }
      Some((_, 't')) => {
        Ok(Left('\t'))
      }
      Some((_, '\\')) => {
        Ok(Left('\\'))
      }
      Some((_, 'd')) if enable_class => {
        Ok(Right(CharClass::Digit))
      }
      Some((_, 'w')) if enable_class => {
        Ok(Right(CharClass::Word))
      }
      Some((i, 'U')) => {
        if let Some((_, '{')) = self.chars.next() {
          ()
        } else {
          syntax_error(start, i + 1)?
        }

        let cp_start = i + 2;
        let cp_end;
        loop {
          match self.chars.next() {
            Some((i, '}')) => {
              cp_end = i;
              break;
            }
            None => {
              syntax_error(start, self.input.len())?
            }
            _ => {}
          }
        }

        if cp_start == cp_end {
          syntax_error(start, cp_end)?
        } else {
          let ch = u32::from_str_radix(&self.input[cp_start..cp_end], 16)
            .map_err(|_| ())
            .and_then(|cp| std::char::from_u32(cp).ok_or(()))
            .map_err(|_| RegexError {
              kind: RegexErrorKind::InvalidCodePoint,
              span: (cp_start, cp_end),
            })?;

          Ok(Left(ch))
        }
      }
      Some((_, c)) => {
        Ok(Left(c))
      }
      None => {
        self.eof_error()?
      }
    }
  }

  fn match_char(&mut self, c: char) -> Result<char> {
    self.match_char_if(|x| x == c)
  }

  fn match_char_if(&mut self, f: impl Fn(char) -> bool) -> Result<char> {
    if let Some((i, c)) = self.chars.next() {
      if f(c) {
        Ok(c)
      } else {
        syntax_error(i, i + c.len_utf8())?
      }
    } else {
      self.eof_error()?
    }
  }

  fn advance(&mut self) {
    self.chars.next();
  }

  fn eof_error(&self) -> Result<!> {
    Err(RegexError {
      kind: RegexErrorKind::SyntaxError,
      span: (self.input.len(), self.input.len())
    })
  }
}

fn syntax_error(start: usize, end: usize) -> Result<!> {
  Err(RegexError {
    kind: RegexErrorKind::SyntaxError,
    span: (start, end),
  })
}


#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  mod string_literal {
    use super::*;

    #[test]
    fn trivial() {
      assert_debug_snapshot!(parse_string_literal("a.-+&Ro1", 100));
    }

    #[test]
    fn empty() {
      assert_debug_snapshot!(parse_string_literal("", 100));
    }

    #[test]
    fn single_char() {
      assert_debug_snapshot!(parse_string_literal("+", 100));
    }

    #[test]
    fn escape() {
      assert_debug_snapshot!(parse_string_literal(r"ab\U{41}\r\nn\tt\d", 100));
    }
  }

  mod regex {
    use super::*;

    #[test]
    fn empty() {
      assert_debug_snapshot!(parse_regex(r"", 100));
    }

    #[test]
    fn trivial() {
      assert_debug_snapshot!(parse_regex("aRo1", 100));
    }

    #[test]
    fn escape() {
      assert_debug_snapshot!(parse_regex(r"ab\U{41}\r\nn\tt\.\+\d", 100));
    }

    #[test]
    fn alt_empty() {
      assert_debug_snapshot!(parse_regex(r"||ab", 100));
    }

    #[test]
    fn concat_precedes_alt() {
      assert_debug_snapshot!(parse_regex(r"A1\.|\d|ab", 100));
    }

    #[test]
    fn repetition_precedence() {
      assert_debug_snapshot!(parse_regex(r"ab+\||.*|ab?|", 100));
    }

    #[test]
    fn multi_repetition() {
      assert_debug_snapshot!(parse_regex(r"ab+?|\d+*", 100));
    }

    #[test]
    fn parenthesis() {
      assert_debug_snapshot!(parse_regex(r"(ab|\w\d+)+|\.", 100));
    }

    #[test]
    fn char_set_trivial() {
      assert_debug_snapshot!(parse_regex(r"[1234567890]+", 100));
    }

    #[test]
    fn char_set_range() {
      assert_debug_snapshot!(parse_regex(r"[0-9]+", 100));
    }

    #[test]
    fn char_set_multi_range() {
      assert_debug_snapshot!(parse_regex(r"[0-9a-zA-Z]+", 100));
    }

    #[test]
    fn char_set_mixed() {
      assert_debug_snapshot!(parse_regex(r"[0-9abcA-Z.]+", 100));
    }

    #[test]
    fn char_set_class() {
      assert_debug_snapshot!(parse_regex(r"[a-zA-Z_][\w_]*", 100));
    }

    #[test]
    fn char_set_escape() {
      assert_debug_snapshot!(parse_regex(r"([a-zA-\U{45}_] )", 100));
    }

    #[test]
    fn char_set_hyphen() {
      assert_debug_snapshot!(parse_regex(r"([-[-z ---_-] )", 100));
    }

    #[test]
    fn repetition_mixed() {
      assert_debug_snapshot!(parse_regex(r"\d+(\.\d*)?", 100));
    }
  }
}