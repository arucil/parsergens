use std::collections::VecDeque;
use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::Display;
use std::fmt;
use super::UserParseError;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Copy, Debug)]
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
  Colon,
  Asterisk,
  QuestionMark,
  Plus,
  Arrow,

  Enter,
  Leave,
  Separator,
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
  InvalidIndent,
}

pub struct Lexer<'a> {
  input: &'a str,
  chars: Peekable<CharIndices<'a>>,
  layout_stack: Vec<usize>,
  layout_base_indent: Option<usize>,
  buffer: VecDeque<<Self as Iterator>::Item>,
  cur_line_indent: usize,
  bol: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.char_indices().peekable(),
      layout_stack: vec![],
      layout_base_indent: None,
      buffer: VecDeque::new(),
      cur_line_indent: 0,
      bol: true,
    }
  }

  fn advance(&mut self) -> Option<(usize, char)> {
    self.chars.next()
  }

  fn gen_token(&mut self, kind: TokenKind, start: usize, end: usize) {
    if kind.is_layout_start() {
      self.layout_base_indent = Some(self.cur_line_indent);
    }

    let token = Token {
      kind,
      text: &self.input[start..end],
    };
    self.buffer.push_back(Ok((start, token, end)));
  }

  fn gen_error(&mut self, kind: LexErrorKind, start: usize, end: usize) {
    let error = LexError {
      kind,
      span: (start, end),
    };
    self.buffer.push_back(Err(UserParseError::LexError(error)));
  }

  fn single_char_token(&mut self, kind: TokenKind, index: usize) {
    self.gen_token(kind, index, index + 1);
  }

  fn lex_quoted(
    &mut self,
    start: usize,
    quote: char,
    kind: TokenKind,
    unclosed_err_kind: LexErrorKind,
  ) {
    let mut unescaped = true;
    let mut end = start;
    loop {
      match self.advance() {
        Some((t, '\\')) => {
          unescaped = !unescaped;
          end += t + 1;
        }
        Some((k, c)) if c == quote && unescaped => {
          self.advance();
          break self.gen_token(kind, start, k + 1);
        }
        Some((_, '\n')) | None => {
          break self.gen_error(unclosed_err_kind, start, end);
        }
        Some((t, c)) => {
          unescaped = true;
          end += t + c.len_utf8();
        }
      }
    }
  }

  fn gen_tokens(&mut self) -> Option<()> {
    self.chars.peek()?;

    // skip whitespace
    while let Some(&(_, c@(' ' | '\n'))) = self.chars.peek() {
      if c == '\n' {
        self.bol = true;
        self.cur_line_indent = 0;
      } else {
        self.cur_line_indent += 1;
      }
      self.advance();
    }

    // TODO pop all layout at EOF
    // FIXME BOF should not emit Separator

    let (j, c) = self.advance()?;

    // skip comment
    if j + 2 <= self.input.len() && &self.input[j..j + 2] == "//" {
      loop {
        let &(_, c) = self.chars.peek()?;
        if c == '\n' {
          return self.gen_tokens();
        } else {
          self.advance();
        }
      }
    }

    if let Some(base_indent) = self.layout_base_indent.take() {
      let start = j - self.cur_line_indent;
      let end = j;

      if self.cur_line_indent > base_indent {
        self.layout_stack.push(self.cur_line_indent);

        self.gen_token(TokenKind::Enter, start, end);
      } else {
        self.gen_error(LexErrorKind::InvalidIndent, start, end);
      }
    } else if self.bol {
      self.bol = false;

      while let Some(&indent) = self.layout_stack.last() {
        if self.cur_line_indent >= indent {
          break;
        }
      }

      let base_indent = self.layout_stack.last().cloned().unwrap_or(0);
      if self.cur_line_indent == base_indent {
        let start = j - self.cur_line_indent;
        let end = j;
        self.gen_token(TokenKind::Separator, start, end);
      } else {
        // do nothing
      }
    }

    match c {
      '/' => {
        self.lex_quoted(j, '/', TokenKind::Regex, LexErrorKind::UnclosedRegex);
      }
      '"' => {
        self.lex_quoted(j, '"', TokenKind::String, LexErrorKind::UnclosedString);
      }
      '%' => self.single_char_token(TokenKind::Percent, j),
      '=' => self.single_char_token(TokenKind::Assign, j),
      '|' => self.single_char_token(TokenKind::Or, j),
      ',' => self.single_char_token(TokenKind::Comma, j),
      ':' => self.single_char_token(TokenKind::Colon, j),
      '*' => self.single_char_token(TokenKind::Asterisk, j),
      '+' => self.single_char_token(TokenKind::Plus, j),
      '?' => self.single_char_token(TokenKind::QuestionMark, j),
      '(' => self.single_char_token(TokenKind::LParen, j),
      ')' => self.single_char_token(TokenKind::RParen, j),
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
          self.advance();
        }

        let text = &self.input[j..last + 1];
        let kind = match text {
          "start" => TokenKind::Start,
          "token" => TokenKind::Token,
          "skip" => TokenKind::Skip,
          _ => TokenKind::Ident,
        };

        self.gen_token(kind, j, last + 1);
      }
      _ => {
        self.buffer.push_back(Err(UserParseError::LexError(LexError {
          kind: LexErrorKind::InvalidChar,
          span: (j, j + c.len_utf8()),
        })));
      }
    }

    Some(())
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Spanned<Token<'a>, usize, UserParseError>;

  fn next(&mut self) -> Option<Self::Item> {
    while self.buffer.is_empty() {
      self.gen_tokens()?;
    }
    self.buffer.pop_front()
  }
}

impl<'a> Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<{:?}: {:?}>", self.kind, self.text)
  }
}

impl TokenKind {
  fn is_layout_start(&self) -> bool {
    match self {
      Self::Colon => true,
      _ => false,
    }
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