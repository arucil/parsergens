use super::{Lexer, util, Token, TokenId};

pub struct Tokens<'lexer, 'input> {
  lexer: &'lexer Lexer,
  input: &'input str,
  pos: usize,
}

#[derive(Debug)]
pub struct Error {
  pub char: char,
  pub start: usize,
  pub end: usize,
}

impl<'lexer, 'input> Tokens<'lexer, 'input> {
  pub(super) fn new(lexer: &'lexer Lexer, input: &'input str) -> Self {
    Self {
      lexer,
      input,
      pos: 0,
    }
  }

  fn peek_char(&mut self) -> Option<char> {
    self.input[self.pos..].chars().next()
  }

  fn advance(&mut self) {
    self.pos += 1;
    while !self.input.is_char_boundary(self.pos) {
      self.pos += 1;
    }
  }
}

impl<'lexer, 'input> Iterator for Tokens<'lexer, 'input> {
  type Item = Result<Token<'input>, Error>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.pos == self.input.len() {
      return None;
    }

    let mut state = self.lexer.dfa.start();
    let mut start = self.pos;
    let mut end = start;
    let mut token_kind: Option<TokenId> = None;

    loop {
      let no_move;

      match self.peek_char() {
        Some(c) => {
          let char_interval = util::find_char_interval(
            c as u32, &self.lexer.char_intervals);

          if let Some(next_state) = self.lexer.dfa.transition(state, char_interval) {
            self.advance();
            state = next_state;
            no_move = false;
          } else {
            no_move = true;
          }
        }
        None => {
          no_move = true;
        }
      }

      if no_move {
        self.pos = end;

        if end == start {
          let char = self.peek_char().unwrap();
          let start = self.pos;
          self.advance();
          let end = self.pos;

          return Some(Err(Error {
            char,
            start,
            end,
          }));
        } else if let Some(kind) = token_kind {
          return Some(Ok(Token {
            kind,
            text: &self.input[start..end],
            start,
            end,
          }));
        } else if end == self.input.len() {
          return None;
        } else {
          state = self.lexer.dfa.start();
          start = end;
          continue;
        }
      }

      if let Some(&id) = self.lexer.dfa.result(state) {
        end = self.pos;

        token_kind = if self.lexer.skip.contains(&id) {
          None
        } else {
          Some(id)
        };
      }
    }
  }
}