use std::str::CharIndices;
use std::iter::Peekable;
use super::{Lexer, util};

pub struct Tokens<'lexer, 'input> {
  lexer: &'lexer Lexer,
  input: &'input str,
  chars: Peekable<CharIndices<'input>>,
  error: bool,
}

#[derive(Debug)]
pub struct Token<'lexer, 'input> {
  pub kind: &'lexer str,
  pub text: &'input str,
  pub start: usize,
  pub end: usize,
}

impl<'lexer, 'input> Tokens<'lexer, 'input> {
  pub(super) fn new(lexer: &'lexer Lexer, input: &'input str) -> Self {
    Self {
      lexer,
      input,
      chars: input.char_indices().peekable(),
      error: false,
    }
  }
}

impl<'lexer, 'input> Iterator for Tokens<'lexer, 'input> {
  type Item = Result<Token<'lexer, 'input>, ()>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.error {
      return None;
    }

    let mut state = self.lexer.dfa.start();
    let mut start = self.chars.peek()?.0;

    loop {
      let end;
      match self.chars.peek() {
        Some(&(i, c)) => {
          let char_interval = util::find_char_interval(
            c as u32, &self.lexer.char_intervals);
            if c == '*' {
              println!("{:?} {:?}", state, self.lexer.dfa.result(state));
            }

          if let Some(next_state) = self.lexer.dfa.transition(state, char_interval) {
            self.chars.next();
            state = next_state;
            continue;
          }

          end = i;
        }
        None => {
          end = self.input.len();
        }
      }

      if let Some(&id) = self.lexer.dfa.result(state) {
        if let Some(kind) = self.lexer.token_names.get_by_left(&id) {
          return Some(Ok(Token {
            kind,
            text: &self.input[start..end],
            start,
            end,
          }));
        } else if end < self.input.len() {
          state = self.lexer.dfa.start();
          start = end;
        } else {
          return None;
        }
      } else {
        self.error = true;
        return Some(Err(()));
      }
    }
  }
}