    static DFA_TRANSITIONS: [(u32, u32); __(dfa_tx_len)] = [__(tx)];
    static DFA_STATE_BASE: [usize; __(state_base_len)] = [__(base)];
    static DFA_ACCEPT_STATES: [i32; __(state_base_len)] = [__(accept_states)];
    const DFA_START: u32 = __(start);

    static LEXER_CHAR_INTERVALS: [u32; __(char_intervals_len)] = [__(char_intervals)];

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Token<'input> {
      pub kind: TokenKind,
      pub text: &'input str,
      pub start: usize,
      pub end: usize,
    }

    #[derive(Debug, Clone)]
    pub struct Tokens<'input> {
      input: &'input str,
      pos: usize,
    }

    #[derive(Debug, Clone)]
    pub struct Error {
      pub char: char,
      pub start: usize,
      pub end: usize,
    }

    pub fn lex(input: &str) -> Tokens {
      Tokens::new(input)
    }

    impl<'input> Tokens<'input> {
      fn new(input: &'input str) -> Self {
        Self {
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

    pub fn find_char_interval(char: u32, char_intervals: &[u32]) -> u32 {
      let mut lo = 0;
      let mut hi = char_intervals.len();

      while lo < hi {
        let mid = (lo + hi) / 2;
        if char_intervals[mid] > char {
          hi = mid;
        } else {
          lo = mid + 1;
        }
      }

      lo as u32 - 1
    }

    fn transition(state: u32, c: u32) -> Option<u32> {
      let tx = DFA_TRANSITIONS[DFA_STATE_BASE[state as usize] + c as usize];
      if tx.0 == state && tx.1 != 0 {
        Some(tx.1 - 1)
      } else {
        None
      }
    }

    impl<'input> Iterator for Tokens<'input> {
      type Item = ::std::result::Result<Token<'input>, Error>;

      fn next(&mut self) -> ::std::option::Option<Self::Item> {
        use ::std::option::Option::{self, *};

        if self.pos == self.input.len() {
          return Option::None;
        }

        let mut state = DFA_START;
        let mut start = self.pos;
        let mut end = start;
        let mut token_kind: Option<TokenKind> = None;

        loop {
          let no_move;

          match self.peek_char() {
            Some(c) => {
              let char_interval = find_char_interval(
                c as u32, &LEXER_CHAR_INTERVALS);

              if let Some(next_state) = transition(state, char_interval) {
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
              state = DFA_START;
              start = end;
              continue;
            }
          }

          let accept = DFA_ACCEPT_STATES[state as usize];
          if accept > 0 {
            end = self.pos;
            token_kind = Some(unsafe { ::std::mem::transmute::<_, TokenKind>(accept - 1) });
          } else if accept < 0 {
            end = self.pos;
            token_kind = None;
          }
        }
      }
    }