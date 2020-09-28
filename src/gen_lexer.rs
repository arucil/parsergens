use grammar::{Lexer, Map, TokenId, Set};
use grammar::lexer::State;
use codegen::Scope;

pub fn gen(
  lexer: &Option<Lexer>,
  tokens: &Map<TokenId, String>,
  scope: &mut Scope,
) {
  let lexer = if let Some(lexer) = lexer {
    lexer
  } else {
    return;
  };

  super::gen_1d_table("DFA_TRANSITIONS", "(u32, u32)", &lexer.dfa.transitions, w)?;
  super::gen_1d_table("DFA_STATE_DISP", "usize", &lexer.dfa.state_disp, w)?;
  super::gen_1d_table("LEXER_CHAR_INTERVALS", "u32", &lexer.char_intervals, w)?;

  /*
  let num_states = lexer.dfa.state_disp.len();
  let accept_states = (0..num_states as u32).map(|s| {
    if let Some(token) = lexer.dfa.accept_states.get(&State(s)) {
      if tokens.contains_key(token) {
        token.id() as i32 + 1
      } else {
        -1
      }
    } else {
      0
    }
  }).join(", ");
  */

  writeln!(w, "{}", r##"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'input> {
  pub kind: TokenKind,
  pub text: &'input str,
  pub start: usize,
  pub end: usize,
}
    "##.trim_end())?;

  writeln!(w, "{}", r##"
#[derive(Debug, Clone)]
pub struct Tokens<'input> {
  input: &'input str,
  pos: usize,
}
    "##.trim_end())?;

  writeln!(w, "{}", r##"
#[derive(Debug, Clone)]
pub struct Error {
  pub char: char,
  pub start: usize,
  pub end: usize,
}
    "##.trim_end())?;

  writeln!(w, "{}", r##"
pub fn lex(input: &str) -> Tokens {
  Tokens::new(input)
}
    "##.trim_end())?;

  writeln!(w, "{}", r##"
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
    "##.trim_end())?;

  writeln!(w, "{}", r##"
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
    "##.trim_end())?;

  writeln!(w, "{}", r##"
fn transition(state: u32, c: u32) -> Option<u32> {
  let tx = DFA_TRANSITIONS[DFA_STATE_DISP[state as usize] + c as usize];
  if tx.0 == state && tx.1 != 0 {
    Some(tx.1 - 1)
  } else {
    None
  }
}
    "##.trim_end())?;

  writeln!(w, "{}", r##"
impl<'input> Iterator for Tokens<'input> {
  type Item = ::std::result::Result<Token<'input>, Error>;

  fn next(&mut self) -> ::std::option::Option<Self::Item> {
    use ::std::option::Option::{self, *};

    if self.pos == self.input.len() {
      return Option::None;
    }
    "##.trim_end())?;

  writeln!(w, "    let mut state = {};", lexer.dfa.start)?;

  writeln!(w, "{}", r##"
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
    "##.trim_end())?;

  gen_accept_states(&lexer.dfa.accept_states, &lexer.skip, w)?;

  writeln!(w, "{}", r##"
    }
  }
}
    "##.trim_end())?;

  Ok(())
}

fn gen_accept_states(
  accept_states: &Map<State, TokenId>,
  skip: &Set<TokenId>,
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"      match state {"##)?;

  if !skip.is_empty() {
    write!(w, "        ")?;
    let mut bar = false;
    for (state, token) in accept_states {
      if skip.contains(token) {
        if bar {
          write!(w, "| ")?;
        }
        bar = true;
        write!(w, "{} ", state.0)?;
      }
    }
    writeln!(w, "{}", r##"=> { end = self.pos; token_kind = None; }"##)?;
  }

  for (state, token) in accept_states {
    if !skip.contains(token) {
      writeln!(w,
        "{} => {{ end = self.pos; token_kind = Some({}); }}",
        state.0,
        token.id())?;
    }
  }

  writeln!(w, "{}", r##"
        _ => {}
      }
  "##.trim_end())?;

  Ok(())
}