use grammar::{Lexer, Map, TokenId, Set};
use grammar::lexer::State;
use codegen::{Scope, Function};
use itertools::Itertools;

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

  super::gen_1d_table("DFA_TRANSITIONS", "(u32, u32)", &lexer.dfa.transitions, scope);
  super::gen_1d_table("DFA_STATE_DISP", "usize", &lexer.dfa.state_disp, scope);
  super::gen_1d_table("LEXER_CHAR_INTERVALS", "u32", &lexer.char_intervals, scope);

  scope.new_const("DFA_START", "u32").value(lexer.dfa.start.to_string());

  scope.new_struct("Token")
    .vis("pub")
    .generic("'input")
    .derive("Debug")
    .derive("Clone")
    .derive("PartialEq")
    .derive("Eq")
    .field_pub("kind", "TokenKind")
    .field_pub("text", "&'input str")
    .field_pub("start", "usize")
    .field_pub("end", "usize");

  scope.new_struct("Tokens")
    .vis("pub")
    .generic("'input")
    .field("input", "&'input str")
    .field("pos", "usize");

  scope.new_struct("Error")
    .vis("pub")
    .derive("Debug")
    .derive("Clone")
    .field_pub("char", "char")
    .field_pub("start", "usize")
    .field_pub("end", "usize");

  scope.new_fn("lex")
    .vis("pub")
    .arg("input", "&str")
    .ret("Tokens")
    .line("Tokens::new(input)");

  let impl_toks = scope.new_impl("Tokens")
    .generic("'input")
    .target_generic("'input");
  impl_toks.new_fn("new")
    .arg("input", "&'input str")
    .ret("Self")
    .line("Self {\n  input,\n  pos: 0,\n}");
  impl_toks.new_fn("peek_char")
    .arg_mut_self()
    .ret("Option<char>")
    .line("self.input[self.pos..].chars().next()");
  impl_toks.new_fn("advance")
    .arg_mut_self()
    .line("self.pos += 1;")
    .line(r"
while !self.input.is_char_boundary(self.pos) {
  self.pos += 1;
}");

  scope.new_fn("find_char_interval")
    .arg("char", "u32")
    .arg("char_intervals", "&[u32]")
    .ret("u32")
    .line("let mut lo = 0;")
    .line("let mut hi = char_intervals.len();")
    .line(r"
while lo < hi {
  let mid = (lo + hi) / 2;
  if char_intervals[mid] > char {
    hi = mid;
  } else {
    lo = mid + 1;
  }
}
")
    .line("lo as u32 - 1");

  scope.new_fn("transition")
    .arg("state", "u32")
    .arg("c", "u32")
    .ret("Option<u32>")
    .line("let tx = DFA_TRANSITIONS[DFA_STATE_DISP[state as usize] + c as usize];")
    .line(r"
if tx.0 == state && tx.1 != 0 {
  Some(tx.1 - 1)
} else {
  None
}
");

  let impl_iter_toks = scope.new_impl("Tokens")
    .generic("'input")
    .target_generic("'input")
    .impl_trait("Iterator");
  impl_iter_toks.associate_type("Item", "::std::result::Result<Token<'input>, Error>");

  let impl_iter_toks_next = impl_iter_toks.new_fn("next")
    .arg_mut_self()
    .ret("::std::option::Option<Self::Item>")
    .line("use ::std::option::Option::{self, *};")
    .line(r"
if self.pos == self.input.len() {
  return Option::None;
}
")
    .line("let mut state = DFA_START;")
    .line(format!("let mut start = self.pos;"))
    .line(format!("let mut end = start;"))
    .line(format!("let mut token_kind: Option<TokenKind> = None;"))
    .line(r"
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
  }");

  gen_accept_states(&lexer.dfa.accept_states, &lexer.skip, tokens, impl_iter_toks_next);

  impl_iter_toks_next.line("  }");
}

fn gen_accept_states(
  accept_states: &Map<State, TokenId>,
  skip: &Set<TokenId>,
  tokens: &Map<TokenId, String>,
  func: &mut Function,
) {
  func.line("  match state {");

  if !skip.is_empty() {
    func.line(format!("    {} => {{ end = self.pos; token_kind = None; }}",
      accept_states.iter().filter(|&(_, tok)| skip.contains(tok))
        .map(|(state, _)| state.0.to_string())
        .join(" | ")));
  }

  for (state, token) in accept_states {
    if !skip.contains(token) {
      func.line(format!("    {} => {{ end = self.pos; token_kind = Some(TokenKind::{}); }}",
        state.0,
        tokens[token]));
    }
  }

  func.line("    _ => {}");
  func.line("  }");
}