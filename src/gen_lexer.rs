use grammar::{Lexer, Map, TokenId};
use grammar::lexer::State;
use std::path::Path;
use itertools::Itertools;
use std::fmt::{self, Write};
use super::indent_writer::IndentWriter;

pub fn gen(
  lexer: &Option<Lexer>,
  tokens: &Map<TokenId, String>,
  w: &mut IndentWriter<impl Write>,
) -> fmt::Result {
  let lexer = if let Some(lexer) = lexer {
    lexer
  } else {
    return Ok(());
  };

  gen_transition_table(&lexer.dfa.transitions, w)?;

  gen_displacement_table(&lexer.dfa.state_disp, w)?;

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

  let char_intervals_len = lexer.char_intervals.len();
  let char_intervals = lexer.char_intervals.iter().join(", ");
  
  let start = lexer.dfa.start;

  let path = Path::new(file!()).parent().unwrap().join("templates/lexer.tpl.rs");
  crate::tpl_engine::process(path, |name| {
    match name {
      "dfa_tx_len" => dfa_tx_len.to_string(),
      "tx" => tx.to_string(),
      "state_base_len" => state_base_len.to_string(),
      "base" => base.to_string(),
      "accept_states" => accept_states.to_string(),
      "start" => start.to_string(),
      "char_intervals_len" => char_intervals_len.to_string(),
      "char_intervals" => char_intervals.to_string(),
      _ => panic!("unknown param: {}", name),
    }
  }).unwrap()
}

fn gen_transition_table(
  transitions: &[(u32, u32)],
  w: &mut impl Write,
) -> fmt::Result {
  write!(w, "static DFA_TRANSITIONS: [(u32, u32); {}] = [", transitions.len())?;

  for t in transitions {
    write!(w, "({}, {}), ", t.0, t.1)?;
  }

  writeln!(w, "];")
}

fn gen_displacement_table(
  disp: &[usize],
  w: &mut impl Write,
) -> fmt::Result {
  write!(w, "static DFA_STATE_DISP: [usize; {}] = [", disp.len())?;

  for t in disp {
    write!(w, "{}, ", t)?;
  }

  writeln!(w, "];")
}