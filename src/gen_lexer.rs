use grammar::{Lexer, Map, TokenId};
use grammar::lexer::State;
use std::path::Path;
use itertools::Itertools;

pub fn gen(
  lexer: &Option<Lexer>,
  tokens: &Map<TokenId, String>
) -> String {
  let lexer = if let Some(lexer) = lexer {
    lexer
  } else {
    return String::new();
  };

  let dfa_tx_len = lexer.dfa.transitions.len();
  let tx = lexer.dfa.transitions.iter().map(|(a,b)| format!("({}, {})", a, b)).join(", ");

  let state_base_len = lexer.dfa.state_base.len();
  let base = lexer.dfa.state_base.iter().join(", ");

  let accept_states = (0..state_base_len as u32).map(|s| {
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