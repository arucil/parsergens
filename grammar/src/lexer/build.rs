use std::collections::BTreeSet;
use super::super::grammar_parser::ast::*;
use super::super::grammar_parser::regex::{RegexError, RegexErrorKind};
use super::nfa::{State, Nfa};
use super::nfa_builder::NfaBuilder;
use super::{Lexer, TokenId, LexerError};
use super::util;
use crate::{Set, BiMap};

pub fn build(decls: &[&TokenDecl], skips: &[&SkipDecl]) -> Result<Lexer, LexerError> {
  if decls.is_empty() {
    return Err(LexerError::NoTokens);
  }

  validate_decls(decls, skips)?;

  let char_intervals = compute_char_intervals(decls, skips);

  let mut nfa_builder = Nfa::builder();
  let start = nfa_builder.state();
  let mut token_gen = TokenGen::default();
  let mut token_names = BiMap::new();

  for decl in decls {
    let accept = add_regex_to_nfa(
      &mut nfa_builder, &decl.pattern.regex, start, &char_intervals);
    let token = token_gen.gen();
    token_names.insert(token, decl.name.1.clone());

    nfa_builder.accept(accept, decl.pattern.source.0 .0, token);
  }

  for decl in skips {
    let accept = add_regex_to_nfa(
      &mut nfa_builder, &decl.pattern.regex, start, &char_intervals);
    let token = token_gen.gen();

    nfa_builder.accept(accept, decl.pattern.source.0 .0, token);
  }

  let nfa = nfa_builder.build();
  let dfa = nfa.to_dfa(start).minimize();

  Ok(Lexer {
    dfa,
    char_intervals,
    token_names,
  })
}

fn add_regex_to_nfa(
  builder: &mut NfaBuilder<u32, usize, TokenId>,
  regex: &Regex,
  enter: State,
  char_intervals: &[u32],
) -> State {
  match regex {
    Regex::Empty => {
      enter
    }
    Regex::Any => {
      let exit = builder.state();
      add_intervals_to_nfa(builder, enter, exit, 0, char_intervals.len() as u32);
      exit
    }
    Regex::Char(c) => {
      let exit = builder.state();
      let interval = util::find_char_interval(*c as u32, char_intervals);
      builder.transition(enter, exit, Some(interval));
      exit
    }
    Regex::CharSet(set, negated) => {
      let exit = builder.state();
      if *negated {
        let mut neg_set = (0..char_intervals.len() as u32).collect::<Set<_>>();
        for item in set {
          match item {
            CharSetItem::Char(c) => {
              let interval = util::find_char_interval(*c as u32, char_intervals);
              neg_set.remove(&interval);
            }
            CharSetItem::CharClass(class) => {
              apply_char_class(*class, char_intervals, |lower, upper| {
                for i in lower..upper {
                  neg_set.remove(&i);
                }
              });
            }
            CharSetItem::Range(a, b) => {
              let lower = util::find_char_interval(*a as u32, char_intervals);
              let upper = util::find_char_interval(*b as u32 + 1, char_intervals);
              for i in lower..upper {
                neg_set.remove(&i);
              }
            }
          }
        }

        for interval_ix in neg_set {
          builder.transition(enter, exit, Some(interval_ix));
        }
      } else {
        for item in set {
          match item {
            CharSetItem::Char(c) => {
              let interval_ix = util::find_char_interval(*c as u32, char_intervals);
              builder.transition(enter, exit, Some(interval_ix));
            }
            CharSetItem::CharClass(class) => {
              add_char_class_to_nfa(builder, enter, exit, *class, char_intervals);
            }
            CharSetItem::Range(a, b) => {
              let lower = util::find_char_interval(*a as u32, char_intervals);
              let upper = util::find_char_interval(*b as u32 + 1, char_intervals);
              add_intervals_to_nfa(builder, enter, exit, lower, upper);
            }
          }
        }
      }
      exit
    }
    Regex::CharClass(class) => {
      let exit = builder.state();
      add_char_class_to_nfa(builder, enter, exit, *class, char_intervals);
      exit
    }
    Regex::Alt(alts) => {
      let exit = builder.state();
      for alt in alts {
        let last = add_regex_to_nfa(builder, alt, enter, char_intervals);
        builder.transition(last, exit, None);
      }
      exit
    }
    Regex::Concat(items) => {
      items.iter().fold(enter, |enter, item| {
        add_regex_to_nfa(builder, item, enter, char_intervals)
      })
    }
    Regex::Optional(item) => {
      let exit = add_regex_to_nfa(builder, item, enter, char_intervals);
      builder.transition(enter, exit, None);
      exit
    }
    Regex::Many(item) => {
      let exit = builder.state();
      builder.transition(enter, exit, None);
      let temp = add_regex_to_nfa(builder, item, exit, char_intervals);
      builder.transition(temp, exit, None);
      exit
    }
    Regex::Many1(item) => {
      let temp = builder.state();
      builder.transition(enter, temp, None);
      let exit = add_regex_to_nfa(builder, item, temp, char_intervals);
      builder.transition(exit, temp, None);
      exit
    }
  }
}

fn add_char_class_to_nfa(
  builder: &mut NfaBuilder<u32, usize, TokenId>,
  enter: State,
  exit: State,
  class: CharClass,
  char_intervals: &[u32]
) {
  apply_char_class(class, char_intervals, |lower, upper| {
    add_intervals_to_nfa(builder, enter, exit, lower, upper);
  });
}

fn apply_char_class<F>(
  class: CharClass,
  char_intervals: &[u32],
  mut action: F
)
  where F: FnMut(u32, u32)
{
  for (c0, c1) in class.ranges() {
    let lower = util::find_char_interval(c0 as u32, char_intervals);
    let upper = util::find_char_interval(c1 as u32 + 1, char_intervals);
    action(lower, upper);
  }
}

fn add_intervals_to_nfa(
  builder: &mut NfaBuilder<u32, usize, TokenId>,
  enter: State,
  exit: State,
  lower_ix: u32,
  upper_ix: u32
) {
  for i in lower_ix..upper_ix {
    builder.transition(enter, exit, Some(i));
  }
}

fn compute_char_intervals(
  decls: &[&TokenDecl],
  skips: &[&SkipDecl]
) -> Vec<u32> {
  let mut char_intervals = BTreeSet::new();
  char_intervals.insert(0);

  for decl in decls {
    collect_char_intervals(&decl.pattern.regex, &mut char_intervals);
  }

  for decl in skips {
    collect_char_intervals(&decl.pattern.regex, &mut char_intervals);
  }

  char_intervals.into_iter().collect()
}

fn collect_char_intervals(regex: &Regex, intervals: &mut BTreeSet<u32>) {
  match regex {
    Regex::Any => {}
    Regex::Empty => {}
    Regex::Char(c) => {
      intervals.insert(*c as u32);
      intervals.insert(*c as u32 + 1);
    }
    Regex::CharSet(set, _) => {
      for item in set {
        match item {
          CharSetItem::Char(c) => {
            intervals.insert(*c as u32);
            intervals.insert(*c as u32 + 1);
          }
          CharSetItem::CharClass(class) => {
            collect_char_class_intervals(class, intervals);
          }
          CharSetItem::Range(a, b) => {
            intervals.insert(*a as u32);
            intervals.insert(*b as u32 + 1);
          }
        }
      }
    }
    Regex::CharClass(class) => {
      collect_char_class_intervals(class, intervals);
    }
    Regex::Alt(items) | Regex::Concat(items) => {
      for item in items {
        collect_char_intervals(item, intervals);
      }
    }
    Regex::Optional(item) | Regex::Many(item) | Regex::Many1(item) => {
      collect_char_intervals(item, intervals);
    }
  }
}

fn collect_char_class_intervals(class: &CharClass, intervals: &mut BTreeSet<u32>) {
  for (c0, c1) in class.ranges() {
    intervals.insert(c0 as u32);
    intervals.insert(c1 as u32 + 1);
  }
}

fn validate_decls(
  decls: &[&TokenDecl],
  skips: &[&SkipDecl]
) -> Result<(), LexerError> {
  for decl in decls {
    if regex_accepts_empty_string(&decl.pattern.regex) {
      return Err(LexerError::RegexError(RegexError {
        kind: RegexErrorKind::Empty,
        span: decl.pattern.source.0,
      }));
    }
  }

  for decl in skips {
    if regex_accepts_empty_string(&decl.pattern.regex) {
      return Err(LexerError::RegexError(RegexError {
        kind: RegexErrorKind::Empty,
        span: decl.pattern.source.0,
      }));
    }
  }

  Ok(())
}

fn regex_accepts_empty_string(regex: &Regex) -> bool {
  match regex {
    Regex::Empty => true,
    Regex::Alt(alts) => alts.iter().any(regex_accepts_empty_string),
    Regex::Concat(items) => items.iter().all(regex_accepts_empty_string),
    Regex::Many(_) | Regex::Optional(_) => true,
    Regex::Many1(item) => regex_accepts_empty_string(item),
    Regex::Any | Regex::Char(_) | Regex::CharClass(_) => false,
    Regex::CharSet(set, negated) => {
      if *negated {
        // TODO this needs further consideration
        false
      } else {
        char_set_accepts_empty_string(set)
      }
    }
  }
}

fn char_set_accepts_empty_string(set: &[CharSetItem]) -> bool {
  set.iter()
    .all(|item| {
      match item {
        CharSetItem::Char(_) => false,
        CharSetItem::CharClass(_) => false,
        CharSetItem::Range(a, b) => a > b,
      }
    })
}

#[derive(Default)]
struct TokenGen(u32);

impl TokenGen {
  fn gen(&mut self) -> TokenId {
    let i = self.0;
    self.0 += 1;
    TokenId(i)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::super::grammar_parser;
  use insta::assert_debug_snapshot;

  #[test]
  fn longest_match() {
    let ast = grammar_parser::parse(r#"
%token IF "if"
%token IN "int"
%token ID /[a-zA-Z_][\w_]*/
    "#).unwrap();

    let decls = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Token(decl) => Some(decl),
        _ => None,
      })
      .collect::<Vec<_>>();

    let char_intervals = compute_char_intervals(&decls, &[]);

    let mut nfa_builder = NfaBuilder::new();
    let start = nfa_builder.state();
    let mut token_gen = TokenGen::default();

    for decl in decls {
      let accept = add_regex_to_nfa(
        &mut nfa_builder, &decl.pattern.regex, start, &char_intervals);
      let token = token_gen.gen();

      nfa_builder.accept(accept, !decl.pattern.source.0 .0, token);
    }

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(start);

    assert_debug_snapshot!((
      char_intervals.iter().enumerate().collect::<Vec<_>>(),
      nfa,
      dfa,
    ));
  }

  #[test]
  fn regex_repetition() {
    let ast = grammar_parser::parse(r#"
%token INT /\d+(\.\d*)?/
    "#).unwrap();

    let decls = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Token(decl) => Some(decl),
        _ => None,
      })
      .collect::<Vec<_>>();

    let char_intervals = compute_char_intervals(&decls, &[]);

    let mut nfa_builder = NfaBuilder::new();
    let start = nfa_builder.state();
    let mut token_gen = TokenGen::default();

    for decl in decls {
      let accept = add_regex_to_nfa(
        &mut nfa_builder, &decl.pattern.regex, start, &char_intervals);
      let token = token_gen.gen();

      nfa_builder.accept(accept, !decl.pattern.source.0 .0, token);
    }

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(start);

    assert_debug_snapshot!((
      char_intervals.iter().enumerate().collect::<Vec<_>>(),
      nfa,
      dfa,
    ));
  }

  #[test]
  fn line_comment() {
    let ast = grammar_parser::parse(r#"
%token COMMENT /#[^\n]*/
    "#).unwrap();

    let decls = ast.iter()
      .filter_map(|decl| match &decl.1 {
        Decl::Token(decl) => Some(decl),
        _ => None,
      })
      .collect::<Vec<_>>();

    let char_intervals = compute_char_intervals(&decls, &[]);

    let mut nfa_builder = NfaBuilder::new();
    let start = nfa_builder.state();
    let mut token_gen = TokenGen::default();

    for decl in decls {
      let accept = add_regex_to_nfa(
        &mut nfa_builder, &decl.pattern.regex, start, &char_intervals);
      let token = token_gen.gen();

      nfa_builder.accept(accept, !decl.pattern.source.0 .0, token);
    }

    let nfa = nfa_builder.build();
    let dfa = nfa.to_dfa(start);

    assert_debug_snapshot!((
      char_intervals.iter().enumerate().collect::<Vec<_>>(),
      nfa,
      dfa,
    ));
  }
}