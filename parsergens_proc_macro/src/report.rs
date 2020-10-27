use grammar::{GrammarError, GrammarErrorKind};
use std::fmt::Write;
use std::path::Path;

pub(crate) fn report(
  path: impl AsRef<Path>,
  input: impl AsRef<str>,
  err: lr::Error
) -> String {
  match err {
    lr::Error::GrammarError(err) => report_grammar_error(path, input, err),
    lr::Error::ReduceReduceConflict(err) => report_rr_conflict(err),
    lr::Error::ShiftReduceConflict(err) => report_sr_conflict(err),
    lr::Error::PrecConflict(err) => report_prec_conflict(err),
    lr::Error::AssocConflict(err) => report_assoc_conflict(err),
  }
}

fn report_grammar_error(
  path: impl AsRef<Path>,
  input: impl AsRef<str>,
  err: GrammarError
) -> String {
  let lines = input.as_ref()[..err.span.0].split('\n').collect::<Vec<_>>();
  let line = lines.len();
  let col = lines.last().unwrap().chars().count() + 1;
  let error = match err.kind {
    GrammarErrorKind::MissingDecl => "missing declaration",
    GrammarErrorKind::NameConflict => "name conflict",
    GrammarErrorKind::ParseError => "syntax error",
    GrammarErrorKind::NameNotFound => "name not found",
    GrammarErrorKind::TokenDeclConflict => "token declaration conflict",
  };

  let mut buf = String::new();
  writeln!(&mut buf,
    "{} at {}:{}:{}",
    error,
    path.as_ref().display(),
    line,
    col
  ).unwrap();
  writeln!(&mut buf,
    "message: {}", err.message
  ).unwrap();

  buf
}

fn report_rr_conflict(
  err: lr::ReduceReduceConflictError
) -> String {
  let mut buf = String::new();

  writeln!(&mut buf,
    "reduce-reduce conflict at state:\n"
  ).unwrap();

  for item in &err.state_items {
    writeln!(&mut buf,
      "  {}", item,
    ).unwrap();
  }

  writeln!(&mut buf,
    "\nwhich can be reduced by:\n\n  {}\n\nor:\n\n  {}\n\nwhen the lookahead is {}",
    err.reduce1,
    err.reduce2,
    err.lookahead,
  ).unwrap();

  buf
}

fn report_sr_conflict(
  err: lr::ShiftReduceConflictError
) -> String {
  let mut buf = String::new();

  writeln!(&mut buf,
    "shift-reduce conflict at state:\n"
  ).unwrap();

  for item in &err.state_items {
    writeln!(&mut buf,
      "  {}", item,
    ).unwrap();
  }

  writeln!(&mut buf,
    "\nwhich can shift {}\nor reduce by:\n\n  {}",
    err.shift,
    err.reduce,
  ).unwrap();

  buf
}

fn report_prec_conflict(
  err: lr::PrecConflictError
) -> String {
  let mut buf = String::new();

  writeln!(&mut buf,
    "precedence conflict at state:\n"
  ).unwrap();

  for item in &err.state_items {
    writeln!(&mut buf,
      "  {}", item,
    ).unwrap();
  }

  writeln!(&mut buf,
    "\nthe production:\n\n  {}\n\nand the production:\n\n  {}\n\nhave different precedence",
    err.prod1,
    err.prod2,
  ).unwrap();

  buf
}

fn report_assoc_conflict(
  err: lr::AssocConflictError
) -> String {
  let mut buf = String::new();

  writeln!(&mut buf,
    "associativity conflict at state:\n"
  ).unwrap();

  for item in &err.state_items {
    writeln!(&mut buf,
      "  {}", item,
    ).unwrap();
  }

  writeln!(&mut buf,
    "\nthe production:\n\n  {}\n\nand the production:\n\n  {}\n\nhave different associativity",
    err.prod1,
    err.prod2,
  ).unwrap();

  buf
}