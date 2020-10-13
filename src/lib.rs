#![feature(or_patterns)]

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::*;
use quote::{ToTokens, quote};
use std::fs;
use std::path::Path;
use parser_spec::ParserKind;
use grammar::{GrammarError, GrammarErrorKind};
use std::fmt::{self, Write};
use codegen::{Module, Scope, Formatter, Format};
use itertools::Itertools;

mod parser_spec;
mod gen_token_enum;
mod gen_lexer;
mod gen_parser;

#[proc_macro_error]
#[proc_macro]
pub fn parsergen(expr: TokenStream) -> TokenStream {
  use std::io::prelude::*;

  let spec = parse_macro_input!(expr as parser_spec::ParserSpec);

  let source = spec.file_path.span().source_file().path();
  let source_path = source.parent().unwrap().join(spec.file_path.value());
  let source_path = dunce::canonicalize(source_path).unwrap();

  let grammar = fs::read_to_string(&source_path).unwrap();

  let parser = match spec.kind {
    ParserKind::Lr => {
      lr::build(&grammar, lr::ParserKind::Clr)
    }
    ParserKind::Lalr => {
      lr::build(&grammar, lr::ParserKind::Lalr)
    }
  };

  let parser = match parser {
    Ok(parser) => parser,
    Err(err) => report(source_path, grammar, err),
  };

  let vis = spec.vis.to_token_stream().to_string();
  let mod_name = spec.mod_name.to_token_stream().to_string();

  let module = gen_mod(
    &vis,
    &mod_name,
    &parser,
  );

  let output_file = format!("{}.rs", mod_name);

  let output_path = Path::new(env!("OUT_DIR")).join(&output_file);

  let mut file = fs::OpenOptions::new()
    .create(true)
    .write(true)
    .truncate(true)
    .open(&output_path)
    .expect(&format!("path: {:?}", output_path));

  let mut output = String::new();
  let mut fmt = Formatter::new(&mut output);
  fmt.set_indent(2);
  module.fmt(&mut fmt).unwrap();

  writeln!(&mut file, "{}", output).unwrap();

  quote!(
    include!(concat!(env!("OUT_DIR"), "/", #output_file));
  ).into()
}

fn gen_mod(
  vis: &str,
  mod_name: &str,
  parser: &lr::Parser,
) -> Module {
  let mut mo = Module::new(mod_name);
  mo.vis(vis);
  let scope = mo.scope();

  scope.new_attr("allow").arg_delimited(
    "dead_code, non_camel_case_types, unused_parens, unused_mut");
  scope.new_attr("allow").arg_delimited(
    "unused_variables, unused_braces, non_snake_case");

  for code in &parser.user_code {
    scope.raw(code);
  }

  let tokens = gen_token_enum::gen(&parser.tokens, scope);
  gen_lexer::gen(&parser.lexer, &tokens, scope);
  gen_parser::gen(&parser, scope);

  mo
}

fn gen_1d_table(
  table_name: &str,
  cell_type: &str,
  table: &[impl fmt::Debug],
  scope: &mut Scope,
) {
  let ty = format!("[{}; {}]", cell_type, table.len());
  let value = table.iter().map(|x| format!("{:?}", x)).join(", ");
  let value = format!("[{}]", value);
  scope.new_static(table_name, ty).value(value);
}

fn gen_2d_table(
  table_name: &str,
  cell_type: &str,
  table: &[Vec<impl fmt::Debug>],
  scope: &mut Scope,
) {
  let rows = table.len();
  let cols = table[0].len();

  let ty = format!("[[{}; {}]; {}]", cell_type, cols, rows);
  let value = table.iter().map(|row|
    format!("[{}]", row.iter().map(|x| format!("{:?}", x)).join(", ")))
    .join(", ");
  let value = format!("[{}]", value);
  scope.new_static(table_name, ty).value(value);
}

fn report(
  path: impl AsRef<Path>,
  input: impl AsRef<str>,
  err: lr::Error
) -> ! {
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
) -> ! {
  let lines = input.as_ref()[..err.span.0].split('\n').collect::<Vec<_>>();
  let line = lines.len() + 1;
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

  panic!("{}", buf)
}

fn report_rr_conflict(
  err: lr::ReduceReduceConflictError
) -> ! {
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

  panic!("{}", buf)
}

fn report_sr_conflict(
  err: lr::ShiftReduceConflictError
) -> ! {
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

  panic!("{}", buf)
}

fn report_prec_conflict(
  err: lr::PrecConflictError
) -> ! {
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

  panic!("{}", buf)
}

fn report_assoc_conflict(
  err: lr::AssocConflictError
) -> ! {
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

  panic!("{}", buf)
}

trait ExpectWith {
  type Ok;
  type Error;

  fn expect_with<F, S>(self, f: F) -> Self::Ok
    where
      F: FnOnce(Self::Error) -> S,
      S: AsRef<str>;
}

impl<T, E> ExpectWith for std::result::Result<T, E> {
  type Ok = T;
  type Error = E;

  fn expect_with<F, S>(self, f: F) -> T
    where
      F: FnOnce(Self::Error) -> S,
      S: AsRef<str>,
  {
    match self {
      Ok(v) => v,
      Err(err) => panic!("{}", f(err).as_ref()),
    }
  }
}