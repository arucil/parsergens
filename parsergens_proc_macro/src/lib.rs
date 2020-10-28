#![feature(or_patterns)]

use proc_macro::TokenStream;
use proc_macro_error::{proc_macro_error, emit_error, abort_call_site};
use syn::*;
use quote::{ToTokens, quote};
use std::fs;
use std::path::Path;
use parser_spec::ParserKind;
use codegen::{Module, Formatter, Format};

mod parser_spec;

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
    Err(errors) => {
      for error in errors {
        emit_error!("{}", lr::report::report(&source_path, &grammar, error));
      }
      abort_call_site!("building parser failed");
    }
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

  lr::gen_decls(parser, scope);

  mo
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