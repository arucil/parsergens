#![feature(or_patterns)]

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::*;
use quote::{ToTokens, quote};
use std::fs;
use std::path::Path;
use std::io::prelude::*;
use parser_spec::ParserKind;

mod parser_spec;
mod gen_token_enum;
mod gen_lexer;
mod gen_parser;
mod tpl_engine;

#[proc_macro_error]
#[proc_macro]
pub fn parsergen(expr: TokenStream) -> TokenStream {
  let spec = parse_macro_input!(expr as parser_spec::ParserSpec);

  let source = spec.file_path.span().source_file().path();
  let source_path = source.parent().unwrap().join(spec.file_path.value());
  let source_path = dunce::canonicalize(source_path).unwrap();

  let grammar = fs::read_to_string(source_path).unwrap();

  let parser = match spec.kind {
    ParserKind::Slr => {
      lr::slr::build(&grammar).unwrap()
    }
    _ => todo!()
  };

  let user_code = parser.user_code.join("\n");

  let (token_enum, tokens) = gen_token_enum::gen(&parser.tokens);
  let lexer = gen_lexer::gen(&parser.lexer, &tokens);
  let parser = gen_parser::gen(&parser);

  let vis = spec.vis;
  let mod_name = spec.mod_name;

  let output = format!(
    r##"
{vis} mod {mod_name} {{
  #![allow(dead_code, non_camel_case_types, unused_parens, unused_mut)]
  #![allow(unused_variables, unused_braces, non_snake_case)]
  {user_code}
  {token_enum}
  {lexer}
  {parser}
}}
    "##,
    vis = vis.to_token_stream(),
    mod_name = mod_name.to_token_stream(),
    user_code = user_code,
    token_enum = token_enum,
    lexer = lexer,
    parser = parser);

  let output_file = format!("{}.rs", mod_name);

  let output_path = Path::new(env!("OUT_DIR")).join(&output_file);

  let mut file = fs::OpenOptions::new()
    .create(true)
    .write(true)
    .truncate(true)
    .open(&output_path)
    .expect(&format!("path: {:?}", output_path));

  writeln!(&mut file, "{}", output).unwrap();

  quote!(
    include!(concat!(env!("OUT_DIR"), "/", #output_file));
  ).into()
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