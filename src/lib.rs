
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::*;
use quote::quote;
use quote::format_ident;
use std::fs;
use parser_spec::ParserKind;

mod parser_spec;
mod gen_token_enum;

#[proc_macro_error]
#[proc_macro]
pub fn parsergen(expr: TokenStream) -> TokenStream {
  let spec = parse_macro_input!(expr as parser_spec::ParserSpec);

  let source = spec.file_path.span().source_file().path();
  let file_path = source.parent().unwrap().join(spec.file_path.value());
  let file_path = dunce::canonicalize(file_path).unwrap();

  let grammar = fs::read_to_string(file_path).unwrap();

  let parser = match spec.kind {
    ParserKind::Slr => {
      lr::slr::build(&grammar).unwrap()
    }
    _ => todo!()
  };

  let (token_enum, tokens) = gen_token_enum::gen_token_enum(&parser.tokens);

  todo!()
}
