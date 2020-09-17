
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::*;
use quote::quote;
use std::fs;
use parser_spec::ParserKind;

mod parser_spec;
mod gen_token_enum;
mod gen_lexer;
mod gen_parser;

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

  let mut user_code = quote!{};
  for code in &parser.user_code {
    let code = code.parse::<TokenStream>().expect("valid user code");
    let code = parse_macro_input!(code as Item);
    user_code = quote!{ #user_code #code };
  }

  let (token_enum, tokens) = gen_token_enum::gen(&parser.tokens);
  let lexer = gen_lexer::gen(&parser.lexer, &tokens);
  let parser = gen_parser::gen(&parser, &tokens);

  let vis = spec.vis;
  let mod_name = spec.mod_name;

  (quote! {
    #vis mod #mod_name {
      #user_code
      #token_enum
      #lexer
      #parser
    }
  }).into()
}
