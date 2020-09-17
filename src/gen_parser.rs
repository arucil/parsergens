use lr::Parser;
use grammar::{Map, TokenId};
use syn::Ident;
use quote::*;
use heck::SnakeCase;

pub fn gen(
  parser: &Parser,
  tokens: &Map<TokenId, Ident>
) -> impl ToTokens {
  let action_row_len = parser.action.len();
  let action_col_len = parser.action[0].len();
  let action = parser.action.iter().map(|row| {
    let row = row.iter();
    quote!{ [#(#row),*] }
  });

  let goto_row_len = parser.goto.len();
  let goto_col_len = parser.goto[0].len();
  let goto = parser.goto.iter().map(|row| {
    let row = row.iter();
    quote!{ [#(#row),*] }
  });

  let prods_len = parser.prods.len();
  let prods = parser.prods.iter().map(|(a, b, c)| {
    let c = format_ident!("{}", format!("{:?}", c));
    quote! { ( #a, #b, ProductionKind::#c) }
  });

  let start = parser.start.iter().map(|(name, state)| {
    let name = name.replace(&['\'', '-'][..], "_");
    let fn_name = format_ident!("parse_{}", name.to_snake_case());

    let ty = parser.

    quote! {
      pub fn #fn_name(&mut self) 
    }
  });

  quote! {
    static ACTION: [[i32; #action_col_len]; #action_row_len] = [#(#action),*];
    static GOTO: [[u32; #goto_col_len]; #goto_row_len] = [#(#goto),*];
    static PRODUCTIONS: [(usize, u32, ProductionKind); #prods_len] = [#(#prods),*];

    enum ProductionKind {
      Ordinary,
      RepetitionFirst,
      RepetitionRest,
    }

    pub struct Parser<I> {
      tokens: I
    }

    pub fn with_input(input: &str) -> Parser<Tokens> {
      Parser { tokens: lex(input) }
    }

    pub fn with_tokens<'input, I: 'input>(
      tokens: I
    ) -> Parser<I>
      where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
    {
      Parser { tokens }
    }

    impl<I> Parser<I>
      where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
    {
    }
  }
}