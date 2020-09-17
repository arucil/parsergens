use grammar::{BiMap, Map, TokenId};
use quote::*;
use syn::Ident;
use heck::CamelCase;

pub fn gen_token_enum(
  tokens: &BiMap<TokenId, String>
) -> (impl ToTokens, Map<TokenId, Ident>) {
  let variants = tokens.iter().map(|(id, name)| {
    let name = name.replace(&['\'', '-'][..], "_").to_camel_case();
    let name = format_ident!("{}", name);
    (*id, name)
  });
  let tokens = variants.collect::<Map<_, _>>();
  let variants = tokens.iter().map(|(id, name)| {
    let id = id.id();
    quote! { #name = #id }
  });

  let e = quote! {
    pub enum TokenKind {
      #(#variants),*
    }
  };

  (e, tokens)
}