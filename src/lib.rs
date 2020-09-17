#![feature(proc_macro_hygiene)]

use proc_macro::TokenStream;
use syn::*;
use syn::parse::*;
use quote::quote;
use quote::format_ident;

#[proc_macro]
pub fn parsergen(expr: TokenStream) -> TokenStream {
  todo!()
}