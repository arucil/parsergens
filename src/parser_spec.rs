use syn::*;
use syn::parse::*;
use proc_macro_error::abort;

pub(crate) struct ParserSpec {
  pub mod_name: Ident,
  pub kind: ParserKind, 
  pub file_path: LitStr,
}

pub(crate) enum ParserKind {
  Slr,
  Lr,
  Lalr,
}

impl Parse for ParserSpec {
  fn parse(input: ParseStream) -> Result<Self> {
    input.parse::<Token![mod]>()?;
    let mod_name = input.parse::<Ident>()?;
    input.parse::<Token![:]>()?;
    let kind = input.parse::<Ident>()?;
    let content;
    parenthesized!(content in input);
    let file_path = content.parse::<LitStr>()?;

    let kind = match kind.to_string().to_ascii_lowercase().as_str() {
      "slr" => ParserKind::Slr,
      "lr" => ParserKind::Lr,
      "lalr" => ParserKind::Lalr,
      _ => abort!(kind, "unsupported parser type"),
    };

    Ok(Self {
      mod_name,
      kind,
      file_path,
    })
  }
}