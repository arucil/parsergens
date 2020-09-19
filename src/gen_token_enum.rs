use grammar::{BiMap, Map, TokenId};
use heck::CamelCase;
use itertools::Itertools;

pub fn gen(
  tokens: &BiMap<TokenId, String>
) -> (String, Map<TokenId, String>) {
  let variants = tokens.iter().map(|(id, name)| {
    let name = name.replace(&['\'', '-'][..], "_").to_camel_case();
    (*id, name)
  });
  let tokens = variants.collect::<Map<_, _>>();
  let variants = tokens.iter().map(|(id, name)| {
    format!("{name} = {id},", name = name, id = id.id())
  }).join("\n    ");

  let e = format!(
    r##"
  #[repr(u32)]
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum TokenKind {{
    {variants}
  }}
    "##,
    variants = variants,
  );

  (e, tokens)
}