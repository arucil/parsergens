use grammar::{Map, TokenId};
use heck::CamelCase;
use codegen::{Scope, Enum};

pub fn gen(
  tokens: &Map<TokenId, String>,
  scope: &mut Scope,
) -> Map<TokenId, String> {
  let mut en = Enum::new("TokenKind");
  en.vis("pub")
    .repr("u32")
    .derive("Debug")
    .derive("Clone")
    .derive("Copy")
    .derive("PartialEq")
    .derive("Eq");

  let mut token_names = Map::new();
  for (id, name) in tokens {
    let name = name.replace(&['\'', '-'][..], "_").to_camel_case();
    en.new_variant(&format!("r#{} = {}", name, id.id()));
    token_names.insert(*id, name);
  }

  token_names
}