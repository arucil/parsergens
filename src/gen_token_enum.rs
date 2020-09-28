use grammar::{Map, TokenId};
use heck::CamelCase;
use std::fmt::{self, Write};
use super::IndentWriter;

pub fn gen(
  tokens: &Map<TokenId, String>,
  w: &mut IndentWriter<impl Write>,
) -> Result<Map<TokenId, String>, fmt::Error> {
  writeln!(w, "{}", r##"
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {{
  "##.trim_end())?;

  let mut token_names = Map::new();
  for (id, name) in tokens {
    let name = name.replace(&['\'', '-'][..], "_").to_camel_case();
    writeln!(w, "  r#{} = {},", name, id.id())?;
    token_names.insert(*id, name);
  }

  writeln!(w, "}}")?;

  Ok(token_names)
}