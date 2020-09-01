use super::ast::Regex;

#[derive(Debug)]
pub struct RegexError {
  pub kind: RegexErrorKind,
  pub span: (usize, usize),
}

#[derive(Debug)]
pub enum RegexErrorKind {
  SyntaxError,
}

pub fn parse_regex(
  input: &str,
  start: usize,
  end: usize
) -> Result<Regex, RegexError> {
  todo!()
}