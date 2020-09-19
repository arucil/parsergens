use std::path::Path;
use std::io;
use std::fs;
use once_cell::sync::Lazy;
use regex::{Regex, Captures};

static PARAM_REGEX: Lazy<Regex> = Lazy::new(||
  Regex::new(r#"(?x) (?:  \#\[ __ \( ([\w_]+) \) \]  |  __ \(  ([\w_]+)  \) )"#).unwrap());

pub fn process<P, F, S>(
  file: P,
  mut provider: F
) -> io::Result<String>
  where
    P: AsRef<Path>,
    F: FnMut(&str) -> S,
    S: ToString,
{
  let content = fs::read_to_string(file)?;

  Ok(PARAM_REGEX.replace_all(&content, |captures: &Captures| {
    let name = captures.get(1)
      .unwrap_or_else(||
        captures.get(2).unwrap())
      .as_str();
    provider(name).to_string()
  }).into_owned())
}