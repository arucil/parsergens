use std::fmt::{self, Write};

pub struct IndentWriter<W> {
  inner: W,
  bol: bool,
  indent: usize,
}


impl<W: Write> IndentWriter<W> {
  pub fn new(inner: W) -> Self {
    Self {
      inner,
      bol: true,
      indent: 0,
    }
  }

  fn write_indent(&mut self) -> fmt::Result {
    write!(&mut self.inner, "{:1$}", "", self.indent * 2)
  }

  pub fn indent(&mut self) {
    self.indent += 1;
  }

  pub fn dedent(&mut self) {
    assert!(self.indent > 0);
    self.indent -= 1;
  }

  pub fn into_inner(self) -> W {
    self.inner
  }
}

impl<W: Write> Write for IndentWriter<W> {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    let mut first_line = true;
    for line in s.split('\n') {
      if !first_line {
        self.write_char('\n')?;
        self.bol = true;
      }

      if self.bol && !line.is_empty() {
        self.write_indent()?;
        self.bol = false;
      }

      self.write_str(line)?;

      first_line = false;
    }
    Ok(())
  }
}