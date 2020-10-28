use ::codegen::Scope;
use std::fmt;
use crate::Parser;

mod gen_token_enum;
mod gen_lexer;
mod gen_parser;

pub fn gen_decls(
  parser: &Parser,
  scope: &mut Scope
) {
  scope.new_attr("allow").arg_delimited(
    "dead_code, non_camel_case_types, unused_parens, unused_mut");
  scope.new_attr("allow").arg_delimited(
    "unused_variables, unused_braces, non_snake_case");

  for code in &parser.user_code {
    scope.raw(code);
  }

  let tokens = gen_token_enum::gen(&parser.tokens, scope);
  gen_lexer::gen(&parser.lexer, &tokens, scope);
  gen_parser::gen(&parser, scope);
}

fn gen_1d_table(
  table_name: &str,
  cell_type: &str,
  table: &[impl fmt::Debug],
  scope: &mut Scope,
) {
  let ty = format!("[{}; {}]", cell_type, table.len());

  let value = if table.is_empty() {
    "[]".to_owned()
  } else {
    let table = table.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>();
    let max_len = table.iter().map(|x| x.len()).max().unwrap();
    let num_col = 80 / (max_len + 2);

    let mut value = "[".to_owned();
    let mut i = 0;
    for x in table {
      if i % num_col == 0 {
        value.push_str("\n  ");
      }

      value.push_str(&format!("{:>1$}", x, max_len));
      value.push_str(", ");

      i += 1;
    }

    value.push_str("\n]");

    value
  };

  scope.new_static(table_name, ty).value(value);
}