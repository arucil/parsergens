use grammar::LoweredGrammar;

#[derive(Debug)]
pub struct CompressedTables {
  /// ACTION table + GOTO table
  parse_table: Vec<i32>,

  /// displacement values for rows of ACTION table.
  action_disp: Vec<isize>,

  /// displacement values for rows of GOTO table.
  goto_disp: Vec<isize>,

  /// default values for rows of ACTION table.
  action_default: Vec<i32>,

  /// default values for rows of GOTO table.
  goto_default: Vec<i32>,

  check_table: Vec<u32>,
}

pub fn compress_tables(
  grammar: &LoweredGrammar,
  action: Vec<Vec<i32>>,
  goto: Vec<Vec<u32>>,
) -> CompressedTables {
}