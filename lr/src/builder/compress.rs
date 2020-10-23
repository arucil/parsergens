use grammar::{LoweredGrammar, HashMap};
use std::hash::Hash;
use num_traits::int::PrimInt;

#[derive(Debug, Default)]
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
  let mut tables = CompressedTables::default();

  compress_action_table(
    grammar,
    &mut tables.parse_table,
    &mut tables.check_table,
    &mut tables.action_disp,
    &mut tables.action_default,
    action,
  );

  tables
}

fn compress_action_table(
  grammar: &LoweredGrammar,
  parse_table: &mut Vec<i32>,
  check_table: &mut Vec<u32>,
  action_disp: &mut Vec<isize>,
  action_default: &mut Vec<i32>,
  action: Vec<Vec<i32>>,
) {
}

fn make_row<T>(mut values: Vec<T>) -> Row<T>
where
  T: Eq + Hash + PrimInt
{
  let mut count = HashMap::<T, usize>::default();
  let mut num_zeros = 0;

  for &v in &values {
    if v == T::zero() {
      num_zeros += 1;
    } else {
      *count.entry(v).or_default() += 1;
    }
  }

  let (default, num_default) = count.into_iter().max_by_key(|x| x.1).unwrap();

  // clear default values
  for v in &mut values {
    if *v == default {
      *v = T::zero();
    }
  }

  Row {
    default,
    num_default: num_default + num_zeros,
    values,
  }
}

struct Row<T> {
  default: T,
  num_default: usize,
  values: Vec<T>,
}