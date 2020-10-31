use grammar::{LoweredGrammar, HashMap};
use std::hash::Hash;
use num_traits::int::PrimInt;

#[derive(Debug, Default)]
pub struct CompressedTables {
  /// ACTION table + GOTO table
  pub parse_table: Vec<i32>,

  /// displacement values for rows of ACTION table.
  pub action_disp: Vec<isize>,

  /// displacement values for rows of GOTO table.
  pub goto_disp: Vec<isize>,

  /// default values for rows of ACTION table.
  pub action_default: Vec<i32>,

  /// default values for rows of GOTO table.
  pub goto_default: Vec<i32>,

  /// -1 in `check_table` represents an empty entry.
  pub check_table: Vec<i32>,
}

#[derive(Debug, Default)]
struct CompressState {
  parse_table: Vec<i32>,
  check_table: Vec<i32>,
  first_empty: usize,
}

pub fn compress_tables(
  grammar: &LoweredGrammar,
  action: Vec<Vec<i32>>,
  goto: Vec<Vec<u32>>,
) -> CompressedTables {
  let mut state = CompressState::default();
  let num_tokens = grammar.tokens.len() + 1; // includes EOF

  let (action_disp, action_default) = compress_action_table(
    &mut state,
    action,
  );

  let (goto_disp, goto_default) = compress_goto_table(
    &mut state,
    goto,
    num_tokens,
  );

  while state.check_table.last() == Some(&-1) {
    state.check_table.pop();
    state.parse_table.pop();
  }

  for x in &mut state.parse_table {
    if *x == super::tables::ERROR_ACTION {
      *x = 0;
    }
  }

  CompressedTables {
    parse_table: state.parse_table,
    check_table: state.check_table,
    action_disp,
    action_default,
    goto_disp,
    goto_default,
  }
}

/// Returns displacement table and default value table.
fn compress_action_table(
  state: &mut CompressState,
  action: Vec<Vec<i32>>,
) -> (Vec<isize>, Vec<i32>) {
  let mut action = action.into_iter()
    .enumerate()
    .map(|(i, row)| make_row(i, row, |x| x > 0 || x == super::tables::ERROR_ACTION))
    .collect::<Vec<_>>();

  let default_table = action.iter().map(|row| row.default).collect::<Vec<_>>();

  action.sort_by_key(|row| row.num_default);
  let action = action;

  let mut disp_map = HashMap::<&[i32], isize>::default();
  let mut disp_table = vec![0; action.len()];

  for row in &action {
    if let Some(&disp) = disp_map.get(&row.values[..]) {
      // merge rows with exactly the same actions.
      disp_table[row.index] = disp;
    } else {
      // check_base is token id
      let disp = write_row(state, row, 0, false, true);
      disp_map.insert(&row.values, disp);
      disp_table[row.index] = disp;
    }
  }

  (disp_table, default_table)
}

/// Returns displacement table and default value table.
fn compress_goto_table(
  state: &mut CompressState,
  goto: Vec<Vec<u32>>,
  num_tokens: usize,
) -> (Vec<isize>, Vec<i32>) {
  let mut goto = goto.into_iter()
    .enumerate()
    .map(|(i, row)| make_row(i, row, |_| false))
    .collect::<Vec<_>>();

  let default_table = goto.iter().map(|row| row.default as i32).collect::<Vec<_>>();

  goto.sort_by_key(|row| row.num_default);
  let goto = goto;

  let mut disp_table = vec![0; goto.len()];

  for row in &goto {
    // check_base is token NT + num_tokens
    let disp = write_row(state, row, num_tokens, true, false);
    disp_table[row.index] = disp;
  }

  (disp_table, default_table)
}

/// Returns the displacement.
fn write_row<T: PrimInt>(
  state: &mut CompressState,
  row: &Row<T>,
  check_base: usize,
  check_row: bool,
  merge_rows: bool,
) -> isize {
  let mut disp = state.first_empty as isize;

  for &v in &row.values {
    if v == T::zero() {
      disp -= 1;
    } else {
      break;
    }
  }

  if state.first_empty as isize - disp == row.values.len() as isize {
    return -(row.values.len() as isize);
  }

  if merge_rows {
    'find_disp: loop {
      let mut i = disp;
      for (j, &v) in row.values.iter().enumerate() {
        if i >= 0 && (i as usize) < state.check_table.len() &&
          state.check_table[i as usize] >= 0
        {
          let i = i as usize;
          let next = if v == T::zero() {
            // two rows that have unoverlapped non-zero values are not allowed
            // to be merged.
            state.check_table[i] == j as i32
          } else {
            state.check_table[i] != j as i32 || state.parse_table[i] != v.to_i32().unwrap()
          };
          if next {
            disp += 1;
            continue 'find_disp;
          }
        }
        i += 1;
      }
      break;
    }
  } else {
    'find_disp2: loop {
      let mut i = disp;
      for &v in &row.values {
        if !(v == T::zero() || i < 0 ||
          (i as usize) >= state.check_table.len() ||
          state.check_table[i as usize] < 0)
        {
          disp += 1;
          continue 'find_disp2;
        }
        i += 1;
      }
      break;
    }
  }

  let row_end = (disp + row.values.len() as isize) as usize;
  if row_end > state.check_table.len() {
    state.check_table.resize(row_end, -1);
    state.parse_table.resize(row_end, 0);
  }

  for (i, &v) in row.values.iter().enumerate() {
    if v != T::zero() {
      let j = (disp + i as isize) as usize;
      assert!(state.check_table[j] == -1);
      state.parse_table[j] = v.to_i32().unwrap();
      state.check_table[j] = if check_row {
        (check_base + row.index) as i32
      } else {
        (check_base + i) as i32
      };
    }
  }

  while state.first_empty < state.check_table.len() &&
    state.check_table[state.first_empty] == -1
  {
    state.first_empty += 1;
  }

  disp
}

fn make_row<T, F>(
  index: usize,
  mut values: Vec<T>,
  ignore_value: F,
) -> Row<T>
where
  T: Hash + PrimInt,
  F: Fn(T) -> bool,
{
  let mut count = HashMap::<T, usize>::default();
  let mut num_zeros = 0;

  for &v in &values {
    if v == T::zero() {
      num_zeros += 1;
    } else if !ignore_value(v) {
      *count.entry(v).or_default() += 1;
    }
  }

  if let Some((default, num_default)) = count.into_iter().max_by_key(|x| x.1) {
    // clear default values
    for v in &mut values {
      if *v == default {
        *v = T::zero();
      }
    }

    Row {
      index,
      default,
      num_default: num_default + num_zeros,
      values,
    }
  } else {
    Row {
      index,
      default: T::zero(),
      num_default: num_zeros,
      values,
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
struct Row<T> {
  index: usize,
  default: T,
  num_default: usize,
  values: Vec<T>,
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;
  use super::*;

  #[test]
  fn make_row_all_zeros() {
    let vec = vec![0; 13];
    let row = make_row(1, vec.clone(), |_| false);
    assert_eq!(row, Row {
      index: 1,
      default: 0,
      num_default: 13,
      values: vec,
    });
  }

  #[test]
  fn make_row_most_frequent() {
    let vec1 = vec![0, 0, 7, 2, 0, 13, 7, 2, 0, 7, 11];
    let vec2 = vec![0, 0, 0, 2, 0, 13, 0, 2, 0, 0, 11];
    let row = make_row(1, vec1, |_| false);
    assert_eq!(row, Row {
      index: 1,
      default: 7,
      num_default: 7,
      values: vec2,
    });
  }

  #[test]
  fn write_rows() {
    let mut state = CompressState::default();
    let row1 = make_row(1, vec![0, 0, 0, 2, 3, 2, 0], |_| false);

    assert_eq!(
      -4,
      write_row(&mut state, &row1, 10, false, true)
    );

    assert_eq!(
      vec![3, 0, 0],
      state.parse_table,
    );

    assert_eq!(
      vec![14, -1, -1],
      state.check_table,
    );

    let row2 = make_row(1, vec![5, 8, 5, 0, -3, 0, 2, -1], |_| false);

    assert_eq!(
      0,
      write_row(&mut state, &row2, 10, false, true)
    );

    assert_eq!(
      vec![3, 8, 0, 0, -3, 0, 2, -1],
      state.parse_table,
    );

    assert_eq!(
      vec![14, 11, -1, -1, 14, -1, 16, 17],
      state.check_table,
    );

    let row3 = make_row(1, vec![0; 8], |_| false);

    assert_eq!(
      -8,
      write_row(&mut state, &row3, 10, false, true)
    );

    assert_eq!(
      vec![3, 8, 0, 0, -3, 0, 2, -1],
      state.parse_table,
    );

    assert_eq!(
      vec![14, 11, -1, -1, 14, -1, 16, 17],
      state.check_table,
    );

    let row4 = make_row(1, vec![3, 1, 1, 6, 7, 1, 1, 55], |_| false);

    assert_eq!(
      5,
      write_row(&mut state, &row4, 10, false, true)
    );

    assert_eq!(
      vec![3, 8, 0, 0, -3, 3, 2, -1, 6, 7, 0, 0, 55],
      state.parse_table,
    );

    assert_eq!(
      vec![14, 11, -1, -1, 14, 10, 16, 17, 13, 14, -1, -1, 17],
      state.check_table,
    );

    let row5 = make_row(1, vec![0, 0, 12, 6, 5, 5, 0, 5], |_| false);

    assert_eq!(
      0,
      write_row(&mut state, &row5, 10, false, true)
    );

    assert_eq!(
      vec![3, 8, 12, 6, -3, 3, 2, -1, 6, 7, 0, 0, 55],
      state.parse_table,
    );

    assert_eq!(
      vec![14, 11, 12, 13, 14, 10, 16, 17, 13, 14, -1, -1, 17],
      state.check_table,
    );

    let row6 = make_row(1, vec![1, 2, 3, 0, 4, 4, 0, 0], |_| false);

    assert_eq!(
      13,
      write_row(&mut state, &row6, 10, false, true)
    );

    assert_eq!(
      vec![3, 8, 12, 6, -3, 3, 2, -1, 6, 7, 0, 0, 55, 1, 2, 3, 0, 0, 0, 0, 0],
      state.parse_table,
    );

    assert_eq!(
      vec![14, 11, 12, 13, 14, 10, 16, 17, 13, 14, -1, -1, 17, 10, 11, 12, -1, -1, -1, -1, -1],
      state.check_table,
    );
  }

  #[test]
  fn action_table() {
    let mut state = CompressState::default();

    let (disp, default) = compress_action_table(
      &mut state,
      vec![
        vec![0, 1, -4, -4, 2],
        vec![3, 4, 0, 5, 0],
        vec![6, 0, 7, 0, -12],
        vec![0, 0, 0, 8, 0],
        vec![9, -3, 0, 0, 10],
      ],
    );

    assert_eq!(
      state.parse_table,
      vec![3, 4, 1, 5, 6, 2, 7, 9, 8, 0, 0, 10],
    );

    assert_eq!(
      state.check_table,
      vec![0, 1, 1, 3, 0, 4, 2, 0, 3, -1, -1, 4],
    );

    assert_eq!(
      disp,
      vec![1, 0, 4, 5, 7],
    );

    assert_eq!(
      default,
      vec![-4, 0, -12, 0, -3],
    );
  }

  #[test]
  fn goto_table() {
    let mut state = CompressState::default();

    let (disp, default) = compress_goto_table(
      &mut state,
      vec![
        vec![0, 1, 4, 4, 2],
        vec![3, 4, 9, 5, 9],
        vec![6, 12, 7, 0, 12],
        vec![0, 1, 1, 8, 0],
        vec![9, 3, 0, 3, 10],
      ],
      100
    );

    assert_eq!(
      state.parse_table,
      vec![3, 4, 1, 5, 6, 2, 7, 9, 8, 0, 0, 10],
    );

    assert_eq!(
      state.check_table,
      vec![101, 101, 100, 101, 102, 100, 102, 104, 103, -1, -1, 104],
    );

    assert_eq!(
      disp,
      vec![1, 0, 4, 5, 7],
    );

    assert_eq!(
      default,
      vec![4, 9, 12, 1, 3],
    );
  }

  #[test]
  fn action_table_duplicate_rows() {
    let mut state = CompressState::default();

    let (disp, default) = compress_action_table(
      &mut state,
      vec![
        vec![0, 1, -4, -4, 2],
        vec![3, 4, 0, 5, 0],
        vec![6, 0, 7, 0, -12],
        vec![0, 0, 0, 8, 0],
        vec![9, -3, 0, 0, 10],
        vec![9, 0, 0, -1, 10],
        vec![9, 0, 0, 0, 0],
      ],
    );

    assert_eq!(
      state.parse_table,
      vec![3, 4, 1, 5, 6, 2, 7, 9, 8, 9, 0, 10, 0, 0],
    );

    assert_eq!(
      state.check_table,
      vec![0, 1, 1, 3, 0, 4, 2, 0, 3, 0, -1, 4, -1, -1],
    );

    assert_eq!(
      disp,
      vec![1, 0, 4, 5, 7, 7, 9],
    );

    assert_eq!(
      default,
      vec![-4, 0, -12, 0, -3, -1, 0],
    );
  }
}