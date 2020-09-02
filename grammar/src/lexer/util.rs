
pub fn find_char_interval(char: u32, char_intervals: &[u32]) -> u32 {
  let mut lo = 0;
  let mut hi = char_intervals.len();

  while lo < hi {
    let mid = (lo + hi) / 2;
    if char_intervals[mid] > char {
      hi = mid;
    } else {
      lo = mid + 1;
    }
  }

  lo as u32 - 1
}
