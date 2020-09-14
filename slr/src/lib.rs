mod build;

#[cfg(not(debug_assertions))]
type Map<K, V> = std::collections::HashMap<K, V>;

#[cfg(debug_assertions)]
type Map<K, V> = indexmap::IndexMap<K, V>;

#[cfg(not(debug_assertions))]
type Set<K> = std::collections::HashSet<K>;

#[cfg(debug_assertions)]
type Set<K> = indexmap::IndexSet<K>;

#[cfg(not(debug_assertions))]
type BiMap<K, V> = bimap::BiHashMap<K, V>;

#[cfg(debug_assertions)]
type BiMap<K, V> = bimap::BiBTreeMap<K, V>;

#[derive(Debug)]
struct SlrParser {
  /// positive: shift (n - 1)  
  /// zero: error
  /// negative: reduce (-n - 1)
  /// MIN: accept
  action: Vec<Vec<i32>>,
  /// positive: goto (n - 1)
  /// zero: error
  goto: Vec<Vec<u32>>,
  /// (length of RHS of the production, non-terminal id)
  prods: Vec<(usize, u32)>,
  nt_names: Vec<String>,
  /// (non-terminal name, starting state)
  start: Map<String, u32>,
  eof_index: usize,
  lexer: grammar::Lexer,
}