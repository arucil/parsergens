#[cfg(not(debug_assertions))]
type Map<K, V> = std::collections::HashMap<K, V>;

#[cfg(debug_assertions)]
type Map<K, V> = indexmap::IndexMap<K, V>;

mod build;