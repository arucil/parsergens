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