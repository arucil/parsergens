
fn ocaml() {
  let input = include_str!("fixtures/ocaml.pg");
  let _parser = lr::build(input, lr::ParserKind::Lalr).unwrap();
}

use criterion::{criterion_group, criterion_main, Criterion};

fn ocaml_benchmark(c: &mut Criterion) {
  c.bench_function("ocaml", |b| b.iter(|| ocaml()));
}

criterion_group!{
  name = benches;
  config = Criterion::default().significance_level(0.1).sample_size(10);
  targets = ocaml_benchmark
}
criterion_main!(benches);