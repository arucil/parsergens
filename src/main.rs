use std::env;
use std::process;
use getopts::Options;

fn main() {
  let args = env::args().collect::<Vec<_>>();
  let prog = args[0].clone();
  let mut opts = Options::new();
  opts.optopt("t", "type",
    "Type of parser generation algorithm. Defaults to LALR.\n\
      Supported types: LALR, CLR (case insensitive)",
    "TYPE");
  opts.optflag("h", "help", "Print this message");

  let matches = match opts.parse(&args[1..]) {
    Ok(m) => m,
    Err(err) => {
      eprintln!("{}", err);
      process::exit(1);
    }
  };

  if matches.opt_present("h") {
    print_usage(prog, opts);
    return;
  }

  let ty = matches.opt_str("t");
  let path = if matches.free.len() == 1 {
    matches.free[0].clone()
  } else {
    print_usage(prog, opts);
    process::exit(1);
  };

}

fn print_usage(prog: String, opts: Options) {
  let brief = format!("Usage: {} [options] PATH", prog);
  print!("{}", opts.usage(&brief));
}