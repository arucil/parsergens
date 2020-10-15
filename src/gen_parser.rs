use lr::{Parser, Symbol, Nonterminal, NonterminalKind, ProductionKind, Production};
use heck::SnakeCase;
use itertools::Itertools;
use nanoid::nanoid;
use codegen::{Scope, Variant, EnumVariant, Function, Impl};

pub fn gen(
  parser: &Parser,
  scope: &mut Scope,
) {

  super::gen_2d_table("ACTION", "i32", &parser.action, scope);
  super::gen_2d_table("GOTO", "u32", &parser.goto, scope);
  super::gen_1d_table("PRODUCTIONS", "(usize, u32)",
    &parser.prods.iter().map(|prod| (prod.rhs_len, prod.nt)).collect::<Vec<_>>(),
    scope);

  let nt_names = parser.nts.iter()
    .map(|nt| {
      if nt.kind == NonterminalKind::User {
        format!("r#{}", nt.name.replace(&['\'', '-'][..], "__"))
      } else {
        format!("_{}", nanoid!(10).replace(&['~', '-'][..], "__"))
      }
    })
    .collect::<Vec<_>>();

  let nt_types = parser.nts.iter()
    .map(|nt| compute_nt_type(nt, parser))
    .collect::<Vec<_>>();

  gen_nt_enum(&nt_names, &nt_types, scope);
  gen_nt_enum_impl(&nt_names, &nt_types, scope);

  gen_parser_struct(scope);
  gen_input_func(scope);
  gen_parse_error_struct(scope);

  let user_state_tuple = format!("({})",
    parser.user_state.iter().map(|x| &x.state).join(", "));

  gen_reduce_func(&parser.prods, &nt_names, &nt_types, &user_state_tuple, scope);

  gen_parser_impl(&parser, &nt_names, &nt_types, &user_state_tuple, scope);
}

fn gen_nt_enum(
  nt_names: &[String],
  nt_types: &[String],
  scope: &mut Scope,
) {
  let enu = scope.new_enum("NtType")
    .generic("'input")
    .push_variant({
      let mut v = Variant::new("_Token");
      v.tuple("Token<'input>");
      v
    });

  for (name, ty) in nt_names.iter().zip(nt_types) {
    enu.push_variant({
      let mut v = Variant::new(name);
      v.tuple(ty);
      v
    });
  }
}

fn gen_nt_enum_impl(
  nt_names: &[String],
  nt_types: &[String],
  scope: &mut Scope,
) {
  let imp = scope.new_impl("NtType")
    .generic("'input")
    .target_generic("'input");

  imp.new_fn("assert_token")
    .arg_self()
    .ret("Token<'input>")
    .line(r"
match self {
  Self::_Token(tok) => tok,
  _ => unreachable!(),
}
");

  for (name, ty) in nt_names.iter().zip(nt_types) {
    let fn_nt_name = if name.starts_with("r#") {
      &name[2..]
    } else {
      name
    };

    imp.new_fn(format!("assert_{}", fn_nt_name))
      .ret(ty)
      .arg_self()
      .line(format!(r"
match self {{
  Self::{}(v) => v,
  _ => unreachable!(),
}}
", name));
  }
}

fn gen_parser_struct(
  scope: &mut Scope,
) {
  scope.new_struct("Parser")
    .vis("pub")
    .generic("'input")
    .generic("I")
    .field("tokens", "I")
    .field("token", "Option<Token<'input>>")
    .field("token_kind", "usize");
}

fn gen_input_func(
  scope: &mut Scope,
) {
  scope.new_fn("with_input")
    .vis("pub")
    .generic("'input")
    .arg("input", "&'input str")
    .ret("Parser<'input, Tokens<'input>>")
    .line("Parser::new(lex(input))");

  scope.new_fn("with_tokens")
    .vis("pub")
    .generic("'input")
    .generic("I")
    .arg("tokens", "I")
    .ret("Parser<'input, I::IntoIter>")
    .bound("I", "IntoIterator<Item=::std::result::Result<Token<'input>, Error>>")
    .line("Parser::new(tokens.into_iter())");
}

fn gen_parse_error_struct(
  scope: &mut Scope,
) {
  scope.new_enum("ParseError")
    .vis("pub")
    .derive("Debug")
    .derive("Clone")
    .derive("PartialEq")
    .derive("Eq")
    .generic("'input")
    .push_variant({
      let mut v = Variant::new("InvalidChar");
      v.named("char", "char")
        .named("start", "usize")
        .named("end", "usize");
      v
    })
    .push_variant({
      let mut v = Variant::new("InvalidToken");
      v.tuple("Token<'input>");
      v
    })
    .push_variant(Variant::new("UnexpectedEof"));
}

fn gen_reduce_func(
  prods: &[Production],
  nt_names: &[String],
  nt_types: &[String],
  user_state_tuple: &str,
  scope: &mut Scope,
) {
  let func = scope.new_fn("reduce")
    .generic("'input")
    .arg("nt", "usize")
    .arg("rhs", "Vec<NtType<'input>>")
    .arg("user_state", format!("&mut {}", user_state_tuple))
    .ret("NtType<'input>")
    .line("match nt {");

  gen_prod_actions(prods, nt_names, nt_types, user_state_tuple, func);

  func.line("  _ => unreachable!(),")
    .line("}");
}

fn compute_nt_type(
  nt: &Nonterminal,
  parser: &Parser,
) -> String {
  if let Some(ty) = &nt.ty {
    ty.clone()
  } else {
    match nt.kind {
      NonterminalKind::User => format!("()"),
      NonterminalKind::Repetition | NonterminalKind::Optional => {
        let skip = if nt.kind == NonterminalKind::Repetition {
          1
        } else {
          0
        };
        let types = parser.prods[nt.range.start + 1].symbols.iter().skip(skip)
          .map(|sym| {
            match sym {
              Symbol::Token(_) => format!("Token<'input>"),
              Symbol::Nonterminal(nt) => {
                compute_nt_type(&parser.nts[*nt as usize], parser)
              }
            }
          })
          .join(", ");

        if nt.kind == NonterminalKind::Repetition {
          format!("Vec<({})>", types)
        } else {
          format!("Option<({})>", types)
        }
      }
    }
  }
}

fn gen_prod_actions(
  prods: &[Production],
  nt_names: &[String],
  nt_types: &[String],
  user_state_tuple: &str,
  func: &mut Function,
) {
  for (i, prod) in prods.iter().enumerate() {
    let nt = prod.nt as usize;
    if let Some(action) = &prod.action {
      gen_user_prod_action(i, prod, nt_names, nt_types, action, user_state_tuple, func);
    } else {
      match prod.kind {
        ProductionKind::Ordinary => {
          func.line(format!("  {} => NtType::{}(()),", i, nt_names[nt]));
        }
        ProductionKind::RepetitionEpsilon => {
          func.line(format!("  {} => NtType::{}(vec![]),", i, nt_names[nt]));
        }
        ProductionKind::RepetitionFirst => {
          func.line(format!("  {} => {{", i));
          func.line("    let mut rhs = rhs.into_iter();");
          func.line(format!("    NtType::{}(vec![(", nt_names[nt]));
          gen_prod_action_init_args(&prod.symbols, &nt_names, func);
          func.line("    )])");
          func.line("  }");
        }
        ProductionKind::RepetitionRest => {
          func.line(format!("  {} => {{", i));
          func.line("    let mut rhs = rhs.into_iter();");
          func.line(format!("    let mut result = rhs.next().unwrap().assert_{}();",
            nt_names[nt]));
          func.line("    result.push((");
          gen_prod_action_init_args(&prod.symbols[1..], &nt_names, func);
          func.line("    ));");
          func.line(format!("    NtType::{}(result)", nt_names[nt]));
          func.line("  }");
        }
      }
    }
  }
}

fn gen_user_prod_action(
  index: usize,
  prod: &Production,
  nt_names: &[String],
  nt_types: &[String],
  action: &str,
  user_state_tuple: &str,
  func: &mut Function,
) {
  func.line(format!("  {} => {{", index));
  func.line("    let mut rhs = rhs.into_iter();");
  func.line("    fn semantic_action<'input>(");
  func.line(format!("      state: &mut {},", user_state_tuple));

  for (i, sym) in prod.symbols.iter().enumerate() {
    match sym {
      Symbol::Token(_) => {
        func.line(format!("      mut __{}: Token<'input>,", i));
      }
      Symbol::Nonterminal(nt) => {
        func.line(format!("      mut __{}: {},", i, nt_types[*nt as usize]));
      }
    }
  }

  func.line(format!("    ) -> {} {{\n{}\n}}", nt_types[prod.nt as usize], action));

  func.line(format!("    NtType::{}(semantic_action(user_state,",
    nt_names[prod.nt as usize]));

  gen_prod_action_init_args(&prod.symbols, &nt_names, func);
  func.line("    ))");
  func.line("  }");
}

fn gen_prod_action_init_args(
  symbols: &[Symbol],
  nt_names: &[String],
  func: &mut Function,
) {
  for sym in symbols {
    match sym {
      Symbol::Token(_) => {
        func.line("      rhs.next().unwrap().assert_token(),");
      }
      Symbol::Nonterminal(nt) => {
        let nt_name = &nt_names[*nt as usize];
        let nt_name = if nt_name.starts_with("r#") {
          &nt_name[2..]
        } else {
          nt_name
        };
        func.line(format!("      rhs.next().unwrap().assert_{}(),", nt_name));
      }
    }
  }
}

fn gen_parser_impl(
  parser: &Parser,
  nt_names: &[String],
  nt_types: &[String],
  user_state_tuple: &str,
  scope: &mut Scope,
) {
  let imp = scope.new_impl("Parser")
    .generic("'input")
    .generic("I")
    .target_generic("'input")
    .target_generic("I")
    .bound("I", "Iterator<Item=::std::result::Result<Token<'input>, Error>>");

  imp.new_fn("new")
    .arg("tokens", "I")
    .ret("Self")
    .line(r"
Self {
  tokens,
  token: None,
  token_kind: 0,
}
");

  let user_state_names = (0..parser.user_state.len()).map(|i| {
    format!("us_{}", i)
  }).collect::<Vec<_>>();

  gen_start_fn(
    parser,
    &nt_names,
    &nt_types,
    &user_state_names,
    imp);

  gen_get_token_fn(parser.eof_index, imp);

  gen_parse_fn(user_state_tuple, imp);
}

fn gen_start_fn(
  parser: &Parser,
  nt_names: &[String],
  nt_types: &[String],
  user_state_names: &[String],
  imp: &mut Impl,
) {
  for (name, ep) in &parser.entry_points {
    let name = name.replace(&['\'', '-'][..], "__");
    let fn_name = format!("parse_{}", name.to_snake_case());
    let ty = &nt_types[ep.real_start_nt as usize];

    let nt_name = format!("{}", nt_names[ep.real_start_nt as usize]);

    let func = imp.new_fn(fn_name)
      .vis("pub")
      .arg_mut_self();

    for (name, ty) in user_state_names.iter().zip(&parser.user_state) {
      func.arg(name, &ty.state);
    }

    func.ret(format!("::std::result::Result<{}, ParseError<'input>>", ty));

    let nt_name = if nt_name.starts_with("r#") {
      &nt_name[2..]
    } else {
      &nt_name
    };

    func.line(format!(
      "Ok(self.parse({state}, {accept_prod}, ({user_state_names}))?.assert_{nt_name}())",
      state = ep.start_state,
      accept_prod = ep.accept_prod,
      nt_name = nt_name,
      user_state_names = user_state_names.join(", ")));
  }
}

fn gen_get_token_fn(
  eof: usize,
  imp: &mut Impl,
) {
  imp.new_fn("get_token")
    .arg_mut_self()
    .ret("::std::result::Result<(), ParseError<'input>>")
    .line(r"
let token = self.tokens.next()
  .transpose()
  .map_err(|err| ParseError::InvalidChar {
    char: err.char,
    start: err.start,
    end: err.end,
  })?;")
    .line(format!(r"
if let Some(token) = &token {{
  self.token_kind = token.kind as usize;
}} else {{
  self.token_kind = {};
}}", eof))
    .line(r"self.token = token;")
    .line("Ok(())");
}

fn gen_parse_fn(
  user_state_tuple: &str,
  imp: &mut Impl,
) {
  imp.new_fn("parse")
    .arg_mut_self()
    .arg_mut("state", "usize")
    .arg_mut("accept_prod", "usize")
    .arg_mut("user_state", user_state_tuple)
    .ret("::std::result::Result<NtType<'input>, ParseError<'input>>")
    .line("let mut stack = ::std::vec::Vec::<(usize, NtType)>::new();")
    .line("self.get_token()?;")
    .line(r#"
loop {
  let action = ACTION[state][self.token_kind];

  if action > 0 {
    stack.push((state, NtType::_Token(self.token.take().unwrap())));
    state = (action - 1) as usize;
    self.get_token()?;
  } else if action < 0 {
    let prod = (!action) as usize;
    if prod == accept_prod {
      return Ok(stack.pop().unwrap().1);
    }

    let (rhs_len, nt) = PRODUCTIONS[prod];
    let state0 = if rhs_len == 0 {
      state
    } else {
      stack[stack.len() - rhs_len].0
    };

    state = GOTO[state0 as usize][nt as usize] as usize - 1;

    let rhs = stack.drain(stack.len() - rhs_len..)
      .map(|(_, x)| x)
      .collect::<Vec<_>>();
    let ty = reduce(prod, rhs, &mut user_state);

    stack.push((state0, ty));
  } else {
    if let Some(token) = self.token.take() {
      return Err(ParseError::InvalidToken(token));
    } else {
      return Err(ParseError::UnexpectedEof);
    }
  }
}
"#);
}