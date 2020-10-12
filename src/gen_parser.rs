use lr::{Parser, Symbol, Nonterminal, NonterminalKind, ProductionKind, Production};
use heck::SnakeCase;
use itertools::Itertools;
use nanoid::nanoid;
use codegen::{Scope, Variant, EnumVariant, Function};

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

  let user_state = parser.user_state.iter().map(|x| &x.state).join(", ");

  gen_reduce_func(&parser.prods, &nt_names, &nt_types, &user_state, scope);

  gen_parser_impl(&parser, &nt_names, &nt_types, scope);

  let user_state_names = (0..parser.user_state.len()).map(|i| {
    format!("us_{}", i)
  }).collect::<Vec<_>>();

  let user_state_params = user_state_names.iter()
    .zip(parser.user_state.iter())
    .map(|(name, ty)| {
      format!(r##"{name} : {ty}"##, name = name, ty = ty.state)
    }).join(", ");

  let start_fn = compute_start_fn(
    parser,
    &nt_names,
    &nt_types,
    &user_state_names.join(", "),
    &user_state_params);

  let eof = parser.eof_index;
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
    imp.new_fn(format!("assert_{}", name))
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
    .generic("'input")
    .arg("input", "&'input str")
    .ret("Parser<'input, Tokens<'input>>")
    .line("Parser::new(lex(input))");

  scope.new_fn("with_tokens")
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
  user_state: &str,
  scope: &mut Scope,
) {
  let func = scope.new_fn("reduce")
    .generic("'input")
    .arg("nt", "usize")
    .arg("rhs", "Vec<NtType<'input>>")
    .arg("user_state", format!("&mut ({})", user_state))
    .ret("NtType<'input>")
    .line("match nt {");

  gen_prod_actions(prods, nt_names, nt_types, user_state, func);

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
  user_state: &str,
  func: &mut Function,
) {
  for (i, prod) in prods.iter().enumerate() {
    if let Some(action) = &prod.action {
      gen_user_prod_action(i, prod, nt_names, nt_types, user_state, action, func);
    } else {
      match prod.kind {
        ProductionKind::Ordinary => {
          func.line(format!("  {} => NtType::{}(())", i, nt_names[i]));
        }
        ProductionKind::RepetitionEpsilon => {
          func.line(format!("  {} => NtType::{}(vec![])", i, nt_names[i]));
        }
        ProductionKind::RepetitionFirst => {
          func.line(format!("  {} => {{", i));
          func.line("    let mut rhs = rhs.into_iter();");
          func.line(format!("    NtType::{}(vec![(", nt_names[i]));
          gen_prod_action_init_args(&prod.symbols, &nt_names, func);
          func.line("    )])");
          func.line("  }");
        }
        ProductionKind::RepetitionRest => {
          func.line(format!("  {} => {{", i));
          func.line("    let mut rhs = rhs.into_iter();");
          func.line(format!("    let mut result = rhs.next().unwrap().assert_{}()",
            nt_names[i]));
          func.line("    result.push((");
          gen_prod_action_init_args(&prod.symbols[1..], &nt_names, func);
          func.line("    ));");
          func.line(format!("    NtType::{}(result)", nt_names[i]));
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
  user_state: &str,
  func: &mut Function,
) {
  func.line(format!("  {} => {{", index));
  func.line("    fn semantic_action<'input>(");
  func.line(format!("      state: &mut ({})", user_state));

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

  func.line(format!("    ) -> {} {{\n{}\n}}", nt_types[index], action));

  func.line(format!("    NtType::{}(semantic_action(user_state,", nt_names[index]));

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
        func.line("      rhs.next().unwrap().assert_token(),");
        func.line(format!("      rhs.next().unwrap().assert_{}(),", nt_names[*nt as usize]));
      }
    }
  }
}

fn gen_parser_impl(
  parser: &Parser,
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
}

fn compute_start_fn(
  parser: &Parser,
  nt_names: &[String],
  nt_types: &[String],
  user_state_names: &str,
  user_state_params: &str,
) -> String {
  parser.start.iter().map(|(name, (nt, state))| {
    let name = name.replace(&['\'', '-'][..], "_");
    let fn_name = format!("parse_{}", name.to_snake_case());
    let ty = &nt_types[*nt as usize];

    let nt_name = format!("{}", nt_names[*nt as usize]);

    format!(
      r##"
      pub fn {fn_name}(
        &mut self,
        {user_state_params}
      ) -> ::std::result::Result<{ty}, ParseError<'input>> {{
        Ok(self.parse({state} as usize, ({user_state_names}))?.assert_{nt_name}())
      }}
      "##,
      fn_name = fn_name,
      user_state_params = user_state_params,
      user_state_names = user_state_names,
      nt_name = nt_name,
      state = state,
      ty = ty,
    )
  }).join("\n")
}