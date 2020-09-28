use lr::{Parser, Symbol, Nonterminal, NonterminalKind, ProductionKind, Production};
use heck::SnakeCase;
use itertools::Itertools;
use nanoid::nanoid;
use std::fmt::{self, Write};
use super::IndentWriter;

pub fn gen(
  parser: &Parser,
  w: &mut IndentWriter<impl Write>,
) -> fmt::Result {

  super::gen_2d_table("ACTION", "i32", &parser.action, w)?;
  super::gen_2d_table("GOTO", "u32", &parser.goto, w)?;
  super::gen_1d_table("PRODUCTIONS", "(usize, u32)",
    &parser.prods.iter().map(|prod| (prod.rhs_len, prod.nt)).collect::<Vec<_>>(),
    w)?;

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

  gen_nt_enum(&nt_names, &nt_types, w)?;
  gen_nt_enum_impl(&nt_names, &nt_types, w)?;

  gen_parser_struct(w)?;
  gen_input_func(w)?;
  gen_parse_error_struct(w)?;

  let user_state = parser.user_state.iter().map(|x| &x.state).join(", ");

  gen_reduce_func(&parser.prods, &nt_names, &nt_types, &user_state, w)?;

  gen_parse_impl(&parser, &nt_names, &nt_types, w)?;

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
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"
enum NtType<'input> {
  _Token(Token<'input>),
  "##.trim_end())?;

  for (name, ty) in nt_names.iter().zip(nt_types) {
    writeln!(w, "  {}({}),", name, ty)?;
  }

  Ok(())
}

fn gen_nt_enum_impl(
  nt_names: &[String],
  nt_types: &[String],
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"
impl<'input> NtType<'input> {
  fn assert_token(self) -> Token<'input> {
    match self {
      Self::_Token(tok) => tok,
      _ => unreachable!(),
    }
  }
  "##.trim_end())?;

  for (name, ty) in nt_names.iter().zip(nt_types) {
    writeln!(w,
      r##"
  fn assert_{nt_name}(self) -> {nt_type} {{
    match self {{
      Self::{nt_name}(v) => v,
      _ => unreachable!(),
    }}
  }}"##,
      nt_name = name,
      nt_type = ty,
    )?;
  }

  Ok(())
}

fn gen_parser_struct(
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"
pub struct Parser<'input, I> {
  tokens: I,
  token: Option<Token<'input>>,
  token_kind: usize,
}
  "##.trim_end())
}

fn gen_input_func(
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"
pub fn with_input<'input>(
  input: &'input str,
) -> Parser<'input, Tokens<'input>> {
  Parser::new(lex(input))
}
  "##.trim_end())?;

  writeln!(w, "{}", r##"
pub fn with_tokens<'input, I>(
  tokens: I,
) -> Parser<'input, I::IntoIter>
  where I: IntoIterator<Item=::std::result::Result<Token<'input>, Error>>
{
  Parser::new(tokens.into_iter())
}
  "##.trim_end())
}

fn gen_parse_error_struct(
  w: &mut impl Write,
) -> fmt::Result {
  writeln!(w, "{}", r##"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError<'input> {
  InvalidChar {
    char: char,
    start: usize,
    end: usize,
  },
  InvalidToken(Token<'input>),
  UnexpectedEof,
}
  "##.trim_end())
}

fn gen_reduce_func(
  prods: &[Production],
  nt_names: &[String],
  nt_types: &[String],
  user_state: &str,
  w: &mut IndentWriter<impl Write>,
) -> fmt::Result {
  writeln!(w, r##"
fn reduce<'input>(
  nt: usize,
  rhs: Vec<NtType<'input>>,
  user_state: &mut ({user_state}),
) -> NtType<'input> {{
  match nt {{"##,
  user_state = user_state);

  w.indent();
  w.indent();
  gen_prod_actions(prods, nt_names, nt_types, user_state, w)?;
  w.dedent();
  w.dedent();

  writeln!(w, "{}", r##"
    _ => unreachable!(),
  }
}
  "##.trim_end())
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
  w: &mut IndentWriter<impl Write>,
) -> fmt::Result {
  for (i, prod) in prods.iter().enumerate() {
    if let Some(action) = &prod.action {
      gen_user_prod_action(i, prod, nt_names, nt_types, user_state, action, w)?;
    } else {
      match prod.kind {
        ProductionKind::Ordinary => {
          writeln!(w,
            r##"{i} => NtType::{nt_name}(()),"##,
            i = i,
            nt_name = nt_names[i],
          )?;
        }
        ProductionKind::RepetitionEpsilon => {
          writeln!(w,
            r##"{i} => NtType::{nt_name}(vec![]),"##,
            i = i,
            nt_name = nt_names[i],
          )?;
        }
        ProductionKind::RepetitionFirst => {
          writeln!(w, "{} => {{", i)?;
          w.indent();
          writeln!(w, "let mut rhs = rhs.into_iter();")?;
          write!(w,
            "NtType::{nt_name}(vec![(",
            nt_name = nt_names[i])?;
          gen_prod_action_init_args(&prod.symbols, &nt_names, w)?;
          writeln!(w, ")])")?;
          w.dedent();
          writeln!(w, "}}")?;
        }
        ProductionKind::RepetitionRest => {
          writeln!(w, "{} => {{", i)?;
          w.indent();
          writeln!(w, "let mut rhs = rhs.into_iter();")?;
          writeln!(w, "let mut result = rhs.next().unwrap().assert_{}()", nt_names[i])?;
          write!(w, "result.push((")?;
          gen_prod_action_init_args(&prod.symbols[1..], &nt_names, w)?;
          writeln!(w, "))")?;
          writeln!(w, "NtType::{}(result)", nt_names[i])?;
          w.dedent();
          writeln!(w, "}}")?;
        }
      }
    }
  }
  Ok(())
}

fn gen_user_prod_action(
  index: usize,
  prod: &Production,
  nt_names: &[String],
  nt_types: &[String],
  action: &str,
  user_state: &str,
  w: &mut IndentWriter<impl Write>,
) -> fmt::Result {
  writeln!(w, "{} => {{", index)?;

  w.indent();
  writeln!(w, "{}", r##"fn semantic_action<'input>("##)?;

  w.indent();
  writeln!(w, "state: &mut ({}),", user_state)?;
  for (i, sym) in prod.symbols.iter().enumerate() {
    match sym {
      Symbol::Token(_) => {
        writeln!(w,
          "mut __{i}: Token<'input>,",
          i = i)?;
      }
      Symbol::Nonterminal(nt) => {
        writeln!(w,
          "mut __{i}: {ty},",
          i = i,
          ty = nt_types[*nt as usize])?;
      }
    }
  }
  w.dedent();

  writeln!(w, ") -> {} {{", nt_types[index])?;

  w.indent();
  writeln!(w, "{}", action)?;
  w.dedent();
  writeln!(w, "}}")?;

  write!(w,
    "NtType::{nt_name}(semantic_action(user_state, ",
    nt_name = nt_names[index])?;

  gen_prod_action_init_args(&prod.symbols, &nt_names, w)?;
  writeln!(w, "))")?;

  w.dedent();
  writeln!(w, "}}")
}

fn gen_prod_action_init_args(
  symbols: &[Symbol],
  nt_names: &[String],
  w: &mut impl Write,
) -> fmt::Result {
  for sym in symbols {
    match sym {
      Symbol::Token(_) => {
        write!(w, r##"rhs.next().unwrap().assert_token(), "##)?;
      }
      Symbol::Nonterminal(nt) => {
        write!(w,
          r##"rhs.next().unwrap().assert_{nt_name}(), "##,
          nt_name = &nt_names[*nt as usize],
        )?;
      }
    }
  }
  Ok(())
}

fn gen_parser_impl(
  parser: &Parser,
  w: &mut IndentWriter<impl Write>
) -> fmt::Result {
  writeln!(w, "{}", r##"
impl<'input, I> Parser<'input, I>
  where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
{
  "##.trim_end())?;
  w.indent();

  writeln!(w, "{}", r##"
fn new(tokens: I) -> Self {
  Self {
    tokens,
    token: None,
    token_kind: 0,
  }
}
  "##.trim_end())?;

  writeln!(w, "{}", r##"
  "##.trim_end())?;
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