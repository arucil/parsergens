use lr::{Parser, Symbol, Nonterminal, NonterminalKind, ProductionKind};
use heck::SnakeCase;
use std::path::Path;
use itertools::Itertools;
use nanoid::nanoid;

pub fn gen(
  parser: &Parser,
) -> String {
  let action_row_len = parser.action.len();
  let action_col_len = parser.action[0].len();
  let action = parser.action.iter().map(|row| {
    format!("[{}]", row.iter().join(", "))
  }).join(", ");

  let goto_row_len = parser.goto.len();
  let goto_col_len = parser.goto[0].len();
  let goto = parser.goto.iter().map(|row| {
    format!("[{}]", row.iter().join(", "))
  }).join(", ");

  let prods_len = parser.prods.len();
  let prods = parser.prods.iter().map(|prod| {
    let rhs_len = prod.rhs_len;
    let nt = prod.nt;
    format!("({}, {})", rhs_len, nt)
  }).join(", ");

  let nt_names = parser.nts.iter()
    .map(|nt| {
      if nt.kind == NonterminalKind::User {
        nt.name.replace(&['\'', '-'][..], "_")
      } else {
        format!("_{}", nanoid!(10).replace('~', "_"))
      }
    })
    .collect::<Vec<_>>();

  let nt_types = parser.nts.iter()
    .map(|nt| compute_nt_type(nt, parser))
    .collect::<Vec<_>>();

  let nt_variants = (0..parser.nts.len())
    .map(|i| {
      let name = &nt_names[i];
      let ty = &nt_types[i];

      format!("{}({})", name, ty)
    }).join(", ");

  let nt_asserts = compute_nt_asserts(parser, &nt_names, &nt_types);

  let user_state = parser.user_state.iter().map(|x| &x.state).join(", ");

  let prod_actions = compute_prod_actions(parser, &nt_names, &nt_types, &user_state);

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

  let path = Path::new(file!()).parent().unwrap().join("templates/parser.tpl.rs");
  crate::tpl_engine::process(path, |name| {
    match name {
      "action_col_len" => action_col_len.to_string(),
      "action_row_len" => action_row_len.to_string(),
      "action" => action.to_string(),
      "goto_col_len" => goto_col_len.to_string(),
      "goto_row_len" => goto_row_len.to_string(),
      "goto" => goto.to_string(),
      "prods_len" => prods_len.to_string(),
      "prods" => prods.to_string(),
      "nt_variants" => nt_variants.to_string(),
      "user_state" => user_state.to_string(),
      "prod_actions" => prod_actions.to_string(),
      "start_fn" => start_fn.to_string(),
      "nt_asserts" => nt_asserts.to_string(),
      "eof" => eof.to_string(),
      _ => panic!("unknown param: {}", name),
    }
  }).unwrap()
}

fn compute_nt_asserts(
  parser: &Parser,
  nt_names: &[String],
  nt_types: &[String],
) -> String {
  (0..parser.nts.len())
    .map(|i| {
      let nt_name = &nt_names[i];
      let nt_type = &nt_types[i];

      format!(
        r##"
        fn assert_{nt_name}(self) -> {nt_type} {{
          match self {{
            Self::{nt_name}(v) => v,
            _ => unreachable!(),
          }}
        }}
        "##,
        nt_name = nt_name,
        nt_type = nt_type,
      )
    }).collect()
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

fn compute_prod_actions(
  parser: &Parser,
  nt_names: &[String],
  nt_types: &[String],
  user_state: &str,
) -> String {
  parser.prods.iter().enumerate().map(|(i, prod)| {
    let nt_name = &nt_names[prod.nt as usize];
    let nt_type = &nt_types[prod.nt as usize];

    if let Some(action) = &prod.action {
      let init_params = prod.symbols.iter().enumerate()
        .map(|(i, sym)| {
          let ty = match sym {
            Symbol::Token(_) => format!("Token<'input>"),
            Symbol::Nonterminal(nt) => nt_types[*nt as usize].clone(),
          };
          format!("mut __{i} : {ty}", i = i, ty = ty)
        })
        .join(", ");

      let init_args = compute_prod_action_init_args(&prod.symbols, &nt_names);

      format!(
        r##"
        {i} => {{
          let mut rhs = rhs.into_iter();
          fn semantic_action<'input>(
            state: &mut ({user_state}),
            {init_params}
          ) -> {nt_type} {{
            {action}
          }}
          NtType::{nt_name}(semantic_action(user_state, {init_args}))
        }}
        "##,
        i = i,
        init_args = init_args,
        user_state = user_state,
        init_params = init_params,
        nt_type = nt_type,
        action = action,
        nt_name = nt_name,
      )
    } else {
      match prod.kind {
        ProductionKind::Ordinary => {
          format!(
            r##"{i} => NtType::{nt_name}(()),"##,
            i = i,
            nt_name = nt_name,
          )
        }
        ProductionKind::RepetitionEpsilon => {
          format!(
            r##"{i} => NtType::{nt_name}(vec![]),"##,
            i = i,
            nt_name = nt_name,
          )
        }
        ProductionKind::RepetitionFirst => {
          let init_args = compute_prod_action_init_args(&prod.symbols, &nt_names);
          format!(
            r##"
            {i} => {{
              let mut rhs = rhs.into_iter();
              NtType::{nt_name}(vec![({init_args})])
            }}
            "##,
            i = i,
            nt_name = nt_name,
            init_args = init_args,
          )
        }
        ProductionKind::RepetitionRest => {
          let init_args = compute_prod_action_init_args(&prod.symbols[1..], &nt_names);
          format!(
            r##"
            {i} => {{
              let mut rhs = rhs.into_iter();
              let mut result = rhs.next().unwrap().assert_{nt_name}();
              result.push(({init_args}));
              NtType::{nt_name}(result)
            }}
            "##,
            i = i,
            nt_name = nt_name,
            init_args = init_args,
          )
        }
      }
    }
  }).join("\n")
}

fn compute_prod_action_init_args(
  symbols: &[Symbol],
  nt_names: &[String],
) -> String {
  symbols.iter().map(|sym| {
    match sym {
      Symbol::Token(_) => {
        format!(r##"rhs.next().unwrap().assert_token()"##)
      }
      Symbol::Nonterminal(nt) => {
        format!(
          r##"rhs.next().unwrap().assert_{nt_name}()"##,
          nt_name = &nt_names[*nt as usize],
        )
      }
    }
  }).join(", ")
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