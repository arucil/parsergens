use lr::{Parser, Symbol};
use syn::*;
use quote::*;
use proc_macro::{TokenStream, TokenTree, Group, Spacing, Delimiter};
use heck::SnakeCase;
use std::num::NonZeroUsize;

pub fn gen(
  parser: &Parser,
) -> impl ToTokens {
  let action_row_len = parser.action.len();
  let action_col_len = parser.action[0].len();
  let action = parser.action.iter().map(|row| {
    let row = row.iter();
    quote!{ [#(#row),*] }
  });

  let goto_row_len = parser.goto.len();
  let goto_col_len = parser.goto[0].len();
  let goto = parser.goto.iter().map(|row| {
    let row = row.iter();
    quote!{ [#(#row),*] }
  });

  let prods_len = parser.prods.len();
  let prods = parser.prods.iter().map(|prod| {
    let rhs_len = prod.rhs_len;
    let nt = prod.nt;
    quote! { ( #rhs_len, #nt ) }
  });

  let nt_names = parser.nts.iter()
    .map(|(name, _)| format_ident!("{}", name.replace(&['\'', '-'][..], "_")))
    .collect::<Vec<_>>();

  let nt_variants = parser.nts.iter().enumerate()
    .map(|(i, (_, ty))| {
      let name = &nt_names[i];

      let ty = if let Some(ty) = ty {
        let ty = syn::parse_str::<Type>(ty)
          .expect_with(|err|
            format!("invalid non-terminal type: {}, error: {}", ty, err));
        quote!{ #ty }
      } else {
        quote!{ () }
      };

      quote! { #name(#ty) }
    });

  let prod_actions = parser.prods.iter().enumerate().map(|(i, prod)| {
    let nt_name = &nt_names[prod.nt as usize];

    if let Some(action) = &prod.action {
      let action0 = format!("{{ {} }}", action);
      let action = action0.parse::<TokenStream>()
        .expect_with(|err|
          format!("invalid semantic action: {}, error: {}", action0, err));
      let action = replace_place_holder(action);
      let action = syn::parse::<Block>(action.clone())
        .expect_with(|err|
          format!("invalid semantic action: {}, error: {}", action0, err));

      let init_rhs = prod.symbols.iter().enumerate().map(|(i, sym)| {
        let id = format_ident!("__{}", i);

        match sym {
          Symbol::Token(_) => {
            quote!{
              let mut #id = match rhs.next().unwrap() {
                NtType::_Token(tok) => tok,
                _ => unreachable!(),
              };
            }
          }
          Symbol::Nonterminal(nt) => {
            let nt_name = &nt_names[*nt as usize];
            quote!{
              let mut #id = match rhs.next().unwrap() {
                NtType::#nt_name(v) => v,
                _ => unreachable!(),
              };
            }
          }
        }
      });

      quote!{
        #i => {
          let mut rhs = rhs.into_iter();
          #(#init_rhs)*
          NtType::#nt_name(#action)
        }
      }
    } else {
      quote!{ #i => NtType::#nt_name(()) }
    }
  });

  let user_state = parser.user_state.iter().map(|state| {
    syn::parse_str::<Type>(&state.state)
      .expect_with(|err|
        format!("invalid user state: {}, error: {}", state.state, err))
  }).collect::<Vec<_>>();

  let user_state_lifetime = parser.user_state.iter().filter_map(|state| {
    if let Some(lifetime) = &state.lifetime {
      let lifetime = syn::parse_str::<Lifetime>(&lifetime)
        .expect_with(|err|
          format!("invalid user state lifetime: {}, error: {}", lifetime, err));

      Some(quote!{ #lifetime, })
    } else {
      None
    }
  }).collect::<Vec<_>>();
  let user_state_lifetime = &user_state_lifetime;

  let user_state_names = (0..parser.user_state.len()).map(|i| {
    format_ident!("us_{}", i)
  }).collect::<Vec<_>>();

  let user_state_params = user_state_names.iter()
    .zip(user_state.iter())
    .map(|(name, ty)| {
      quote!{ #name : #ty }
    }).collect::<Vec<_>>();

  let start = parser.start.iter().map(|(name, (nt, state))| {
    let name = name.replace(&['\'', '-'][..], "_");
    let fn_name = format_ident!("parse_{}", name.to_snake_case());

    let ty = if let Some(ty) = &parser.nts[*nt as usize].1 {
      let ty = syn::parse_str::<Type>(ty)
        .expect_with(|err| format!("invalid type: {}, error: {}", ty, err));
      quote!{ #ty }
    } else {
      quote!{ () }
    };

    let nt_name = format_ident!("{}", nt_names[*nt as usize]);

    quote! {
      pub fn #fn_name(&mut self) -> ::std::result::Result<#ty, ParseError<'input>> {
        match self.parse(#state as usize)? {
          NtType::#nt_name(ty) => Ok(ty),
          _ => unreachable!(),
        }
      }
    }
  });

  let eof = parser.eof_index;

  quote! {
    static ACTION: [[i32; #action_col_len]; #action_row_len] = [#(#action),*];
    static GOTO: [[u32; #goto_col_len]; #goto_row_len] = [#(#goto),*];
    static PRODUCTIONS: [(usize, u32); #prods_len] = [#(#prods),*];

    enum NtType<'input> {
      _Token(Token<'input>),
      #(#nt_variants),*
    }

    pub struct Parser<#(#user_state_lifetime)* 'input, I> {
      tokens: I,
      token: Option<Token<'input>>,
      token_kind: usize,
      user_state: (#(#user_state),*),
    }

    pub fn with_input<#(#user_state_lifetime)* 'input>(
      input: &'input str,
      #(#user_state_params),*
    ) -> Parser<#(#user_state_lifetime)* 'input, Tokens> {
      Parser::new(lex(input), #(#user_state_names),*)
    }

    pub fn with_tokens<#(#user_state_lifetime)* 'input, I>(
      tokens: I,
      #(#user_state_params),*
    ) -> Parser<#(#user_state_lifetime)* 'input, I>
      where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
    {
      Parser::new(tokens, #(#user_state_names),*)
    }

    #[derive(Debug, Clone)]
    pub enum ParseError<'input> {
      InvalidChar {
        char: char,
        start: usize,
        end: usize,
      },
      InvalidToken(Token<'input>),
      UnexpectedEof,
    }

    fn reduce<#(#user_state_lifetime)* 'input>(
      nt: usize,
      rhs: Vec<NtType<'input>>,
      state: &mut (#(#user_state),*),
    ) -> NtType<'input> {
      match nt {
        #(#prod_actions),* ,
        _ => unreachable!(),
      }
    }

    impl<#(#user_state_lifetime)* 'input, I> Parser<#(#user_state_lifetime)* 'input, I>
      where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
    {
      fn new(tokens: I, #(#user_state_params),*) -> Self {
        Self {
          tokens,
          token: None,
          token_kind: 0,
          user_state: (#(#user_state_names),*),
        }
      }

      #(#start)*

      fn get_token(&mut self) -> ::std::result::Result<(), ParseError<'input>> {
        let token = self.tokens.next()
          .transpose()
          .map_err(|err| ParseError::InvalidChar {
            char: err.char,
            start: err.start,
            end: err.end,
          })?;

        if let Some(token) = &token {
          self.token_kind = token.kind as usize;
        } else {
          self.token_kind = #eof;
        }

        self.token = token;
        Ok(())
      }

      fn parse(
        &mut self,
        mut state: usize
      ) -> ::std::result::Result<NtType<'input>, ParseError<'input>> {
        let mut stack = ::std::vec::Vec::<(usize, NtType)>::new();
        self.get_token()?;

        loop {
          let action = ACTION[state][self.token_kind];

          if action > 0 {
            stack.push((state, NtType::_Token(self.token.take().unwrap())));
            state = (action - 1) as usize;
            self.get_token()?;
          } else if action == ::std::i32::MIN {
            return Ok(stack.pop().unwrap().1);
          } else if action < 0 {
            let prod = (!action) as usize;
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
            let ty = reduce(prod, rhs, &mut self.user_state);

            stack.push((state0, ty));
          } else {
            if let Some(token) = self.token.take() {
              return Err(ParseError::InvalidToken(token));
            } else {
              return Err(ParseError::UnexpectedEof);
            }
          }
        }
      }
    }
  }
}

fn replace_place_holder(action: TokenStream) -> TokenStream {
  let mut input = action.into_iter().peekable();
  let mut output = TokenStream::new();

  while let Some(tree) = input.next() {
    match tree {
      TokenTree::Group(g) => {
        output.extend_one(TokenTree::Group(
          Group::new(
            g.delimiter(),
            replace_place_holder(g.stream()))));
      }
      TokenTree::Punct(punct) => {
        if punct.as_char() == '$' && punct.spacing() == Spacing::Alone {
          if let Some(TokenTree::Literal(lit)) = input.peek() {
            if lit.to_string().chars().all(|c| c.is_ascii_digit()) {
              let ix = lit.to_string().parse::<NonZeroUsize>().expect("valid RHS index");
              let ix = format_ident!("__{}", ix.get() - 1);
              let replaced = quote!(#ix).into();

              output.extend_one(TokenTree::Group(
                Group::new(Delimiter::Parenthesis, replaced)
              ));

              input.next();
              continue;
            }
          }
        }
        output.extend_one(TokenTree::Punct(punct));
      }
      x => output.extend_one(x),
    }
  }

  output
}

trait ExpectWith {
  type Ok;
  type Error;

  fn expect_with<F, S>(self, f: F) -> Self::Ok
    where
      F: FnOnce(Self::Error) -> S,
      S: AsRef<str>;
}

impl<T> ExpectWith for Result<T> {
  type Ok = T;
  type Error = syn::Error;

  fn expect_with<F, S>(self, f: F) -> T
    where
      F: FnOnce(Self::Error) -> S,
      S: AsRef<str>,
  {
    match self {
      Ok(v) => v,
      Err(err) => panic!("{}", f(err).as_ref()),
    }
  }
}

impl<T> ExpectWith for std::result::Result<T, proc_macro::LexError> {
  type Ok = T;
  type Error = proc_macro::LexError;

  fn expect_with<F, S>(self, f: F) -> T
    where
      F: FnOnce(Self::Error) -> S,
      S: AsRef<str>,
  {
    match self {
      Ok(v) => v,
      Err(err) => panic!("{}", f(err).as_ref()),
    }
  }
}