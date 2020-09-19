    static ACTION: [[i32; __(action_col_len)]; __(action_row_len)] = [__(action)];
    static GOTO: [[u32; __(goto_col_len)]; __(goto_row_len)] = [__(goto)];
    static PRODUCTIONS: [(usize, u32); __(prods_len)] = [__(prods)];

    enum NtType<'input> {
      _Token(Token<'input>),
      __(nt_variants)
    }

    impl<'input> NtType<'input> {
      #[__(nt_asserts)]
      fn assert_token(self) -> Token<'input> {
        match self {
          Self::_Token(tok) => tok,
          _ => unreachable!(),
        }
      }
    }

    pub struct Parser<'input, I> {
      tokens: I,
      token: Option<Token<'input>>,
      token_kind: usize,
    }

    pub fn with_input<'input>(
      input: &'input str,
    ) -> Parser<'input, Tokens<'input>> {
      Parser::new(lex(input))
    }

    pub fn with_tokens<'input, I>(
      tokens: I,
    ) -> Parser<'input, I::IntoIter>
      where I: IntoIterator<Item=::std::result::Result<Token<'input>, Error>>
    {
      Parser::new(tokens.into_iter())
    }

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

    fn reduce<'input>(
      nt: usize,
      rhs: Vec<NtType<'input>>,
      user_state: &mut (__(user_state)),
    ) -> NtType<'input> {
      match nt {
        #[__(prod_actions)]
        _ => unreachable!(),
      }
    }

    impl<'input, I> Parser<'input, I>
      where I: Iterator<Item=::std::result::Result<Token<'input>, Error>>
    {
      fn new(tokens: I) -> Self {
        Self {
          tokens,
          token: None,
          token_kind: 0,
        }
      }

      #[__(start_fn)]

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
          self.token_kind = __(eof);
        }

        self.token = token;
        Ok(())
      }

      fn parse(
        &mut self,
        mut state: usize,
        mut user_state: (__(user_state)),
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
      }
    }