use lr::Parser;

pub  fn parse<'a>(
  parser: &Parser,
  input: &'a str,
  start: &str
) -> Vec<String> {
  let mut events = vec![];
  let mut state = parser.entry_points[start].start_state;
  let accept_prod = parser.entry_points[start].accept_prod;
  let mut stack: Vec<(u32, String)> = vec![];
  let mut tokens = parser.lexer.as_ref().unwrap().lex(input);
  let mut token = tokens.next().transpose().unwrap();

  let goto_check_base = (parser.eof_index + 1) as i32;

  loop {
    let token_kind = token.as_ref()
      .map(|t| t.kind.index())
      .unwrap_or(parser.eof_index);
    let token_text = token.as_ref().map(|t| t.text).unwrap_or("EOF");
    let token_start = token.as_ref().map(|t| t.start).unwrap_or(input.len());
    let token_end = token.as_ref().map(|t| t.end).unwrap_or(input.len());

    let i = parser.parse_tables.action_disp[state as usize] + token_kind as isize;
    let action = if i < 0 || i as usize >= parser.parse_tables.parse_table.len() ||
      parser.parse_tables.check_table[i as usize] != token_kind as i32
    {
      parser.parse_tables.action_default[state as usize]
    } else {
      parser.parse_tables.parse_table[i as usize]
    };

    if action > 0 {
      let event = format!("shift  {}", token_text);
      events.push(event);
      stack.push((state, token_text.to_owned()));
      state = action as u32;
      token = tokens.next().transpose().unwrap();
    } else if action < 0 {
      if !action as usize == accept_prod {
        // the default entry of ACTION table can delay error to next shift, and
        // perform redundant reductions. If the reduction performed is ACCEPT,
        // no shift will be performed. So we make sure EOF is reached before
        // accepting.
        //
        // miniyacc doesn't need this check, because its augmented non-terminal,
        // S' -> S $, the $ (EOF) token need to be shifted to finish the parsing.
        if token_kind == parser.eof_index {
          events.push(format!("accept"));
        } else {
          let event = format!("error token {} at {}:{}, expected EOF",
            token_text, token_start, token_end);
          events.push(event);
        }
        break;
      }

      let mut event = format!("reduce ");
      let prod = &parser.prods[(!action) as usize];
      let state0 = if prod.rhs_len == 0 {
        state
      } else {
        stack[stack.len() - prod.rhs_len].0
      };
      let nt_name = &parser.nts[prod.nt as usize].name;

      let i = parser.parse_tables.goto_disp[prod.nt as usize] + state0 as isize;
      let check = goto_check_base + prod.nt as i32;
      state = if i < 0 || i as usize >= parser.parse_tables.parse_table.len() ||
        parser.parse_tables.check_table[i as usize] != check
      {
        parser.parse_tables.goto_default[prod.nt as usize] as u32
      } else {
        parser.parse_tables.parse_table[i as usize] as u32
      };

      event.push_str(nt_name);
      event.push_str(" ->");
      for (_, text) in stack.drain(stack.len() - prod.rhs_len..) {
        event.push_str(" ");
        event.push_str(&text);
      }
      stack.push((state0, nt_name.to_owned()));

      events.push(event);
    } else {
      let event = format!("error token {} at {}:{}", token_text, token_start, token_end);
      events.push(event);
      break;
    }
  }

  events
}