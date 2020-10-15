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

  loop {
    let token_kind = token.as_ref()
      .map(|t| t.kind.id() as usize)
      .unwrap_or(parser.eof_index);
    let token_text = token.as_ref().map(|t| t.text).unwrap_or("EOF");
    let token_start = token.as_ref().map(|t| t.start).unwrap_or(input.len());
    let token_end = token.as_ref().map(|t| t.end).unwrap_or(input.len());

    let action = parser.action[state as usize][token_kind];

    if action > 0 {
      let event = format!("shift  {}", token_text);
      events.push(event);
      stack.push((state, token_text.to_owned()));
      state = (action - 1) as u32;
      token = tokens.next().transpose().unwrap();
    } else if action < 0 {
      if !action as usize == accept_prod {
        events.push(format!("accept"));
        break;
      }

      let mut event = format!("reduce ");
      let prod = &parser.prods[!action as usize];
      let state0 = if prod.rhs_len == 0 {
        state
      } else {
        stack[stack.len() - prod.rhs_len].0
      };
      let nt_name = &parser.nts[prod.nt as usize].name;

      state = parser.goto[state0 as usize][prod.nt as usize] - 1;

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