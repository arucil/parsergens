#![feature(or_patterns, never_type)]

use lalrpop_util::ParseError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::files::SimpleFile;

mod grammar_parser;

pub mod lexer;
pub mod grammar;

pub use crate::grammar::*;
pub use lexer::{Lexer, TokenId, TokenIdGen};
pub use grammar_parser::ast::Assoc;

use grammar_parser::ast;
use grammar_parser::lex::{Token, LexErrorKind, LexError};
use grammar_parser::regex::{RegexError, RegexErrorKind};
use grammar_parser::UserParseError;
use lexer::LexerError;

#[cfg(not(debug_assertions))]
pub type Map<K, V> = std::collections::HashMap<K, V>;

#[cfg(debug_assertions)]
pub type Map<K, V> = indexmap::IndexMap<K, V>;

#[cfg(not(debug_assertions))]
pub type BiMap<K, V> = bimap::BiHashMap<K, V>;

#[cfg(debug_assertions)]
pub type BiMap<K, V> = bimap::BiBTreeMap<K, V>;

#[cfg(not(debug_assertions))]
pub type Set<K> = std::collections::HashSet<K>;

#[cfg(debug_assertions)]
pub type Set<K> = indexmap::IndexSet<K>;


#[derive(Debug)]
pub struct GrammarError {
  kind: GrammarErrorKind,
  message: String,
  span: (usize, usize),
}

#[derive(Debug)]
pub enum GrammarErrorKind {
  ParseError,
  TokenDeclConflict,
  MissingDecl,
  NameNotFound,
  NameConflict,
}

pub fn build(grammar: &str) -> Result<Grammar, GrammarError> {
  let ast = grammar_parser::parse(grammar)
    .map_err(|err| err.into())?;

  let mut token_decls = vec![];
  let mut ext_token_decls = vec![];
  let mut skip_decls = vec![];
  let mut start_decls = vec![];
  let mut rule_decls = vec![];
  let mut user_decls = vec![];
  let mut assoc_decls = vec![];
  let mut state_decls = vec![];

  for decl in ast {
    match decl.1 {
      ast::Decl::Token(decl) => token_decls.push(decl),
      ast::Decl::ExtToken(decl) => ext_token_decls.push(decl),
      ast::Decl::Skip(decl) => skip_decls.push(decl),
      ast::Decl::Start(decl) => start_decls.push(decl),
      ast::Decl::Rule(decl) => rule_decls.push(decl),
      ast::Decl::User(decl) => user_decls.push(decl),
      ast::Decl::Assoc(decl) => assoc_decls.push(decl),
      ast::Decl::State(decl) => state_decls.push(decl),
    }
  }

  let assocs = assoc_decls.into_iter()
    .flat_map(|decl| {
      let assoc = decl.assoc.1;
      decl.names.into_iter().map(move |name| (name.1, (assoc, !(name.0 .0 as u32))))
    })
    .collect::<Map<_, _>>();

  let user_state = state_decls.into_iter()
    .map(|decl| UserState {
      lifetime: decl.lifetime.map(|x| x.1),
      state: decl.state.1
    })
    .collect::<Vec<_>>();

  let lexer;
  let tokens;
  if ext_token_decls.is_empty() {
    if token_decls.is_empty() && skip_decls.is_empty() {
      return Err(GrammarError {
        kind: GrammarErrorKind::MissingDecl,
        message: format!("missing declarations"),
        span: (0, grammar.len()),
      });
    }

    let (l, t) = Lexer::new(&token_decls, &skip_decls).map_err(|err| {
      match err {
        LexerError::RegexError(err) => {
          err.into()
        }
      }
    })?;
    lexer = Some(l);
    tokens = t;
  } else {
    if !token_decls.is_empty() || !skip_decls.is_empty() {
      return Err(GrammarError {
        kind: GrammarErrorKind::TokenDeclConflict,
        message: format!("token declaration conflict"),
        span: (0, grammar.len()),
      });
    }

    lexer = None;
    tokens = ext_token_decls.into_iter()
      .scan(TokenIdGen::default(), |id_gen, decl| Some((id_gen.gen(), decl.name.1)))
      .collect();
  }

  let user_code = user_decls.into_iter().map(|decl| decl.code.1).collect();

  let mut nt_id_gen = NonterminalIdGen::default();
  let nts = rule_decls.iter()
    .map(|decl| {
      if tokens.get_by_right(&decl.name.1).is_some() {
        return Err(GrammarError {
          kind: GrammarErrorKind::NameConflict,
          message: format!("rule name collides with token name"),
          span: decl.name.0,
        });
      }

      let id = nt_id_gen.gen();

      Ok((id, decl.name.1.clone()))
    })
    .collect::<Result<BiMap<_, _>, GrammarError>>()?;

  let start_nts = start_decls.iter()
    .map(|decl| {
      if let Some(&id) = nts.get_by_right(&decl.name.1) {
        Ok(id)
      } else {
        Err(GrammarError {
          kind: GrammarErrorKind::NameNotFound,
          message: format!("nonterminal not found"),
          span: decl.name.0,
        })
      }
    })
    .collect::<Result<Set<_>, GrammarError>>()?;

  let mut rules = vec![];
  let mut nt_metas = Map::new();

  for rule_decl in &rule_decls {
    let nt = *nts.get_by_right(&rule_decl.name.1).unwrap();
    let start = rules.len();

    for alt in &rule_decl.alts {
      let items = match &alt.1.terms {
        ast::RuleAltTerms::Epsilon => vec![],
        ast::RuleAltTerms::Terms(terms) => {
          convert_terms(terms, &nts, &tokens)?
        }
      };
      let prec = match &alt.1.prec {
        Some(name) => {
          Some(assocs.get(&name.1)
            .cloned()
            .ok_or_else(|| GrammarError {
            kind: GrammarErrorKind::NameNotFound,
            message: format!("precedence symbol not found"),
            span: name.0,
          })?)
        }
        None => None,
      };
      let action = alt.1.action.as_ref().map(|code| code.1.clone());

      rules.push(Rule {
        nt,
        items,
        prec,
        action,
      });
    }

    let ty = rule_decl.ty.as_ref().map(|t| t.1.clone());
    nt_metas.insert(nt, NonterminalMetadata {
      range: start..rules.len(),
      ty,
    });
  }

  Ok(Grammar {
    rules,
    start_nts,
    nts,
    nt_metas,
    lexer,
    tokens,
    user_code,
    user_state,
  })
}

fn convert_terms(
  terms: &[ast::Term],
  nts: &BiMap<NonterminalId, String>,
  tokens: &BiMap<TokenId, String>,
) -> Result<Vec<Item>, GrammarError> {
  let mut items = vec![];

  for term in terms {
    match term {
      ast::Term::Symbol(sym) => {
        if let Some(&id) = nts.get_by_right(&sym.1) {
          items.push(Item::Nonterminal(id));
        } else if let Some(&id) = tokens.get_by_right(&sym.1) {
          items.push(Item::Token(id));
        } else {
          return Err(GrammarError {
            kind: GrammarErrorKind::NameNotFound,
            message: format!("nonterminal or token not found"),
            span: sym.0,
          });
        }
      }
      ast::Term::Optional(terms) => {
        items.push(Item::Optional(convert_terms(terms, nts, tokens)?));
      }
      ast::Term::Many(terms) => {
        items.push(Item::Many(convert_terms(terms, nts, tokens)?));
      }
      ast::Term::Many1(terms) => {
        items.push(Item::Many1(convert_terms(terms, nts, tokens)?));
      }
    }
  }

  Ok(items)
}

pub fn report_error(input: &str, error: &GrammarError) {
  let writer = StandardStream::stderr(ColorChoice::Always);
  let config = term::Config::default();
  let files = SimpleFile::new("", input);

  let diagnostic = Diagnostic::error();
  let diagnostic = match error.kind {
    GrammarErrorKind::ParseError => {
      diagnostic.with_message("syntax error")
    }
    GrammarErrorKind::NameConflict => {
      diagnostic.with_message("name conflict")
    }
    GrammarErrorKind::NameNotFound => {
      diagnostic.with_message("name not found")
    }
    GrammarErrorKind::TokenDeclConflict => {
      diagnostic.with_message(
        "token declaration and external token declaration cannot exist simultaneously")
    }
    GrammarErrorKind::MissingDecl => {
      diagnostic.with_message("missing token declarations")
    }
  };
  let diagnostic = diagnostic.with_labels(vec![
    Label::primary((), error.span.0..error.span.1).with_message(&error.message)
  ]);

  term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}

impl<'a> Into<GrammarError> for ParseError<usize, Token<'a>, UserParseError> {
  fn into(self) -> GrammarError {
    match self {
      Self::InvalidToken { location } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid token or EOF"),
          span: (location, location),
        }
      }
      Self::UnrecognizedEOF { location, expected } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("expected {}, found EOF", expected.join(", ")),
          span: (location, location),
        }
      }
      Self::UnrecognizedToken { token, expected } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("expected {}, found {}", expected.join(", "), token.1),
          span: (token.0, token.2),
        }
      }
      Self::ExtraToken { token } => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: token.1.to_string(),
          span: (token.0, token.2),
        }
      }
      Self::User { error } => {
        match error {
          UserParseError::LexError(error) => error.into(),
          UserParseError::RegexError(error) => error.into(),
          UserParseError::MissingType(span) => {
            GrammarError {
              kind: GrammarErrorKind::ParseError,
              message: format!(
                "non-terminal type must be defined if any production has semantic action"),
              span,
            }
          }
        }
      }
    }
  }
}

impl<'a> Into<GrammarError> for LexError {
  fn into(self) -> GrammarError {
    match self.kind {
      LexErrorKind::UnclosedRegex => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("unclosed regex"),
          span: self.span,
        }
      }
      LexErrorKind::UnclosedString => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("unclosed string"),
          span: self.span,
        }
      }
      LexErrorKind::UnclosedCodeBlock => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("unclosed code block"),
          span: self.span,
        }
      }
      LexErrorKind::InvalidChar => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid character"),
          span: self.span,
        }
      }
      LexErrorKind::InvalidIndent => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid indentation"),
          span: self.span,
        }
      }
    }
  }
}

impl<'a> Into<GrammarError> for RegexError {
  fn into(self) -> GrammarError {
    match self.kind {
      RegexErrorKind::SyntaxError => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("regex syntax error"),
          span: self.span,
        }
      }
      RegexErrorKind::InvalidCodePoint => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("invalid code point"),
          span: self.span,
        }
      }
      RegexErrorKind::Empty => {
        GrammarError {
          kind: GrammarErrorKind::ParseError,
          message: format!("regex or string may accept empty string"),
          span: self.span,
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_debug_snapshot;

  #[test]
  fn expr() {
    let input = r#"
%start expr

%token PLUS "+"
%token MINUS "-"
%token MUL "*"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token NUMBER /\d+(\.\d*)?/
%token IDENT /[a-zA-Z][\w_]*/

%skip /[ \n]/

expr = expr PLUS expr
  | expr MINUS expr
  | expr MUL expr
  | expr DIV expr
  | factor

factor: {Expr} =
    NUMBER   { $1.parse::<f64>().unwrap() }
  | LPAREN expr RPAREN { $2 }
  | MINUS factor
  | IDENT
  | IDENT LPAREN param-list RPAREN

param-list =
    ()
  | expr ( COMMA expr )*
    "#;

    assert_debug_snapshot!(build(input));
  }
}