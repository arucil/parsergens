use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

mod lex;
mod ast;

fn main() {
    let input = "(21)";
    let lexer = lex::Lexer::new(input);
    println!("{:?}", grammar::documentParser::new().parse(input, lexer));
}
