#[macro_use]
extern crate text_io;
extern crate monkey;
use monkey::interpreter::Interpreter;
use monkey::lexer::{Lexer, Token};
use monkey::parser::Parser;
use std::io::{self, Write};

fn main() {
    loop {
        print!("🙈 🙊 🙉 ");
        io::stdout().flush().unwrap();
        let line: String = read!("{}\n");
        let l = Lexer::new(&line);
        println!("tokens: {:?}", l.into_iter().collect::<Vec<Token>>());
        let l = Lexer::new(&line);
        let p = match Parser::new(l).parse_program() {
            Ok(program) => program,
            Err(e) => {
                println!("error: {}", e);
                break;
            }
        };
        println!("parsed: {}", p);
        let interpreted = Interpreter::new().eval(&p);
        println!("interpreted: {}", interpreted);
    }
}
