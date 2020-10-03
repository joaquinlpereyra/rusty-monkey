#[macro_use]
extern crate text_io;
extern crate monkey;
use monkey::interpreter::Interpreter;
use monkey::lexer::{Lexer, Token};
use monkey::parser::Parser;
use std::collections::HashMap;
use std::io::{self, Write};

fn main() {
    let mut interpreter = Interpreter::new();
    loop {
        print!("🙈 🙊 🙉 ");
        io::stdout().flush().unwrap();
        let line: String = read!("{}\n");
        let l_debug = Lexer::new(&line);
        let tokens: Vec<Token> = l_debug.into_iter().collect();
        println!("tokens: {:?}", tokens);
        let l = Lexer::new(&line);
        let p = match Parser::new(l).parse_program() {
            Ok(program) => program,
            Err(e) => {
                println!("error: {}", e);
                continue;
            }
        };
        println!("parsed: {}", p);
        let interpreted = &mut interpreter.eval(&p);
        println!("interpreted: {}", interpreted);
    }
}
