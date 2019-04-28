#[macro_use]
extern crate text_io;
extern crate monkey;
use monkey::lexer::{Lexer, Token};
use monkey::parser::{Parser, ParserError};
use std::io::{self, Write};

fn main() {
    loop {
        print!("🙈 🙊 🙉 ");
        io::stdout().flush().unwrap();
        let line: String = read!("{}\n");
        let l = Lexer::new(&line);
        println!("tokens: {:?}", l.into_iter().collect::<Vec<Token>>());
        let l = Lexer::new(&line);
        let p = Parser::new(l).parse_program();
        match p {
            Ok(program) => println!("parsed: {}", program),
            Err(e) => println!("parsed: {}", e),
        };
    }
}
