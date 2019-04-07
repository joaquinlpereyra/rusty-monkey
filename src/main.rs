#[macro_use]
extern crate text_io;
extern crate monkey;
use monkey::lexer::{Lexer, Token};
use std::io::{self, Write};

fn main() {
    loop {
        print!("🙈 🙊 🙉 ");
        io::stdout().flush().unwrap();
        let line: String = read!("{}\n");
        let l = Lexer::new(&line);
        println!("{:?}", l.into_iter().collect::<Vec<Token>>());
    }
}
