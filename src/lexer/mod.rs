//! # Lexer
//! The lexer and tokens for Monkey.
//! Has the necessary structs and methods to
//! transform a string to a series of tokens.
//! # Example
//! ```
//! use monkey::lexer::{Lexer, Token, Token::{Let, Ident, Assign, Int, Semicolon}};
//! let program = Lexer::new(String::from("let answer = 42;"));
//! let tokens = program.into_iter().collect::<Vec<Token>>();
//! assert_eq!(tokens, vec![Let, Ident("answer".to_owned()), Assign, Int("42".to_owned()), Semicolon]);
//! ```
use std::fmt;

/// Describes all the tokens for Monkey
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Special tokens
    Illegal(String),
    EOF,

    // Identifers and literals
    Ident(String),
    Int(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Not,
    LT,
    GT,
    EQ,
    NotEQ,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Let,
    Fn,
    True,
    False,
    If,
    Else,
    Return,
    For,
    In,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Token::Illegal(s) => s,
            Token::EOF => "EOF",
            Token::Ident(s) => s,
            Token::Int(s) => s,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Slash => "/",
            Token::Asterisk => "*",
            Token::Not => "!",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEQ => "!=",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Fn => "fn",
            Token::Let => "let",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::For => "for",
            Token::In => "in",
        };
        write!(f, "{}", string)
    }
}

/// The lexer is the workhouse of this module.
/// It's designed to parse a Monkey string
/// into tokens.
#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    position: usize,  // current position in input
    ch: Option<char>, // the char we are reading currently
}

// TODO: support unicode, with emojis and all!
impl Lexer {
    /// Create a new Lexer given some input string.
    /// # Example
    /// ```
    /// use monkey::lexer::{Lexer, Token};
    /// let program = Lexer::new("let answer = 42;".to_owned());
    /// let tokens = program.into_iter().collect::<Vec<Token>>();
    /// assert_eq!(tokens.len(), 5);
    /// ```
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            ch: None,
        };

        // if I try to d o: ch: input.chars().nth(0)
        // it complains the lexer has already taken ownership
        // of the input :(
        l.ch = l.input.chars().nth(0);

        l
    }

    /// Get the next_token from the Lexer.
    /// You most probably want to call into_iter() instead of using
    /// this method, but you may want to use this if you absolutely
    /// do not want give away ownership of the the lexer.
    /// # Example
    /// ```
    /// use monkey::lexer::{Token, Lexer};
    /// let mut program = Lexer::new("let answer = 42;".to_owned());
    /// let mut t = program.next_token();
    /// assert_eq!(t, Token::Let);
    /// t = program.next_token();
    /// assert_eq!(t, Token::Ident(String::from("answer")));
    /// t = program.next_token();
    /// assert_eq!(t, Token::Assign);
    /// t = program.next_token();
    /// assert_eq!(t, Token::Int(String::from("42")));
    /// t = program.next_token();
    /// assert_eq!(t, Token::Semicolon);
    /// ```
    pub fn next_token(&mut self) -> Token {
        while self.ch.is_some() && self.ch.unwrap().is_whitespace() {
            self.read_char()
        }
        if self.ch.is_none() {
            return Token::EOF;
        }
        let token = match self.ch.unwrap() {
            '=' if self.peak_char() == Some('=') => {
                self.read_char();
                Token::EQ
            }
            '!' if self.peak_char() == Some('=') => {
                self.read_char();
                Token::NotEQ
            }
            '!' => Token::Not,
            '=' => Token::Assign,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,
            v => match self.read_word() {
                "let" => Token::Let,
                "fn" => Token::Fn,
                "if" => Token::If,
                "else" => Token::Else,
                "return" => Token::Return,
                "true" => Token::True,
                "false" => Token::False,
                "for" => Token::For,
                "in" => Token::In,
                w if v.is_alphabetic() => Token::Ident(w.to_owned()),
                w if v.is_numeric() => Token::Int(w.to_owned()),
                w => Token::Illegal(w.to_owned()),
            },
        };
        self.read_char();
        token
    }

    fn peak_char(&self) -> Option<char> {
        self.input.chars().nth(self.position + 1)
    }

    fn read_char(&mut self) {
        self.ch = self.peak_char();
        self.position += 1;
    }

    fn is_delimiter(c: char) -> bool {
        return c == ';'
            || c == ' '
            || c == '('
            || c == ')'
            || c == '}'
            || c == '{'
            || c == ','
            || c == '='
            || c == '!'
            || c == '*'
            || c == '+'
            || c == '-'
            || c == '/'
            || c.is_whitespace();
    }

    fn read_word(&mut self) -> &str {
        let pos = self.position;
        while self.ch.is_some() && !Lexer::is_delimiter(self.ch.unwrap()) {
            self.read_char();
        }

        let end_word = self.position;
        // readjust position to the character
        // last character of the word, so we can just call .next()
        // and get the inmediate next token
        if self.position > 0 {
            self.position -= 1;
        }

        &self.input[pos..end_word]
    }
}

impl IntoIterator for Lexer {
    type Item = Token;
    type IntoIter = ::std::vec::IntoIter<Token>;

    fn into_iter(mut self) -> Self::IntoIter {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            match self.next_token() {
                Token::EOF => break,
                t => tokens.push(t),
            };
        }
        tokens.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normal_code() {
        let input = "
let ten = 10;
let add = fn(x, y) {
     x + y;
};
let result = add(five, ten);

if (5 > 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";
        let expected: Vec<Token> = vec![
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Fn,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(String::from("5")),
            Token::GT,
            Token::Int(String::from("10")),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(String::from("10")),
            Token::EQ,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::NotEQ,
            Token::Int(String::from("9")),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut l = Lexer::new(String::from(input));
        for t in expected {
            assert_eq!(t, l.next_token());
        }
    }

    #[test]
    fn test_special_characters() {
        let input = "=+(){},;!-/*<>";
        let expected: Vec<Token> = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Not,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::LT,
            Token::GT,
        ];
        let mut l = Lexer::new(String::from(input));
        for t in expected {
            assert_eq!(t, l.next_token());
        }
    }

    #[test]
    fn test_delimiters_work_ok() {
        let input = "
let a_foo = 10;
.
";
        let expected = vec![
            Token::Let,
            Token::Ident(String::from("a_foo")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Illegal(String::from(".")),
        ];
        let mut l = Lexer::new(String::from(input));
        for t in expected {
            assert_eq!(t, l.next_token());
        }
    }
}
