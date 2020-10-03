//! # Lexer
//! The lexer and tokens for Monkey.
//! Has the necessary structs and methods to
//! transform a string to a series of tokens.
//! # Example
//! ```
//! use monkey::lexer::{Lexer, Token, Token::{Let, Ident, Assign, Int, Semicolon}};
//! let program = Lexer::new("let answer = 42;");
//! let tokens = program.into_iter().collect::<Vec<Token>>();
//! assert_eq!(tokens, vec![Let, Ident("answer"), Assign, Int("42"), Semicolon]);
//! ```
use std::fmt;

/// Describes all the tokens for Monkey
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'a> {
    // Special tokens
    Illegal(&'a str),
    EOF,

    // Identifers and literals
    Ident(&'a str),
    Int(&'a str),

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
    Fn,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    For,
    In,
}

impl<'a> fmt::Display for Token<'a> {
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
#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,  // current position in input
    ch: Option<char>, // the char we are reading currently
}

// TODO: support unicode, with emojis and all!
impl<'a> Lexer<'a> {
    /// Create a new Lexer given some input string.
    /// # Example
    /// ```
    /// use monkey::lexer::{Lexer, Token};
    /// let program = Lexer::new("let answer = 42;");
    /// let tokens = program.into_iter().collect::<Vec<Token>>();
    /// assert_eq!(tokens.len(), 5);
    /// ```
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            position: 0,
            ch: input.chars().nth(0),
        }
    }

    /// Get the next_token from the Lexer.
    /// You most probably want to call into_iter() instead of using
    /// this method, but you may want to use this if you absolutely
    /// do not want give away ownership of the the lexer.
    /// # Example
    /// ```
    /// use monkey::lexer::{Token, Lexer};
    /// let mut program = Lexer::new("let answer = 42;");
    /// let mut t = program.next_token();
    /// assert_eq!(t, Token::Let);
    /// t = program.next_token();
    /// assert_eq!(t, Token::Ident("answer"));
    /// t = program.next_token();
    /// assert_eq!(t, Token::Assign);
    /// t = program.next_token();
    /// assert_eq!(t, Token::Int("42"));
    /// t = program.next_token();
    /// assert_eq!(t, Token::Semicolon);
    /// ```
    pub fn next_token(&mut self) -> Token<'a> {
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
                w if v.is_alphabetic() => Token::Ident(w),
                w if v.is_numeric() => Token::Int(w),
                w => Token::Illegal(w),
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

    fn read_word(&mut self) -> &'a str {
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

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token<'a>;
    type IntoIter = ::std::vec::IntoIter<Token<'a>>;

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
            Token::Ident("ten"),
            Token::Assign,
            Token::Int("10"),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add"),
            Token::Assign,
            Token::Fn,
            Token::LParen,
            Token::Ident("x"),
            Token::Comma,
            Token::Ident("y"),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x"),
            Token::Plus,
            Token::Ident("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result"),
            Token::Assign,
            Token::Ident("add"),
            Token::LParen,
            Token::Ident("five"),
            Token::Comma,
            Token::Ident("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5"),
            Token::GT,
            Token::Int("10"),
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
            Token::Int("10"),
            Token::EQ,
            Token::Int("10"),
            Token::Semicolon,
            Token::Int("10"),
            Token::NotEQ,
            Token::Int("9"),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut l = Lexer::new(input);
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
        let mut l = Lexer::new(input);
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
            Token::Ident("a_foo"),
            Token::Assign,
            Token::Int("10"),
            Token::Semicolon,
            Token::Illegal("."),
        ];
        let mut l = Lexer::new(input);
        for t in expected {
            assert_eq!(t, l.next_token());
        }
    }
}
