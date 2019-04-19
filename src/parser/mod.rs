//! The main parser module.
//! It has a Parser struct
//! which constructs AST nodes
//! given a lexer.
use super::lexer::{self, Token};
mod ast;
use std::collections::HashMap;
use std::error::Error;
use std::{fmt, mem, result};

// Silly result alias.
type Result<T> = result::Result<T, Box<ParserError>>;

enum OperatorPrecedence {
    Lowest = 0,
    Equals = 1,
    LessOrGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

// ParserError is the error returned by the parser
// when it encounters an unexpected token.
#[derive(Debug)]
struct ParserError {
    expected: String,
    got: String,
}

impl<'a> ParserError {
    fn new(expected: &Token<'a>, got: &Token<'a>) -> Box<ParserError> {
        Box::new(ParserError {
            expected: expected.to_string(),
            got: got.to_string(),
        })
    }

    fn new_from_strings(expected: String, got: String) -> Box<ParserError> {
        Box::new(ParserError { expected, got })
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid syntax. expected token {}", self.expected)
    }
}

impl Error for ParserError {}

/// The parser is responsible
/// from translating from a series
/// of tokens to actual ASTs nodes.
struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
}

impl<'a> Parser<'a> {
    // Create a new parser.
    // Depends on a lexer capable of iterating over
    // the tokens.
    pub fn new(mut lexer: lexer::Lexer<'a>) -> Parser {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            current_token,
            peek_token,
        }
    }

    // Read the next token and return the current one.
    fn next_token(&mut self) -> Token<'a> {
        // mem::replace is pretty useful
        // allows the borrow checker to relax a bit, enjoy life!
        let next = mem::replace(&mut self.peek_token, self.lexer.next_token());
        mem::replace(&mut self.current_token, next)
    }

    // Parses a program.
    pub fn parse_program(&mut self) -> Result<ast::Program<'a>> {
        let mut statements = Vec::new();
        while self.current_token != Token::EOF {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
            self.next_token();
        }

        Ok(ast::Program { statements })
    }

    // Parses a single statament, which should be found
    // at the current token.
    fn parse_statement(&mut self) -> Result<ast::Statement<'a>> {
        match &self.current_token {
            Token::Let => self.parse_let_stmt().map(|stmt| ast::Statement::Let(stmt)),
            Token::Return => self
                .parse_return_stmt()
                .map(|stmt| ast::Statement::Return(stmt)),
            t => self
                .parse_expression_statement()
                .map(|stmt| ast::Statement::Expression(stmt)),
        }
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Result<ast::Expression<'a>> {
        let expr = self.prefix_parse_fn()?;
        Ok(expr)
    }

    fn prefix_parse_fn(&mut self) -> Result<ast::Expression<'a>> {
        match &self.current_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_int(),
            t => Err(ParserError::new_from_strings(
                "a valid token".to_string(),
                t.to_string(),
            )),
        }
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression<'a>> {
        Ok(ast::Expression::Literal(ast::LiteralNode {
            token: self.current_token.clone(),
        }))
    }

    fn parse_int(&mut self) -> Result<ast::Expression<'a>> {
        let int_token = self.current_token.clone();
        let n = match int_token {
            Token::Int(n) => match n.parse::<i64>() {
                Ok(n) => n,
                Err(e) => return Err(ParserError::new(&Token::Int("?"), &self.current_token)),
            },
            _ => return Err(ParserError::new(&Token::Int("?"), &self.current_token)),
        };
        Ok(ast::Expression::Integer(ast::IntegerNode {
            token: int_token,
            int: n,
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatementNode<'a>> {
        let token = self.current_token.clone();
        let expr = self.parse_expression(OperatorPrecedence::Lowest).unwrap();
        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }
        return Ok(ast::ExpressionStatementNode { token, expr });
    }

    // Parses a return statements, which sould start at the next token
    fn parse_return_stmt(&mut self) -> Result<ast::ReturnNode<'a>> {
        let return_token = self.next_token();

        // for now, ignore the RHS of the return statement
        while self.current_token != Token::Semicolon {
            self.next_token();
        }
        Ok(ast::ReturnNode {
            token: return_token,
            value: ast::Expression::Literal(ast::LiteralNode {
                token: Token::Int("10"),
            }),
        })
    }

    // Parses a let statement, which should start at the next token.
    fn parse_let_stmt(&mut self) -> Result<ast::LetNode<'a>> {
        let let_token = self.next_token();

        // if the current toxen is not an ident, wrong syntax!
        match &self.current_token {
            Token::Ident(_) => {}
            t => return Err(ParserError::new(&Token::Ident("?"), &t)),
        }

        let ident_token = self.next_token();

        // if the current token is not now an assign, wrong syntax!
        match &self.current_token {
            Token::Assign => {}
            t => return Err(ParserError::new(&Token::Assign, &t)),
        }

        // for now, ignore the RHS of the let statement
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        return Ok(ast::LetNode {
            token: let_token,
            name: ident_token.to_string(),
            // TODO: clearly hardcoding expression
            value: ast::Expression::Literal(ast::LiteralNode {
                token: lexer::Token::Int("5"),
            }),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_parser(input: &str) -> Parser {
        let l = lexer::Lexer::new(input);
        Parser::new(l)
    }

    #[test]
    fn test_next_token() {
        let mut p = make_parser("let answer = 42;");
        assert_eq!(p.current_token, Token::Let);
        p.next_token();
        assert_eq!(p.current_token, Token::Ident("answer"));
    }

    #[test]
    fn test_let_stmt() {
        let mut p = make_parser(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );
        let program = p.parse_program();
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        let expected_stmts = vec![
            ast::LetNode {
                token: Token::Let,
                name: "x".to_string(),
                value: ast::Expression::Literal(ast::LiteralNode {
                    token: Token::Int("5"),
                }),
            },
            ast::LetNode {
                token: Token::Let,
                name: "y".to_string(),
                value: ast::Expression::Literal(ast::LiteralNode {
                    token: Token::Int("10"),
                }),
            },
            ast::LetNode {
                token: Token::Let,
                name: "foobar".to_string(),
                value: ast::Expression::Literal(ast::LiteralNode {
                    token: Token::Int("838383"),
                }),
            },
        ];
        for (stmt, expected) in program.statements.into_iter().zip(expected_stmts) {
            match stmt {
                ast::Statement::Let(node) => {
                    assert_eq!(node.name, expected.name);
                    assert_eq!(node.token, expected.token);
                }
                _ => panic!("expected only statements"),
            }
        }
    }

    #[test]
    fn test_gives_error() {
        let mut p = make_parser("let x 5");
        let program = p.parse_program();
        assert!(program.is_err());
        let err = program.unwrap_err().to_string();
        assert_eq!("invalid syntax. expected token =", err)
    }

    #[test]
    fn test_parse_returns() {
        let mut p = make_parser(
            "
return 5;
return 10;
return add(15);
",
        );
        let program = p.parse_program();
        assert!(program.is_ok());
        assert_eq!(program.unwrap().statements.len(), 3);
    }

    #[test]
    fn test_parse_expression_stmts() {
        let mut p = make_parser(
            "
foobar
",
        );
        let program = p.parse_program();
        assert!(program.is_ok());
        let program = program.unwrap();
        let stmt = program.statements.get(0).unwrap();
        match stmt {
            ast::Statement::Expression(n) => assert_eq!(n.token, Token::Ident("foobar")),
            _ => panic!("expected stmt expression"),
        }
    }
}
