//! The main parser module.
//! It has a Parser struct
//! which constructs AST nodes
//! given a lexer.
use super::lexer::{self, Token};
mod ast;
use std::convert::From;
use std::error::Error;
use std::{fmt, mem, result};

// Silly result alias.
type Result<T> = result::Result<T, Box<ParserError>>;

// Represents an optional expression.
// Useful for binary expressions.
type MaybeExpr<'a> = Option<Box<ast::Expression<'a>>>;

enum OperatorPrecedence {
    Lowest,
    Equals,
    LessOrGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl OperatorPrecedence {
    fn into(self) -> i8 {
        match self {
            OperatorPrecedence::Lowest => 0,
            OperatorPrecedence::Equals => 1,
            OperatorPrecedence::LessOrGreater => 2,
            OperatorPrecedence::Sum => 3,
            OperatorPrecedence::Product => 4,
            OperatorPrecedence::Prefix => 5,
            OperatorPrecedence::Call => 6,
        }
    }
}

fn precedence<'a>(t: &Token<'a>) -> OperatorPrecedence {
    match t {
        Token::Slash | Token::Asterisk => OperatorPrecedence::Product,
        Token::Plus | Token::Minus => OperatorPrecedence::Sum,
        Token::EQ | Token::NotEQ => OperatorPrecedence::Equals,
        Token::GT | Token::LT => OperatorPrecedence::LessOrGreater,
        _ => OperatorPrecedence::Lowest,
    }
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

    fn checked_skip(&mut self, expected: Token) -> Result<()> {
        if self.peek_token == expected {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::new(&self.peek_token, &expected))
        }
    }

    // Returns the OperatorPrecedence for the current token.
    fn peek_token_precedence(&self) -> OperatorPrecedence {
        precedence(&self.peek_token)
    }

    // Returns the OperatorPrecedence for the current token.
    fn current_token_precedence(&self) -> OperatorPrecedence {
        precedence(&self.current_token)
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
            _ => self
                .parse_expression_statement()
                .map(|stmt| ast::Statement::Expression(stmt)),
        }
    }

    // Parses the expression at the current token.
    // Takes the precedence of the last scanned token as argument.
    // As default, you shoul pass OperatorPrecedence::Lowest,
    // so any actual precedence takes over.
    fn parse_expression(&mut self, prec: OperatorPrecedence) -> Result<ast::Expression<'a>> {
        let mut expr = self.prefix_parse()?;
        let prev_precedence: i8 = prec.into();
        let mut current_token_precedence = self.current_token_precedence().into();
        while self.current_token != Token::Semicolon && prev_precedence < current_token_precedence {
            expr = self.infix_parse(Some(Box::new(expr)))?;
            current_token_precedence = self.current_token_precedence().into();
        }
        Ok(expr)
    }

    // Parses a prefix expression.
    fn prefix_parse(&mut self) -> Result<ast::Expression<'a>> {
        match &self.current_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_int(),
            Token::True | Token::False => self.parse_bool(),
            Token::Not | Token::Plus | Token::Minus => self.parse_prefix_op(),
            Token::LParen => self.parse_paren_group(),
            Token::If => self.parse_if_expression(),
            // expressions can only start with identifiers,
            // integers, nots, plus or minus!
            t => Err(ParserError::new_from_strings(
                "+, !, -, ( or an identifier or literal".to_string(),
                t.to_string(),
            )),
        }
    }

    // Parses an infix expression.
    // Takes an optional pointer to a Left Hand Side expression.
    // If given, this will return the LHS joined with the RHS
    // Produces an error if the current token is not valid.
    fn infix_parse(&mut self, lhs: MaybeExpr<'a>) -> Result<ast::Expression<'a>> {
        match &self.current_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EQ
            | Token::NotEQ
            | Token::LT
            | Token::GT => self.parse_infix_expr(lhs),
            t => Err(ParserError::new_from_strings(
                "+, -, /, *, ==, !=, <, >".to_string(),
                t.to_string(),
            )),
        }
    }

    // Parses an infix expression, withouth checking if the current token is
    // a valid one.
    fn parse_infix_expr(&mut self, lhs: MaybeExpr<'a>) -> Result<ast::Expression<'a>> {
        let precedence = self.current_token_precedence();
        let op = self.next_token();
        let rhs = match self.parse_expression(precedence) {
            Ok(expr) => Some(Box::new(expr)),
            e => return e,
        };
        let expr = ast::Expression::Binary(ast::BinaryNode { op, lhs, rhs });
        Ok(expr)
    }

    // Parse an if expression.
    // Current token should be positioned at the beginning IF keyword.
    // IF ( condition ) { expr } else { expr }
    // ^
    fn parse_if_expression(&mut self) -> Result<ast::Expression<'a>> {
        let token = self.next_token();
        // skip the (
        match self.next_token() {
            Token::LParen => {}
            t => return Err(ParserError::new(&Token::RParen, &t)),
        }
        let condition = Box::new(self.parse_expression(OperatorPrecedence::Lowest)?);
        // skip the )
        match self.next_token() {
            Token::RParen => {}
            t => return Err(ParserError::new(&Token::RParen, &t)),
        }
        let then = Box::new(self.parse_block_stmt()?);
        let alternative = match self.peek_token {
            Token::Else => {
                // skip to else token
                self.next_token();
                Some(Box::new(self.parse_block_stmt()?))
            }
            _ => None,
        };
        return Ok(ast::Expression::IfElse(ast::IfElseNode {
            token,
            condition,
            then,
            alternative,
        }));
    }

    // Parse a block statement. Current token should be positioned
    // at the beginning left brace.
    // When the function finished, current token will be the rightmost
    // curly brace.
    // { BLOCK }
    // ^       $
    // BEGIN   END
    fn parse_block_stmt(&mut self) -> Result<ast::Statement<'a>> {
        // next token should be {, skip it
        let token = match self.next_token() {
            Token::LBrace => Token::LBrace,
            t => return Err(ParserError::new(&Token::LBrace, &t)),
        };
        let mut stmts = Vec::new();
        while self.current_token != Token::RBrace {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }
        Ok(ast::Statement::Block(ast::BlockNode { token, stmts }))
    }

    // Parses an identifier.
    fn parse_identifier(&mut self) -> Result<ast::Expression<'a>> {
        Ok(ast::Expression::Literal(ast::LiteralNode {
            token: self.next_token(),
        }))
    }

    // Parses a group inside a parenthesis, like (2 + 3)
    // Current token should be pointing to the lefthand (
    // (2 + 3)
    // ^
    fn parse_paren_group(&mut self) -> Result<ast::Expression<'a>> {
        // skip the (
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::Lowest);
        if self.next_token() != Token::RParen {
            return Err(ParserError::new(&Token::RParen, &self.peek_token));
        }
        return expr;
    }

    // Parses a boolean expression
    fn parse_bool(&mut self) -> Result<ast::Expression<'a>> {
        let bool_token = self.next_token();
        let bool_value = match bool_token {
            Token::True => true,
            Token::False => false,
            _ => {
                return Err(ParserError::new_from_strings(
                    format!("{} | {}", Token::True, Token::False),
                    bool_token.to_string(),
                ))
            }
        };
        Ok(ast::Expression::Boolean(ast::BooleanNode {
            token: bool_token,
            value: bool_value,
        }))
    }

    // Parses a prefix operation.
    fn parse_prefix_op(&mut self) -> Result<ast::Expression<'a>> {
        let token = self.next_token();
        let rhs = match self.parse_expression(OperatorPrecedence::Prefix) {
            Ok(expr) => Box::new(expr),
            e => return e,
        };

        Ok(ast::Expression::Prefix(ast::PrefixNode {
            op: token,
            expr: rhs,
        }))
    }

    // Parses an integer literal.
    fn parse_int(&mut self) -> Result<ast::Expression<'a>> {
        let int_token = self.next_token();
        let n = match int_token {
            Token::Int(n) => match n.parse::<i64>() {
                Ok(n) => n,
                Err(_) => return Err(ParserError::new(&Token::Int("?"), &self.current_token)),
            },
            _ => return Err(ParserError::new(&Token::Int("?"), &self.current_token)),
        };
        Ok(ast::Expression::Integer(ast::IntegerNode {
            token: int_token,
            int: n,
        }))
    }

    // Parses an expression statement.
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

    #[test]
    fn test_prefix_operators() {
        let mut p = make_parser(
            "
!5;
-15;
",
        );
        let program = p.parse_program();
        assert!(program.is_ok());
        let stmts = program.unwrap().statements;
        struct Checks<'a> {
            op: &'a str,
            int: i64,
        }
        let expected = vec![Checks { op: "!", int: 5 }, Checks { op: "-", int: 15 }];
        for (stmt, expected) in stmts.into_iter().zip(expected) {
            match stmt {
                ast::Statement::Expression(node) => match node.expr {
                    ast::Expression::Prefix(n) => {
                        assert_eq!(n.op.to_string(), expected.op);
                        match *n.expr {
                            ast::Expression::Integer(n) => assert_eq!(n.int, expected.int),
                            _ => panic!(),
                        }
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_binary_expr() {
        let mut p = make_parser(
            "
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;
",
        );
        let program = p.parse_program();
        assert!(program.is_ok());
        let stmts = program.unwrap().statements;
        struct Checks<'a> {
            op: &'a str,
            lhs: i64,
            rhs: i64,
        }
        let ops = vec!["+", "-", "*", "/", ">", "<", "==", "!="];
        let mut expectations = Vec::new();
        for op in ops {
            expectations.push(Checks {
                op: op,
                lhs: 5,
                rhs: 5,
            })
        }
        for (stmt, expected) in stmts.into_iter().zip(expectations) {
            match stmt {
                ast::Statement::Expression(node) => match node.expr {
                    ast::Expression::Binary(n) => {
                        assert_eq!(n.op.to_string(), expected.op);
                        match n.lhs {
                            Some(boxed) => match *boxed {
                                ast::Expression::Integer(n) => assert_eq!(n.int, expected.lhs),
                                _ => panic!("lhs is not an int!"),
                            },
                            _ => panic!("no lhs!"),
                        }
                        match n.rhs {
                            Some(boxed) => match *boxed {
                                ast::Expression::Integer(n) => assert_eq!(n.int, expected.rhs),
                                _ => panic!("rhs is not an int!"),
                            },
                            _ => panic!("no rhs!"),
                        }
                    }
                    expr => panic!(
                        "all expressions should be binary expressions, got: {:?}",
                        expr
                    ),
                },
                _ => panic!("all statements should be expression statements"),
            }
        }
    }

    #[test]
    fn test_precedences() {
        let mut p = make_parser(
            "-a * b;
            -a * b + c;
            a + b -c * 2;
            2 > 5 == false;
            1 + (2 + 3) + 4;",
        );
        let program = p.parse_program().unwrap();
        let program_string = format!("{}", program);
        assert_eq!(program.statements.len(), 5);
        assert_eq!(
            program_string,
            "((-a) * (b))
(((-a) * (b)) + (c))
(((a) + (b)) - ((c) * (2)))
(((2) > (5)) == (false))
(((1) + ((2) + (3))) + (4))"
        );
    }

    #[test]
    fn test_boolean_expr() {
        let mut p = make_parser("true;false;false;true");
        let program = p.parse_program().unwrap();
        for stmt in program.statements {
            match stmt {
                ast::Statement::Expression(n) => match n.expr {
                    ast::Expression::Boolean(_) => {}
                    _ => panic!("everything should be boolean!"),
                },
                _ => panic!("everything should be a expression stmt!"),
            }
        }
    }

    #[test]
    fn test_if_else_expression_without_alternative() {
        let mut p = make_parser("if (x < y) {x}");
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        if let ast::Statement::Expression(n) = program.statements.get(0).unwrap() {
            if let ast::Expression::IfElse(n) = &n.expr {
                assert_eq!(n.condition.to_string(), "(x) < (y)");
                assert_eq!(
                    n.then.to_string(),
                    "{
\t (x)
}"
                );
                assert!(n.alternative.is_none());
            } else {
                panic!("expected if else expression")
            }
        } else {
            panic!("expected expression statement!")
        }
    }

    fn test_if_else_expression_with_alternative() {
        let mut p = make_parser("if (x < y) {x} else {y}");
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        if let ast::Statement::Expression(n) = program.statements.get(0).unwrap() {
            if let ast::Expression::IfElse(_) = &n.expr {
                assert_eq!(
                    program.to_string(),
                    "IF ((x) < (y)) THEN {
\t x
}
ELSE {
\t y
}
"
                );
            } else {
                panic!("expected if else expression")
            }
        } else {
            panic!("expected expression statement!")
        }
    }
}