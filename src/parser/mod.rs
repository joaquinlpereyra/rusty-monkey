//! The main parser module.
//! It has a Parser struct
//! which constructs AST nodes
//! given a lexer.
use super::lexer::{self, Token};
pub mod ast;
use std::convert::From;
use std::error::Error;
use std::{fmt, mem, result};

// I would **NOT** use this to learn how to make a parser.
// Just saying.

// Silly result alias.
type Result<T> = result::Result<T, Box<ParserError>>;

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
        Token::LParen => OperatorPrecedence::Call,
        _ => OperatorPrecedence::Lowest,
    }
}

// ParserError is the error returned by the parser
// when it encounters an unexpected token.
#[derive(Debug)]
pub struct ParserError {
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

    fn new_with_expression(expected: &Token<'a>, got: &ast::Expression<'a>) -> Box<ParserError> {
        ParserError::new_from_strings(expected.to_string(), got.to_string())
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
pub struct Parser<'a> {
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

    // Skips the current token if it matched with the expected one,
    // if not, it will produce an error.
    fn checked_skip(&mut self, expected: Token) -> Result<()> {
        if self.current_token == expected {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::new(&expected, &self.peek_token))
        }
    }

    // Skips the current token if it matches what is provided in the
    // is parameter.
    fn skip_if(&mut self, is: Token) {
        if self.current_token == is {
            self.next_token();
        }
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
    // As default, you should pass OperatorPrecedence::Lowest,
    // so any actual precedence takes over.
    fn parse_expression(&mut self, prec: OperatorPrecedence) -> Result<ast::Expression<'a>> {
        let mut expr = self.prefix_parse()?;
        let prev_precedence: i8 = prec.into();
        let mut current_token_precedence = self.current_token_precedence().into();
        while self.current_token != Token::Semicolon && prev_precedence < current_token_precedence {
            expr = self.infix_parse(Box::new(expr))?;
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
            Token::Fn => self.parse_fn(),
            t => Err(ParserError::new_from_strings(
                "+, !, -, (, if, or an identifier or literal".to_string(),
                t.to_string(),
            )),
        }
    }

    // Parses an infix expression.
    // Takes an optional pointer to a Left Hand Side expression.
    // If given, this will return the LHS joined with the RHS
    // Produces an error if the current token is not valid.
    fn infix_parse(&mut self, lhs: Box<ast::Expression<'a>>) -> Result<ast::Expression<'a>> {
        match &self.current_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EQ
            | Token::NotEQ
            | Token::LT
            | Token::GT => self.parse_infix_expr(lhs),
            Token::LParen => self.parse_fn_call(lhs),
            t => Err(ParserError::new_from_strings(
                "+, -, /, *, ==, !=, <, >, or a function call".to_string(),
                t.to_string(),
            )),
        }
    }

    // Parses an infix expression, withouth checking if the current token is
    // a valid one.
    fn parse_infix_expr(&mut self, lhs: Box<ast::Expression<'a>>) -> Result<ast::Expression<'a>> {
        let precedence = self.current_token_precedence();
        let op = self.next_token();
        let rhs = match self.parse_expression(precedence) {
            Ok(expr) => Box::new(expr),
            e => return e,
        };
        let expr = ast::Expression::Binary(ast::BinaryNode { op, lhs, rhs });
        Ok(expr)
    }

    // Parses a call to a function. Will start at function name and finish at last parenthesis.
    // let x = add(2 + 3);
    //         ^          $
    fn parse_fn_call(&mut self, fun: Box<ast::Expression<'a>>) -> Result<ast::Expression<'a>> {
        let token = self.next_token();
        let args = self.parse_fn_args()?;
        Ok(ast::Expression::FnCall(ast::FnCallNode {
            token,
            fun,
            args,
        }))
    }

    // Parse an if expression.
    // Current token should be positioned at the beginning IF keyword.
    // The current token will be the rightmost } at the end of the function.
    // IF ( condition ) { expr } else { expr }
    // ^                                     $
    // Else clause is optional!
    // IF ( condition ) { expr }
    // ^                       $
    fn parse_if_expression(&mut self) -> Result<ast::Expression<'a>> {
        let token = self.next_token();
        self.checked_skip(Token::LParen)?;
        let condition = Box::new(self.parse_expression(OperatorPrecedence::Lowest)?);
        self.checked_skip(Token::RParen)?;
        let then = Box::new(self.parse_block_stmt()?);
        let alternative = match self.peek_token {
            Token::Else => {
                self.checked_skip(Token::RBrace)?;
                self.checked_skip(Token::Else)?;
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

    // Parses a function.
    // Current token should be positioned at the FN token.
    // let foo  = fn (x,y) { x + y; }
    //            ^                 $
    fn parse_fn(&mut self) -> Result<ast::Expression<'a>> {
        self.checked_skip(Token::Fn)?;
        self.checked_skip(Token::LParen)?;
        let params = self.parse_fn_args()?;

        // check if someome is triying to define
        // a function with an expression as arguments
        let is_literal = |arg: &&ast::Expression| match arg {
            ast::Expression::Literal(_) => true,
            _ => false,
        };
        if let Some(param) = params.iter().find(|param| !is_literal(param)) {
            return Err(ParserError::new_with_expression(
                &Token::Ident("identifier"),
                &param,
            ));
        }

        let body = Box::new(self.parse_block_stmt()?);
        self.skip_if(Token::RBrace);
        self.skip_if(Token::Semicolon);
        Ok(ast::Expression::Fn(ast::FnNode {
            token: Token::Fn,
            params,
            body,
        }))
    }

    // Parses a function arguments.
    // Current token must be the first argument, and it will end outside
    // the last parenthesis.
    // fn(x, y, z) { something }
    //    ^        $
    fn parse_fn_args(&mut self) -> Result<Vec<ast::Expression<'a>>> {
        let mut args = Vec::new();
        // no arguments for this function
        if let Token::RParen = self.current_token {
            return Ok(args);
        }
        // surely there is at least one argument...
        args.push(self.parse_expression(OperatorPrecedence::Lowest)?);
        // and continue on with the rest...
        while self.current_token == Token::Comma {
            self.checked_skip(Token::Comma)?;
            let arg = self.parse_expression(OperatorPrecedence::Lowest)?;
            args.push(arg);
        }
        self.checked_skip(Token::RParen)?;
        Ok(args)
    }

    // Parse a block statement. Current token should be positioned
    // at the beginning left brace.
    // When the function finished, current token will be the rightmost
    // curly brace.
    // { BLOCK }
    // ^       $
    // BEGIN   END
    fn parse_block_stmt(&mut self) -> Result<ast::Statement<'a>> {
        self.checked_skip(Token::LBrace)?;
        let mut stmts = Vec::new();
        while self.current_token != Token::RBrace {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);

            // a block may have a semicolon before it end
            // skip it!
            self.skip_if(Token::Semicolon);
        }
        // it nevers gets here, error is in parse_statement
        Ok(ast::Statement::Block(ast::BlockNode {
            token: Token::LBrace,
            stmts,
        }))
    }

    // Parses an identifier.
    fn parse_identifier(&mut self) -> Result<ast::Expression<'a>> {
        Ok(ast::Expression::Literal(ast::LiteralNode {
            token: self.next_token(),
        }))
    }

    // Parses a group inside a parenthesis, like (2 + 3)
    // Current token should be pointing to the lefthand (
    // At the end of the function, current token will be
    // the next after the rightmost ).
    // (2 + 3) .
    // ^       $
    fn parse_paren_group(&mut self) -> Result<ast::Expression<'a>> {
        self.checked_skip(Token::LParen)?;
        let expr = self.parse_expression(OperatorPrecedence::Lowest);
        if self.next_token() != Token::RParen {
            return Err(ParserError::new(&Token::RParen, &self.peek_token));
        }
        return expr;
    }

    // Parses a boolean expression
    fn parse_bool(&mut self) -> Result<ast::Expression<'a>> {
        let token = self.next_token();
        let value = match token {
            Token::True => true,
            Token::False => false,
            _ => {
                return Err(ParserError::new_from_strings(
                    format!("{} | {}", Token::True, Token::False),
                    token.to_string(),
                ))
            }
        };
        Ok(ast::Expression::Boolean(ast::BooleanNode { token, value }))
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
        let expr = self.parse_expression(OperatorPrecedence::Lowest)?;
        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }
        return Ok(ast::ExpressionStatementNode { token, expr });
    }

    // Parses a return statement. The current token should be
    // the return keyword.
    fn parse_return_stmt(&mut self) -> Result<ast::ReturnNode<'a>> {
        let token = self.next_token();
        let value = self.parse_expression(OperatorPrecedence::Lowest)?;
        Ok(ast::ReturnNode { token, value })
    }

    // Parses a let statement, which should start at the current token.
    // let x = 2;
    // ^
    fn parse_let_stmt(&mut self) -> Result<ast::LetNode<'a>> {
        let let_token = self.next_token();

        // if the current toxen is not an ident, wrong syntax!
        match &self.current_token {
            Token::Ident(_) => {}
            t => return Err(ParserError::new(&Token::Ident("?"), &t)),
        }

        let ident_token = self.next_token();
        self.checked_skip(Token::Assign)?;
        let value = self.parse_expression(OperatorPrecedence::Lowest)?;

        return Ok(ast::LetNode {
            token: let_token,
            name: ident_token.to_string(),
            value,
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
let y = 10 + 3;
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
                value: ast::Expression::Binary(ast::BinaryNode {
                    op: Token::Plus,
                    lhs: Box::new(ast::Expression::Integer(ast::IntegerNode {
                        token: Token::Int("10"),
                        int: 10,
                    })),
                    rhs: Box::new(ast::Expression::Integer(ast::IntegerNode {
                        token: Token::Int("3"),
                        int: 3,
                    })),
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
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);
        match program.statements.get(0).unwrap() {
            ast::Statement::Return(n) => match &n.value {
                ast::Expression::Integer(n) => {
                    assert_eq!(n.int, 5);
                }
                _ => panic!("expected int!"),
            },
            _ => panic!("expected return stmt!"),
        };
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
                        match *n.lhs {
                            ast::Expression::Integer(n) => assert_eq!(n.int, expected.lhs),
                            _ => panic!("lhs is not an int!"),
                        };
                        match *n.rhs {
                            ast::Expression::Integer(n) => assert_eq!(n.int, expected.rhs),
                            _ => panic!("rhs is not an int!"),
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

    #[test]
    fn test_if_else_expression_with_alternative() {
        let mut p = make_parser("if (x < y) {x} else {y}");
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        if let ast::Statement::Expression(n) = program.statements.get(0).unwrap() {
            if let ast::Expression::IfElse(_) = &n.expr {
                assert_eq!(
                    program.to_string(),
                    "(IF (x) < (y) THEN {
\t (x)
}
ELSE {
\t (y)
})"
                );
            } else {
                panic!("expected if else expression")
            }
        } else {
            panic!("expected expression statement!")
        }
    }

    #[test]
    fn test_fn_literal() {
        let mut p = make_parser("fn(x, y) { x + y; }");
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        match stmt {
            ast::Statement::Expression(n) => match &n.expr {
                ast::Expression::Fn(n) => {
                    assert_eq!(n.params.len(), 2);
                    assert_eq!(
                        n.body.to_string(),
                        "{
\t ((x) + (y))
}"
                    )
                }
                _ => panic!("expected fn lit!"),
                // };
            },
            _ => panic!("expected expression stmt!"),
        }
    }

    #[test]
    fn test_fn_call() {
        let mut p = make_parser("add(1, 2 * 3, 4 + 5);");
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        assert_eq!(
            stmt.to_string(),
            "(add(1
(2) * (3)
(4) + (5)))"
        )
    }
}
