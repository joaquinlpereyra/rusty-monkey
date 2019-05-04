//! The interpreter module.
//! Will interpret the results of an AST tree
//! produced by monkey::parser.
//! AST nodes can be found in monkey::parser::ast
use super::lexer::Token;
use super::parser::ast;
use std::fmt;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };

// An object represents every construct on the Monkey language.
// Most objects represent a single value.
#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean { value: bool },
    Integer { value: i64 },
    Return { value: Box<Object> },
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match &self {
            Object::Boolean { value } => format!("{}", value),
            Object::Integer { value } => format!("{}", value),
            Object::Return { value } => format! {"return {}", value},
            Object::Null => "null".to_string(),
        };
        write!(f, "{}", s)
    }
}

/// Evaluates a program.
/// # Example
/// ```
/// use monkey::lexer::Lexer;
/// use monkey::parser::Parser;
/// use monkey::interpreter::{self, Object};
/// let tokens = Lexer::new("5;");
/// let ast = Parser::new(tokens).parse_program().unwrap();
/// let result = interpreter::eval(&ast);
/// assert_eq!(result, Object::Integer{value: 5});
/// ```
pub fn eval(program: &ast::Program) -> Object {
    eval_stmts(&program.statements)
}

fn eval_stmts(stmts: &Vec<ast::Statement>) -> Object {
    stmts.iter().map(eval_stmt).last().unwrap_or(NULL)
}

fn eval_stmt(stmt: &ast::Statement) -> Object {
    match stmt {
        ast::Statement::Expression(node) => eval_expr(&node.expr),
        ast::Statement::Block(node) => eval_stmts(&node.stmts),
        t => unimplemented!("{}", t.to_string()),
    }
}

fn eval_expr(expr: &ast::Expression) -> Object {
    match expr {
        ast::Expression::Integer(node) => Object::Integer { value: node.int },
        ast::Expression::Boolean(node) if node.value => TRUE,
        ast::Expression::Boolean(_) => FALSE,
        ast::Expression::Prefix(node) => eval_prefix_expr(&node.op, eval_expr(&node.expr)),
        ast::Expression::IfElse(node) => {
            match (truthy(eval_expr(&node.condition)), &node.alternative) {
                (true, _) => eval_stmt(&node.then),
                (false, Some(stmt)) => eval_stmt(&stmt),
                (false, None) => NULL,
            }
        }
        ast::Expression::Binary(node) => {
            eval_binary_expr(eval_expr(&node.lhs), &node.op, eval_expr(&node.rhs))
        }
        t => unimplemented!("{}", t.to_string()),
    }
}

fn truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean { value } => value,
        Object::Integer { value } => value != 0,
        Object::Null => false,
    }
}

fn eval_prefix_expr<'a>(t: &Token<'a>, obj: Object) -> Object {
    match t {
        Token::Not => match obj {
            Object::Boolean { value: v } if v => FALSE,
            Object::Boolean { value: _ } => TRUE,
            Object::Null => TRUE,
            _ => FALSE,
        },
        Token::Minus => match obj {
            Object::Integer { value: v } => Object::Integer { value: -v },
            _ => Object::Null,
        },
        _ => Object::Null,
    }
}

fn eval_binary_expr<'a>(lhs: Object, op: &Token, rhs: Object) -> Object {
    match (lhs, rhs) {
        (Object::Integer { value: l }, Object::Integer { value: r }) => match op {
            Token::Minus => Object::Integer { value: l - r },
            Token::Plus => Object::Integer { value: l + r },
            Token::Asterisk => Object::Integer { value: l * r },
            Token::Slash => Object::Integer { value: l / r },
            Token::EQ => Object::Boolean { value: l == r },
            Token::NotEQ => Object::Boolean { value: l != r },
            Token::GT => Object::Boolean { value: l > r },
            Token::LT => Object::Boolean { value: l < r },
            _ => Object::Null,
        },
        (Object::Boolean { value: l }, Object::Boolean { value: r }) => match op {
            Token::EQ => Object::Boolean { value: l == r },
            Token::NotEQ => Object::Boolean { value: l != r },
            _ => Object::Null,
        },
        _ => Object::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::Lexer;
    use super::super::parser::Parser;
    use super::*;

    struct Case<'a, T> {
        input: &'a str,
        expected: T,
    }

    impl<'a, T> Case<'a, T> {
        fn new(input: &'a str, expected: T) -> Case<'a, T> {
            Case { input, expected }
        }
    }

    fn quick_eval<'a, T>(case: &Case<'a, T>) -> Object {
        let l = Lexer::new(case.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let object = eval(&program.unwrap());
        object
    }

    #[test]
    fn test_eval_integers() {
        let cases = vec![
            Case::new("5", 5),
            Case::new("10;", 10),
            Case::new("-10", -10),
            Case::new("5 + 5 + 5 + 5 - 10", 10),
            Case::new("2 * 2 * 2 * 2 * 2", 32),
            Case::new("-50 + 100 + -50", 0),
            Case::new("20 + 2 * -10", 0),
            Case::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Integer { value } => assert_eq!(value, case.expected),
                _ => panic!("unexpected object type int, got: {}", object),
            }
        }
    }

    #[test]
    fn test_eval_bool() {
        let cases = vec![
            Case::new("true;", true),
            Case::new("false", false),
            Case::new("2 > 4", false),
            Case::new("1 == 1", true),
            Case::new("1 != 1", false),
            Case::new("1 < 1", false),
            Case::new("true == true", true),
            Case::new("true != false", true),
            Case::new("(1 < 2) == false", false),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Boolean { value } => assert_eq!(value, case.expected),
                _ => panic!("unexpected object type bool, got: {}", object),
            };
        }
    }

    #[test]
    fn test_bang_operator() {
        let cases = vec![
            Case::new("!true", false),
            Case::new("!false", true),
            Case::new("!5", false),
            Case::new("!!5", true),
            Case::new("!!true", true),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Boolean { value } => assert_eq!(value, case.expected),
                _ => panic!("unexpected object type bool, got: {}", object),
            }
        }
    }

    #[test]
    fn test_if_else_expr() {
        let int_cases = vec![
            Case::new("if (true) { 10 }", 10),
            Case::new("if (false) { 10 } else { 5 } ", 5),
            Case::new("if (1) {10}", 10),
            Case::new("if (1 < 2 ) {10} else {5}", 10),
            Case::new("if (1 > 2 ) {10} else {5}", 5),
        ];
        for case in int_cases {
            let object = quick_eval(&case);
            match object {
                Object::Integer { value } => assert_eq!(value, case.expected),
                _ => panic!("unexpected object type integer, got: {}", object),
            }
        }
        let null_program = quick_eval(&Case::new("if (false) {1}", Object::Null));
        match null_program {
            Object::Null => {}
            _ => panic!("unexpected object type null, got: {}", null_program),
        }
    }
}
