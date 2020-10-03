//! The interpreter module.
//! Will interpret the results of an AST tree
//! produced by monkey::parser.
//! AST nodes can be found in monkey::parser::ast
use super::lexer::Token;
use super::parser::ast;
use std::collections::HashMap;
use std::fmt;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };

// An object represents every construct on the Monkey language.
// Most objects represent a single value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean { value: bool },
    Integer { value: i64 },
    Return { value: Box<Object> },
    Error { msg: String },
    Null,
}

impl Object {
    fn type_str(&self) -> &str {
        match self {
            Object::Boolean { value: _ } => "BOOLEAN",
            Object::Integer { value: _ } => "INTEGER",
            Object::Return { value: _ } => "RETURN",
            Object::Error { msg: _ } => "ERR",
            Object::Null => "NULL",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match &self {
            Object::Boolean { value } => format!("{}", value),
            Object::Integer { value } => format!("{}", value),
            Object::Return { value } => format! {"return {}", value},
            Object::Error { msg } => format!("{}", msg),
            Object::Null => "null".to_string(),
        };
        write!(f, "{}", s)
    }
}

fn err_type_mismatch(lhs: &Object, op: &Token, rhs: &Object) -> Object {
    Object::Error {
        msg: format!(
            "type mismatch: {} {} {}",
            lhs.type_str(),
            op,
            rhs.type_str()
        ),
    }
}

fn err_unknown_op(lhs: &Object, op: &Token, rhs: &Object) -> Object {
    Object::Error {
        msg: format!(
            "unknown operator: {} {} {}",
            lhs.type_str(),
            op,
            rhs.type_str()
        ),
    }
}

fn err_identifier_not_found(ident: &Token) -> Object {
    Object::Error {
        msg: format!("identifier not found: {}", ident),
    }
}

struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(&mut self, ident: &Token) -> Option<Object> {
        let name = match ident {
            Token::Ident(s) => s,
            _ => unreachable!("calling get on non ident token: {}", ident),
        };
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, ident: &str, obj: Object) {
        let ident = ident.to_owned();
        self.store.insert(ident, obj);
    }
}

/// Evaluates a program.
/// # Example
/// ```
/// use monkey::lexer::Lexer;
/// use monkey::parser::Parser;
/// use monkey::interpreter::{Interpreter, Object};
/// let tokens = Lexer::new(String::from("5;"));
/// let ast = Parser::new(tokens).parse_program().unwrap();
/// let result = Interpreter::new().eval(&ast);
/// assert_eq!(result, Object::Integer{value: 5});
/// ```
pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { env: Env::new() }
    }

    fn new_with_env(env: Env) -> Interpreter {
        Interpreter { env }
    }

    // Evaluates a program, consuming the interpreter.
    pub fn eval(&mut self, program: &ast::Program) -> Object {
        match self.eval_stmts(&program.statements) {
            Object::Return { value } => *value,
            err @ Object::Error { .. } => err,
            obj => obj,
        }
    }

    fn eval_stmts(&mut self, stmts: &Vec<ast::Statement>) -> Object {
        let mut result = NULL;
        for stmt in stmts {
            result = match self.eval_stmt(&stmt) {
                obj @ Object::Return { .. } | obj @ Object::Error { .. } => return obj,
                obj => obj,
            }
        }
        result
    }

    fn eval_stmt(&mut self, stmt: &ast::Statement) -> Object {
        match stmt {
            ast::Statement::Expression(node) => self.eval_expr(&node.expr),
            ast::Statement::Block(node) => self.eval_stmts(&node.stmts),
            ast::Statement::Return(node) => match self.eval_expr(&node.value) {
                o @ Object::Error { .. } => o,
                o => Object::Return { value: Box::new(o) },
            },
            ast::Statement::Let(node) => match self.eval_expr(&node.value) {
                o @ Object::Error { .. } => o,
                // o => Object::Return { value: Box::new(o) },
                o => {
                    self.env.set(&node.name, o);
                    NULL
                }
            },
            ast::Statement::ForLoop(node) => {
                let range = self.eval_expr(&node.range);
                if let Object::Integer { value } = range {
                    for _i in 0..value {
                        self.env.set(&node.ident, Object::Integer { value: _i });
                        self.eval_stmt(&node.body);
                    }
                    NULL
                } else {
                    Object::Error {
                        msg: "range is not an integer".to_owned(),
                    }
                }
            }
        }
    }

    fn eval_expr(&mut self, expr: &ast::Expression) -> Object {
        match expr {
            ast::Expression::Integer(node) => Object::Integer { value: node.int },
            ast::Expression::Boolean(node) if node.value => TRUE,
            ast::Expression::Boolean(_) => FALSE,
            ast::Expression::Prefix(node) => match self.eval_expr(&node.expr) {
                e @ Object::Error { .. } => e,
                o => self.eval_prefix_expr(&node.op, o),
            },
            ast::Expression::IfElse(node) => match self.eval_expr(&node.condition) {
                e @ Object::Error { .. } => e,
                cond => match (Interpreter::truthy(cond), &node.alternative) {
                    (true, _) => self.eval_stmt(&node.then),
                    (false, Some(stmt)) => self.eval_stmt(&stmt),
                    (false, None) => NULL,
                },
            },
            ast::Expression::Binary(node) => {
                match (self.eval_expr(&node.lhs), self.eval_expr(&node.rhs)) {
                    (e @ Object::Error { .. }, _) | (_, e @ Object::Error { .. }) => e,
                    (lhs, rhs) => self.eval_binary_expr(lhs, &node.op, rhs),
                }
            }
            ast::Expression::Literal(node) => match self.env.get(&node.token) {
                None => err_identifier_not_found(&node.token),
                Some(obj) => obj,
            },
            t => unimplemented!("{}", t.to_string()),
        }
    }

    fn truthy(obj: Object) -> bool {
        match obj {
            Object::Boolean { value } => value,
            Object::Integer { value } => value != 0,
            _ => false,
        }
    }

    fn eval_prefix_expr(&self, t: &Token, obj: Object) -> Object {
        match t {
            Token::Not => match obj {
                Object::Boolean { value: v } if v => FALSE,
                Object::Boolean { value: _ } => TRUE,
                Object::Null => TRUE,
                _ => FALSE,
            },
            Token::Minus => match obj {
                Object::Integer { value: v } => Object::Integer { value: -v },
                _ => err_type_mismatch(&Object::Null, t, &obj),
            },
            _ => unreachable!("there's a problem with the parser!"),
        }
    }

    fn eval_binary_expr(&self, lhs: Object, op: &Token, rhs: Object) -> Object {
        if lhs.type_str() != rhs.type_str() {
            return err_type_mismatch(&lhs, op, &rhs);
        }
        match (&lhs, &rhs) {
            (Object::Integer { value: l }, Object::Integer { value: r }) => match op {
                Token::Minus => Object::Integer { value: l - r },
                Token::Plus => Object::Integer { value: l + r },
                Token::Asterisk => Object::Integer { value: l * r },
                Token::Slash => Object::Integer { value: l / r },
                Token::EQ => Object::Boolean { value: l == r },
                Token::NotEQ => Object::Boolean { value: l != r },
                Token::GT => Object::Boolean { value: l > r },
                Token::LT => Object::Boolean { value: l < r },
                _ => err_unknown_op(&lhs, op, &rhs),
            },
            (Object::Boolean { value: l }, Object::Boolean { value: r }) => match op {
                Token::EQ => Object::Boolean { value: l == r },
                Token::NotEQ => Object::Boolean { value: l != r },
                _ => err_unknown_op(&lhs, op, &rhs),
            },
            _ => err_unknown_op(&lhs, op, &rhs),
        }
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
        let l = Lexer::new(case.input.to_owned());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut interpreter = Interpreter::new();
        let object = interpreter.eval(&program.unwrap());
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
                _ => panic!(
                    "unexpected object type int, got: {} in case {}",
                    object, case.input
                ),
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

    #[test]
    fn test_return_stmt() {
        let cases = vec![
            Case::new("return 0", 0),
            Case::new("if (10 > 1) {return 10;}", 10),
            Case::new("if (10 > 1) { if (10 > 1) {return 10;} return 1;}", 10),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Integer { value } => assert_eq!(value, case.expected),
                _ => panic!(
                    "unexpected object type integer, got: {} in case {}",
                    object, case.input
                ),
            }
        }
    }

    #[test]
    fn test_error_msg() {
        let cases = vec![
            Case::new("5 + true", "type mismatch: INTEGER + BOOLEAN"),
            Case::new("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            Case::new("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            Case::new("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            Case::new("foobar", "identifier not found: foobar"),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Error { msg } => assert_eq!(msg, case.expected),
                _ => panic!(
                    "did not got error, instead: {} on case {}",
                    object, case.input
                ),
            }
        }
    }

    #[test]
    fn test_let_stmts() {
        let cases = vec![
            Case::new("let a = 5; a", 5),
            Case::new("let a = 5 * 5; a", 25),
            Case::new("let a = 5; let b = 10; b*a;", 50),
            Case::new("let a = 5; let b = 10; let c = a * b + 10; c", 60),
        ];
        for case in cases {
            let object = quick_eval(&case);
            match object {
                Object::Integer { value } => assert_eq!(value, case.expected),
                _ => panic!(
                    "did not got error, instead: {} on case {}",
                    object, case.input
                ),
            }
        }
    }
}
