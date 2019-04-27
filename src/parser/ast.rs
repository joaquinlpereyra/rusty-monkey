//! The AST module defines all the nodes
//! and a convenience enum to pattern match
//! when you are expecting a node.
use super::lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression<'a> {
    Literal(LiteralNode<'a>),
    Integer(IntegerNode<'a>),
    Prefix(PrefixNode<'a>),
    Binary(BinaryNode<'a>),
    Boolean(BooleanNode<'a>),
    IfElse(IfElseNode<'a>),
    Fn(FnNode<'a>),
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Literal(n) => n.token.to_string(),
            Expression::Boolean(n) => n.token.to_string(),
            Expression::Integer(n) => n.int.to_string(),
            Expression::Prefix(n) => format!("{}{}", n.op, *n.expr),
            Expression::Fn(n) => format!(
                "fn {} ({}) {}",
                n.token,
                n.params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                n.body,
            ),
            Expression::IfElse(n) => format!(
                "IF {} THEN {}\nELSE {}",
                n.condition,
                n.then,
                match &n.alternative {
                    Some(stmt) => stmt.to_string(),
                    None => "".to_string(),
                }
            ),
            Expression::Binary(n) => format!(
                "({}) {} ({})",
                match &n.lhs {
                    Some(expr) => expr.to_string(),
                    None => "".to_string(),
                },
                n.op,
                match &n.rhs {
                    Some(expr) => expr.to_string(),
                    None => "".to_string(),
                },
            ),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement<'a> {
    Let(LetNode<'a>),
    Return(ReturnNode<'a>),
    Expression(ExpressionStatementNode<'a>),
    Block(BlockNode<'a>),
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Statement::Let(n) => format!("LET {}", n.token),
            Statement::Return(n) => format!("RET {}", n.value),
            Statement::Expression(n) => format!("({})", n.expr),
            Statement::Block(n) => format!(
                "{{\n\t {}\n}}",
                n.stmts
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LiteralNode<'a> {
    pub token: Token<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerNode<'a> {
    pub token: Token<'a>,
    pub int: i64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetNode<'a> {
    pub token: Token<'a>,
    pub name: String,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnNode<'a> {
    pub token: Token<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExpressionStatementNode<'a> {
    pub token: Token<'a>,
    pub expr: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixNode<'a> {
    pub op: Token<'a>,
    pub expr: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryNode<'a> {
    pub op: Token<'a>,
    pub lhs: Option<Box<Expression<'a>>>,
    pub rhs: Option<Box<Expression<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BooleanNode<'a> {
    pub token: Token<'a>,
    pub value: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElseNode<'a> {
    pub token: Token<'a>,
    pub condition: Box<Expression<'a>>,
    pub then: Box<Statement<'a>>,
    pub alternative: Option<Box<Statement<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockNode<'a> {
    pub token: Token<'a>,
    pub stmts: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnNode<'a> {
    pub token: Token<'a>,
    pub params: Vec<Expression<'a>>,
    pub body: Box<Statement<'a>>,
}
