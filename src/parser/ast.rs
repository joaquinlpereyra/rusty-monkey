//! The AST module defines all the nodes
//! and a convenience enum to pattern match
//! when you are expecting a node.
use super::lexer::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression<'a> {
    Literal(LiteralNode<'a>),
    Integer(IntegerNode<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement<'a> {
    Let(LetNode<'a>),
    Return(ReturnNode<'a>),
    Expression(ExpressionStatementNode<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
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
