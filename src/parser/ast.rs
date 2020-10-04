//! The AST module defines all the nodes
//! and a convenience enum to pattern match
//! when you are expecting a node.
use super::lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Literal(LiteralNode),
    Integer(IntegerNode),
    Prefix(PrefixNode),
    Binary(BinaryNode),
    Boolean(BooleanNode),
    IfElse(IfElseNode),
    Fn(FnNode),
    FnCall(FnCallNode),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Literal(n) => n.token.to_string(),
            Expression::Boolean(n) => n.token.to_string(),
            Expression::Integer(n) => n.int.to_string(),
            Expression::Prefix(n) => format!("{}{}", n.op, *n.expr),
            Expression::FnCall(n) => format!(
                "{}({})",
                n.fun,
                n.args
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Expression::Fn(n) => format!(
                "FN ({}) {}",
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
            Expression::Binary(n) => {
                format!("({}) {} ({})", &n.lhs.to_string(), n.op, &n.rhs.to_string(),)
            }
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(LetNode),
    Return(ReturnNode),
    Expression(ExpressionStatementNode),
    Block(BlockNode),
    ForLoop(ForLoopNode),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Statement::Let(n) => format!("LET {} = {}", n.name, n.value),
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
            Statement::ForLoop(n) => format!("FOR {} IN {} {{ {} }}", n.ident, n.range, n.body,),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
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
pub struct LiteralNode {
    pub token: Token,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerNode {
    pub token: Token,
    pub int: i64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetNode {
    pub token: Token,
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnNode {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExpressionStatementNode {
    pub token: Token,
    pub expr: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixNode {
    pub op: Token,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryNode {
    pub op: Token,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BooleanNode {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElseNode {
    pub token: Token,
    pub condition: Box<Expression>,
    pub then: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockNode {
    pub token: Token,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnNode {
    pub token: Token,
    pub params: Vec<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnCallNode {
    pub token: Token,
    pub fun: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ForLoopNode {
    pub token: Token,
    pub range: Expression,
    pub ident: String,
    pub body: Box<Statement>,
}
