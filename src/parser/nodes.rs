#![allow(dead_code)]

#[derive(Debug, Clone)]
pub struct StatementList {
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub function_name: String,
    pub body: StatementList,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    StatementList(StatementList),
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    BitwiseComplement,
    LogicalNegation,
    Negation,
}