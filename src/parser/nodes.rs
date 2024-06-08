#![allow(dead_code)]

// this is a recursive descent parser

#[derive(Debug)]
pub struct StatementList {
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug)]
pub enum Statement {
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_value: Box<Expression>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub function_name: String,
    pub body: StatementList,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    StatementList(StatementList),
}

#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum UnaryOp {
    BitwiseComplement,
    LogicalNegation,
}