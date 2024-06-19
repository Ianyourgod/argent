#![allow(dead_code)]

#[derive(Debug, Clone)]
pub enum Statement {
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    BreakStatement,
    ContinueStatement,
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    Compound(CompoundStatement),
    Empty,
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
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub function_name: String,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub kind: String, // just int rn
    pub ident: Identifier,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct CompoundStatement {
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    Assignment(Identifier, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
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

// TODO: improve line stuff for errors