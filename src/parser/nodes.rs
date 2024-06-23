#![allow(dead_code)]

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub return_value: Box<Expression>,
}

#[derive(Debug, Clone,PartialEq )]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub function_name: String,
    pub params: Vec<FunctionArg>,
    pub body: Box<Statement>,
    pub return_type: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArg {
    pub kind: String,
    pub ident: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub kind: String, // just int rn
    pub ident: Identifier,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    pub statements: Vec<Box<Statement>>,
}

// allow debug clone and equal (compare? wtv its called)
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    Assignment(Identifier, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Box<Expression>>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    BitwiseComplement,
    LogicalNegation,
    Negation,
    Reference,
}

// TODO: improve line stuff for errors