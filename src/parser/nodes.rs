#![allow(dead_code)]

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub function_definitions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    BreakStatement(String),
    ContinueStatement(String),
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
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub function_name: String,
    pub params: Vec<FunctionArg>,
    pub body: Box<Statement>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArg {
    pub kind: Type,
    pub ident: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub kind: Type,
    pub ident: Identifier,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    pub statements: Vec<Box<Statement>>,
}

// allow debug clone and equal (compare? wtv its called)
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal, Option<Type>),
    Var(Identifier, Option<Type>),
    BinOp(Box<Expression>, BinOp, Box<Expression>, Option<Type>),
    UnaryOp(UnaryOp, Box<Expression>, Option<Type>),
    Assignment(Identifier, Box<Expression>, Option<Type>),
    FunctionCall(String, Vec<Box<Expression>>, Option<Type>),
    Cast(Box<Expression>, Option<Type>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    GenericInt(i32),
    GenericNumber(i32),
    I64(i64),
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::GenericInt(_) => Type::GenericInt,
            Literal::GenericNumber(_) => Type::GenericNumber,
            Literal::I64(_) => Type::I64,
        }
    }

    pub fn as_i32(&self) -> i32 {
        match self {
            Literal::GenericInt(val) => *val,
            Literal::GenericNumber(val) => *val,
            Literal::I64(val) => *val as i32,
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Literal::GenericInt(val) => *val as i64,
            Literal::GenericNumber(val) => *val as i64,
            Literal::I64(val) => *val,
        }
    }
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
    Modulo,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    GenericNumber,
    GenericInt,
    I32,
    I64,
    Fn(Vec<Type>, Box<Type>),
    //Identifier(Identifier)
}

// TODO: improve line stuff for errors