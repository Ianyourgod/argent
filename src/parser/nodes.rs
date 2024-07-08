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
    Generic32(u32), // 0 <-> i32::MAX
    Generic64(u64), // i32::MAX + 1 <-> I64::MAX
    I32(i32), // i32::MIN <-> -1
    I64(i64), // i64::MIN <-> i32::MIN - 1
    U64(u64), // i64::MAX + 1 <-> u64::MAX
    // no u32 as its just in the generic64 range
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Generic32(_) => Type::Generic32,
            Literal::Generic64(_) => Type::Generic64,
            Literal::I32(_) => Type::GenericInt,
            Literal::I64(_) => Type::I64,
            Literal::U64(_) => Type::Generic64,
        }
    }

    pub fn as_i32(&self) -> i32 {
        match self {
            Literal::I32(val) => *val,
            Literal::Generic32(val) => *val as i32,
            _ => panic!("Literal is not an i32")
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Literal::I64(val) => *val,
            Literal::Generic64(val) => *val as i64,
            _ => panic!("Literal is not an i64")
        }
    }

    pub fn as_u32(&self) -> u32 {
        match self {
            Literal::Generic32(val) => *val,
            _ => panic!("Literal is not a u32")
        }
    }

    pub fn as_u64(&self) -> u64 {
        match self {
            Literal::U64(val) => *val,
            Literal::Generic64(val) => *val,
            _ => panic!("Literal is not a u64")
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
    Generic32,
    GenericInt,
    Generic64,
    I32,
    I64,
    U32,
    U64,
    Bool,
    Fn(Vec<Type>, Box<Type>),
    //Identifier(Identifier)
}

// TODO: improve line stuff for errors