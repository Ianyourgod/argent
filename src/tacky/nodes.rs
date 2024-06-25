#![allow(dead_code)]

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub function_definitions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub function_name: String,
    pub body: CompoundInstruction,
    pub return_type: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundInstruction {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Return),
    Unary(Unary),
    Binary(Binary),
    Copy(Copy),
    Jump(String),
    JumpIfZero(String, Value),
    JumpIfNotZero(String, Value),
    Label(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Copy {
    pub src: Value,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub return_value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub src: Value,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Complement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub src1: Value,
    pub src2: Value,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Identifier(String),
    Constant(i32),
    Empty,
}