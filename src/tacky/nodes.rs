#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub function_definitions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub function_name: String,
    pub body: CompoundInstruction,
    pub arguments: Vec<(String, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundInstruction {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Value),
    Unary(Unary),
    Binary(Binary),
    Copy(Copy),
    Jump(String),
    JumpIfZero(String, Value),
    JumpIfNotZero(String, Value),
    Label(String),
    FunCall(FunCall),
    SignExtend(Value, Value),
    Truncate(Value, Value),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunCall {
    pub function_name: String,
    pub arguments: Vec<Value>,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Copy {
    pub src: Value,
    pub dest: Value,
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
    Modulo,
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
    Constant(Constant),
    Empty,
}

impl Value {
    pub fn is_empty(&self) -> bool {
        match self {
            Value::Empty => true,
            _ => false,
        }
    }

    pub fn is_identifier(&self) -> bool {
        match self {
            Value::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn as_identifier(&self) -> &str {
        match self {
            Value::Identifier(val) => val,
            _ => panic!("Value is not an identifier"),
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Value::Constant(_) => true,
            _ => false,
        }
    }

    pub fn as_constant(&self) -> &Constant {
        match self {
            Value::Constant(val) => val,
            _ => panic!("Value is not a constant"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    I32(i32),
    I64(i64),
}

impl Constant {
    pub fn as_i32(&self) -> i32 {
        match self {
            Constant::I32(val) => *val,
            Constant::I64(val) => *val as i32,
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Constant::I64(val) => *val,
            Constant::I32(val) => *val as i64,
        }
    }

    pub fn is_i32(&self) -> bool {
        match self {
            Constant::I32(_) => true,
            _ => false,
        }
    }

    pub fn is_i64(&self) -> bool {
        match self {
            Constant::I64(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    I64,
    Fn(Vec<Type>, Box<Type>),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub table: HashMap<String, Type>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Type) {
        self.table.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Type> {
        self.table.get(key)
    }
}