#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<FunctionDefinition>,
}

impl Program {
    pub fn new(statements: Vec<FunctionDefinition>) -> Self {
        Self {
            statements,
        }
    }

    pub fn push(&mut self, statement: FunctionDefinition) {
        self.statements.push(statement);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub var_map: HashMap<String, i8>,
    pub stack_offset: i8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub function_name: String,
    pub instructions: Vec<Instruction>,
    pub context: Context,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    I64,
    U32,
    U64,
    Bool,
    Fn(Vec<Type>, Box<Type>),
    Identifier(String),
}

impl FunctionDefinition {
    pub fn new(function_name: String, instructions: Vec<Instruction>, context: Context, return_type: Type) -> Self {
        Self {
            function_name,
            instructions,
            context,
            return_type,
        }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Add(BinOp),
    Sub(BinOp),
    Nor(BinOp),
    And(BinOp),
    Xor(BinOp),
    Rsh(UnaryOp),
    Ldi(UnaryOp),
    Adi(UnaryOp),
    Label(String),
    Jump(String),
    JumpCC(CondCode, String),
    Call(String),
    Ret,
    Lod(BinOp),
    Str(BinOp),
    Cmp(UnaryOp),
    Mov(UnaryOp),
    Lsh(UnaryOp),
    Inc(UnaryOp),
    Dec(UnaryOp),
    Not(UnaryOp),

    Push(Operand),
    Pop(Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondCode {
    E,
    NE,
    GE,
    L,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub a: Operand,
    pub b: Operand,
    pub dest: Operand,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub operand: Operand,
    pub dest: Operand,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Reg),
    Immediate(i8),
    Memory(u8),
    Pseudo(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    RSP, // aka r14
    RBP, // aka r15
}

impl Operand {
    pub fn displ(&self) -> String {
        match self {
            Operand::Register(reg) =>  format!("{}", *reg as usize),
            Operand::Immediate(imm) => format!("{}", imm),
            Operand::Memory(idx) => format!("{}", idx),
            Operand::Pseudo(ident) => format!("{}", ident),
        }
    }
}