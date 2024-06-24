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
    pub var_map: HashMap<String, usize>,
    pub stack_offset: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub function_name: String,
    pub instructions: Vec<Instruction>,
    pub context: Context,
    pub return_type: String,
}

impl FunctionDefinition {
    pub fn new(function_name: String, instructions: Vec<Instruction>, context: Context, return_type: String) -> Self {
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
    Mov(BinOp),
    Ret,
    AllocateStack(usize),
    Push(UnaryOp),
    Pop(UnaryOp),
    Add(BinOp),
    Sub(BinOp),
    Mul(BinOp),
    Div(UnaryOp),
    Neg(UnaryOp),
    Cdq,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub dest: Operand,
    pub src: Operand,
    pub suffix: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub operand: Operand,
    pub suffix: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Reg),
    Immediate(i32),
    StackAllocate(usize),
    Pseudo(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> core::fmt::Result {
        match self {
            Operand::Register(reg) => {
                let reg = match reg {
                    Reg::AX => "%eax",
                    Reg::DX => "%edx",
                    Reg::R10 => "%r10d",
                    Reg::R11 => "%r11d",
                };
                write!(f, "{}", reg)
            },
            Operand::Immediate(imm) => write!(f, "${}", imm),
            Operand::StackAllocate(idx) => write!(f, "-{}({})", idx, "%rbp"),
            Operand::Pseudo(ident) => write!(f, "%{}", ident.name),
        }
    }
}