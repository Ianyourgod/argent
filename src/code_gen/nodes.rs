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
    Label(String),
    Cmp(BinOp),
    Jump(String),
    JumpCC(CondCode, String),
    SetCC(CondCode, Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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

impl Operand {
    pub fn displ(&self, size: Option<i8>) -> String {
        match self {
            Operand::Register(reg) => {
                let reg = match reg {
                    Reg::AX => "ax",
                    Reg::DX => "edx",
                    Reg::R10 => "r10d",
                    Reg::R11 => "r11d",
                };
                if reg == "ax" {
                    if size.is_some() {
                        match size.unwrap() {
                            1 => format!("%al"),
                            2 => format!("%ax"),
                            4 => format!("%eax"),
                            8 => format!("%rax"),
                            _ => panic!(),
                        }
                    } else {
                        format!("%e{}", reg)
                    }
                } else {
                    format!("%{}", reg)
                }
            },
            Operand::Immediate(imm) => format!("${}", imm),
            Operand::StackAllocate(idx) => format!("-{}({})", idx, "%rbp"),
            Operand::Pseudo(ident) => format!("%{}", ident.name),
        }
    }
}