#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Display;

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
    Mov(BinOp),
    Movsx(Operand, Operand),
    Ret,
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(Operand),
    Pop(UnaryOp),
    Add(BinOp),
    Sub(BinOp),
    Mul(BinOp),
    IDiv(UnaryOp),
    Div(UnaryOp),
    Neg(UnaryOp),
    Cdq(Suffix),
    Label(String),
    Cmp(BinOp),
    Jump(String),
    JumpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Call(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub dest: Operand,
    pub src: Operand,
    pub suffix: Suffix,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub operand: Operand,
    pub suffix: Suffix,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Reg),
    Immediate(i64),
    StackAllocate(isize),
    Pseudo(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11
}

#[derive(Debug, Clone, PartialEq)]
pub enum Suffix {
    B,
    W,
    L,
    Q,
}

impl Display for Suffix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> core::fmt::Result {
        match self {
            Suffix::B => write!(f, "b"),
            Suffix::W => write!(f, "w"),
            Suffix::L => write!(f, "l"),
            Suffix::Q => write!(f, "q"),
        }
    }
}

fn reg_to_str(reg: Reg, size: Option<u8>) -> String {
    format!("%{}", match reg {
        Reg::AX => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "al",
                    2 => "ax",
                    4 => "eax",
                    8 => "rax",
                    _ => panic!(),
                }
            } else {
                "eax"
            }
        },
        Reg::CX => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "cl",
                    2 => "cx",
                    4 => "ecx",
                    8 => "rcx",
                    _ => panic!(),
                }
            } else {
                "ecx"
            }
        },
        Reg::DX => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "dl",
                    2 => "dx",
                    4 => "edx",
                    8 => "rdx",
                    _ => panic!(),
                }
            } else {
                "edx"
            }
        },
        Reg::DI => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "dil",
                    2 => "di",
                    4 => "edi",
                    8 => "rdi",
                    _ => panic!(),
                }
            } else {
                "edi"
            }
        },
        Reg::SI => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "sil",
                    2 => "si",
                    4 => "esi",
                    8 => "rsi",
                    _ => panic!(),
                }
            } else {
                "esi"
            }
        },
        Reg::R8 => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "r8b",
                    2 => "r8w",
                    4 => "r8d",
                    8 => "r8",
                    _ => panic!(),
                }
            } else {
                "r8d"
            }
        },
        Reg::R9 => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "r9b",
                    2 => "r9w",
                    4 => "r9d",
                    8 => "r9",
                    _ => panic!(),
                }
            } else {
                "r9d"
            }
        },
        Reg::R10 => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "r10b",
                    2 => "r10w",
                    4 => "r10d",
                    8 => "r10",
                    _ => panic!(),
                }
            } else {
                "r10d"
            }
        },
        Reg::R11 => {
            if size.is_some() {
                match size.unwrap() {
                    1 => "r11b",
                    2 => "r11w",
                    4 => "r11d",
                    8 => "r11",
                    _ => panic!(),
                }
            } else {
                "r11d"
            }
        },
    })
}

impl Operand {
    pub fn displ(&self, size: Option<u8>) -> String {
        match self {
            Operand::Register(reg) => reg_to_str(*reg, size),
            Operand::Immediate(imm) => format!("${}", imm),
            Operand::StackAllocate(idx) => format!("-{}({})", idx, "%rbp"),
            Operand::Pseudo(ident) => format!("%{}", ident.name),
        }
    }
}