#![allow(dead_code)]

use crate::code_gen;

pub struct Emitter {
    pub program: code_gen::nodes::Program,
}

impl Emitter {
    pub fn new(program: code_gen::nodes::Program) -> Self {
        Self {
            program,
        }
    }

    pub fn emit(&self) -> String {
        let mut output = ".globl main\n\n".to_string();

        for function in &self.program.statements {
            output.push_str(&format!("{}:\n", function.function_name));
            output.push_str("    push %rbp\n");
            output.push_str("    mov %rsp, %rbp\n");
            for instruction in &function.instructions {
                match instruction {
                    code_gen::nodes::Instruction::Mov(mov) => {
                        output.push_str(&format!("    mov{} {}, {}\n", if mov.suffix.is_some() {mov.suffix.clone().unwrap()} else {String::new()}, mov.src, mov.dest));
                    }
                    code_gen::nodes::Instruction::Ret => {
                        output.push_str("    leave\n");
                        output.push_str("    ret\n");
                    }
                    code_gen::nodes::Instruction::Push(push) => {
                        output.push_str(&format!("    push{} {}\n", if push.suffix.is_some() {push.suffix.clone().unwrap()} else {String::new()}, push.operand));
                    }
                    code_gen::nodes::Instruction::Pop(pop) => {
                        output.push_str(&format!("    pop{} {}\n", if pop.suffix.is_some() {pop.suffix.clone().unwrap()} else {String::new()}, pop.operand));
                    }
                    code_gen::nodes::Instruction::Add(add) => {
                        output.push_str(&format!("    add{} {}, {}\n", if add.suffix.is_some() {add.suffix.clone().unwrap()} else {String::new()}, add.src, add.dest));
                    }
                    code_gen::nodes::Instruction::Sub(sub) => {
                        output.push_str(&format!("    sub{} {}, {}\n", if sub.suffix.is_some() {sub.suffix.clone().unwrap()} else {String::new()}, sub.src, sub.dest));
                    }
                    code_gen::nodes::Instruction::Neg(neg) => {
                        output.push_str(&format!("    neg{} {}\n", if neg.suffix.is_some() {neg.suffix.clone().unwrap()} else {String::new()}, neg.dest));
                    }
                    code_gen::nodes::Instruction::AllocateStack(allocate_stack) => {
                        output.push_str(&format!("    sub ${}, %rsp\n", allocate_stack));
                    }
                    _ => panic!()
                }
            }
            output.push_str("    leave\n");
            output.push_str("    xor %eax, %eax\n"); // zero out %eax
            output.push_str("    ret\n");
        }

        output
    }
}