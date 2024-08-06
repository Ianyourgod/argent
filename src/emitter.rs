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

    fn cond_code_to_str(&self, cond_code: &code_gen::nodes::CondCode) -> String {
        match cond_code {
            code_gen::nodes::CondCode::E => "e",
            code_gen::nodes::CondCode::NE => "ne",
            code_gen::nodes::CondCode::GE => "ge",
            code_gen::nodes::CondCode::L => "l",
        }.to_string()
    }

    pub fn emit(&self) -> String {
        let mut output = ".globl main\n\n".to_string();

        for function in &self.program.statements {
            output.push_str(&format!(".{}\n", function.function_name));
            output.push_str("    push %rbp\n");
            output.push_str("    mov %rsp, %rbp\n");
            for instruction in &function.instructions {
                match instruction {
                    code_gen::nodes::Instruction::Mov(mov) => {
                        output.push_str(&format!("mov {}, {}\n", mov.operand.displ(), mov.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Ret => {
                        // exit function
                        output.push_str(&format!("lod 15 15 -1\n"));
                        output.push_str("inc 14\n");

                        // actual return
                        output.push_str("ret\n");
                    }
                    code_gen::nodes::Instruction::Push(operand) => {
                        output.push_str(&format!("str 14 {} 0\n", operand.displ()));
                        output.push_str("dec 14\n");
                    }
                    code_gen::nodes::Instruction::Pop(pop) => {
                        output.push_str(&format!("lod 14 {} -1\n", pop.displ()));
                        output.push_str("inc 14\n");
                    }
                    code_gen::nodes::Instruction::Add(add) => {
                        output.push_str(&format!("add {} {} {}\n", add.a.displ(), add.b.displ(), add.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Sub(sub) => {
                        output.push_str(&format!("sub {} {} {}\n", sub.a.displ(), sub.b.displ(), sub.dest.displ()));
                    }
                    /*
                    code_gen::nodes::Instruction::AllocateStack(allocate_stack) => {
                        output.push_str(&format!("    sub ${}, %rsp\n", allocate_stack));
                    }
                    code_gen::nodes::Instruction::DeallocateStack(deallocate_stack) => {
                        output.push_str(&format!("    add ${}, %rsp\n", deallocate_stack));
                    }
                    */
                    code_gen::nodes::Instruction::Nor(nor) => {
                        output.push_str(&format!("nor {} {} {}\n", nor.a.displ(), nor.b.displ(), nor.dest.displ()));
                    }
                    code_gen::nodes::Instruction::And(and) => {
                        output.push_str(&format!("and {} {} {}\n", and.a.displ(), and.b.displ(), and.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Xor(xor) => {
                        output.push_str(&format!("xor {} {} {}\n", xor.a.displ(), xor.b.displ(), xor.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Rsh(rsh) => {
                        output.push_str(&format!("rsh {} {}\n", rsh.operand.displ(), rsh.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Lsh(lsh) => {
                        output.push_str(&format!("lsh {} {}\n", lsh.operand.displ(), lsh.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Ldi(ldi) => {
                        output.push_str(&format!("ldi {} {}\n", ldi.dest.displ(), ldi.operand.displ()));
                    }
                    code_gen::nodes::Instruction::Adi(adi) => {
                        output.push_str(&format!("adi {} {}\n", adi.operand.displ(), adi.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Lod(lod) => {
                        output.push_str(&format!("lod {} {} {}\n", lod.a.displ(), lod.dest.displ(), lod.b.displ()));
                    }
                    code_gen::nodes::Instruction::Str(str) => {
                        output.push_str(&format!("str {} {} {}\n", str.dest.displ(), str.b.displ(), str.a.displ()));
                    }
                    code_gen::nodes::Instruction::Cmp(cmp) => {
                        output.push_str(&format!("cmp {}, {}\n", &cmp.operand.displ(), cmp.dest.displ()));
                    }
                    code_gen::nodes::Instruction::Label(label) => {
                        output.push_str(&format!("{}:\n", label));
                    }
                    code_gen::nodes::Instruction::Jump(jump) => {
                        output.push_str(&format!("jmp {}\n", jump));
                    }
                    code_gen::nodes::Instruction::JumpCC(cond_code, jump) => {
                        output.push_str(&format!("    BRH {} {}\n", self.cond_code_to_str(cond_code), jump));
                    }
                    code_gen::nodes::Instruction::Call(call) => {
                        output.push_str(&format!("    CAL {}\n", call));
                    }
                    code_gen::nodes::Instruction::Inc(inc) => {
                        output.push_str(&format!("inc {}\n", inc.operand.displ()));
                    }
                    code_gen::nodes::Instruction::Dec(dec) => {
                        output.push_str(&format!("dec {}\n", dec.operand.displ()));
                    }
                    code_gen::nodes::Instruction::Not(not) => {
                        output.push_str(&format!("not {} {}\n", not.operand.displ(), not.dest.displ()));
                    }
                    //#[allow(unreachable_patterns)]
                    //_ => panic!()
                }
            }
            output.push_str("    leave\n");
            output.push_str("    xor %eax, %eax\n"); // zero out %eax
            output.push_str("    ret\n");
        }

        output.push_str(".section .note.GNU-stack,\"\",@progbits\n");

        output
    }
}