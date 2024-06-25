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

    fn get_size_of_suffix(&self, suffix: &Option<String>) -> i8 {
        match suffix {
            Some(s) => {
                match s.as_str() {
                    "b" => 1,
                    "w" => 2,
                    "l" => 4,
                    "q" => 8,
                    _ => panic!()
                }
            }
            None => 4
        }
    }

    fn displ_op(&self, operand: &code_gen::nodes::Operand, suffix: &Option<String>) -> String {
        operand.displ(Some(self.get_size_of_suffix(suffix)))
    }

    fn get_suffix(&self, suffix: &Option<String>) -> String {
        if suffix.is_some() {suffix.clone().unwrap()} else {String::new()}
    }

    fn cond_code_to_str(&self, cond_code: &code_gen::nodes::CondCode) -> String {
        match cond_code {
            code_gen::nodes::CondCode::E => "e",
            code_gen::nodes::CondCode::NE => "ne",
            code_gen::nodes::CondCode::G => "g",
            code_gen::nodes::CondCode::GE => "ge",
            code_gen::nodes::CondCode::L => "l",
            code_gen::nodes::CondCode::LE => "le",
        }.to_string()
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
                        output.push_str(&format!("    mov{} {}, {}\n", if mov.suffix.is_some() {mov.suffix.clone().unwrap()} else {String::new()}, self.displ_op(&mov.src, &mov.suffix), self.displ_op(&mov.dest, &mov.suffix)));
                    }
                    code_gen::nodes::Instruction::Ret => {
                        output.push_str("    leave\n");
                        output.push_str("    ret\n");
                    }
                    code_gen::nodes::Instruction::Push(push) => {
                        output.push_str(&format!("    push{} {}\n", self.get_suffix(&push.suffix), self.displ_op(&push.operand, &push.suffix)));
                    }
                    code_gen::nodes::Instruction::Pop(pop) => {
                        output.push_str(&format!("    pop{} {}\n", self.get_suffix(&pop.suffix), self.displ_op(&pop.operand, &pop.suffix)));
                    }
                    code_gen::nodes::Instruction::Add(add) => {
                        output.push_str(&format!("    add{} {}, {}\n", self.get_suffix(&add.suffix), self.displ_op(&add.src, &add.suffix), self.displ_op(&add.dest, &add.suffix)));
                    }
                    code_gen::nodes::Instruction::Sub(sub) => {
                        output.push_str(&format!("    sub{} {}, {}\n", self.get_suffix(&sub.suffix), self.displ_op(&sub.src, &sub.suffix), self.displ_op(&sub.dest, &sub.suffix)));
                    }
                    code_gen::nodes::Instruction::Mul(mul) => {
                        output.push_str(&format!("    imul{} {}, {}\n", self.get_suffix(&mul.suffix), self.displ_op(&mul.src, &mul.suffix), self.displ_op(&mul.dest, &mul.suffix)));
                    }
                    code_gen::nodes::Instruction::Div(div) => {
                        output.push_str(&format!("    idiv{} {}\n", self.get_suffix(&div.suffix), self.displ_op(&div.operand, &div.suffix)));
                    }
                    code_gen::nodes::Instruction::Neg(neg) => {
                        output.push_str(&format!("    neg{} {}\n", self.get_suffix(&neg.suffix), self.displ_op(&neg.operand, &neg.suffix)));
                    }
                    code_gen::nodes::Instruction::Cdq => {
                        output.push_str("    cdq\n");
                    }
                    code_gen::nodes::Instruction::AllocateStack(allocate_stack) => {
                        output.push_str(&format!("    sub ${}, %rsp\n", allocate_stack));
                    }
                    code_gen::nodes::Instruction::Cmp(cmp) => {
                        output.push_str(&format!("    cmp{} {}, {}\n", self.get_suffix(&cmp.suffix), self.displ_op(&cmp.src, &cmp.suffix), self.displ_op(&cmp.dest, &cmp.suffix)));
                    }
                    code_gen::nodes::Instruction::Label(label) => {
                        output.push_str(&format!("{}:\n", label));
                    }
                    code_gen::nodes::Instruction::Jump(jump) => {
                        output.push_str(&format!("    jmp {}\n", jump));
                    }
                    code_gen::nodes::Instruction::JumpCC(cond_code, jump) => {
                        output.push_str(&format!("    j{} {}\n", self.cond_code_to_str(cond_code), jump));
                    }
                    code_gen::nodes::Instruction::SetCC(cond_code, operand) => {
                        output.push_str(&format!("    set{} {}\n", self.cond_code_to_str(cond_code), self.displ_op(operand, &Some("b".to_string()))));
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