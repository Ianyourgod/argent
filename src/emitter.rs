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
            code_gen::nodes::CondCode::E => "eq",
            code_gen::nodes::CondCode::NE => "ne",
            code_gen::nodes::CondCode::GE => "ge",
            code_gen::nodes::CondCode::L => "lt",
        }.to_string()
    }

    fn get_type(&self, operand: &code_gen::nodes::Operand) -> &str {
        match operand {
            code_gen::nodes::Operand::Register(reg) => "register",
            code_gen::nodes::Operand::Immediate(imm) => "immediate",
            code_gen::nodes::Operand::Memory(mem) => "memory",
            _ => panic!("how the hell")
        }
    }

    fn reg(&self, reg: &code_gen::nodes::Operand) -> String {
        match reg {
            code_gen::nodes::Operand::Register(_) => reg.displ(),
            not_reg => panic!("{} ({}) used as register", not_reg.displ(), self.get_type(not_reg))
        }
    }

    fn imm(&self, imm: &code_gen::nodes::Operand) -> String {
        match imm {
            code_gen::nodes::Operand::Immediate(imm) => format!("{}", imm),
            not_imm => panic!("{} ({}) used as immediate", not_imm.displ(), self.get_type(not_imm))
        }
    }

    fn mem(&self, mem: &code_gen::nodes::Operand) -> String {
        match mem {
            code_gen::nodes::Operand::Memory(mem) => format!("{}", mem),
            not_mem => panic!("{} ({}) used as memory", not_mem.displ(), self.get_type(not_mem))
        }
    }

    pub fn emit(&self) -> String {
        let mut output = "ldi 14 239\nmov 14 15\ncal ..main\nhlt\n\n".to_string();

        for function in &self.program.statements {
            output.push_str(&format!(".{}\n", function.function_name));
            // set up stack frame
            // push base pointer
            output.push_str(&format!("str 14 15 0\n"));
            output.push_str("dec 14\n");
            // move stack pointer to base pointer
            output.push_str("mov 14 15\n");
            for instruction in &function.instructions {
                match instruction {
                    code_gen::nodes::Instruction::Mov(mov) => {
                        output.push_str(&format!("mov {} {}\n", self.reg(&mov.operand), self.reg(&mov.dest)));
                    }
                    code_gen::nodes::Instruction::Ret => {
                        // exit function
                        output.push_str(&format!("lod 15 15 -1\n"));
                        output.push_str("inc 14\n");

                        // actual return
                        output.push_str("ret\n");
                    }
                    code_gen::nodes::Instruction::Push(operand) => {
                        output.push_str(&format!("str 14 {} 0\n", self.reg(operand)));
                        output.push_str("dec 14\n");
                    }
                    code_gen::nodes::Instruction::Pop(pop) => {
                        output.push_str(&format!("lod 14 {} -1\n", self.reg(pop)));
                        output.push_str("inc 14\n");
                    }
                    code_gen::nodes::Instruction::Add(add) => {
                        output.push_str(&format!("add {} {} {}\n", self.reg(&add.a), self.reg(&add.b), self.reg(&add.dest)));
                    }
                    code_gen::nodes::Instruction::Sub(sub) => {
                        output.push_str(&format!("sub {} {} {}\n", self.reg(&sub.a), self.reg(&sub.b), self.reg(&sub.dest)));
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
                        output.push_str(&format!("nor {} {} {}\n", self.reg(&nor.a), self.reg(&nor.b), self.reg(&nor.dest)));
                    }
                    code_gen::nodes::Instruction::And(and) => {
                        output.push_str(&format!("and {} {} {}\n", self.reg(&and.a), self.reg(&and.b), self.reg(&and.dest)));
                    }
                    code_gen::nodes::Instruction::Xor(xor) => {
                        output.push_str(&format!("xor {} {} {}\n", self.reg(&xor.a), self.reg(&xor.b), self.reg(&xor.dest)));
                    }
                    code_gen::nodes::Instruction::Rsh(rsh) => {
                        output.push_str(&format!("rsh {} {}\n", self.reg(&rsh.operand), self.reg(&rsh.dest)));
                    }
                    code_gen::nodes::Instruction::Lsh(lsh) => {
                        output.push_str(&format!("lsh {} {}\n", self.reg(&lsh.operand), self.reg(&lsh.dest)));
                    }
                    code_gen::nodes::Instruction::Ldi(ldi) => {
                        output.push_str(&format!("ldi {} {}\n", self.reg(&ldi.dest), self.imm(&ldi.operand)));
                    }
                    code_gen::nodes::Instruction::Adi(adi) => {
                        output.push_str(&format!("adi {} {}\n", self.reg(&adi.dest), self.imm(&adi.operand)));
                    }
                    code_gen::nodes::Instruction::Lod(lod) => {
                        output.push_str(&format!("lod {} {} -{}\n", self.reg(&lod.a), self.reg(&lod.dest), self.mem(&lod.b)));
                    }
                    code_gen::nodes::Instruction::Str(_str) => {
                        output.push_str(&format!("str {} {} -{}\n", self.reg(&_str.dest), self.reg(&_str.a), self.mem(&_str.b)));
                    }
                    code_gen::nodes::Instruction::Cmp(cmp) => {
                        output.push_str(&format!("cmp {} {}\n", self.reg(&cmp.operand), self.reg(&cmp.dest)));
                    }
                    code_gen::nodes::Instruction::Label(label) => {
                        output.push_str(&format!("{}\n", label));
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
                        output.push_str(&format!("inc {}\n", self.reg(&inc.operand)));
                    }
                    code_gen::nodes::Instruction::Dec(dec) => {
                        output.push_str(&format!("dec {}\n", self.reg(&dec.operand)));
                    }
                    code_gen::nodes::Instruction::Not(not) => {
                        output.push_str(&format!("not {} {}\n", self.reg(&not.operand), self.reg(&not.dest)));
                    }
                    //#[allow(unreachable_patterns)]
                    //_ => panic!()
                }
            }
            output.push_str(&format!("lod 15 15 -1\n"));
            output.push_str("inc 14\n");
            output.push_str("ldi r1 0\n"); // zero out r1
            output.push_str("ret\n");
        }

        output
    }
}