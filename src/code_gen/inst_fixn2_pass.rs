use crate::code_gen::nodes;

// all this does is make sure that the mov doesnt have an immediate thats too big (i.e. 32 bits)

pub struct Pass {
    pub program: nodes::Program,
}

impl Pass {
    pub fn new(program: &nodes::Program) -> Pass {
        Pass { program: program.clone() }
    }

    pub fn run(&self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        for function in self.program.statements.clone() {
            let mut instructions: Vec<nodes::Instruction> = Vec::new();
            
            instructions.push(nodes::Instruction::AllocateStack((function.context.stack_offset + 15) & !15)); // Align stack to 16 bytes

            for statement in function.instructions {
                self.emit_instruction(&statement, &mut instructions);
            }

            program.statements.push(nodes::FunctionDefinition::new(
                function.function_name.clone(),
                instructions,
                function.context,
                function.return_type.clone(),
            ));
        }

        program
    }

    fn arg_is_immediate(&self, arg: &nodes::Operand) -> (bool, i64) {
        match arg {
            nodes::Operand::Immediate(val) => (true, *val),
            _ => (false, 0),
        }
    }

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                let src_is_immediate = self.arg_is_immediate(&mov.src);
                let dest_is_memory = match &mov.dest {
                    nodes::Operand::StackAllocate(_) => true,
                    _ => false,
                };

                if src_is_immediate.0 && src_is_immediate.1 > i32::MAX as i64 && dest_is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: mov.src.clone(),
                        suffix: mov.suffix.clone(),
                    }));

                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: mov.dest.clone(),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: mov.suffix.clone(),
                    }));
                } else {
                    instructions.push(statement.clone());
                }
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }
}