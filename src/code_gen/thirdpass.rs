use crate::code_gen::nodes;

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
            
            instructions.push(nodes::Instruction::AllocateStack(function.context.stack_offset));

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

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                if let nodes::Operand::StackAllocate(idx1) = &mov.dest {
                    if let nodes::Operand::StackAllocate(idx2) = &mov.src {
                        instructions.push(nodes::Instruction::Mov(nodes::Mov {
                            dest: nodes::Operand::Register(nodes::Reg::R10d),
                            src: nodes::Operand::StackAllocate(*idx2),
                            suffix: None,
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::Mov {
                            dest: nodes::Operand::StackAllocate(*idx1),
                            src: nodes::Operand::Register(nodes::Reg::R10d),
                            suffix: None,
                        }));
                        return;
                    }
                }

                instructions.push(nodes::Instruction::Mov(nodes::Mov {
                    dest: mov.dest.clone(),
                    src: mov.src.clone(),
                    suffix: mov.suffix.clone(),
                }));
            },
            nodes::Instruction::Neg(ref neg) => {
                instructions.push(nodes::Instruction::Neg(nodes::Neg {
                    dest: neg.dest.clone(),
                    suffix: neg.suffix.clone(),
                }));
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }
}