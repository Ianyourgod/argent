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

    fn args_are_memory(&self, arg1: &nodes::Operand, arg2: &nodes::Operand) -> (bool, usize, usize) {
        match arg1 {
            nodes::Operand::StackAllocate(idx1) => {
                match arg2 {
                    nodes::Operand::StackAllocate(idx2) => {
                        return (true, *idx1, *idx2);
                    },
                    _ => {
                        return (false, 0, 0);
                    },
                }
            },
            _ => {
                return (false, 0, 0);
            },
        }
    }

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&mov.dest, &mov.src);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::StackAllocate(idx2),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::StackAllocate(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: None,
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    dest: mov.dest.clone(),
                    src: mov.src.clone(),
                    suffix: mov.suffix.clone(),
                }));
            },
            nodes::Instruction::Add(ref add) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&add.dest, &add.src);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::StackAllocate(idx2),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Add(nodes::BinOp {
                        dest: nodes::Operand::StackAllocate(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: None,
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Add(nodes::BinOp {
                    dest: add.dest.clone(),
                    src: add.src.clone(),
                    suffix: add.suffix.clone(),
                }));
            }
            nodes::Instruction::Sub(ref sub) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&sub.dest, &sub.src);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::StackAllocate(idx2),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                        dest: nodes::Operand::StackAllocate(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: None,
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                    dest: sub.dest.clone(),
                    src: sub.src.clone(),
                    suffix: sub.suffix.clone(),
                }));
            }
            nodes::Instruction::Mul(ref mul) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&mul.dest, &mul.src);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::StackAllocate(idx2),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::StackAllocate(idx1),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::StackAllocate(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R11),
                        suffix: None,
                    }));
                    return;
                }

                if let nodes::Operand::StackAllocate(idx1) = &mul.dest {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::StackAllocate(*idx1),
                        suffix: None,
                    }));
                    instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: mul.src.clone(),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::StackAllocate(*idx1),
                        src: nodes::Operand::Register(nodes::Reg::R11),
                        suffix: None,
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                    dest: mul.dest.clone(),
                    src: mul.src.clone(),
                    suffix: mul.suffix.clone(),
                }));
            }
            _ => {
                instructions.push(statement.clone());
            },
        }
    }
}