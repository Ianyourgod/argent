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

    fn args_are_memory(&self, arg1: &nodes::Operand, arg2: &nodes::Operand) -> (bool, u8, u8) {
        match arg1 {
            nodes::Operand::Memory(idx1) => {
                match arg2 {
                    nodes::Operand::Memory(idx2) => {
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

    fn arg_is_immediate(&self, arg: &nodes::Operand) -> bool {
        match arg {
            nodes::Operand::Immediate(_) => true,
            _ => false,
        }
    }

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&mov.dest, &mov.src);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::Memory(idx2),
                        suffix: mov.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Memory(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: mov.suffix.clone(),
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
                        src: nodes::Operand::Memory(idx2),
                        suffix: add.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Add(nodes::BinOp {
                        dest: nodes::Operand::Memory(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: add.suffix.clone(),
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
                        src: nodes::Operand::Memory(idx2),
                        suffix: sub.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                        dest: nodes::Operand::Memory(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: sub.suffix.clone(),
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
                        src: nodes::Operand::Memory(idx2),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::Memory(idx1),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Memory(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R11),
                        suffix: mul.suffix.clone(),
                    }));
                    return;
                }

                if let nodes::Operand::Memory(idx1) = &mul.dest {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: nodes::Operand::Memory(*idx1),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: mul.src.clone(),
                        suffix: mul.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Memory(*idx1),
                        src: nodes::Operand::Register(nodes::Reg::R11),
                        suffix: mul.suffix.clone(),
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                    dest: mul.dest.clone(),
                    src: mul.src.clone(),
                    suffix: mul.suffix.clone(),
                }));
            }
            nodes::Instruction::IDiv(ref div) => {
                if let nodes::Operand::Immediate(val) = &div.operand {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::Immediate(*val),
                        suffix: div.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::IDiv(nodes::UnaryOp {
                        operand: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: div.suffix.clone(),
                    }));
                    return;
                }

                instructions.push(statement.clone());
            },
            nodes::Instruction::Div(ref div) => {
                if let nodes::Operand::Immediate(val) = &div.operand {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::Immediate(*val),
                        suffix: div.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::IDiv(nodes::UnaryOp {
                        operand: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: div.suffix.clone(),
                    }));
                    return;
                }

                instructions.push(statement.clone());
            },
            nodes::Instruction::Cmp(ref cmp) => {
                let (is_memory, idx1, idx2) = self.args_are_memory(&cmp.dest, &cmp.src);
                let dest_is_immediate = self.arg_is_immediate(&cmp.dest);
                if is_memory {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: nodes::Operand::Memory(idx2),
                        suffix: cmp.suffix.clone(),
                    }));
                    if dest_is_immediate {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            dest: nodes::Operand::Register(nodes::Reg::R11),
                            src: nodes::Operand::Memory(idx1),
                            suffix: cmp.suffix.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            dest: nodes::Operand::Register(nodes::Reg::R11),
                            src: nodes::Operand::Register(nodes::Reg::R10),
                            suffix: cmp.suffix.clone(),
                        }));
                        return;
                    }
                    instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                        dest: nodes::Operand::Memory(idx1),
                        src: nodes::Operand::Register(nodes::Reg::R10),
                        suffix: cmp.suffix.clone(),
                    }));
                    return;
                }

                if dest_is_immediate {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: cmp.src.clone(),
                        suffix: cmp.suffix.clone(),
                    }));
                    instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R11),
                        src: cmp.dest.clone(),
                        suffix: cmp.suffix.clone(),
                    }));
                    return;
                }

                instructions.push(statement.clone());
            },
            nodes::Instruction::Movsx(src, dst) => {
                let src_is_immediate = self.arg_is_immediate(src);
                let dst_is_mem = match dst {
                    nodes::Operand::Memory(_) => true,
                    _ => false,
                };

                if src_is_immediate {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                        src: src.clone(),
                        suffix: nodes::Suffix::L,
                    }));

                    if dst_is_mem {
                        instructions.push(nodes::Instruction::Movsx(nodes::Operand::Register(nodes::Reg::R10), nodes::Operand::Register(nodes::Reg::R11)));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Register(nodes::Reg::R11),
                            dest: dst.clone(),
                            suffix: nodes::Suffix::Q,
                        }));
                        return;
                    }

                    instructions.push(nodes::Instruction::Movsx(nodes::Operand::Register(nodes::Reg::R10), dst.clone()));
                }

                if dst_is_mem {
                    instructions.push(nodes::Instruction::Movsx(src.clone(), nodes::Operand::Register(nodes::Reg::R11)));
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        src: nodes::Operand::Register(nodes::Reg::R11),
                        dest: dst.clone(),
                        suffix: nodes::Suffix::Q,
                    }));
                    return;
                }

                instructions.push(nodes::Instruction::Movsx(src.clone(), dst.clone()));
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }
}