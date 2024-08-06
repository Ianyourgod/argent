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

    fn arg_is_memory(&self, arg: &nodes::Operand) -> (bool, u8) {
        match arg {
            nodes::Operand::Memory(idx) => {
                return (true, *idx);
            },
            _ => {
                return (false, 0);
            },
        }
    }

    fn arg_to_reg(&self, arg: &nodes::Operand, instructions: &mut Vec<nodes::Instruction>, reg: nodes::Reg) {
        match arg {
            nodes::Operand::Memory(idx) => {
                instructions.push(nodes::Instruction::Lod(nodes::BinOp {
                    a: nodes::Operand::Memory(*idx),
                    b: nodes::Operand::Immediate(0),
                    dest: nodes::Operand::Register(reg),
                }));
            },
            nodes::Operand::Immediate(val) => {
                instructions.push(nodes::Instruction::Ldi(nodes::UnaryOp {
                    operand: nodes::Operand::Immediate(*val),
                    dest: nodes::Operand::Register(reg),
                }));
            },
            _ => {
                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand: arg.clone(),
                    dest: nodes::Operand::Register(reg),
                }));
            },
        }
    }

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                /*
                our mov cmds are:
                mov {reg} to {reg}
                ldi {imm} to {reg}
                lod {adr (reg)}+{offset} to {reg}
                str {adr (reg)}+{offset} to {mem}
                 */

                let (src_is_mem, src_idx) = self.arg_is_memory(&mov.operand);
                let (dest_is_mem, dest_idx) = self.arg_is_memory(&mov.dest);

                if !src_is_mem && !dest_is_mem {
                    instructions.push(statement.clone());
                    return;
                }

                self.arg_to_reg(&mov.operand, instructions, nodes::Reg::R10);

                if dest_is_mem {
                    instructions.push(nodes::Instruction::Str(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::R10),
                        b: nodes::Operand::Immediate(0),
                        dest: nodes::Operand::Memory(dest_idx),
                    }));
                    return;
                } else {
                    instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                        operand: nodes::Operand::Register(nodes::Reg::R10),
                        dest: mov.dest.clone(),
                    }));
                    return;
                }
            },
            nodes::Instruction::Add(ref add) => {
                let (a_is_mem, a_idx) = self.arg_is_memory(&add.a);
                let (b_is_mem, b_idx) = self.arg_is_memory(&add.b);
                let (dest_is_mem, dest_idx) = self.arg_is_memory(&add.dest);

                // get a into R10 and b into R11
                self.arg_to_reg(&add.a, instructions, nodes::Reg::R10);
                self.arg_to_reg(&add.b, instructions, nodes::Reg::R11);

                if dest_is_mem {
                    instructions.push(nodes::Instruction::Add(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::R10),
                        b: nodes::Operand::Register(nodes::Reg::R11),
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                    }));
                    instructions.push(nodes::Instruction::Str(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::R10),
                        b: nodes::Operand::Immediate(0),
                        dest: nodes::Operand::Memory(dest_idx),
                    }));
                    return;
                }
                instructions.push(nodes::Instruction::Add(nodes::BinOp {
                    a: nodes::Operand::Register(nodes::Reg::R10),
                    b: nodes::Operand::Register(nodes::Reg::R11),
                    dest: add.dest.clone(),
                }));
            }
            nodes::Instruction::Sub(ref sub) => {
                let (a_is_mem, a_idx) = self.arg_is_memory(&sub.a);
                let (b_is_mem, b_idx) = self.arg_is_memory(&sub.b);
                let (dest_is_mem, dest_idx) = self.arg_is_memory(&sub.dest);

                // get a into R10 and b into R11
                self.arg_to_reg(&sub.a, instructions, nodes::Reg::R10);
                self.arg_to_reg(&sub.b, instructions, nodes::Reg::R11);

                if dest_is_mem {
                    instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::R10),
                        b: nodes::Operand::Register(nodes::Reg::R11),
                        dest: nodes::Operand::Register(nodes::Reg::R10),
                    }));
                    instructions.push(nodes::Instruction::Str(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::R10),
                        b: nodes::Operand::Immediate(0),
                        dest: nodes::Operand::Memory(dest_idx),
                    }));
                    return;
                }
                instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                    a: nodes::Operand::Register(nodes::Reg::R10),
                    b: nodes::Operand::Register(nodes::Reg::R11),
                    dest: sub.dest.clone(),
                }));
            }
            nodes::Instruction::Cmp(ref cmp) => {
                let (operand_is_mem, operand_idx) = self.arg_is_memory(&cmp.operand);

                // cmp is converted to `sub {operand} {dest} r0`

                self.arg_to_reg(&cmp.operand, instructions, nodes::Reg::R10);
                self.arg_to_reg(&cmp.dest, instructions, nodes::Reg::R11);

                instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                    a: nodes::Operand::Register(nodes::Reg::R10),
                    b: nodes::Operand::Register(nodes::Reg::R11),
                    dest: nodes::Operand::Register(nodes::Reg::R0),
                }));
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }
}