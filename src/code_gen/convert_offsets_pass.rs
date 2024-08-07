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

    fn get_type(&self, operand: &nodes::Operand) -> &str {
        match operand {
            nodes::Operand::Register(reg) => "register",
            nodes::Operand::Immediate(imm) => "immediate",
            nodes::Operand::Memory(mem) => "memory",
            _ => panic!("how the hell")
        }
    }

    fn get_mem_val(&self, mem: &nodes::Operand) -> u8 {
        match mem {
            nodes::Operand::Memory(mem) => *mem,
            not_mem => panic!("{} ({}) used as memory", not_mem.displ(), self.get_type(not_mem))
        }
    }

    fn emit_instruction(&self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            nodes::Instruction::Lod(lod) => {
                let mem = self.get_mem_val(&lod.b);

                if mem > 8 {
                    // sub from reg to get the correct offset
                    instructions.push(nodes::Instruction::Adi(nodes::UnaryOp {
                        operand: nodes::Operand::Immediate(-((mem - 8) as i16) as u8),
                        dest: lod.a.clone(),
                    })); 
                    instructions.push(nodes::Instruction::Lod(nodes::BinOp {
                        a: lod.a.clone(),
                        b: nodes::Operand::Memory(8),
                        dest: lod.dest.clone(),
                    }));
                    instructions.push(nodes::Instruction::Adi(nodes::UnaryOp {
                        operand: nodes::Operand::Immediate(mem - 8),
                        dest: lod.a.clone(),
                    }));
                    return;
                }

                instructions.push(statement.clone());
            }
            nodes::Instruction::Str(stor) => {
                let mem = self.get_mem_val(&stor.b);

                if mem > 8 {
                    // sub from reg to get the correct offset
                    instructions.push(nodes::Instruction::Adi(nodes::UnaryOp {
                        operand: nodes::Operand::Immediate(-((mem - 8) as i16) as u8),
                        dest: stor.a.clone(),
                    })); 
                    instructions.push(nodes::Instruction::Str(nodes::BinOp {
                        a: stor.a.clone(),
                        b: nodes::Operand::Memory(8),
                        dest: stor.dest.clone(),
                    }));
                    instructions.push(nodes::Instruction::Adi(nodes::UnaryOp {
                        operand: nodes::Operand::Immediate(mem - 8),
                        dest: stor.a.clone(),
                    }));
                    return;
                }

                instructions.push(statement.clone());
            }
            _ => instructions.push(statement.clone())
        }
    }
}