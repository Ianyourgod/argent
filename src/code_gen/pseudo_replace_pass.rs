use crate::code_gen::nodes;
use crate::tacky;

pub struct Pass {
    pub program: nodes::Program,
    symbol_table: tacky::nodes::SymbolTable,
}

impl Pass {
    pub fn new(program: &nodes::Program, symbol_table: tacky::nodes::SymbolTable) -> Pass {
        Pass { program: program.clone(), symbol_table }
    }

    pub fn run(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        for function in self.program.statements.clone() {
            self.emit_function_definition(function, &mut program);
        }

        program
    }

    fn emit_function_definition(&mut self, mut function: nodes::FunctionDefinition, program: &mut nodes::Program) {
        let mut instructions: Vec<nodes::Instruction> = Vec::new();

        for statement in &function.instructions {
            self.emit_instruction(statement, &mut instructions, &mut function.context);
        }

        program.statements.push(nodes::FunctionDefinition::new(
            function.function_name.clone(),
            instructions,
            function.context,
            function.return_type.clone(),
        ));
    }

    fn emit_instruction(&mut self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>, context: &mut nodes::Context) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                let dest = self.emit_operand(&mov.dest, context);
                let operand = self.emit_operand(&mov.operand, context);

                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Add(ref add) => {
                let dest = self.emit_operand(&add.dest, context);
                let a = self.emit_operand(&add.a, context);
                let b = self.emit_operand(&add.b, context);

                instructions.push(nodes::Instruction::Add(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::Sub(ref sub) => {
                let dest = self.emit_operand(&sub.dest, context);
                let a = self.emit_operand(&sub.a, context);
                let b = self.emit_operand(&sub.b, context);

                instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::Cmp(ref cmp) => {
                let dest = self.emit_operand(&cmp.dest, context);
                let operand = self.emit_operand(&cmp.operand, context);

                instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Push(ref operand) => {
                let operand = self.emit_operand(&operand, context);

                instructions.push(nodes::Instruction::Push(operand));
            }
            nodes::Instruction::Pop(ref operand) => {
                let operand = self.emit_operand(&operand, context);

                instructions.push(nodes::Instruction::Pop(operand));
            }
            nodes::Instruction::Nor(ref nor) => {
                let dest = self.emit_operand(&nor.dest, context);
                let a = self.emit_operand(&nor.a, context);
                let b = self.emit_operand(&nor.b, context);

                instructions.push(nodes::Instruction::Nor(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::And(ref and) => {
                let dest = self.emit_operand(&and.dest, context);
                let a = self.emit_operand(&and.a, context);
                let b = self.emit_operand(&and.b, context);

                instructions.push(nodes::Instruction::And(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::Xor(ref xor) => {
                let dest = self.emit_operand(&xor.dest, context);
                let a = self.emit_operand(&xor.a, context);
                let b = self.emit_operand(&xor.b, context);

                instructions.push(nodes::Instruction::Xor(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::Rsh(ref rsh) => {
                let dest = self.emit_operand(&rsh.dest, context);
                let operand = self.emit_operand(&rsh.operand, context);

                instructions.push(nodes::Instruction::Rsh(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Lsh(ref lsh) => {
                let dest = self.emit_operand(&lsh.dest, context);
                let operand = self.emit_operand(&lsh.operand, context);

                instructions.push(nodes::Instruction::Lsh(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Inc(ref inc) => {
                let dest = self.emit_operand(&inc.dest, context);
                let operand = self.emit_operand(&inc.operand, context);

                instructions.push(nodes::Instruction::Inc(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Dec(ref dec) => {
                let dest = self.emit_operand(&dec.dest, context);
                let operand = self.emit_operand(&dec.operand, context);

                instructions.push(nodes::Instruction::Dec(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Not(ref not) => {
                let dest = self.emit_operand(&not.dest, context);
                let operand = self.emit_operand(&not.operand, context);

                instructions.push(nodes::Instruction::Not(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Ldi(ref ldi) => {
                let dest = self.emit_operand(&ldi.dest, context);
                let operand = self.emit_operand(&ldi.operand, context);

                instructions.push(nodes::Instruction::Ldi(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Adi(ref adi) => {
                let dest = self.emit_operand(&adi.dest, context);
                let operand = self.emit_operand(&adi.operand, context);

                instructions.push(nodes::Instruction::Adi(nodes::UnaryOp {
                    operand,
                    dest,
                }));
            },
            nodes::Instruction::Lod(ref lod) => {
                let dest = self.emit_operand(&lod.dest, context);
                let a = self.emit_operand(&lod.a, context);
                let b = self.emit_operand(&lod.b, context);

                instructions.push(nodes::Instruction::Lod(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            nodes::Instruction::Str(ref str) => {
                let dest = self.emit_operand(&str.dest, context);
                let a = self.emit_operand(&str.a, context);
                let b = self.emit_operand(&str.b, context);

                instructions.push(nodes::Instruction::Str(nodes::BinOp {
                    a,
                    b,
                    dest,
                }));
            },
            _ => {
                instructions.push(statement.clone());
            }
        }
    }

    fn emit_operand(&mut self, operand: &nodes::Operand, context: &mut nodes::Context) -> nodes::Operand {
        match operand {
            nodes::Operand::Pseudo(ref identifier) => {
                let offset = context.var_map.get(&identifier.clone());
                match offset {
                    Some(offset) => {
                        nodes::Operand::Memory(*offset as u8)
                    },
                    None => {
                        let size = 1;

                        let offset = context.stack_offset;

                        context.stack_offset += size;
                        context.var_map.insert(identifier.clone(), context.stack_offset);
                        nodes::Operand::Memory(context.stack_offset as u8)
                    },
                }
            },
            _ => operand.clone(),
        }
    }
}