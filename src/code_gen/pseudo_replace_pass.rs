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
                let dest = self.emit_operand(&mov.dest, instructions, context);
                let src = self.emit_operand(&mov.src, instructions, context);

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    dest,
                    src,
                    suffix: mov.suffix.clone(),
                }));
            },
            nodes::Instruction::Neg(ref neg) => {
                let operand = self.emit_operand(&neg.operand, instructions, context);

                instructions.push(nodes::Instruction::Neg(nodes::UnaryOp {
                    operand,
                    suffix: neg.suffix.clone(),
                }));
            },
            nodes::Instruction::Add(ref add) => {
                let dest = self.emit_operand(&add.dest, instructions, context);
                let src = self.emit_operand(&add.src, instructions, context);

                instructions.push(nodes::Instruction::Add(nodes::BinOp {
                    dest,
                    src,
                    suffix: add.suffix.clone(),
                }));
            },
            nodes::Instruction::Sub(ref sub) => {
                let dest = self.emit_operand(&sub.dest, instructions, context);
                let src = self.emit_operand(&sub.src, instructions, context);

                instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                    dest,
                    src,
                    suffix: sub.suffix.clone(),
                }));
            },
            nodes::Instruction::Mul(ref mul) => {
                let dest = self.emit_operand(&mul.dest, instructions, context);
                let src = self.emit_operand(&mul.src, instructions, context);

                instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                    dest,
                    src,
                    suffix: mul.suffix.clone(),
                }));
            },
            nodes::Instruction::Div(ref div) => {
                let operand = self.emit_operand(&div.operand, instructions, context);

                instructions.push(nodes::Instruction::Div(nodes::UnaryOp {
                    operand,
                    suffix: div.suffix.clone(),
                }));
            },
            nodes::Instruction::Cmp(ref cmp) => {
                let dest = self.emit_operand(&cmp.dest, instructions, context);
                let src = self.emit_operand(&cmp.src, instructions, context);

                instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                    dest,
                    src,
                    suffix: cmp.suffix.clone(),
                }));
            },
            nodes::Instruction::SetCC(ref cond_code, ref operand) => {
                let operand = self.emit_operand(&operand, instructions, context);

                instructions.push(nodes::Instruction::SetCC(cond_code.clone(), operand));
            },
            nodes::Instruction::Push(ref operand) => {
                let operand = self.emit_operand(&operand, instructions, context);

                instructions.push(nodes::Instruction::Push(operand));
            },
            nodes::Instruction::Movsx(src, dst) => {
                let src = self.emit_operand(&src, instructions, context);
                let dst = self.emit_operand(&dst, instructions, context);

                instructions.push(nodes::Instruction::Movsx(src, dst));
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }

    fn emit_operand(&mut self, operand: &nodes::Operand, instructions: &mut Vec<nodes::Instruction>, context: &mut nodes::Context) -> nodes::Operand {
        match operand {
            nodes::Operand::Pseudo(ref identifier) => {
                let offset = context.var_map.get(&identifier.name);
                match offset {
                    Some(offset) => {
                        nodes::Operand::StackAllocate(*offset as isize)
                    },
                    None => {
                        let size = if self.symbol_table.get(&identifier.name).unwrap() == &tacky::nodes::Type::I32 {
                            4
                        } else {
                            8
                        };

                        let offset = context.stack_offset % 8;

                        if size == 8 && offset != 0 {
                            context.stack_offset += 8 - offset;
                        }
                        context.stack_offset += size;
                        context.var_map.insert(identifier.name.clone(), context.stack_offset);
                        nodes::Operand::StackAllocate(context.stack_offset as isize)
                    },
                }
            },
            _ => operand.clone(),
        }
    }
}