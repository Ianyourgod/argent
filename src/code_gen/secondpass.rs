use std::collections::HashMap;

use crate::code_gen::nodes;

pub struct Pass {
    pub program: nodes::Program,
}

impl Pass {
    pub fn new(program: &nodes::Program) -> Pass {
        Pass { program: program.clone() }
    }

    pub fn run(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        for function in self.program.statements.clone() {
            let mut instructions: Vec<nodes::Instruction> = Vec::new();
            let mut context = nodes::Context { var_map: HashMap::new(), stack_offset: 8 };
            
            for statement in function.instructions {
                self.emit_instruction(&statement, &mut instructions, &mut context);
            }

            program.statements.push(nodes::FunctionDefinition::new(
                function.function_name.clone(),
                instructions,
                context,
                function.return_type.clone(),
            ));
        }

        program
    }

    fn emit_instruction(&mut self, statement: &nodes::Instruction, instructions: &mut Vec<nodes::Instruction>, context: &mut nodes::Context) {
        match statement {
            nodes::Instruction::Mov(ref mov) => {
                let dest = self.emit_operand(&mov.dest, instructions, context);
                let src = self.emit_operand(&mov.src, instructions, context);

                instructions.push(nodes::Instruction::Mov(nodes::Mov {
                    dest,
                    src,
                    suffix: mov.suffix.clone(),
                }));
            },
            nodes::Instruction::Neg(ref neg) => {
                let dest = self.emit_operand(&neg.dest, instructions, context);

                instructions.push(nodes::Instruction::Neg(nodes::Neg {
                    dest,
                    suffix: neg.suffix.clone(),
                }));
            },
            _ => {
                instructions.push(statement.clone());
            },
        }
    }

    fn emit_operand(&mut self, operand: &nodes::Operand, instructions: &mut Vec<nodes::Instruction>, context: &mut nodes::Context) -> nodes::Operand {
        match operand {
            &nodes::Operand::Pseudo(ref identifier) => {
                let offset = context.var_map.get(&identifier.name);
                match offset {
                    Some(offset) => {
                        nodes::Operand::StackAllocate(*offset)
                    },
                    None => {
                        context.stack_offset += 4;
                        context.var_map.insert(identifier.name.clone(), context.stack_offset);
                        nodes::Operand::StackAllocate(context.stack_offset)
                    },
                }
            },
            _ => operand.clone(),
        }
    }
}