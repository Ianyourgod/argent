use crate::code_gen::nodes;
use crate::tacky;

const BYTE: &str = "b";
const WORD: &str = "w";
const LONG: &str = "l";
const QUAD: &str = "q";

pub struct Pass {
    pub program: tacky::nodes::Program,
}

impl Pass {
    pub fn new(program: &tacky::nodes::Program) -> Pass {
        Pass { program: program.clone() }
    }

    pub fn run(&self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        for function in self.program.function_definitions.clone() {
            self.emit_function_definition(&function, &mut program);
        }

        program
    }

    fn emit_function_definition(&self, function: &tacky::nodes::FunctionDefinition, program: &mut nodes::Program) {
        let mut instructions: Vec<nodes::Instruction> = Vec::new();
        let context = nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 };

        for (i, arg) in function.arguments.iter().enumerate() {
            if i < 6 {
                let reg = match i {
                    0 => nodes::Reg::DI,
                    1 => nodes::Reg::SI,
                    2 => nodes::Reg::DX,
                    3 => nodes::Reg::CX,
                    4 => nodes::Reg::R8,
                    5 => nodes::Reg::R9,
                    _ => panic!(),
                };

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: nodes::Operand::Register(reg),
                    dest: nodes::Operand::Pseudo(nodes::Identifier { name: arg.0.clone() }),
                    suffix: Some(LONG.to_string()),
                }));
            } else {
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: nodes::Operand::StackAllocate(-8 * (i - 6) as isize - 16),
                    dest: nodes::Operand::Pseudo(nodes::Identifier { name: arg.0.clone() }),
                    suffix: Some(LONG.to_string()),
                }));
            }
        }

        for statement in &function.body.instructions {
            self.emit_instruction(statement, &mut instructions);
        }

        program.statements.push(nodes::FunctionDefinition::new(
            function.function_name.clone(),
            instructions,
            context,
            self.convert_type(&function.return_type),
        ));
    }

    fn convert_type(&self, ty: &tacky::nodes::Type) -> nodes::Type {
        match ty {
            tacky::nodes::Type::Int => nodes::Type::Int,
            tacky::nodes::Type::Fn(ref args, ref ret) => {
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.convert_type(arg));
                }
                nodes::Type::Fn(arg_types, Box::new(self.convert_type(ret)))
            }
            tacky::nodes::Type::Identifier(ident) => nodes::Type::Identifier(ident.clone()),
        }
    }

    fn emit_instruction(&self, statement: &tacky::nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            tacky::nodes::Instruction::Return(return_value) => {
                let value = self.emit_value(return_value);
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: value,
                    dest: nodes::Operand::Register(nodes::Reg::AX),
                    suffix: Some(LONG.to_string()),
                }));
                instructions.push(nodes::Instruction::Ret);
            }
            tacky::nodes::Instruction::Unary(unary) => {
                match unary.operator {
                    tacky::nodes::UnaryOperator::Negate => {
                        let src = self.emit_value(&unary.src);
                        let dest = self.emit_value(&unary.dest);
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src,
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Neg(nodes::UnaryOp {
                            operand: dest,
                            suffix: Some(LONG.to_string()),
                        }));
                    }
                    _ => panic!("Unsupported unary operator"),
                }
            }
            tacky::nodes::Instruction::Binary(binary) => {
                match binary.operator {
                    tacky::nodes::BinaryOperator::Add => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Add(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: Some(LONG.to_string()),
                        }));
                    },
                    tacky::nodes::BinaryOperator::Subtract => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);

                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: Some(LONG.to_string()),
                        }));
                    },
                    tacky::nodes::BinaryOperator::Multiply => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: Some(LONG.to_string()),
                        }));
                    },
                    tacky::nodes::BinaryOperator::Divide => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: nodes::Operand::Register(nodes::Reg::AX),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Cdq);
                        instructions.push(nodes::Instruction::Div(nodes::UnaryOp {
                            operand: src2,
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Register(nodes::Reg::AX),
                            dest,
                            suffix: Some(LONG.to_string()),
                        }));
                    },
                    tacky::nodes::BinaryOperator::And => panic!(),
                    tacky::nodes::BinaryOperator::Or => panic!(),
                    tacky::nodes::BinaryOperator::GreaterThan => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::G, dest));
                    },
                    tacky::nodes::BinaryOperator::GreaterThanEqual => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::GE, dest));
                    },
                    tacky::nodes::BinaryOperator::LessThan => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::L, dest));
                    },
                    tacky::nodes::BinaryOperator::LessThanEqual => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::LE, dest));
                    },
                    tacky::nodes::BinaryOperator::Equal => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::E, dest));
                    },
                    tacky::nodes::BinaryOperator::NotEqual => {
                        let src1 = self.emit_value(&binary.src1);
                        let src2 = self.emit_value(&binary.src2);
                        let dest = self.emit_value(&binary.dest);
                        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                            src: src1,
                            dest: src2.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                            suffix: Some(LONG.to_string()),
                        }));
                        instructions.push(nodes::Instruction::SetCC(nodes::CondCode::NE, dest));
                    },
                }
            }
            tacky::nodes::Instruction::Copy(copy) => {
                let src = self.emit_value(&copy.src);
                let dest = self.emit_value(&copy.dest);
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src,
                    dest,
                    suffix: Some(LONG.to_string()),
                }));
            }
            tacky::nodes::Instruction::Jump(label) => {
                instructions.push(nodes::Instruction::Jump(label.clone()));
            }
            tacky::nodes::Instruction::JumpIfZero(label, value) => {
                let value = self.emit_value(value);
                instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                    src: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                    suffix: Some(LONG.to_string()),
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, label.clone()));
            }
            tacky::nodes::Instruction::JumpIfNotZero(label, value) => {
                let value = self.emit_value(value);
                instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                    src: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                    suffix: Some(LONG.to_string()),
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::NE, label.clone()));
            }
            tacky::nodes::Instruction::Label(label) => {
                instructions.push(nodes::Instruction::Label(label.clone()));
            },
            tacky::nodes::Instruction::FunCall(fun_call) => {
                let arg_registers = vec![nodes::Reg::DI, nodes::Reg::SI, nodes::Reg::DX, nodes::Reg::CX, nodes::Reg::R8, nodes::Reg::R9];

                let mut register_args: Vec<nodes::Operand> = Vec::new();
                let mut stack_args: Vec<nodes::Operand> = Vec::new();

                for (i, arg) in fun_call.arguments.iter().enumerate() {
                    if i < 6 {
                        register_args.push(self.emit_value(arg));
                    } else {
                        stack_args.push(self.emit_value(arg));
                    }
                }

                let stack_padding = stack_args.len() % 8;

                if stack_padding > 0 {
                    instructions.push(nodes::Instruction::AllocateStack(stack_padding));
                }

                for (i, arg) in register_args.iter().enumerate() {
                    instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                        src: arg.clone(),
                        dest: nodes::Operand::Register(arg_registers[i]),
                        suffix: Some(LONG.to_string()),
                    }));
                }

                // go over stack args in reverse order
                for (i, arg) in stack_args.iter().enumerate().rev() {
                    match arg {
                        nodes::Operand::Immediate(_) => {
                            instructions.push(nodes::Instruction::Push(nodes::UnaryOp {
                                operand: arg.clone(),
                                suffix: Some(QUAD.to_string()),
                            }));
                        },
                        nodes::Operand::Register(_) => {
                            instructions.push(nodes::Instruction::Push(nodes::UnaryOp {
                                operand: arg.clone(),
                                suffix: Some(QUAD.to_string()),
                            }));
                        },
                        neither => {
                            instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                                src: neither.clone(),
                                dest: nodes::Operand::Register(nodes::Reg::AX),
                                suffix: Some(LONG.to_string()),
                            }));
                            instructions.push(nodes::Instruction::Push(nodes::UnaryOp {
                                operand: nodes::Operand::Register(nodes::Reg::AX),
                                suffix: Some(QUAD.to_string()),
                            }));
                        }
                    }
                }

                let calling_name = format!("{}@PLT", fun_call.function_name.clone());

                instructions.push(nodes::Instruction::Call(calling_name));

                let bytes_to_remove = 8 * stack_args.len() + stack_padding;
                if bytes_to_remove > 0 {
                    instructions.push(nodes::Instruction::DeallocateStack(bytes_to_remove));
                }

                let dst = self.emit_value(&fun_call.dest);

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: nodes::Operand::Register(nodes::Reg::AX),
                    dest: dst,
                    suffix: Some(LONG.to_string()),
                }));
            }
        }
    }

    fn emit_value(&self, value: &tacky::nodes::Value) -> nodes::Operand {
        match value {
            tacky::nodes::Value::Identifier(identifier) => {
                nodes::Operand::Pseudo(nodes::Identifier { name: identifier.clone() })
            }
            tacky::nodes::Value::Constant(constant) => {
                nodes::Operand::Immediate(*constant)
            }
            _ => panic!("mmm this shouldnt happen you fucked up")
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tacky::{self, nodes::CompoundInstruction};

    #[test]
    fn test_emit() {
        let tack = tacky::nodes::Program {
            function_definitions: vec![
                tacky::nodes::FunctionDefinition {
                    function_name: "main".to_string(),
                    return_type: tacky::nodes::Type::Int,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Return(tacky::nodes::Value::Constant(0))
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let pass = Pass::new(&tack);
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::Int);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
        assert_eq!(program.statements[0].instructions.len(), 2);
        assert_eq!(program.statements[0].instructions[0], nodes::Instruction::Mov(nodes::BinOp {
            src: nodes::Operand::Immediate(0),
            dest: nodes::Operand::Register(nodes::Reg::AX),
            suffix: Some(LONG.to_string()),
        }));
        assert_eq!(program.statements[0].instructions[1], nodes::Instruction::Ret);
    }

    #[test]
    fn test_emit_unary() {
        let tack = tacky::nodes::Program {
            function_definitions: vec![
                tacky::nodes::FunctionDefinition {
                    function_name: "main".to_string(),
                    return_type: tacky::nodes::Type::Int,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Unary(tacky::nodes::Unary {
                                operator: tacky::nodes::UnaryOperator::Negate,
                                src: tacky::nodes::Value::Constant(1),
                                dest: tacky::nodes::Value::Identifier("a".to_string()),
                            })
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let pass = Pass::new(&tack);
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::Int);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
    }

    #[test]
    fn test_emit_binary() {
        let tack = tacky::nodes::Program {
            function_definitions: vec![
                tacky::nodes::FunctionDefinition {
                    function_name: "main".to_string(),
                    return_type: tacky::nodes::Type::Int,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Binary(tacky::nodes::Binary {
                                operator: tacky::nodes::BinaryOperator::Add,
                                src1: tacky::nodes::Value::Constant(1),
                                src2: tacky::nodes::Value::Constant(2),
                                dest: tacky::nodes::Value::Identifier("a".to_string()),
                            })
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let pass = Pass::new(&tack);
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::Int);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
        
        assert_eq!(program.statements[0].instructions.len(), 2);
        assert_eq!(program.statements[0].instructions[0], nodes::Instruction::Mov(nodes::BinOp {
            src: nodes::Operand::Immediate(1),
            dest: nodes::Operand::Pseudo(nodes::Identifier { name: "a".to_string() }),
            suffix: Some(LONG.to_string()),
        }));
        assert_eq!(program.statements[0].instructions[1], nodes::Instruction::Add(nodes::BinOp {
            src: nodes::Operand::Immediate(2),
            dest: nodes::Operand::Pseudo(nodes::Identifier { name: "a".to_string() }),
            suffix: Some(LONG.to_string()),
        }));
    }
}