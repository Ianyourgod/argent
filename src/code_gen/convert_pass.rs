use crate::code_gen::nodes;
use crate::tacky;

const BYTE: &str = "b";
const WORD: &str = "w";
const LONG: &str = "l";
const QUAD: &str = "q";

pub struct Pass {
    pub program: tacky::nodes::Program,
    symbol_table: tacky::nodes::SymbolTable,
    temp_count: usize,
}

impl Pass {
    pub fn new(program: &tacky::nodes::Program, symbol_table: tacky::nodes::SymbolTable) -> Pass {
        Pass { program: program.clone(), symbol_table, temp_count: 0 }
    }

    pub fn run(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        for function in self.program.function_definitions.clone() {
            self.emit_function_definition(&function, &mut program);
        }

        program
    }

    fn generate_temporary(&mut self) -> String {
        let temp = format!(".ctemp{}", self.temp_count);
        self.temp_count += 1;
        temp
    }

    unsafe fn uint_to_enum(value: u8) -> nodes::Reg {
        unsafe { std::mem::transmute(value) }
    }

    fn emit_function_definition(&mut self, function: &tacky::nodes::FunctionDefinition, program: &mut nodes::Program) {
        let mut instructions: Vec<nodes::Instruction> = Vec::new();
        let context = nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 };

        for (i, arg) in function.arguments.iter().enumerate() {
            if i < 16 {
                let reg = unsafe { Pass::uint_to_enum(i as u8) };

                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand: nodes::Operand::Register(reg),
                    dest: nodes::Operand::Pseudo(arg.0.clone()),
                }));
            } else {
                panic!("Only 16 arguments are supported for now");
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

    fn convert_type(&mut self, ty: &tacky::nodes::Type) -> nodes::Type {
        match ty {
            tacky::nodes::Type::I32 => nodes::Type::I32,
            tacky::nodes::Type::I64 => nodes::Type::I64,
            tacky::nodes::Type::U32 => nodes::Type::U32,
            tacky::nodes::Type::U64 => nodes::Type::U64,
            tacky::nodes::Type::Bool => nodes::Type::Bool,
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

    fn get_type(&mut self, value: &tacky::nodes::Value) -> tacky::nodes::Type {
        match value {
            tacky::nodes::Value::Identifier(ident) => {
                self.symbol_table.get(ident).unwrap().clone()
            }
            tacky::nodes::Value::Constant(constant) => {
                match constant {
                    tacky::nodes::Constant::I32(_) => tacky::nodes::Type::I32,
                    tacky::nodes::Constant::I64(_) => tacky::nodes::Type::I64,
                    tacky::nodes::Constant::U32(_) => tacky::nodes::Type::U32,
                    tacky::nodes::Constant::U64(_) => tacky::nodes::Type::U64,
                    tacky::nodes::Constant::Bool(_) => tacky::nodes::Type::Bool,
                }
            }
            _ => panic!("Unsupported value type"),
        }
    }

    fn emit_instruction(&mut self, statement: &tacky::nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            tacky::nodes::Instruction::Return(return_value) => {
                let value = self.emit_value(return_value);
                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand: value,
                    dest: nodes::Operand::Register(nodes::Reg::R0),
                }));
            }
            tacky::nodes::Instruction::Unary(unary) => {
                match unary.operator {
                    tacky::nodes::UnaryOperator::Negate => {
                        let src = self.emit_value(&unary.src);
                        let dest = self.emit_value(&unary.dest);

                        instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                            a: nodes::Operand::Immediate(0),
                            b: src,
                            dest,
                        }));
                    }
                    _ => panic!("Unsupported unary operator"),
                }
            }
            tacky::nodes::Instruction::Binary(binary) => {
                let src1 = self.emit_value(&binary.src1);
                let src2 = self.emit_value(&binary.src2);
                let dest = self.emit_value(&binary.dest);

                match binary.operator {
                    tacky::nodes::BinaryOperator::Add => {
                        instructions.push(nodes::Instruction::Add(nodes::BinOp {
                            a: src1,
                            b: src2,
                            dest,
                        }));
                    },
                    tacky::nodes::BinaryOperator::Subtract => {
                        instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                            a: src1,
                            b: src2,
                            dest,
                        }));
                    },
                    tacky::nodes::BinaryOperator::And => {
                        instructions.push(nodes::Instruction::And(nodes::BinOp {
                            a: src1,
                            b: src2,
                            dest,
                        }));
                    },
                    tacky::nodes::BinaryOperator::Or => panic!("Unsupported binary operator"),
                    tacky::nodes::BinaryOperator::GreaterThan => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let not_true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, not_true_label.clone()));
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::L, not_true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(not_true_label));
                    },
                    tacky::nodes::BinaryOperator::GreaterThanEqual => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let not_true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::L, not_true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(not_true_label));
                    },
                    tacky::nodes::BinaryOperator::LessThan => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let not_true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::GE, not_true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(not_true_label));
                    },
                    tacky::nodes::BinaryOperator::LessThanEqual => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::L, true_label.clone()));
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(true_label));
                    },
                    tacky::nodes::BinaryOperator::Equal => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(true_label));
                    },
                    tacky::nodes::BinaryOperator::NotEqual => {
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(0),
                            dest: dest.clone(),
                        }));
                        instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                            operand: src1,
                            dest: src2,
                        }));
                        let not_true_label = self.generate_temporary();
                        instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, not_true_label.clone()));
                        instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                            operand: nodes::Operand::Immediate(1),
                            dest,
                        }));
                        instructions.push(nodes::Instruction::Label(not_true_label));
                    },
                }
            }
            tacky::nodes::Instruction::Copy(copy) => {
                let src = self.emit_value(&copy.src);
                let dest = self.emit_value(&copy.dest);

                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand: src,
                    dest,
                }));
            }
            tacky::nodes::Instruction::Jump(label) => {
                instructions.push(nodes::Instruction::Jump(label.clone()));
            }
            tacky::nodes::Instruction::JumpIfZero(label, value) => {
                let value_type = self.get_type(value);

                let value = self.emit_value(value);

                instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                    operand: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, label.clone()));
            }
            tacky::nodes::Instruction::JumpIfNotZero(label, value) => {
                let value_type = self.get_type(value);
                let value = self.emit_value(value);

                instructions.push(nodes::Instruction::Cmp(nodes::UnaryOp {
                    operand: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::NE, label.clone()));
            }
            tacky::nodes::Instruction::Label(label) => {
                instructions.push(nodes::Instruction::Label(label.clone()));
            },
            tacky::nodes::Instruction::FunCall(fun_call) => {
                let mut register_args: Vec<tacky::nodes::Value> = Vec::new();

                for (i, arg) in fun_call.arguments.iter().enumerate() {
                    register_args.push(arg.clone());
                }

                let stack_padding = 0;

                for (i, arg) in register_args.iter().enumerate() {
                    instructions.push(nodes::Instruction::Str(nodes::BinOp {
                        a: nodes::Operand::Register(nodes::Reg::RSP),
                        b: self.emit_value(arg),
                        dest: unsafe { nodes::Operand::Register(Pass::uint_to_enum(i as u8)) },
                    }));
                }

                instructions.push(nodes::Instruction::Call(fun_call.function_name.clone()));

                let dst = self.emit_value(&fun_call.dest);

                instructions.push(nodes::Instruction::Mov(nodes::UnaryOp {
                    operand: nodes::Operand::Register(nodes::Reg::R0),
                    dest: dst,
                }));
            },
        }
    }

    fn emit_value(&mut self, value: &tacky::nodes::Value) -> nodes::Operand {
        if value.is_identifier() {
            let identifier = value.as_identifier();
            return nodes::Operand::Pseudo(identifier.to_string())
        }

        if value.is_constant() {
            let constant = value.as_constant();
            return if constant.is_i32() {
                nodes::Operand::Immediate(constant.as_i32() as i8)
            } else {
                nodes::Operand::Immediate(constant.as_i64() as i8)
            }
        }
        panic!("Unsupported value type");
    }
}

/*
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
                    return_type: tacky::nodes::Type::I32,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Return(tacky::nodes::Value::Constant(tacky::nodes::Constant::I32(0)))
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let pass = Pass::new(&tack, tacky::nodes::SymbolTable::new());
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::I32);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
        assert_eq!(program.statements[0].instructions.len(), 2);
        assert_eq!(program.statements[0].instructions[0], nodes::Instruction::Mov(nodes::BinOp {
            src: nodes::Operand::Immediate(0),
            dest: nodes::Operand::Register(nodes::Reg::AX),
            suffix: nodes::Suffix::L,
        }));
        assert_eq!(program.statements[0].instructions[1], nodes::Instruction::Ret);
    }

    #[test]
    fn test_emit_unary() {
        let tack = tacky::nodes::Program {
            function_definitions: vec![
                tacky::nodes::FunctionDefinition {
                    function_name: "main".to_string(),
                    return_type: tacky::nodes::Type::I32,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Unary(tacky::nodes::Unary {
                                operator: tacky::nodes::UnaryOperator::Negate,
                                src: tacky::nodes::Value::Constant(tacky::nodes::Constant::I32(0)),
                                dest: tacky::nodes::Value::Identifier("a".to_string()),
                            })
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let mut sym_tbl = tacky::nodes::SymbolTable::new();

        sym_tbl.insert("a".to_string(), tacky::nodes::Type::I32);

        let pass = Pass::new(&tack, sym_tbl);
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::I32);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
    }

    #[test]
    fn test_emit_binary() {
        let tack = tacky::nodes::Program {
            function_definitions: vec![
                tacky::nodes::FunctionDefinition {
                    function_name: "main".to_string(),
                    return_type: tacky::nodes::Type::I32,
                    body: CompoundInstruction {
                        instructions: vec![
                            tacky::nodes::Instruction::Binary(tacky::nodes::Binary {
                                operator: tacky::nodes::BinaryOperator::Add,
                                src1: tacky::nodes::Value::Constant(tacky::nodes::Constant::I32(1)),
                                src2: tacky::nodes::Value::Constant(tacky::nodes::Constant::I32(2)),
                                dest: tacky::nodes::Value::Identifier("a".to_string()),
                            })
                        ]
                    },
                    arguments: Vec::new()
                }
            ]
        };

        let mut sym_tbl = tacky::nodes::SymbolTable::new();

        sym_tbl.insert("a".to_string(), tacky::nodes::Type::I32);

        let pass = Pass::new(&tack, sym_tbl);
        let program = pass.run();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].function_name, "main".to_string());
        assert_eq!(program.statements[0].return_type, nodes::Type::I32);
        assert_eq!(program.statements[0].context, nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 });
        
        assert_eq!(program.statements[0].instructions.len(), 2);
        assert_eq!(program.statements[0].instructions[0], nodes::Instruction::Mov(nodes::BinOp {
            src: nodes::Operand::Immediate(1),
            dest: nodes::Operand::Pseudo(nodes::Identifier { name: "a".to_string() }),
            suffix: nodes::Suffix::L,
        }));
        assert_eq!(program.statements[0].instructions[1], nodes::Instruction::Add(nodes::BinOp {
            src: nodes::Operand::Immediate(2),
            dest: nodes::Operand::Pseudo(nodes::Identifier { name: "a".to_string() }),
            suffix: nodes::Suffix::L,
        }));
    }
}
*/