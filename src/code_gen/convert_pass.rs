use crate::code_gen::nodes;
use crate::tacky;

const BYTE: &str = "b";
const WORD: &str = "w";
const LONG: &str = "l";
const QUAD: &str = "q";

pub struct Pass {
    pub program: tacky::nodes::Program,
    symbol_table: tacky::nodes::SymbolTable,
}

impl Pass {
    pub fn new(program: &tacky::nodes::Program, symbol_table: tacky::nodes::SymbolTable) -> Pass {
        Pass { program: program.clone(), symbol_table }
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
                    suffix: self.type_to_suffix(&arg.1),
                }));
            } else {
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: nodes::Operand::StackAllocate(-8 * (i - 6) as isize - 16),
                    dest: nodes::Operand::Pseudo(nodes::Identifier { name: arg.0.clone() }),
                    suffix: nodes::Suffix::L,
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

    fn get_type(&self, value: &tacky::nodes::Value) -> tacky::nodes::Type {
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

    fn type_to_suffix(&self, type_: &tacky::nodes::Type) -> nodes::Suffix {
        match type_ {
            tacky::nodes::Type::I32  => nodes::Suffix::L,
            tacky::nodes::Type::I64  => nodes::Suffix::Q,
            tacky::nodes::Type::U32  => nodes::Suffix::L,
            tacky::nodes::Type::U64  => nodes::Suffix::Q,
            tacky::nodes::Type::Bool => nodes::Suffix::B,
            _ => panic!("Unsupported type: {:?}", type_),
        }
    }

    fn basic_comparison(&self, cond_code: nodes::CondCode, src1: nodes::Operand, src2: nodes::Operand, dest: nodes::Operand, src_type_suf: nodes::Suffix, dst_type_suf: nodes::Suffix, instructions: &mut Vec<nodes::Instruction>) {
        instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
            src: src2.clone(),
            dest: src1,
            suffix: src_type_suf,
        }));
        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
            src: nodes::Operand::Immediate(0),
            dest: dest.clone(),
            suffix: dst_type_suf,
        }));
        instructions.push(nodes::Instruction::SetCC(cond_code, dest));
    }

    fn is_signed(&self, type_: &tacky::nodes::Type) -> bool {
        match type_ {
            tacky::nodes::Type::I32 | tacky::nodes::Type::I64 => true,
            _ => false,
        }
    }

    fn emit_instruction(&self, statement: &tacky::nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            tacky::nodes::Instruction::Return(return_value) => {
                let value = self.emit_value(return_value);
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: value,
                    dest: nodes::Operand::Register(nodes::Reg::AX),
                    suffix: self.type_to_suffix(&self.get_type(return_value)),
                }));
                instructions.push(nodes::Instruction::Ret);
            }
            tacky::nodes::Instruction::Unary(unary) => {
                match unary.operator {
                    tacky::nodes::UnaryOperator::Negate => {
                        let src = self.emit_value(&unary.src);
                        let dest = self.emit_value(&unary.dest);

                        let src_type = self.get_type(&unary.src);

                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src,
                            dest: dest.clone(),
                            suffix: self.type_to_suffix(&src_type),
                        }));
                        instructions.push(nodes::Instruction::Neg(nodes::UnaryOp {
                            operand: dest,
                            suffix: nodes::Suffix::L,
                        }));
                    }
                    _ => panic!("Unsupported unary operator"),
                }
            }
            tacky::nodes::Instruction::Binary(binary) => {
                let src1 = self.emit_value(&binary.src1);
                let src2 = self.emit_value(&binary.src2);
                let dest = self.emit_value(&binary.dest);

                let src1_type = self.get_type(&binary.src1);
                let dest_type = self.get_type(&binary.dest);
                let src_type_suf = self.type_to_suffix(&src1_type);
                let dst_type_suf = self.type_to_suffix(&dest_type);

                match binary.operator {
                    tacky::nodes::BinaryOperator::Add => {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: src_type_suf.clone(),
                        }));
                        instructions.push(nodes::Instruction::Add(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: src_type_suf.clone(),
                        }));
                    },
                    tacky::nodes::BinaryOperator::Subtract => {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: src_type_suf.clone(),
                        }));
                        instructions.push(nodes::Instruction::Sub(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: src_type_suf,
                        }));
                    },
                    tacky::nodes::BinaryOperator::Multiply => {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: dest.clone(),
                            suffix: src_type_suf.clone(),
                        }));
                        instructions.push(nodes::Instruction::Mul(nodes::BinOp {
                            src: src2,
                            dest,
                            suffix: src_type_suf,
                        }));
                    },
                    tacky::nodes::BinaryOperator::Divide => {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: nodes::Operand::Register(nodes::Reg::AX),
                            suffix: src_type_suf.clone(),
                        }));

                        if self.is_signed(&src1_type) {
                            instructions.push(nodes::Instruction::Cdq(src_type_suf.clone()));
                            instructions.push(nodes::Instruction::IDiv(nodes::UnaryOp {
                                operand: src2,
                                suffix: src_type_suf.clone(),
                            }));
                        } else {
                            instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                                src: nodes::Operand::Immediate(0),
                                dest: nodes::Operand::Register(nodes::Reg::DX),
                                suffix: src_type_suf.clone(),
                            })); // zero out rdx
                            instructions.push(nodes::Instruction::Div(nodes::UnaryOp {
                                operand: src2,
                                suffix: src_type_suf.clone(),
                            }));
                        }

                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Register(nodes::Reg::AX),
                            dest,
                            suffix: src_type_suf,
                        }));
                    },
                    tacky::nodes::BinaryOperator::Modulo => {
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: src1,
                            dest: nodes::Operand::Register(nodes::Reg::AX),
                            suffix: src_type_suf.clone(),
                        }));

                        if self.is_signed(&src1_type) {
                            instructions.push(nodes::Instruction::Cdq(src_type_suf.clone()));
                            instructions.push(nodes::Instruction::IDiv(nodes::UnaryOp {
                                operand: src2,
                                suffix: src_type_suf.clone(),
                            }));
                        } else {
                            instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                                src: nodes::Operand::Immediate(0),
                                dest: nodes::Operand::Register(nodes::Reg::DX),
                                suffix: src_type_suf.clone(),
                            })); // zero out rdx
                            instructions.push(nodes::Instruction::Div(nodes::UnaryOp {
                                operand: src2,
                                suffix: src_type_suf.clone(),
                            }));
                        }
                        
                        instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                            src: nodes::Operand::Register(nodes::Reg::DX),
                            dest,
                            suffix: src_type_suf,
                        }));
                    }
                    tacky::nodes::BinaryOperator::And => panic!(),
                    tacky::nodes::BinaryOperator::Or => panic!(),
                    tacky::nodes::BinaryOperator::GreaterThan => {
                        let cond_code = if self.is_signed(&src1_type) { nodes::CondCode::G } else { nodes::CondCode::A };

                        self.basic_comparison(cond_code, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                    tacky::nodes::BinaryOperator::GreaterThanEqual => {
                        let cond_code = if self.is_signed(&src1_type) { nodes::CondCode::GE } else { nodes::CondCode::AE };

                        self.basic_comparison(cond_code, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                    tacky::nodes::BinaryOperator::LessThan => {
                        let cond_code = if self.is_signed(&src1_type) { nodes::CondCode::L } else { nodes::CondCode::B };

                        self.basic_comparison(cond_code, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                    tacky::nodes::BinaryOperator::LessThanEqual => {
                        let cond_code = if self.is_signed(&src1_type) { nodes::CondCode::LE } else { nodes::CondCode::BE };

                        self.basic_comparison(cond_code, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                    tacky::nodes::BinaryOperator::Equal => {
                        self.basic_comparison(nodes::CondCode::E, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                    tacky::nodes::BinaryOperator::NotEqual => {
                        self.basic_comparison(nodes::CondCode::NE, src1, src2, dest, src_type_suf, dst_type_suf, instructions)
                    },
                }
            }
            tacky::nodes::Instruction::Copy(copy) => {
                let src = self.emit_value(&copy.src);
                let dest = self.emit_value(&copy.dest);

                let src_type_suffix = self.type_to_suffix(&self.get_type(&copy.src));

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src,
                    dest,
                    suffix: src_type_suffix,
                }));
            }
            tacky::nodes::Instruction::Jump(label) => {
                instructions.push(nodes::Instruction::Jump(label.clone()));
            }
            tacky::nodes::Instruction::JumpIfZero(label, value) => {
                let value_type = self.get_type(value);
                let value_type_suffix = self.type_to_suffix(&value_type);

                let value = self.emit_value(value);

                instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                    src: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                    suffix: value_type_suffix,
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::E, label.clone()));
            }
            tacky::nodes::Instruction::JumpIfNotZero(label, value) => {
                let value_type = self.get_type(value);
                let value_type_suffix = self.type_to_suffix(&value_type);

                let value = self.emit_value(value);

                instructions.push(nodes::Instruction::Cmp(nodes::BinOp {
                    src: nodes::Operand::Immediate(0),
                    dest: value.clone(),
                    suffix: value_type_suffix,
                }));
                instructions.push(nodes::Instruction::JumpCC(nodes::CondCode::NE, label.clone()));
            }
            tacky::nodes::Instruction::Label(label) => {
                instructions.push(nodes::Instruction::Label(label.clone()));
            },
            tacky::nodes::Instruction::FunCall(fun_call) => {
                let arg_registers = vec![nodes::Reg::DI, nodes::Reg::SI, nodes::Reg::DX, nodes::Reg::CX, nodes::Reg::R8, nodes::Reg::R9];

                let mut register_args: Vec<tacky::nodes::Value> = Vec::new();
                let mut stack_args: Vec<tacky::nodes::Value> = Vec::new();

                for (i, arg) in fun_call.arguments.iter().enumerate() {
                    if i < 6 {
                        register_args.push(arg.clone());
                    } else {
                        stack_args.push(arg.clone());
                    }
                }

                let stack_padding = stack_args.len() % 8;

                if stack_padding > 0 {
                    instructions.push(nodes::Instruction::AllocateStack(stack_padding));
                }

                for (i, arg) in register_args.iter().enumerate() {
                    let mov = nodes::Instruction::Mov(nodes::BinOp {
                        src: self.emit_value(arg),
                        dest: nodes::Operand::Register(arg_registers[i]),
                        suffix: self.type_to_suffix(&self.get_type(arg))
                    });

                    instructions.push(mov);
                }

                // go over stack args in reverse order
                for (i, arg) in stack_args.iter().enumerate().rev() {
                    let assembly_arg = self.emit_value(arg);

                    let type_ = self.get_type(arg);

                    if type_ == tacky::nodes::Type::I64 {
                        instructions.push(nodes::Instruction::Push(assembly_arg));
                        return;
                    }

                    match assembly_arg {
                        nodes::Operand::Register(_) | nodes::Operand::Immediate(_) => {
                            instructions.push(nodes::Instruction::Push(assembly_arg));
                        }
                        neither => {
                            instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                                src: neither.clone(),
                                dest: nodes::Operand::Register(nodes::Reg::AX),
                                suffix: nodes::Suffix::L,
                            }));
                            instructions.push(nodes::Instruction::Push(nodes::Operand::Register(nodes::Reg::AX)));
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

                let dst_type_suffix = self.type_to_suffix(&self.get_type(&fun_call.dest));

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: nodes::Operand::Register(nodes::Reg::AX),
                    dest: dst,
                    suffix: dst_type_suffix,
                }));
            },
            tacky::nodes::Instruction::SignExtend(src, dst) => {
                let src = self.emit_value(src);
                let dst = self.emit_value(dst);
                instructions.push(nodes::Instruction::Movsx(src, dst));
            },
            tacky::nodes::Instruction::Truncate(src, dst) => {
                let src = self.emit_value(src);
                let dst = self.emit_value(dst);

                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src,
                    dest: dst,
                    suffix: nodes::Suffix::L,
                }));
            },
        }
    }

    fn emit_value(&self, value: &tacky::nodes::Value) -> nodes::Operand {
        if value.is_identifier() {
            let identifier = value.as_identifier();
            return nodes::Operand::Pseudo(nodes::Identifier { name: identifier.to_string() })
        }

        if value.is_constant() {
            let constant = value.as_constant();
            return if constant.is_i32() {
                nodes::Operand::Immediate(constant.as_i32() as i64)
            } else {
                nodes::Operand::Immediate(constant.as_i64())
            }
        }
        panic!("Unsupported value type");
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