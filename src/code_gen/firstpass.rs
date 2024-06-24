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
            let mut instructions: Vec<nodes::Instruction> = Vec::new();
            
            for statement in function.body.instructions {
                self.emit_instruction(&statement, &mut instructions);
            }

            program.statements.push(nodes::FunctionDefinition::new(
                function.function_name.clone(),
                instructions,
                nodes::Context { var_map: std::collections::HashMap::new(), stack_offset: 4 },
                function.return_type.clone(),
            ));
        }

        program
    }

    fn emit_instruction(&self, statement: &tacky::nodes::Instruction, instructions: &mut Vec<nodes::Instruction>) {
        match statement {
            tacky::nodes::Instruction::Return(return_statement) => {
                let return_value = self.emit_value(&return_statement.return_value);
                instructions.push(nodes::Instruction::Mov(nodes::BinOp {
                    src: return_value,
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
                    }
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
                    }
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
                    }
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
                    }
                }
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