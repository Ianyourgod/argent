#![allow(dead_code)]
#![allow(unused_variables)]

use crate::parser;
pub mod nodes;

pub struct Tacky {
    pub ast: parser::nodes::Program,
    pub tmp_n: i32,
}

impl Tacky {
    pub fn new(ast: parser::nodes::Program) -> Self {
        Self {
            ast,
            tmp_n: -1,
        }
    }

    fn make_temporary(&mut self) -> String {
        self.tmp_n += 1;
        format!(".tmp{}", self.tmp_n)
    }

    pub fn generate(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            function_definitions: Vec::new(),
        };
        let func_defs = self.ast.function_definitions.clone();
        for statement in func_defs {
            let mut instructions = nodes::CompoundInstruction {
                instructions: Vec::new(),
            };
            self.emit_tacky_statement(&*statement.body, &mut instructions);
            program.function_definitions.push(nodes::FunctionDefinition {
                function_name: statement.function_name.clone(),
                body: instructions,
                return_type: statement.return_type.clone(),
            });
        }
        program
    }

    fn emit_tacky_statement(&mut self, statement: &parser::nodes::Statement, instructions: &mut nodes::CompoundInstruction) -> nodes::Value {
        match statement {
            parser::nodes::Statement::ReturnStatement(return_statement) => {
                let return_value = self.emit_tacky_expression(&*return_statement.return_value, instructions);
                instructions.instructions.push(nodes::Instruction::Return(nodes::Return {
                    return_value
                }));
                nodes::Value::Empty
            }
            parser::nodes::Statement::FunctionDeclaration(ref function_declaration) => {
                let mut body = nodes::CompoundInstruction {
                    instructions: Vec::new(),
                };
                self.emit_tacky_statement(&*function_declaration.body, &mut body);
                nodes::Value::Empty
            }
            parser::nodes::Statement::Compound(ref compound_statement) => {
                for statement in &compound_statement.statements {
                    self.emit_tacky_statement(statement, instructions);
                }
                nodes::Value::Empty
            }
            _ => panic!("Not implemented yet: {:?}", statement)
        }
    }

    fn emit_tacky_expression(&mut self, expression: &parser::nodes::Expression, instructions: &mut nodes::CompoundInstruction) -> nodes::Value {
        match expression {
            parser::nodes::Expression::Literal(literal) => {
                match literal {
                    parser::nodes::Literal::Int(i) => nodes::Value::Constant(*i),
                    _ => panic!("Not implemented yet")
                }
            }
            parser::nodes::Expression::UnaryOp(op, exp) => {
                let src = self.emit_tacky_expression(&*exp, instructions);
                let dest = nodes::Value::Identifier(self.make_temporary());
                instructions.instructions.push(nodes::Instruction::Unary(nodes::Unary {
                    operator: match op {
                        parser::nodes::UnaryOp::Negation => nodes::UnaryOperator::Negate,
                        parser::nodes::UnaryOp::BitwiseComplement => nodes::UnaryOperator::Complement,
                        _ => panic!("Not implemented yet")
                    },
                    src,
                    dest: dest.clone(),
                }));
                dest
            }
            parser::nodes::Expression::BinOp(exp1, op, exp2) => {
                let src1 = self.emit_tacky_expression(&*exp1, instructions);
                let src2 = self.emit_tacky_expression(&*exp2, instructions);
                let dest = nodes::Value::Identifier(self.make_temporary());
                instructions.instructions.push(nodes::Instruction::Binary(nodes::Binary {
                    operator: match op {
                        parser::nodes::BinOp::Add => nodes::BinaryOperator::Add,
                        parser::nodes::BinOp::Subtract => nodes::BinaryOperator::Subtract,
                        parser::nodes::BinOp::Multiply => nodes::BinaryOperator::Multiply,
                        parser::nodes::BinOp::Divide => nodes::BinaryOperator::Divide,
                        _ => panic!("Not implemented yet")
                    },
                    src1,
                    src2,
                    dest: dest.clone(),
                }));
                dest
            }
            _ => panic!("Not implemented yet")
        }
    }
}

