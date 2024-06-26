#![allow(dead_code)]
#![allow(unused_variables)]

use crate::parser;
pub mod nodes;

pub struct Context {
    pub tmp_n: i32,
    pub label_n: i32,
}

pub struct Tacky {
    pub ast: parser::nodes::Program,
    pub context: Context,
}

impl Tacky {
    pub fn new(ast: parser::nodes::Program) -> Self {
        Self {
            ast,
            context: Context {
                tmp_n: -1,
                label_n: -1,
            },
        }
    }

    fn make_temporary(&mut self) -> String {
        self.context.tmp_n += 1;
        format!(".tmp{}", self.context.tmp_n)
    }

    fn make_label(&mut self) -> String {
        self.context.label_n += 1;
        format!(".L{}", self.context.label_n)
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
            instructions.instructions.push(nodes::Instruction::Return(nodes::Value::Constant(0)));
            program.function_definitions.push(nodes::FunctionDefinition {
                function_name: statement.function_name.clone(),
                body: instructions,
                return_type: statement.return_type.clone(),
            });
        }
        program
    }

    fn emit_tacky_statement(&mut self, statement: &parser::nodes::Statement, instructions: &mut nodes::CompoundInstruction) {
        match statement {
            parser::nodes::Statement::ReturnStatement(return_statement) => {
                let return_value = self.emit_tacky_expression(&*return_statement.return_value, instructions);
                instructions.instructions.push(nodes::Instruction::Return(return_value));
            }
            parser::nodes::Statement::FunctionDeclaration(ref function_declaration) => {
                let mut body = nodes::CompoundInstruction {
                    instructions: Vec::new(),
                };
                self.emit_tacky_statement(&*function_declaration.body, &mut body);
            }
            parser::nodes::Statement::Compound(ref compound_statement) => {
                for statement in &compound_statement.statements {
                    self.emit_tacky_statement(statement, instructions);
                }
            }
            parser::nodes::Statement::ExpressionStatement(ref expression) => {
                self.emit_tacky_expression(&*expression.expression, instructions);
            }
            parser::nodes::Statement::VariableDeclaration(ref decl) => {
                let var = nodes::Value::Identifier(decl.ident.value.clone());
                let value = match &decl.expr {
                    Some(value) => self.emit_tacky_expression(&*value, instructions),
                    None => nodes::Value::Constant(0),
                };
                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: value,
                    dest: var,
                }));
            }
            _ => panic!("Not implemented yet: {:?}", statement)
        };
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
                let operator = match op {
                    parser::nodes::BinOp::Add => nodes::BinaryOperator::Add,
                    parser::nodes::BinOp::Subtract => nodes::BinaryOperator::Subtract,
                    parser::nodes::BinOp::Multiply => nodes::BinaryOperator::Multiply,
                    parser::nodes::BinOp::Divide => nodes::BinaryOperator::Divide,
                    parser::nodes::BinOp::LessThan => nodes::BinaryOperator::LessThan,
                    parser::nodes::BinOp::LessThanEqual => nodes::BinaryOperator::LessThanEqual,
                    parser::nodes::BinOp::GreaterThan => nodes::BinaryOperator::GreaterThan,
                    parser::nodes::BinOp::GreaterThanEqual => nodes::BinaryOperator::GreaterThanEqual,
                    parser::nodes::BinOp::Equal => nodes::BinaryOperator::Equal,
                    parser::nodes::BinOp::NotEqual => nodes::BinaryOperator::NotEqual,
                    parser::nodes::BinOp::And => nodes::BinaryOperator::And,
                    parser::nodes::BinOp::Or => nodes::BinaryOperator::Or,
                    #[allow(unreachable_patterns)]
                    _ => panic!("Not implemented yet")
                };

                if operator == nodes::BinaryOperator::And || operator == nodes::BinaryOperator::Or {
                    let src1 = self.emit_tacky_expression(&*exp1, instructions);
                    let end = self.make_label();
                    if operator == nodes::BinaryOperator::And {
                        instructions.instructions.push(nodes::Instruction::JumpIfZero(end.clone(), src1.clone()));
                    } else {
                        instructions.instructions.push(nodes::Instruction::JumpIfNotZero(end.clone(), src1.clone()));
                    }
                    let src2 = self.emit_tacky_expression(&*exp2, instructions);
                    if operator == nodes::BinaryOperator::And {
                        instructions.instructions.push(nodes::Instruction::JumpIfZero(end.clone(), src1.clone()));
                    } else {
                        instructions.instructions.push(nodes::Instruction::JumpIfNotZero(end.clone(), src1.clone()));
                    }
                    let dest = nodes::Value::Identifier(self.make_temporary());
                    instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                        src: nodes::Value::Constant(1),
                        dest: dest.clone(),
                    }));
                    instructions.instructions.push(nodes::Instruction::Label(end));
                    return dest;
                }
                let src1 = self.emit_tacky_expression(&*exp1, instructions);
                let src2 = self.emit_tacky_expression(&*exp2, instructions);
                let dest = nodes::Value::Identifier(self.make_temporary());
                instructions.instructions.push(nodes::Instruction::Binary(nodes::Binary {
                    operator,
                    src1,
                    src2,
                    dest: dest.clone(),
                }));
                dest
            }
            parser::nodes::Expression::Var(ident) => {
                nodes::Value::Identifier(ident.value.clone())
            }
            _ => panic!("Not implemented yet")
        }
    }
}

