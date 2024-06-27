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
            parser::nodes::Statement::IfStatement(ref if_statement) => {
                let cond = self.make_temporary();
                let else_label = self.make_label();
                let condition = self.emit_tacky_expression(&*if_statement.condition, instructions);
                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: condition,
                    dest: nodes::Value::Identifier(cond.clone()),
                }));
                instructions.instructions.push(nodes::Instruction::JumpIfZero(
                    else_label.clone(),
                    nodes::Value::Identifier(cond),
                ));
                self.emit_tacky_statement(&*if_statement.consequence, instructions);
                if if_statement.alternative.is_some() {
                    let end_label = self.make_label();
                    instructions.instructions.push(nodes::Instruction::Jump(end_label.clone()));
                    instructions.instructions.push(nodes::Instruction::Label(else_label));
                    self.emit_tacky_statement(&*if_statement.alternative.as_ref().unwrap(), instructions);
                    instructions.instructions.push(nodes::Instruction::Label(end_label));
                } else {
                    instructions.instructions.push(nodes::Instruction::Label(else_label)); // misnomer, this is the end of the if block
                }
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    #[test]
    fn test_generate() {
        let ast = parser::nodes::Program {
            function_definitions: vec![
                parser::nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::Int(0))),
                            })),
                        ],
                    })),
                    params: vec![],
                },
            ],
        };
        let mut tacky = Tacky::new(ast);
        let program = tacky.generate();
        assert_eq!(program.function_definitions.len(), 1);
        assert_eq!(program.function_definitions[0].function_name, "main");
        assert_eq!(program.function_definitions[0].return_type, "int");
        assert_eq!(program.function_definitions[0].body.instructions.len(), 2);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Return(nodes::Value::Constant(0)));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Constant(0))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }

    #[test]
    fn test_generate_with_variable_declaration() {
        let ast = parser::nodes::Program {
            function_definitions: vec![
                parser::nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::VariableDeclaration(parser::nodes::VariableDeclaration {
                                ident: parser::nodes::Identifier { value: "x".to_string() },
                                expr: Some(Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::Int(42)))),
                                kind: "int".to_string(),
                            })),
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::Var(parser::nodes::Identifier { value: "x".to_string() })),
                            })),
                        ],
                    })),
                    params: vec![],
                },
            ],
        };
        let mut tacky = Tacky::new(ast);
        let program = tacky.generate();
        assert_eq!(program.function_definitions.len(), 1);
        assert_eq!(program.function_definitions[0].function_name, "main");
        assert_eq!(program.function_definitions[0].return_type, "int");
        assert_eq!(program.function_definitions[0].body.instructions.len(), 3);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Copy(nodes::Copy {
            src: nodes::Value::Constant(42),
            dest: nodes::Value::Identifier("x".to_string()),
        }));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Identifier("x".to_string())));
        assert_eq!(program.function_definitions[0].body.instructions[2], nodes::Instruction::Return(nodes::Value::Constant(0))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }

    #[test]
    fn test_generate_with_unary_expression() {
        let ast = parser::nodes::Program {
            function_definitions: vec![
                parser::nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::UnaryOp(parser::nodes::UnaryOp::Negation, Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::Int(42))))),
                            })),
                        ],
                    })),
                    params: vec![],
                },
            ],
        };
        let mut tacky = Tacky::new(ast);
        let program = tacky.generate();
        assert_eq!(program.function_definitions.len(), 1);
        assert_eq!(program.function_definitions[0].function_name, "main");
        assert_eq!(program.function_definitions[0].return_type, "int");
        println!("{:?}", program.function_definitions[0].body.instructions);
        assert_eq!(program.function_definitions[0].body.instructions.len(), 3);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Unary(nodes::Unary {
            operator: nodes::UnaryOperator::Negate,
            src: nodes::Value::Constant(42),
            dest: nodes::Value::Identifier(".tmp0".to_string()),
        }));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Identifier(".tmp0".to_string())));
        assert_eq!(program.function_definitions[0].body.instructions[2], nodes::Instruction::Return(nodes::Value::Constant(0))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }
}