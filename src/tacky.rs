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
    pub symbol_table: nodes::SymbolTable,
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
            symbol_table: nodes::SymbolTable::new()
        }
    }

    fn _make_temporary(&mut self) -> String {
        self.context.tmp_n += 1;
        format!(".tmp{}", self.context.tmp_n)
    }

    fn make_var(&mut self, name: String, type_: nodes::Type) -> nodes::Value {
        self.symbol_table.insert(name.clone(), type_);

        nodes::Value::Identifier(name)
    }

    fn make_tacky_var(&mut self, type_: nodes::Type) -> nodes::Value {
        let name = self._make_temporary();

        self.make_var(name, type_)
    }

    fn make_label(&mut self) -> String {
        self.context.label_n += 1;
        format!(".L{}", self.context.label_n)
    }

    fn convert_type(&self, ty: &parser::nodes::Type) -> nodes::Type {
        match ty {
            parser::nodes::Type::I32 => nodes::Type::I32,
            parser::nodes::Type::I64 => nodes::Type::I64,
            parser::nodes::Type::Fn(ref args, ref ret) => {
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.convert_type(arg));
                }
                nodes::Type::Fn(arg_types, Box::new(self.convert_type(ret)))
            }
            #[allow(unreachable_patterns)]
            _ => {
                panic!("Not implemented yet: {:?}", ty);
            }
            // parser::nodes::Type::Identifier(ident) => nodes::Type::Identifier(ident.value.clone()),
        }
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

            let ret_type = self.convert_type(&statement.return_type);

            // add the args to the symbol table
            for arg in statement.params.iter() {
                self.make_var(arg.ident.value.clone(), self.convert_type(&arg.kind));
            }

            self.emit_tacky_statement(&*statement.body, &mut instructions);
            instructions.instructions.push(nodes::Instruction::Return(nodes::Value::Constant(nodes::Constant::I32(0))));
            program.function_definitions.push(nodes::FunctionDefinition {
                function_name: statement.function_name.clone(),
                body: instructions,
                return_type: ret_type,
                arguments: statement.params.iter().map(|arg| (arg.ident.value.clone(), self.convert_type(&arg.kind))).collect(),
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
                let var = self.make_var(decl.ident.value.clone(), self.convert_type(&decl.kind));
                let value = match &decl.expr {
                    Some(value) => self.emit_tacky_expression(&*value, instructions),
                    None => nodes::Value::Constant(nodes::Constant::I32(0))
                };
                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: value,
                    dest: var,
                }));
            }
            parser::nodes::Statement::IfStatement(ref if_statement) => {
                let cond = self.make_tacky_var(self.get_tacky_type(&if_statement.condition));
                let else_label = self.make_label();
                let condition = self.emit_tacky_expression(&*if_statement.condition, instructions);
                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: condition,
                    dest: cond.clone(),
                }));
                instructions.instructions.push(nodes::Instruction::JumpIfZero(
                    else_label.clone(),
                    cond,
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
            },
            parser::nodes::Statement::WhileStatement(ref while_statement) => {
                let start_label = format!(".L_continue_{}", while_statement.label);
                let end_label = format!(".L_break_{}", while_statement.label);
                instructions.instructions.push(nodes::Instruction::Label(start_label.clone()));
                let cond = self.make_tacky_var(self.get_tacky_type(&while_statement.condition));
                let condition = self.emit_tacky_expression(&*while_statement.condition, instructions);
                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: condition,
                    dest: cond.clone(),
                }));
                instructions.instructions.push(nodes::Instruction::JumpIfZero(
                    end_label.clone(),
                    cond,
                ));
                self.emit_tacky_statement(&*while_statement.body, instructions);
                instructions.instructions.push(nodes::Instruction::Jump(start_label));
                instructions.instructions.push(nodes::Instruction::Label(end_label));
            },
            parser::nodes::Statement::BreakStatement(ref label) => {
                instructions.instructions.push(nodes::Instruction::Jump(format!(".L_break_{}", label)));
            },
            parser::nodes::Statement::ContinueStatement(ref label) => {
                instructions.instructions.push(nodes::Instruction::Jump(format!(".L_continue_{}", label)));
            },
            _ => panic!("Not implemented yet: {:?}", statement)
        };
    }

    fn get_tacky_type(&self, expression: &parser::nodes::Expression) -> nodes::Type {
        self.convert_type(&self.get_parser_type(expression))
    }

    fn get_parser_type(&self, expression: &parser::nodes::Expression) -> parser::nodes::Type {
        match expression {
            parser::nodes::Expression::Literal(_, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for literal");
                }
              type_.clone().unwrap()
            },
            parser::nodes::Expression::UnaryOp(_, exp, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for unary expression");
                }
              type_.clone().unwrap()
            },
            parser::nodes::Expression::BinOp(_, _, _, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for binary expression");
                }
              type_.clone().unwrap()
            },
            parser::nodes::Expression::Var(_, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for variable");
                }
              type_.clone().unwrap()
            },
            parser::nodes::Expression::Assignment(_, _, type_) => {
              type_.clone().unwrap()
            },
            parser::nodes::Expression::FunctionCall(_, _, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for function call");
                }
              type_.clone().unwrap()
            },
            parser::nodes::Expression::Cast(_, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for cast");
                }
                type_.clone().unwrap()
            },
            #[allow(unreachable_patterns)]
            _ => panic!("Not implemented yet")
        }
    }

    fn emit_tacky_expression(&mut self, expression: &parser::nodes::Expression, instructions: &mut nodes::CompoundInstruction) -> nodes::Value {
        match expression {
            parser::nodes::Expression::Literal(literal, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for literal");
                }
                match literal {
                    parser::nodes::Literal::I32(i) => nodes::Value::Constant(nodes::Constant::I32(*i)),
                    parser::nodes::Literal::I64(i) => nodes::Value::Constant(nodes::Constant::I64(*i)),
                    #[allow(unreachable_patterns)]
                    _ => panic!("Not implemented yet")
                }
            },
            parser::nodes::Expression::UnaryOp(op, exp, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for unary expression");
                }

                let type_ = self.convert_type(&type_.as_ref().unwrap());

                let src = self.emit_tacky_expression(&*exp, instructions);
                let dest = self.make_tacky_var(type_);
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
            },
            parser::nodes::Expression::BinOp(exp1, op, exp2, type_) => {
                let type_ = self.convert_type(&type_.as_ref().unwrap());

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
                    let dest = self.make_tacky_var(nodes::Type::I32);
                    instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                        src: nodes::Value::Constant(nodes::Constant::I32(0)),
                        dest: dest.clone(),
                    }));
                    instructions.instructions.push(nodes::Instruction::Label(end));
                    return dest;
                }
                let src1 = self.emit_tacky_expression(&*exp1, instructions);
                let src2 = self.emit_tacky_expression(&*exp2, instructions);
                let dest = self.make_tacky_var(type_);
                instructions.instructions.push(nodes::Instruction::Binary(nodes::Binary {
                    operator,
                    src1,
                    src2,
                    dest: dest.clone(),
                }));
                dest
            },
            parser::nodes::Expression::Var(ident, type_) => {
                nodes::Value::Identifier(ident.value.clone())
            },
            parser::nodes::Expression::Assignment(ident, exp, _) => {
                let value = self.emit_tacky_expression(&*exp, instructions);

                instructions.instructions.push(nodes::Instruction::Copy(nodes::Copy {
                    src: value.clone(),
                    dest: nodes::Value::Identifier(ident.value.clone()),
                }));
                value
            },
            parser::nodes::Expression::FunctionCall(name, args, type_) => {
                let type_ = self.convert_type(&type_.as_ref().unwrap());
                let mut arguments = Vec::new();
                for arg in args {
                    let value = self.emit_tacky_expression(&*arg, instructions);

                    arguments.push(value);
                }
                let dest = self.make_tacky_var(type_);
                instructions.instructions.push(nodes::Instruction::FunCall(nodes::FunCall {
                    function_name: name.clone(),
                    arguments,
                    dest: dest.clone(),
                }));
                dest
            },
            parser::nodes::Expression::Cast(expr, type_) => {
                if type_.is_none() {
                    panic!("Type not specified for cast");
                }

                let u_type = self.convert_type(&type_.as_ref().unwrap());

                let value = self.emit_tacky_expression(&*expr, instructions);

                if self.get_tacky_type(&expr) == u_type {
                    return value;
                }
                let dest = self.make_tacky_var(u_type.clone());

                if u_type == nodes::Type::I64 {
                    instructions.instructions.push(nodes::Instruction::SignExtend(value, dest.clone()));
                } else {
                    let trun_val = if value.is_constant() {
                        return nodes::Value::Constant(nodes::Constant::I32(value.as_constant().as_i32()));
                    } else {
                        value.clone()
                    };

                    instructions.instructions.push(nodes::Instruction::Truncate(trun_val, dest.clone())); // todo: maybe get rid of this?
                }

                dest
            },
            #[allow(unreachable_patterns)]
            item => panic!("Not implemented yet: {:?}", item)
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
                    return_type: parser::nodes::Type::I32,
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::I32(0), Some(parser::nodes::Type::I32))),
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
        assert_eq!(program.function_definitions[0].return_type, nodes::Type::I32);
        assert_eq!(program.function_definitions[0].body.instructions.len(), 2);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Return(nodes::Value::Constant(nodes::Constant::I32(0))));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Constant(nodes::Constant::I32(0)))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }

    #[test]
    fn test_generate_with_variable_declaration() {
        let ast = parser::nodes::Program {
            function_definitions: vec![
                parser::nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: parser::nodes::Type::I32,
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::VariableDeclaration(parser::nodes::VariableDeclaration {
                                ident: parser::nodes::Identifier { value: "x".to_string() },
                                expr: Some(Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::I32(42), Some(parser::nodes::Type::I32)))),
                                kind: parser::nodes::Type::I32,
                            })),
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::Var(parser::nodes::Identifier { value: "x".to_string() }, Some(parser::nodes::Type::I32))),
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
        assert_eq!(program.function_definitions[0].return_type, nodes::Type::I32);
        assert_eq!(program.function_definitions[0].body.instructions.len(), 3);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Copy(nodes::Copy {
            src: nodes::Value::Constant(nodes::Constant::I32(42)),
            dest: nodes::Value::Identifier("x".to_string()),
        }));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Identifier("x".to_string())));
        assert_eq!(program.function_definitions[0].body.instructions[2], nodes::Instruction::Return(nodes::Value::Constant(nodes::Constant::I32(0)))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }

    #[test]
    fn test_generate_with_unary_expression() {
        let ast = parser::nodes::Program {
            function_definitions: vec![
                parser::nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: parser::nodes::Type::I32,
                    body: Box::new(parser::nodes::Statement::Compound(parser::nodes::CompoundStatement {
                        statements: vec![
                            Box::new(parser::nodes::Statement::ReturnStatement(parser::nodes::ReturnStatement {
                                return_value: Box::new(parser::nodes::Expression::UnaryOp(parser::nodes::UnaryOp::Negation, Box::new(parser::nodes::Expression::Literal(parser::nodes::Literal::I32(42), Some(parser::nodes::Type::I32))), Some(parser::nodes::Type::I32))),
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
        assert_eq!(program.function_definitions[0].return_type, nodes::Type::I32);
        assert_eq!(program.function_definitions[0].body.instructions.len(), 3);
        assert_eq!(program.function_definitions[0].body.instructions[0], nodes::Instruction::Unary(nodes::Unary {
            operator: nodes::UnaryOperator::Negate,
            src: nodes::Value::Constant(nodes::Constant::I32(42)),
            dest: nodes::Value::Identifier(".tmp0".to_string()),
        }));
        assert_eq!(program.function_definitions[0].body.instructions[1], nodes::Instruction::Return(nodes::Value::Identifier(".tmp0".to_string())));
        assert_eq!(program.function_definitions[0].body.instructions[2], nodes::Instruction::Return(nodes::Value::Constant(nodes::Constant::I32(0)))); // Return 0 is added by the generator so if theres no return statement it doesnt bug out
    }
}