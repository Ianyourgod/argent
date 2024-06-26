use crate::parser::nodes;

pub struct Pass {
    pub program: nodes::Program,
    context: Context,
}

pub struct Context {
    pub var_map: std::collections::HashMap<String, String>,
    pub tmp_n: i32,
}

impl Context {
    fn new() -> Context {
        Context { var_map: std::collections::HashMap::new(), tmp_n: -1 }
    }

}

impl Pass {
    pub fn new(program: &nodes::Program) -> Pass {
        Pass { program: program.clone(), context: Context::new() }
    }

    pub fn run(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            function_definitions: Vec::new(),
        };

        for function in self.program.function_definitions.clone() {
            let body = match *function.body {
                nodes::Statement::Compound(compound) => compound,
                _ => panic!()
            };

            let mut statements: Vec<Box<nodes::Statement>> = Vec::new();

            for statement in body.statements {
                statements.push(self.resolve_statement(statement));
            }

            let new_function = nodes::FunctionDeclaration {
                function_name: function.function_name.clone(),
                body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements,
                })),
                return_type: function.return_type.clone(),
                params: function.params.clone(), // todo: run resolve vars on params
            };

            program.function_definitions.push(new_function);
        }

        program
    }

    fn make_temporary(&mut self) -> String {
        self.context.tmp_n += 1;
        format!(".localvar{}", self.context.tmp_n)
    }

    fn resolve_statement(&mut self, statement: Box<nodes::Statement>) -> Box<nodes::Statement> {
        match *statement {
            nodes::Statement::VariableDeclaration(decl) => {
                if self.context.var_map.contains_key(&decl.ident.value) {
                    panic!("Variable {} already declared in scope", decl.ident.value);
                }

                let new_ident = self.make_temporary();

                self.context.var_map.insert(decl.ident.value.clone(), new_ident.clone());

                let new_decl = nodes::VariableDeclaration {
                    kind: decl.kind.clone(),
                    ident: nodes::Identifier { value: new_ident },
                    expr: if decl.expr.is_some() { Some(self.resolve_expression(decl.expr.unwrap())) } else { None },
                };

                Box::new(nodes::Statement::VariableDeclaration(new_decl))
            },
            nodes::Statement::ExpressionStatement(expr) => {
                Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                    expression: self.resolve_expression(expr.expression)
                }))
            },
            nodes::Statement::ReturnStatement(ret) => {
                Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
                    return_value: self.resolve_expression(ret.return_value)
                }))
            },
            _ => statement,
        }
    }

    fn resolve_expression(&mut self, expr: Box<nodes::Expression>) -> Box<nodes::Expression> {
        match *expr {
            nodes::Expression::Var(ref ident) => {
                if self.context.var_map.contains_key(&ident.value) {
                    Box::new(nodes::Expression::Var(nodes::Identifier { value: self.context.var_map.get(&ident.value).unwrap().clone() }))
                } else {
                    panic!("Variable {} not found in scope", ident.value);
                }
            },
            nodes::Expression::Assignment(ref ident, ref expr) => {
                if self.context.var_map.contains_key(&ident.value) {
                    Box::new(nodes::Expression::Assignment(nodes::Identifier { value: self.context.var_map.get(&ident.value).unwrap().clone() }, self.resolve_expression(expr.clone())))
                } else {
                    panic!("Variable {} not found in scope", ident.value);
                }
            },
            nodes::Expression::BinOp(ref left, ref op, ref right) => {
                Box::new(nodes::Expression::BinOp(self.resolve_expression(left.clone()), op.clone(), self.resolve_expression(right.clone())))
            },
            nodes::Expression::UnaryOp(ref op, ref expr) => {
                Box::new(nodes::Expression::UnaryOp(op.clone(), self.resolve_expression(expr.clone())))
            },
            _ => expr,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::nodes;

    #[test]
    fn test_variable_resolution() {
        let program = nodes::Program {
            function_definitions: vec![
                nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    params: vec![],
                    body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                        statements: vec![
                            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                                kind: "int".to_string(),
                                ident: nodes::Identifier { value: "a".to_string() },
                                expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                            })),
                            Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                                expression: Box::new(nodes::Expression::Var(nodes::Identifier { value: "a".to_string() })),
                            })),
                        ],
                    })),
                },
            ],
        };

        let mut pass = Pass::new(&program);
        let new_program = pass.run();

        assert_eq!(new_program.function_definitions[0].body, Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
            statements: vec![
                Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                    kind: "int".to_string(),
                    ident: nodes::Identifier { value: ".localvar0".to_string() },
                    expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                })),
                Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                    expression: Box::new(nodes::Expression::Var(nodes::Identifier { value: ".localvar0".to_string() })),
                })),
            ],
        })));
    }

    #[test]
    #[should_panic]
    fn test_variable_resolution_duplicate() {
        let program = nodes::Program {
            function_definitions: vec![
                nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    params: vec![],
                    body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                        statements: vec![
                            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                                kind: "int".to_string(),
                                ident: nodes::Identifier { value: "a".to_string() },
                                expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                            })),
                            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                                kind: "int".to_string(),
                                ident: nodes::Identifier { value: "a".to_string() },
                                expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                            })),
                        ],
                    })),
                },
            ],
        };

        let mut pass = Pass::new(&program);
        pass.run();
    }

    #[test]
    fn test_variable_resolution_assignment() {
        let program = nodes::Program {
            function_definitions: vec![
                nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    params: vec![],
                    body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                        statements: vec![
                            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                                kind: "int".to_string(),
                                ident: nodes::Identifier { value: "a".to_string() },
                                expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                            })),
                            Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                                expression: Box::new(nodes::Expression::Assignment(nodes::Identifier { value: "a".to_string() }, Box::new(nodes::Expression::Literal(nodes::Literal::Int(10)) ))),
                            })),
                        ],
                    })),
                },
            ],
        };

        let mut pass = Pass::new(&program);
        let new_program = pass.run();

        assert_eq!(new_program.function_definitions[0].body, Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
            statements: vec![
                Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                    kind: "int".to_string(),
                    ident: nodes::Identifier { value: ".localvar0".to_string() },
                    expr: Some(Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))),
                })),
                Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                    expression: Box::new(nodes::Expression::Assignment(nodes::Identifier { value: ".localvar0".to_string() }, Box::new(nodes::Expression::Literal(nodes::Literal::Int(10)) ))),
                })),
            ],
        })));
    }

    #[test]
    #[should_panic]
    fn test_variable_resolution_assignment_undeclared() {
        let program = nodes::Program {
            function_definitions: vec![
                nodes::FunctionDeclaration {
                    function_name: "main".to_string(),
                    return_type: "int".to_string(),
                    params: vec![],
                    body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                        statements: vec![
                            Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                                expression: Box::new(nodes::Expression::Assignment(nodes::Identifier { value: "a".to_string() }, Box::new(nodes::Expression::Literal(nodes::Literal::Int(10)) ))),
                            })),
                        ],
                    })),
                },
            ],
        };

        let mut pass = Pass::new(&program);
        pass.run();
    }
}