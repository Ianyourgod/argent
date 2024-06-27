use crate::parser::nodes;

pub struct Pass {
    pub program: nodes::Program,
}

#[derive(Clone)]
pub struct Context {
    pub var_map: VarMap,
}

pub struct VarMap {
    pub map: std::collections::HashMap<String, (String, bool)>,
    pub tmp_n: i32,
}

impl Clone for VarMap {
    fn clone(&self) -> VarMap {
        let mut new_map = self.map.clone();
        for (key, value) in self.map.iter() {
            new_map.insert(key.clone(), (value.0.clone(), false));
        }

        VarMap {
            map: new_map,
            tmp_n: self.tmp_n,
        }
    }
}

impl Context {
    fn new() -> Context {
        Context { var_map: VarMap { map: std::collections::HashMap::new(), tmp_n: -1 } }
    }
}

impl Pass {
    pub fn new(program: &nodes::Program) -> Pass {
        Pass { program: program.clone() }
    }

    pub fn run(&self) -> nodes::Program {
        let mut program = nodes::Program {
            function_definitions: Vec::new(),
        };

        for function in self.program.function_definitions.clone() {
            let body = match *function.body {
                nodes::Statement::Compound(compound) => compound,
                _ => panic!()
            };

            let mut statements: Vec<Box<nodes::Statement>> = Vec::new();

            let mut context = Context::new();

            for statement in body.statements {
                statements.push(self.resolve_statement(statement, &mut context));
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

    fn make_temporary(&self, context: &mut Context) -> String {
        context.var_map.tmp_n += 1;
        format!(".localvar{}", context.var_map.tmp_n)
    }

    fn resolve_statement(&self, statement: Box<nodes::Statement>, context: &mut Context) -> Box<nodes::Statement> {
        match *statement {
            nodes::Statement::VariableDeclaration(decl) => {
                if context.var_map.map.contains_key(&decl.ident.value) && context.var_map.map.get(&decl.ident.value).unwrap().1 {
                    panic!("Variable {} already declared in scope", decl.ident.value);
                }

                let new_ident = self.make_temporary(context);

                context.var_map.map.insert(decl.ident.value.clone(), (new_ident.clone(), true));

                let new_decl = nodes::VariableDeclaration {
                    kind: decl.kind.clone(),
                    ident: nodes::Identifier { value: new_ident },
                    expr: if decl.expr.is_some() { Some(self.resolve_expression(decl.expr.unwrap(), context)) } else { None },
                };

                Box::new(nodes::Statement::VariableDeclaration(new_decl))
            },
            nodes::Statement::ExpressionStatement(expr) => {
                Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                    expression: self.resolve_expression(expr.expression, context)
                }))
            },
            nodes::Statement::ReturnStatement(ret) => {
                Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
                    return_value: self.resolve_expression(ret.return_value, context)
                }))
            },
            nodes::Statement::Compound(compound) => {
                let mut statements: Vec<Box<nodes::Statement>> = Vec::new();

                let mut new_context = context.clone();

                for statement in compound.statements {
                    statements.push(self.resolve_statement(statement, &mut new_context));
                }

                Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements,
                }))
            },
            nodes::Statement::FunctionDeclaration(func) => {
                Box::new(nodes::Statement::FunctionDeclaration(nodes::FunctionDeclaration {
                    function_name: func.function_name.clone(),
                    return_type: func.return_type.clone(),
                    params: func.params.clone(),
                    body: self.resolve_statement(func.body, context),
                }))
            },
            nodes::Statement::IfStatement(ifstmt) => {
                Box::new(nodes::Statement::IfStatement(nodes::IfStatement {
                    condition: self.resolve_expression(ifstmt.condition, context),
                    consequence: self.resolve_statement(ifstmt.consequence, context),
                    alternative: if ifstmt.alternative.is_some() { Some(self.resolve_statement(ifstmt.alternative.unwrap(), context)) } else { None },
                }))
            },
            nodes::Statement::WhileStatement(whilestmt) => {
                let mut new_context = context.clone();

                Box::new(nodes::Statement::WhileStatement(nodes::WhileStatement {
                    condition: self.resolve_expression(whilestmt.condition, &mut new_context),
                    body: self.resolve_statement(whilestmt.body, &mut new_context),
                    label: whilestmt.label.clone(),
                }))
            },
            _ => statement,
        }
    }

    fn resolve_expression(&self, expr: Box<nodes::Expression>, context: &mut Context) -> Box<nodes::Expression> {
        match *expr {
            nodes::Expression::Var(ref ident) => {
                if context.var_map.map.contains_key(&ident.value) {
                    Box::new(nodes::Expression::Var(nodes::Identifier { value: context.var_map.map.get(&ident.value).unwrap().clone().0 }))
                } else {
                    panic!("Variable {} not found in scope", ident.value);
                }
            },
            nodes::Expression::Assignment(ref ident, ref expr) => {
                if context.var_map.map.contains_key(&ident.value) {
                    Box::new(nodes::Expression::Assignment(nodes::Identifier { value: context.var_map.map.get(&ident.value).unwrap().clone().0 }, self.resolve_expression(expr.clone(), context)))
                } else {
                    panic!("Variable {} not found in scope", ident.value);
                }
            },
            nodes::Expression::BinOp(ref left, ref op, ref right) => {
                Box::new(nodes::Expression::BinOp(self.resolve_expression(left.clone(), context), op.clone(), self.resolve_expression(right.clone(), context)))
            },
            nodes::Expression::UnaryOp(ref op, ref expr) => {
                Box::new(nodes::Expression::UnaryOp(op.clone(), self.resolve_expression(expr.clone(), context)))
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

        let pass = Pass::new(&program);
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

        let pass = Pass::new(&program);
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

        let pass = Pass::new(&program);
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

        let pass = Pass::new(&program);
        pass.run();
    }
}