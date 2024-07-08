use crate::parser::nodes;
use crate::semantic_analysis::symbol_table;

pub struct Pass {
    pub program: nodes::Program,
}

struct Context {
    pub func_return_type: nodes::Type,
}

impl Pass {
    pub fn new(program: &nodes::Program) -> Self {
        Self {
            program: program.clone(),
        }
    }

    pub fn run(&self) -> (nodes::Program, symbol_table::SymbolTable) {
        let mut symbol_table = symbol_table::SymbolTable::new();

        // put all the functions in there
        for function in &self.program.function_definitions {
            symbol_table.insert(function.function_name.clone(), nodes::Type::Fn(
                function.params.iter().map(|arg| self.convert_type(&arg.kind)).collect(),
                Box::new(self.convert_type(&function.return_type))
            ));
        }

        let mut program = nodes::Program {
            function_definitions: Vec::new(),
        };

        // now go over the functions contents and add the variables
        for function in &self.program.function_definitions {
            let context = Context {
                func_return_type: function.return_type.clone(),
            };

            for arg in &function.params {
                symbol_table.insert(arg.ident.value.clone(), self.convert_type(&arg.kind));
            }

            let body = match &*function.body {
                nodes::Statement::Compound(compound) => compound.statements.clone(),
                _ => unreachable!(),
            };

            let mut new_body: Vec<Box<nodes::Statement>> = Vec::new();

            for statement in body {
                new_body.push(self.typecheck_statement(statement, &mut symbol_table, &context));
            }

            program.function_definitions.push(nodes::FunctionDeclaration {
                function_name: function.function_name.clone(),
                params: function.params.clone(),
                return_type: function.return_type.clone(),
                body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements: new_body,
                })),
            });
        }

        (program, symbol_table)
    }

    fn convert_to(&self, e: nodes::Expression, t: nodes::Type, symbol_table: &symbol_table::SymbolTable) -> Box<nodes::Expression> {
        let typed_e = self.typecheck_expression(Box::new(e), symbol_table);
        
        if typed_e.1 == t {
            typed_e.0
        } else {
            let can_convert = vec![
                (nodes::Type::GenericInt, nodes::Type::I32),
                (nodes::Type::GenericInt, nodes::Type::I64),
                (nodes::Type::Generic32, nodes::Type::I32),
                (nodes::Type::Generic32, nodes::Type::I64),
                (nodes::Type::Generic32, nodes::Type::U32),
                (nodes::Type::Generic32, nodes::Type::U64),
                (nodes::Type::Generic64, nodes::Type::I64),
                (nodes::Type::Generic64, nodes::Type::U64),
            ];

            if !can_convert.contains(&(typed_e.1.clone(), t.clone())) {
                println!("{:#?}", typed_e.0);
                panic!("Cannot convert {:?} to {:?}", typed_e.1, t);
            }

            Box::new(nodes::Expression::Cast(typed_e.0, Some(t)))
        }
    }

    fn get_common_type(&self, left: nodes::Type, right: nodes::Type) -> nodes::Type {
        if left == right {
            left
        } else {
            nodes::Type::I64
        }
    }

    fn typecheck_statement(&self, statement: Box<nodes::Statement>, symbol_table: &mut symbol_table::SymbolTable, context: &Context) -> Box<nodes::Statement> {
        match *statement {
            nodes::Statement::VariableDeclaration(decl) => {
                let type_ = self.convert_type(&decl.kind);
                let ident = decl.ident.value.clone();

                if symbol_table.get(&ident).is_some() {
                    panic!("Variable {} already declared", ident);
                }
            
                symbol_table.insert(ident.clone(), type_.clone());

                
                let typed_expr = match decl.expr {
                    Some(expr) => self.typecheck_expression(expr, symbol_table),
                    None => {
                        return Box::new(nodes::Statement::Empty);
                    },
                };

                let converted_expr = self.convert_to(*typed_expr.0.clone(), type_.clone(), symbol_table);

                Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                    kind: type_,
                    ident: decl.ident,
                    expr: Some(converted_expr),
                }))
            },
            nodes::Statement::ReturnStatement(ret) => {
                let type_ = self.typecheck_expression(ret.return_value, symbol_table);

                if type_.1 != context.func_return_type {
                    //panic!("Return type mismatch: value {:?} and fn {:?}", type_.1, context.func_return_type);

                    // cast
                    let converted_expr = self.convert_to(*type_.0.clone(), context.func_return_type.clone(), symbol_table);

                    return Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
                        return_value: converted_expr,
                    }));
                }

                Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
                    return_value: type_.0,
                }))
            },
            nodes::Statement::ExpressionStatement(expr) => {
                let expr = self.typecheck_expression(expr.expression, symbol_table).0;

                Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
                    expression: expr,
                }))
            },
            nodes::Statement::IfStatement(if_) => {
                let cond = self.typecheck_expression(if_.condition, symbol_table).0;
                let cnsq = self.typecheck_statement(if_.consequence, symbol_table, context);

                let alt = if let Some(alt) = if_.alternative {
                    Some(self.typecheck_statement(alt, symbol_table, context))
                } else {
                    None
                };

                Box::new(nodes::Statement::IfStatement(nodes::IfStatement {
                    condition: cond,
                    consequence: cnsq,
                    alternative: alt,
                }))
            },
            nodes::Statement::WhileStatement(while_) => {
                let cond = self.typecheck_expression(while_.condition, symbol_table).0;
                let body = self.typecheck_statement(while_.body, symbol_table, context);

                Box::new(nodes::Statement::WhileStatement(nodes::WhileStatement {
                    condition: cond,
                    body: body,
                    label: while_.label.clone(),
                }))
            },
            nodes::Statement::Compound(compound) => {
                let mut new_statements = Vec::new();

                for statement in compound.statements {
                    new_statements.push(self.typecheck_statement(statement, symbol_table, context));
                }

                Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements: new_statements,
                }))
            },
            _ => unimplemented!()
        }
    }

    fn is_comparison(&self, op: nodes::BinOp) -> bool {
        match op {
            nodes::BinOp::Equal | nodes::BinOp::NotEqual |
            nodes::BinOp::LessThan | nodes::BinOp::LessThanEqual |
            nodes::BinOp::GreaterThan | nodes::BinOp::GreaterThanEqual => true,
            _ => false,
        }
    }

    fn typecheck_expression(&self, expression: Box<nodes::Expression>, symbol_table: &symbol_table::SymbolTable) -> (Box<nodes::Expression>, nodes::Type) {
        match *expression {
            nodes::Expression::Assignment(ident, expr, _) => {
                let type_ = if let Some(type_) = symbol_table.get(&ident.value) {
                    type_.clone()
                } else {
                    panic!("Variable {} not declared", ident.value);
                };

                let typed_expr = self.typecheck_expression(expr, symbol_table);

                let converted_expr = self.convert_to(*typed_expr.0.clone(), typed_expr.1.clone(), symbol_table);

                (Box::new(nodes::Expression::Assignment(ident, converted_expr, Some(type_.clone()))), type_)
            },
            nodes::Expression::BinOp(left, op, right, _) => {
                let typed_left = self.typecheck_expression(left, symbol_table);
                let typed_right = self.typecheck_expression(right, symbol_table);

                if op == nodes::BinOp::And || op == nodes::BinOp::Or {
                    return (Box::new(nodes::Expression::BinOp(typed_left.0, op, typed_right.0, Some(nodes::Type::I32))), nodes::Type::Bool);
                }

                let common_type = self.get_common_type(typed_left.1.clone(), typed_right.1.clone());

                let converted_left = self.convert_to(*typed_left.0.clone(), common_type.clone(), symbol_table);
                let converted_right = self.convert_to(*typed_right.0.clone(), common_type.clone(), symbol_table);

                let ret_type = if self.is_comparison(op) {
                    nodes::Type::Bool
                } else {
                    common_type
                };

                (Box::new(nodes::Expression::BinOp(converted_left, op, converted_right, Some(ret_type.clone()))), ret_type)
            },
            nodes::Expression::UnaryOp(op, expr, _) => {
                let typed_expr = self.typecheck_expression(expr, symbol_table);

                (Box::new(nodes::Expression::UnaryOp(op, typed_expr.0, Some(typed_expr.1.clone()))), typed_expr.1)
            },
            nodes::Expression::Var(ident, _) => {
                if let Some(type_) = symbol_table.get(&ident.value) {
                    (Box::new(nodes::Expression::Var(ident, Some(type_.clone()))), type_.clone())
                } else {
                    panic!("Variable {} not declared", ident.value);
                }
            },
            nodes::Expression::Literal(literal, _) => {
                match literal {
                    nodes::Literal::Generic32(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::Generic32(val), Some(nodes::Type::Generic32))), nodes::Type::Generic32),
                    nodes::Literal::I32(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::I32(val), Some(nodes::Type::GenericInt))), nodes::Type::GenericInt),
                    nodes::Literal::I64(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::I64(val), Some(nodes::Type::I64))), nodes::Type::I64),
                    nodes::Literal::U64(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::U64(val), Some(nodes::Type::U64))), nodes::Type::U64),
                    nodes::Literal::Generic64(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::Generic64(val), Some(nodes::Type::Generic64))), nodes::Type::Generic64),
                    nodes::Literal::Bool(val) => (Box::new(nodes::Expression::Literal(nodes::Literal::Bool(val), Some(nodes::Type::Bool))), nodes::Type::Bool),
                    #[allow(unreachable_patterns)]
                    _ => unimplemented!()
                }
            },
            nodes::Expression::FunctionCall(name, args, _) => {
                let op_func_type = symbol_table.get(&name);

                let function_type = match op_func_type {
                    Some(_) => op_func_type.unwrap().clone(),
                    _ => nodes::Type::I32 //panic!("Function {} not declared", name),
                };

                let (params, ret_type) = if let nodes::Type::Fn(ref params, ret_type) = function_type {
                    (params, *ret_type)
                } else {
                    panic!("Variable {} is not a function", name);
                };

                let mut typed_args: Vec<(Box<nodes::Expression>, nodes::Type)> = Vec::new();

                if args.len() != params.len() {
                    panic!("Function {} expects {} arguments, got {}", name, params.len(), args.len());
                }

                for (i, arg) in args.iter().enumerate() {
                    let typed_arg = self.typecheck_expression(arg.clone(), symbol_table);

                    let converted_arg = self.convert_to(*typed_arg.0.clone(), params[i].clone(), symbol_table);

                    typed_args.push((converted_arg, typed_arg.1));
                }

                let func_call = nodes::Expression::FunctionCall(name, typed_args.iter().map(|arg| arg.0.clone()).collect(), Some(ret_type.clone()));

                (Box::new(func_call), ret_type.clone())
            },
            nodes::Expression::Cast(expr, _) => {
                let typed_expr = self.typecheck_expression(expr.clone(), symbol_table);

                (Box::new(nodes::Expression::Cast(expr, Some(typed_expr.1.clone()))), typed_expr.1)
            },
            #[allow(unreachable_patterns)]
            _ => unimplemented!()
        }
    }

    fn convert_type(&self, type_: &nodes::Type) -> nodes::Type {
        match type_ {
            nodes::Type::Generic32 => nodes::Type::Generic32,
            nodes::Type::Generic64 => nodes::Type::Generic64,
            nodes::Type::GenericInt => nodes::Type::GenericInt,
            nodes::Type::I32 => nodes::Type::I32,
            nodes::Type::I64 => nodes::Type::I64,
            nodes::Type::U32 => nodes::Type::U32,
            nodes::Type::U64 => nodes::Type::U64,
            nodes::Type::Bool => nodes::Type::Bool,
            //nodes::Type::Identifier(ident) => typed_ast::Type::Identifier(ident.value.clone()),
            nodes::Type::Fn(params, return_type) => nodes::Type::Fn(
                params.iter().map(|arg| self.convert_type(arg)).collect(),
                Box::new(self.convert_type(return_type))
            ),
        }
    }
}