use crate::parser::nodes;
use crate::semantic_analysis::symbol_table;

pub struct Pass {
    pub program: nodes::Program,
}

impl Pass {
    pub fn new(program: &nodes::Program) -> Self {
        Self {
            program: program.clone(),
        }
    }

    pub fn run(&self) -> symbol_table::SymbolTable {
        let mut symbol_table = symbol_table::SymbolTable::new();

        // put all the functions in there
        for function in &self.program.function_definitions {
            symbol_table.insert(function.function_name.clone(), symbol_table::Type::Fn(
                function.params.iter().map(|arg| self.convert_type(&arg.kind)).collect(),
                Box::new(self.convert_type(&function.return_type))
            ));
        }

        // now go over the functions contents and add the variables
        for function in &self.program.function_definitions {
            for arg in &function.params {
                symbol_table.insert(arg.ident.value.clone(), self.convert_type(&arg.kind));
            }

            let body = match &*function.body {
                nodes::Statement::Compound(compound) => compound.statements.clone(),
                _ => unreachable!(),
            };

            for statement in body {
                self.typecheck_statement(statement, &mut symbol_table);
            }
        }

        symbol_table
    }

    fn typecheck_statement(&self, statement: Box<nodes::Statement>, symbol_table: &mut symbol_table::SymbolTable) {
        match *statement {
            nodes::Statement::VariableDeclaration(decl) => {
                let type_ = self.convert_type(&decl.kind);
                let ident = decl.ident.value;

                let expr_type = match decl.expr {
                    Some(expr) => self.typecheck_expression(expr, symbol_table),
                    None => type_.clone(),
                };

                if symbol_table.get(&ident).is_some() {
                    panic!("Variable {} already declared", ident);
                }

                symbol_table.insert(ident, type_);
            },
            nodes::Statement::ReturnStatement(ret) => {
                let type_ = self.typecheck_expression(ret.return_value, symbol_table);

                // todo: check if the return type is the same as the outer function return type
                if type_ != symbol_table::Type::Int {
                    panic!("Return type is not int");
                }
            },
            nodes::Statement::ExpressionStatement(expr) => {
                self.typecheck_expression(expr.expression, symbol_table);
            },
            nodes::Statement::IfStatement(if_) => {
                self.typecheck_expression(if_.condition, symbol_table);
                self.typecheck_statement(if_.consequence, symbol_table);

                if let Some(alt) = if_.alternative {
                    self.typecheck_statement(alt, symbol_table);
                }
            },
            nodes::Statement::WhileStatement(while_) => {
                self.typecheck_expression(while_.condition, symbol_table);
                self.typecheck_statement(while_.body, symbol_table);
            },
            nodes::Statement::Compound(compound) => {
                for statement in compound.statements {
                    self.typecheck_statement(statement, symbol_table);
                }
            },
            _ => unimplemented!()
        }
    }

    fn typecheck_expression(&self, expression: Box<nodes::Expression>, symbol_table: &symbol_table::SymbolTable) -> symbol_table::Type {
        match *expression {
            nodes::Expression::Assignment(ident, expr) => {
                let type_ = self.typecheck_expression(expr, symbol_table);

                if symbol_table.get(&ident.value).is_none() {
                    panic!("Variable {} not declared", ident.value);
                }

                if symbol_table.get(&ident.value).unwrap() != &type_ {
                    panic!("Variable {} declared as {:?}, but assigned as {:?}", ident.value, symbol_table.get(&ident.value).unwrap(), type_);
                } else {
                    type_
                }
            },
            nodes::Expression::BinOp(left, _, right) => {
                let left_type = self.typecheck_expression(left, symbol_table);
                let right_type = self.typecheck_expression(right, symbol_table);

                if left_type != right_type {
                    panic!("Type mismatch in binop: {:?} and {:?}", left_type, right_type);
                } else {
                    left_type
                }
            },
            nodes::Expression::UnaryOp(_, expr) => {
                self.typecheck_expression(expr, symbol_table)
            },
            nodes::Expression::Var(ident) => {
                if let Some(type_) = symbol_table.get(&ident.value) {
                    type_.clone()
                } else {
                    panic!("Variable {} not declared", ident.value);
                }
            },
            nodes::Expression::Literal(literal) => {
                match literal {
                    nodes::Literal::Int(_) => symbol_table::Type::Int,
                    _ => unimplemented!()
                }
            },
            nodes::Expression::FunctionCall(name, args) => {
                let op_func_type = symbol_table.get(&name);

                let function_type = match op_func_type {
                    Some(_) => op_func_type.unwrap().clone(),
                    _ => symbol_table::Type::Int //panic!("Function {} not declared", name),
                };

                let arg_types = args.iter().map(|arg| self.typecheck_expression(arg.clone(), symbol_table)).collect::<Vec<_>>();

                let ret_type = if let symbol_table::Type::Fn(ref params, ret) = function_type {
                    if params != &arg_types {
                        panic!("Function {} called with wrong arguments", name);
                    }
                    ret
                } else {
                    //panic!("Variable {} is not a function", name);
                    Box::new(symbol_table::Type::Int)
                };

                *ret_type
            },
            #[allow(unreachable_patterns)]
            _ => unimplemented!()
        }
    }

    fn convert_type(&self, type_: &nodes::Type) -> symbol_table::Type {
        match type_ {
            nodes::Type::Int => symbol_table::Type::Int,
            nodes::Type::I64 => symbol_table::Type::I64,
            nodes::Type::Identifier(ident) => symbol_table::Type::Identifier(ident.value.clone()),
            nodes::Type::Fn(params, return_type) => symbol_table::Type::Fn(
                params.iter().map(|arg| self.convert_type(arg)).collect(),
                Box::new(self.convert_type(return_type))
            ),
        }
    }
}