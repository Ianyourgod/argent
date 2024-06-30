use crate::parser::nodes;

pub struct Pass {
    pub program: nodes::Program,
}

pub struct Context {
    pub label: Option<String>,
    pub label_count: i32, // This is a hack to generate unique "labels". a label is just so tacky knows what loop a break or continue statement is referring to
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
            let mut statements: Vec<Box<nodes::Statement>> = Vec::new();

            // todo: change the function definition to be a compound statement instead of a vec of statements
            let body_statements = match *function.body {
                nodes::Statement::Compound(ref compound) => compound.statements.clone(),
                _ => panic!("Expected compound statement"),
            };

            let mut context = Context {
                label: None,
                label_count: 0,
            };

            for statement in body_statements {
                self.label_statement(&statement, &mut statements, &mut context);
            }

            program.function_definitions.push(nodes::FunctionDeclaration {
                function_name: function.function_name.clone(),
                params: function.params.clone(),
                body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements,
                })),
                return_type: function.return_type.clone(),
            });
        }

        program
    }

    fn label_statement(&self, statement: &nodes::Statement, statements: &mut Vec<Box<nodes::Statement>>, context: &mut Context) {
        match statement {
            nodes::Statement::WhileStatement(ref while_statement) => {
                let body_statements = match *while_statement.body {
                    nodes::Statement::Compound(ref compound) => compound.statements.clone(),
                    _ => panic!("Expected compound statement"),
                };


                let old_label = context.label.clone();
                let new_label = context.label_count.to_string();
                context.label = Some(new_label.clone());
                context.label_count += 1;

                let mut new_body_statements: Vec<Box<nodes::Statement>> = Vec::new();

                for statement in body_statements {
                    self.label_statement(&statement, &mut new_body_statements, context);
                }

                context.label = old_label;

                statements.push(Box::new(nodes::Statement::WhileStatement(nodes::WhileStatement {
                    condition: while_statement.condition.clone(),
                    body: Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                        statements: new_body_statements,
                    })),
                    label: new_label,
                })));
            },
            nodes::Statement::BreakStatement(_) => {
                if context.label.is_none() {
                    panic!("Break statement outside of loop");
                }

                statements.push(Box::new(nodes::Statement::BreakStatement(context.label.clone().unwrap())));
            },
            nodes::Statement::ContinueStatement(_) => {
                if context.label.is_none() {
                    panic!("Continue statement outside of loop");
                }

                statements.push(Box::new(nodes::Statement::ContinueStatement(context.label.clone().unwrap())));
            },
            nodes::Statement::Compound(ref compound) => {
                let mut new_statements: Vec<Box<nodes::Statement>> = Vec::new();

                for statement in compound.statements.clone() {
                    self.label_statement(&statement, &mut new_statements, context);
                }

                statements.push(Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements: new_statements,
                })));
            },
            nodes::Statement::IfStatement(ref if_statement) => {
                let mut new_consequence_statements: Vec<Box<nodes::Statement>> = Vec::new();

                let consequence_statements = match *if_statement.consequence {
                    nodes::Statement::Compound(ref compound) => compound.statements.clone(),
                    _ => panic!("Expected compound statement"),
                };

                for statement in consequence_statements {
                    self.label_statement(&statement, &mut new_consequence_statements, context);
                }

                let new_consequence = Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                    statements: new_consequence_statements,
                }));

                let new_alternative = match &if_statement.alternative {
                    Some(alternative) => {
                        let mut new_alternative_statements: Vec<Box<nodes::Statement>> = Vec::new();

                        let alternative_statements = match **alternative {
                            nodes::Statement::Compound(ref compound) => compound.statements.clone(),
                            _ => panic!("Expected compound statement"),
                        };

                        for statement in alternative_statements {
                            self.label_statement(&statement, &mut new_alternative_statements, context);
                        }

                        Some(Box::new(nodes::Statement::Compound(nodes::CompoundStatement {
                            statements: new_alternative_statements,
                        })))
                    },
                    None => None,
                };

                statements.push(Box::new(nodes::Statement::IfStatement(nodes::IfStatement {
                    condition: if_statement.condition.clone(),
                    consequence: new_consequence,
                    alternative: new_alternative,
                })));
            },
            // todo: function declaration
            _ => statements.push(Box::new(statement.clone())),
        }
    }
}