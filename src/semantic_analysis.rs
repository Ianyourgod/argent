use crate::parser;
mod variable_resolution_pass;

pub struct Analysis {
    pub ast: parser::nodes::Program,
}

impl Analysis {
    pub fn new(ast: parser::nodes::Program) -> Self {
        Self {
            ast,
        }
    }

    pub fn run(&mut self) -> parser::nodes::Program {
        let mut first_pass = variable_resolution_pass::Pass::new(&self.ast);

        let program = first_pass.run();

        program
    }
}