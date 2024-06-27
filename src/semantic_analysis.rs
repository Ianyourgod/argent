use crate::parser;
mod variable_resolution_pass;
mod loop_labeling_pass;

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
        let first_pass = variable_resolution_pass::Pass::new(&self.ast);

        let program = first_pass.run();

        let second_pass = loop_labeling_pass::Pass::new(&program);

        let program = second_pass.run();

        program
    }
}