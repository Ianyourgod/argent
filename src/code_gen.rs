#![allow(dead_code)]

use crate::tacky;

pub mod nodes;

mod firstpass;
mod secondpass;
mod thirdpass;

/*
const BYTE: &str = "b";
const WORD: &str = "w";
const LONG: &str = "l";
const QUAD: &str = "q";
*/

pub struct CodeGen {
    pub program: tacky::nodes::Program,
    source: String
}

impl CodeGen {
    pub fn new(program: tacky::nodes::Program, source: Option<String>) -> CodeGen {
        let src: String;
        if source.is_some() {
            src = source.unwrap();
        } else {
            src = String::new();
        }
        
        CodeGen { program, source: src }
    }

    pub fn generate_code(&mut self) -> nodes::Program {
        let fp = firstpass::Pass::new(&self.program);

        let first_pass_output = fp.run();

        let mut sp = secondpass::Pass::new(&first_pass_output);

        let second_pass_output = sp.run();

        let tp = thirdpass::Pass::new(&second_pass_output);

        let third_pass_output = tp.run();

        third_pass_output
    }
}