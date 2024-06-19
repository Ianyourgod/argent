use nix::unistd::execvp;
use std::ffi::{CStr, CString};

mod parser;
mod lexer;
mod code_gen;

fn help(err_code: i32) {
    println!(
"Usage: 
    ./compiler run <filename> - to run the file
    ./compiler compile <filename> - to compile the file
    ./compiler help - to show this message");
    std::process::exit(err_code);
}

fn compile_program(input: String, outfile_name: &String, include_output: bool) {
    let l = lexer::Lexer::new(input.clone());
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    let mut compiler = code_gen::CodeGen::new(program, Some(input));

    // write to file
    std::fs::write("output/temp.s", compiler.generate_code()).unwrap();

    // assemble
    let output = std::process::Command::new("gcc")
        .arg("output/temp.s")
        .arg("-o")
        .arg(outfile_name)
        .output()
        .expect("failed to assemble");

    if include_output {
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        let stderr = std::str::from_utf8(&output.stderr).unwrap();

        if stdout.len() > 0 {
            println!("{}", std::str::from_utf8(&output.stdout).unwrap());
        }
        if stderr.len() > 0 {
            println!("{}", std::str::from_utf8(&output.stderr).unwrap());
            std::process::exit(output.status.code().unwrap());
        }
    }
}

fn main() {
    // check if there is a file to read from in cmd args

    if std::env::args().len() > 1 {
        match std::env::args().nth(1).unwrap().as_str() {
            "help" => {
                help(0);
                panic!();
            },
            "run" => {
                let op_filename = std::env::args().nth(2);
                if op_filename.is_none() {
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, &outfile_name, true);

                let outfile_name_cstr = CString::new(outfile_name).expect("CString::new failed");

                let err = execvp::<&CStr>(&outfile_name_cstr, &[]);

                match err {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("failed to exec: {}", e);
                        std::process::exit(1);
                    }
                }
            },
            "compile" => {
                let op_filename = std::env::args().nth(2);
                if op_filename.is_none() {
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, &outfile_name, true);
            },
            _ => {
                help(2);
                panic!();
            }
        }
    } else {
        help(2);
        panic!();
    }
}
