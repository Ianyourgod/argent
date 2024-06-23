use nix::unistd::execvp;
use std::{ffi::{CStr, CString}, process::exit};

mod parser;
mod lexer;
mod code_gen;

fn help(err_code: i32) {
    println!(
"Usage: 
    argent run <filename> - to run the file
    argent build <filename> - to compile the file
    argent help - to show this message");

    std::process::exit(err_code);
}

fn compile_program(input: String, input_name: String, outfile_name: &String, include_output: bool) {
    let l = lexer::Lexer::new(input.clone());
    let mut p = parser::Parser::new(l);

    p.input_name = input_name;
    p.error_func = Some(error);

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

fn error(filename: String, input: String, error_message: String, line: usize, position: usize, length: usize, error_code: Option<i32>) {
    let lines = input.split('\n').collect::<Vec<&str>>();

    let error_line = lines[line];
    let trimmed_line = error_line.trim_start();
    let top_line = if line > 0 {
        lines[line - 1]
    } else {
        ""
    };
    let trimmed_top_line = top_line.trim_start();

    let final_top_line: &str;
    let final_error_line: &str;

    if top_line.len() - trimmed_top_line.len() < error_line.len() - trimmed_line.len() {
        final_top_line = trimmed_top_line;
        let split = error_line.split_at(top_line.len()-trimmed_top_line.len());
        final_error_line = split.1;
    } else {
        final_error_line = trimmed_line;
        let split = top_line.split_at(error_line.len()-trimmed_line.len());
        final_top_line = split.1;
    }

    let mut error_text = final_top_line.to_string();

    error_text.push_str("\n");
    error_text.push_str(final_error_line);

    let diff = error_line.len() - final_error_line.len();

    let mut arrows = String::new();
    for _ in 0..(position - diff - 1) {
        arrows.push_str(" ");
    }
    for _ in position..(position+length) {
        arrows.push_str("^")
    }

    let position = format!("--> {}:{}:{}", filename, line + 1, position);
    
    println!("{}\n{}\n{}\n{}",
        error_message,
        position,
        error_text,
        arrows
    );

    let code = if error_code.is_some() {
        error_code.unwrap()
    } else { 1 };

    exit(code);
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
                let copied_filename = filename.clone();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, copied_filename, &outfile_name, true);

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
            "build" => {
                let op_filename = std::env::args().nth(2);
                if op_filename.is_none() {
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();
                let copied_filename = filename.clone();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, copied_filename, &outfile_name, true);
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
