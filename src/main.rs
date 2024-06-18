mod parser;
mod lexer;
mod code_gen;

fn help(err_code: i32) {
    println!("Usage: ./main run [filename]");
    std::process::exit(err_code);
}

fn main() {
    let outfile_name: String;
    let input: String;
    // check if there is a file to read from in cmd args

    if std::env::args().len() > 1 {
        match std::env::args().nth(1).unwrap().as_str() {
            "help" => {
                help(0);
                panic!();
            },
            "run" => {
                let filename = std::env::args().nth(2).unwrap();
                outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                input = std::fs::read_to_string(filename).unwrap();
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
        .arg(&outfile_name)
        .output()
        .expect("failed to assemble");

    println!("{}", std::str::from_utf8(&output.stdout).unwrap());
    println!("{}", std::str::from_utf8(&output.stderr).unwrap());

    /* run
    let output = std::process::Command::new("./output")
        .output()
        .expect("failed to run");

    println!("{}", std::str::from_utf8(&output.stdout).unwrap());
    */
}
