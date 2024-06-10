mod parser;
mod lexer;
mod code_gen;

fn main() {
    let mut outfile_name = "output/output".to_string();
    let mut input = String::new();
    // check if there is a file to read from in cmd args
    if std::env::args().len() > 1 {
        let filename = std::env::args().nth(1).unwrap();
        outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
        input = std::fs::read_to_string(filename).unwrap();
    } else {
        std::io::stdin().read_line(&mut input).unwrap();
    }

    let l = lexer::Lexer::new(input);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    let mut compiler = code_gen::CodeGen::new(program);

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
