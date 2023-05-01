mod lib;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    lib::main(
        file_path,
        fs::read_to_string(file_path).expect("Should have been able to read the file"),
    )
}
