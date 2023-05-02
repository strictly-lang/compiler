use clap::Parser;
mod lib;

#[derive(Parser)] // requires `derive` feature
#[command(name = "strictly")]
#[command(bin_name = "strictly")]
enum CLI {
    Compile(CompileArgs),
}

#[derive(clap::Args)]
#[command(author, version, about, long_about = None)]
struct CompileArgs {
    path: std::path::PathBuf,
}

fn main() {
    match CLI::parse() {
        CLI::Compile(compile_options) => {
            println!(
                "{}",
                lib::main(
                    &compile_options.path,
                    std::fs::read_to_string(&compile_options.path).unwrap(),
                )
            )
        }
    }
}
