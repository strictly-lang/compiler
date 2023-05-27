use std::path::PathBuf;

use strictly_ir::input_file::SourceProgram;
use strictly_parse::parse_source_program;

pub fn compile(path: &PathBuf, content: String) -> String {
    let mut db = strictly_db::Database::default();

    let source_program = SourceProgram::new(&mut db, path.to_path_buf(), content);

    let result = parse_source_program(&mut db, source_program);
    dbg!(result);
    return String::new();
}
