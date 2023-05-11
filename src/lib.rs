use std::path::PathBuf;

use strictly_ir::input_file::SourceProgram;

pub fn compile(path: &PathBuf, content: String) -> String {
    let mut db = strictly_db::Database::default();

    let _source_program = SourceProgram::new(&mut db, path.to_path_buf(), content);

    return String::new();
}
