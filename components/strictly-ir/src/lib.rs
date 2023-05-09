pub mod code;
pub mod diagnostic;
pub mod input_file;
pub mod location;

#[salsa::jar(db = Db)]
pub struct Jar(
    input_file::SourceProgram,
    code::Program,
    code::VariableId,
    code::FunctionId,
    code::Function,
    diagnostic::Diagnostics,
);

pub trait Db: salsa::DbWithJar<Jar> {
    fn as_dyn_ir_db(&self) -> &dyn crate::Db;
}
impl<T: salsa::DbWithJar<Jar>> Db for T {
    fn as_dyn_ir_db(&self) -> &dyn crate::Db {
        self
    }
}
