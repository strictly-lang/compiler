use std::path::PathBuf;

#[salsa::input]
pub struct SourceProgram {
    pub path: PathBuf,
    /// The source of the program.
    ///
    /// The `return_ref` annotation makes the `text(db)` getter
    /// return an `&String` that refers directly into the database
    /// rather than returning a clone of the `String`. It is often
    /// used for types, like `String`, that are expensive to clone.
    #[return_ref]
    pub text: String,
}
