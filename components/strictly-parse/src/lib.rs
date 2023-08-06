#![feature(trait_upcasting)]

mod combinators;
mod parser;

use strictly_ir::{code::Program, input_file::SourceProgram, location::Location};

#[salsa::jar(db = Db)]
pub struct Jar(parse_source_program);

pub trait Db: salsa::DbWithJar<Jar> + strictly_ir::Db {}
impl<T> Db for T where T: salsa::DbWithJar<Jar> + strictly_ir::Db {}

#[salsa::tracked]
pub fn parse_source_program(db: &dyn Db, source: SourceProgram) -> Program {
    let mut parser = parser::Parser {
        db,
        source_text: source.text(db),
        anchors: vec![],
        position: Location::start(),
    };
    parser.push_anchor(parser.position);

    let Some(statements) =
        combinators::higher_order::many(&mut parser, combinators::code::root_statement)
        else {
            panic!("Could not parse any statements");
        };

    Program::new(db, statements)
}
