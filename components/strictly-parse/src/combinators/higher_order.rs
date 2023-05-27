use crate::parser::Parser;

pub fn many<T: std::fmt::Debug>(
    parser: &mut Parser,
    f: impl FnOnce(&mut Parser) -> Option<T>,
) -> Option<Vec<T>> {
    None
}
