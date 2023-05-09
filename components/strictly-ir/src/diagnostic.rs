use crate::location::Location;

use derive_new::new;

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

#[derive(new, Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub start: Location,
    pub end: Location,
    pub message: String,
}
