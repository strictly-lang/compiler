use derive_new::new;

/// Represents a specific location into the source string
/// as a utf-8 offset.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location(usize);

impl Location {
    pub fn as_usize(self) -> usize {
        self.0
    }

    pub fn start() -> Self {
        Self(0)
    }
}

impl std::ops::Add<Offset> for Location {
    type Output = Location;

    fn add(self, rhs: Offset) -> Self::Output {
        Location(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for Location {
    type Output = Location;

    fn add(self, rhs: usize) -> Self::Output {
        Location(self.0 + rhs)
    }
}

impl std::ops::AddAssign<usize> for Location {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs
    }
}

impl std::ops::Sub<Location> for Location {
    type Output = Offset;

    fn sub(self, rhs: Location) -> Self::Output {
        Offset(self.0 - rhs.0)
    }
}

/// Represents an offset in the source program relative to some anchor.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offset(usize);

impl Offset {
    pub fn location(self, anchor: Location) -> Location {
        Location(self.0 + anchor.0)
    }
}

/// Stores the location of a piece of IR within the source text.
/// Spans are not stored as absolute values but rather relative to some enclosing anchor
/// (some struct that implements the `Anchor` trait).
/// This way, although the location of the anchor may change, the spans themselves rarely do.
/// So long as a function doesn't convert the span into its absolute form,
/// and thus read the anchor's precise location, it won't need to re-execute, even if the anchor
/// has moved about in the file.
///
/// **NB:** It is your job, when converting the span into relative positions,
/// to supply the correct anchor! For example, the anchor for the expressions
/// within a function body is the function itself.
#[derive(new, Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    /// Start of the span, relative to the anchor.
    pub start: Offset,

    /// End of the span, relative to the anchor.
    pub end: Offset,
}

impl Span {
    /// Returns the absolute (start, end) points of this span, relative to the given anchor.
    pub fn absolute_locations(
        &self,
        db: &dyn crate::Db,
        anchor: &dyn Anchor,
    ) -> (Location, Location) {
        let base = anchor.anchor_location(db);
        (base + self.start, base + self.end)
    }

    /// Compute the absolute start of the span, relative to the given anchor.
    pub fn start(&self, db: &dyn crate::Db, anchor: &dyn Anchor) -> Location {
        self.absolute_locations(db, anchor).0
    }

    /// Compute the absolute end of the span, relative to the given anchor.
    pub fn end(&self, db: &dyn crate::Db, anchor: &impl Anchor) -> Location {
        self.absolute_locations(db, anchor).1
    }
}

pub trait Anchor {
    fn anchor_location(&self, db: &dyn crate::Db) -> Location;
}
