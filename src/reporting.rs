#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {

    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column, }
    }

    pub fn advance_col(&self) -> Position {
        Position::new(self.line, self.column + 1)
    }

    pub fn advance_line(&self) -> Position {
        Position::new(self.line + 1, 0)
    }

    pub fn advance_col_mut(&mut self) {
        *self = self.advance_col();
    }

    pub fn advance_line_mut(&mut self) {
        *self = self.advance_line();
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {

    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end, }
    }

    pub fn extend_to(&self, new_end: Position) -> Span {
        Span::new(self.start, new_end)
    }

    pub fn extend_to_mut(&mut self, new_end: Position) {
        *self = self.extend_to(new_end)
    }
}

pub type Result<T> = ::std::result::Result<T, Report>;

macro_rules! report_error {
    ($($args:tt)+) => { __report!($crate::reporting::ReportKind::Error, $($args)+) };
}


macro_rules! report_warning {
    ($($args:tt)+) => { __report!($crate::reporting::ReportKind::Warning, $($args)+) };
}

macro_rules! __report {
    ($kind:expr, $message:expr) => {
        $crate::reporting::Report {
            kind: $kind,
            message: $message.into(),
            locations: Vec::new(),
        }
    };

    ($kind:expr, $message:expr, $($args:expr),* $(,)*) => {
        $crate::reporting::Report {
            kind: $kind,
            message: format!($message, $($args),*),
            locations: Vec::new(),
        }
    };

    ($kind:expr, $message:expr; $($($locs:expr),+ $(,)*),*) => {
        $crate::reporting::Report {
            kind: $kind,
            message: $message.into(),
            locations: vec![$(__span!($($locs),+)),*],
        }
    };

    ($kind:expr, $message:expr, $($args:expr),* $(,)*; $($($locs:expr),+ $(,)*),*) => {
        $crate::reporting::Report {
            kind: $kind,
            message: format!($message, $($args),*),
            locations: vec![$(__span!($($locs),+)),*],
        }
    };
}

macro_rules! __span {
    ($loc:expr, $message:expr, $($args:expr)* $(,)*) => {
        ($loc, Some(format!($message, $($args),*)))
    };
    ($loc:expr, $message:expr) => {
        ($loc, Some($message.into()))
    };
    ($loc:expr) => {
        ($loc, None)
    };
}

#[derive(Clone, Debug)]
pub struct Report {
    pub kind: ReportKind,
    pub message: String,
    pub locations: Vec<(Span, Option<String>)>,
}

#[derive(Clone, Copy, Debug)]
pub enum ReportKind {
    Error, Warning,
}