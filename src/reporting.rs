use crate::source_file::*;

pub use crate::source_file::Spannable;

macro_rules! fatal_error {
    ($reports:ident; $($args:tt)+) => {{
        $reports.report(__report!($crate::reporting::ReportKind::Error, $($args)+));
        return $reports;
    }};
}

macro_rules! report_error {
    ($reports:ident; $($args:tt)+) => {{
        $reports.report(__report!($crate::reporting::ReportKind::Error, $($args)+));
    }};
}

macro_rules! report_warning {
    ($reports:ident; $($args:tt)+) => {{
        $reports.report(__report!($crate::reporting::ReportKind::Warning, $($args)+));
    }};
}

macro_rules! report_try {
    ($reports:expr; $val:expr) => {
        match $reports.merge($val) {
            Some(x) => x,
            None => return $reports,
        }
    };
    ($reports:expr; $val:expr, $default:expr) => {
        match $reports.merge($val) {
            Some(x) => x,
            None => $default,
        }
    }
}

macro_rules! report_cont {
    ($reports:expr; $val:expr) => {
        match $reports.merge($val) {
            Some(x) => x,
            None => continue,
        }
    }
}

macro_rules! report_break {
    ($reports:expr; $val:expr) => {
        match $reports.merge($val) {
            Some(x) => x,
            None => break,
        }
    }
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

    ($kind:expr, $message:expr; $($($locs:expr);+ $(;)*),*) => {
        $crate::reporting::Report {
            kind: $kind,
            message: $message.into(),
            locations: vec![$(__span!($($locs),+)),*],
        }
    };

    ($kind:expr, $message:expr, $($args:expr),* $(,)*; $($($locs:expr);* $(;)*),*) => {
        $crate::reporting::Report {
            kind: $kind,
            message: format!($message, $($args),*),
            locations: vec![$(__span!($($locs),*)),*],
        }
    };
}

macro_rules! __span {
    ($loc:expr, $message:expr, $($args:expr)* $(,)*) => {
        ($loc.span(), Some(format!($message, $($args),*)))
    };
    ($loc:expr, $message:expr) => {
        ($loc.span(), Some($message.into()))
    };
    ($loc:expr) => {
        ($loc.span(), None)
    };
}

#[derive(Clone, Debug)]
pub struct Report {
    pub kind: ReportKind,
    pub message: String,
    pub locations: Vec<(Span, Option<String>)>,
}

impl Report {

    pub fn format(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("{}: {}",
            match self.kind {
                ReportKind::Error => "error",
                ReportKind::Warning => "warning"
            },
            &self.message,
        ));

        if self.locations.is_empty() {
            lines.push("at <unknown>".into())
        } else {
            // FIXME: handle multiple locations and messages
            let (ref span, _) = self.locations[0];
            lines.push(format!(" at {}:{}:{}", &span.src.path, span.start.line + 1, span.start.column + 1));

            let (top, middle, bottom) = span.lines();

            let line_padding = format!("{}", span.end.line + 1).len().max(2);

            lines.push(format!("{1: >0$} | {2}", line_padding, "", top));

            for (i, str) in middle.iter().enumerate() {
                lines.push(format!("{1: >0$} | {2}", line_padding, span.start.line + 1 + i, str));
                if i == 0 {
                    if span.start.line == span.end.line {
                        let whitespace = span.start.column;
                        let underline = span.end.column - span.start.column;
                        lines.push(format!("{3: >0$} | {3: ^1$}{3:^^2$}", line_padding, whitespace, underline, ""));
                    } else {
                        let whitespace = span.start.column;
                        let underline = str.len() - span.start.column;
                        lines.push(format!("{3: >0$} | {3: ^1$}{3:^^2$}", line_padding, whitespace, underline, ""));
                    }
                } else if i == str.len() - 1 {
                    // we can assume there are multiple lines
                    let whitespace = 0;
                    let underline = span.end.column;
                    lines.push(format!("{3: >0$} | {3: ^1$}{3:^^2$}", line_padding, whitespace, underline, ""));
                } else {
                    let whitespace = 0;
                    let underline = str.len();
                    lines.push(format!("{3: >0$} | {3: ^1$}{3:^^2$}", line_padding, whitespace, underline, ""));
                }
            }

            lines.push(format!("{1: >0$} | {2}", line_padding, "", bottom));
        }

        lines.join("\n")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReportKind {
    Error, Warning,
}

#[derive(Debug)]
pub struct Reported<T> {
    pub result: Option<T>,
    pub reports: Vec<Report>,
}

impl<T> Reported<T> {

    pub fn new() -> Reported<T> {
        Reported {
            result: None,
            reports: Vec::new(),
        }
    }

    pub fn report(&mut self, report: Report) {
        self.reports.push(report);
    }

    pub fn complete(mut self, x: T) -> Reported<T> {
        self.result = Some(x);
        self
    }

    pub fn merge<S>(&mut self, other: Reported<S>) -> Option<S> {
        let Reported {
            result, mut reports,
        } = other;
        self.reports.append(&mut reports);
        result
    }

    pub fn unwrap(self) -> T {
        match self.result {
            Some(x) => x,
            None => {
                for report in self.reports {
                    eprintln!("{:?}", report);
                }
                panic!("aborted due to previous errors");
            }
        }
    }

    pub fn has_errors(&self) -> bool {
        self.reports.iter().any(|r| r.kind == ReportKind::Error)
    }
}