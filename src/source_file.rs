use std::io;
use std::io::prelude::*;
use std::fs;
use std::path;

use std::rc::Rc;

#[derive(Clone)]
pub struct SourceFile {
    pub path: String,
    pub lines: Vec<String>,
}

use std::fmt;

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("path", &self.path)
            .finish()
    }
}

impl SourceFile {

    pub fn from_file<P: AsRef<path::Path>>(path: P) -> io::Result<SourceFile> {
        let name = path.as_ref().to_string_lossy().into_owned();
        let mut file = fs::OpenOptions::new().read(true).open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(SourceFile::from_str(name, &contents))
    }

    pub fn from_str(path: String, source: &str) -> SourceFile {
        SourceFile {
            path,
            lines: source.split("\n").map(Into::into).collect(),
        }
    }
}

// TODO: make SourceFile a phase

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

#[derive(Clone, Debug)]
pub struct Span {
    pub src: Rc<SourceFile>,
    pub start: Position,
    pub end: Position,
}

impl Span {

    #[inline]
    pub fn new(src: Rc<SourceFile>, start: Position, end: Position) -> Span {
        Span { src, start, end, }
    }

    #[inline]
    pub fn extend_to<S: Spannable>(&self, new_end: &S) -> Span {
        Span::new(self.src.clone(), self.start, new_end.span().end)
    }

    #[inline]
    pub fn extend_to_mut<S: Spannable>(&mut self, new_end: &S) {
        *self = self.extend_to(new_end)
    }

    pub fn lines(&self) -> (&str, &[String], &str) {
        let top = if self.start.line == 0 {
            ""
        } else {
            &self.src.lines[self.start.line - 1].as_str()
        };

        let middle = &self.src.lines[self.start.line..self.end.line + 1];

        let bottom = if self.end.line + 1 >= self.src.lines.len() {
            ""
        } else {
            &self.src.lines[self.end.line + 1].as_str()
        };

        (top, middle, bottom)
    }
}

pub trait Spannable {

    fn span(&self) -> Span;
}

impl Spannable for Span {

    #[inline]
    fn span(&self) -> Span {
        self.clone()
    }
}

impl<'a, T> Spannable for &'a T where T: Spannable {

    #[inline]
    fn span(&self) -> Span {
        T::span(self)
    }
}

impl<T> Spannable for Box<T> where T: Spannable {

    #[inline]
    fn span(&self) -> Span {
        T::span(&*self)
    }
}