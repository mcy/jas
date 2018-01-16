extern crate classfile;
extern crate either;
extern crate base64;

#[macro_use]
pub mod reporting;

pub mod ast;
pub mod consts;
pub mod codegen;
pub mod lexer;
pub mod phase;
pub mod source_file;
pub mod util;

use phase::Phase;

use classfile::raw::io;

use std::env;
use std::fs;
use std::path;
use std::io as s_io;
use std::io::prelude::*;
use std::rc::Rc;

fn main() {

    for arg in env::args().into_iter().skip(1) {
        eprintln!("assembling: {}", arg);
        assemble(arg).unwrap();
    }
}

fn assemble<P: AsRef<path::Path>>(path: P) -> s_io::Result<()> {
    let source = Rc::new(source_file::SourceFile::from_file(path)?);
    let tokens = lexer::Lexer::run_and_error(vec![source]);
    let ast = ast::Parser::run_and_error(tokens);
    let sections = codegen::ClassSection::run_and_error(ast);
    let classes = codegen::Generator::run_and_error(sections);

    for (i, (path, class)) in classes.into_iter().enumerate() {

        let path = path.unwrap_or_else(|| vec![format!("unknown{}.class", i)]);

        let mut buf = path::PathBuf::new();
        buf.push("target");
        buf.push("jas");
        buf.extend(path.into_iter());
        if let Some(path) = buf.as_path().parent() {
            fs::create_dir_all(path);
        }

        let mut file = fs::File::create(buf)?;
        io::emit_class(&class, &mut file)?;
    }

    Ok(())
}