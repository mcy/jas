extern crate classfile;

#[macro_use]
pub mod reporting;

pub mod ast;
pub mod lexer;

use classfile::raw::io;

use std::fs::File;
use std::io::prelude::*;

fn main() {

    let lexer = lexer::Lexer::new(include_str!("example.j").into());

    let tokens = lexer.collect::<Vec<_>>();

    let mut parser = ast::Parser::new(tokens);

    let ast = parser.consume_all().unwrap();

    println!("{:?}", ast);

}