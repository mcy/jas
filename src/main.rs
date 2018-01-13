extern crate classfile;
extern crate either;

#[macro_use]
pub mod reporting;

pub mod ast;
pub mod consts;
pub mod lexer;
pub mod codegen;
pub mod trans;

use classfile::raw::io;

use std::fs::File;
use std::io::prelude::*;

fn main() {

    let lexer = lexer::Lexer::new(include_str!("example.j").into());

    let tokens = lexer.collect::<Vec<_>>();

    let mut parser = ast::Parser::new(tokens);

    let ast = parser.consume_all().unwrap();
    println!("{:?}", ast);

    let mut sections = trans::ClassSection::from_instructions(ast).unwrap();

    for section in sections {
        let mut partial = trans::PartialClass::new();
        let x = partial.process_section(section);
        println!("{:?}", partial);
        println!("{:?}", x);
    }

}