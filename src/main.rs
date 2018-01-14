extern crate classfile;
extern crate either;
extern crate base64;

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

    let lexer = lexer::Lexer::new(include_str!("hello_compact.j").into());

    let tokens = lexer.collect::<Vec<_>>();

    let mut parser = ast::Parser::new(tokens);

    let ast = parser.consume_all();
    println!("{:?}", ast);

    let mut sections = trans::ClassSection::from_instructions(ast.unwrap()).unwrap();

    for (i, section) in sections.into_iter().enumerate() {
        let mut partial = trans::PartialClass::new();
        let x = partial.process_section(section);
        println!("{:?}", partial);
        println!("{:?}", x);

        let class = partial.assemble_class();

        let mut file = File::create("Hello.class").unwrap();
        io::emit_class(&class, &mut file).unwrap();
    }

}