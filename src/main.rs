extern crate classfile;

pub mod lexer;

use classfile::raw::io;

use std::fs::File;
use std::io::prelude::*;

fn main() {

    let str = r#"
.class Foo
    .method isZero(I)Z
        aload_1
        ifeq 0x06
        iconst_0
        ireturn
        iconst_1
        ireturn
    .method str()Ljava/lang/String;
        lcm "foo"
        areturn
    .method $char()C
        sipush 'k'
        ireturn
    "#;

    let lexer = lexer::Lexer::new(include_str!("example.j").into());

    let tokens = lexer.collect::<Vec<_>>();
    println!("{:?}", tokens);

}