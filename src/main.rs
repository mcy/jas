
#[macro_use]
extern crate clap;

extern crate classfile;
extern crate base64;

#[macro_use]
pub mod reporting;

pub mod ast;
pub mod disasm;
pub mod consts;
pub mod codegen;
pub mod lexer;
pub mod phase;
pub mod sections;
pub mod source_file;
pub mod util;

use phase::Phase;

use classfile::raw;

use std::fs;
use std::path;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

fn main() {

    let matches = clap_app!(jas =>
        (author: "Miguel Young <mcyoung@mit.edu>")
        (about: "the Java assembler")
        (@arg input: +required "Input files for assembly or disassembly")
        (@arg disassemble: -d --disassemble "Switches to disassemble mode")
        (@arg output: -o --output "Output directory")
    ).get_matches();

    let output = matches.value_of("output").unwrap_or("");
    let disassemble = matches.is_present("disassemble");
    let files = matches.values_of("input").expect("no input files found").collect::<Vec<_>>();

    for file in files {
        if disassemble {
            eprintln!("disassembling: {}", file);
            let assembly = disasm::disassemble_from_file(file.clone()).unwrap();

            let mut buf = path::PathBuf::new();
            if !output.is_empty() {
                buf.push(output);
            }
            if let Some(path) = buf.as_path().parent() {
                fs::create_dir_all(path).unwrap();
            }

            buf.push(format!("disasm_{}.j", file.replace("/", ".")));

            eprintln!("  emitting: {}", buf.display());

            let mut file = fs::File::create(buf).unwrap();
            file.write_all(assembly.as_bytes()).unwrap();

        } else {

            eprintln!("assembling: {}", file);

            for (path, mut class) in assemble(file).unwrap() {
                let mut buf = path::PathBuf::new();
                if !output.is_empty() {
                    buf.push(output);
                }
                buf.extend(path.into_iter());
                if let Some(path) = buf.as_path().parent() {
                    fs::create_dir_all(path);
                }

                eprintln!("  emitting: {}", buf.display());

                let mut file = fs::File::create(buf).unwrap();
                file.write_all(&mut class).unwrap();
            }
        }
    }
}

fn assemble<P: AsRef<path::Path>>(path: P) -> io::Result<Vec<(Vec<String>, Vec<u8>)>> {
    let source = Rc::new(source_file::SourceFile::from_file(path)?);
    let tokens = lexer::Lexer::run_and_error(vec![source]);
    let ast = ast::Parser::run_and_error(tokens);
    let sections = sections::ClassSection::run_and_error(ast);
    let classes = codegen::Generator::run_and_error(sections);

    let mut results = Vec::new();

    for (i, (path, class)) in classes.into_iter().enumerate() {

        let path = path.unwrap_or_else(|| vec![format!("unknown{}.class", i)]);
        let mut bytes = Vec::new();
        raw::io::emit_class(&class, &mut bytes)?;
        results.push((path, bytes));
    }

    Ok(results)
}