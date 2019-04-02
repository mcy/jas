use crate::reporting::*;

use std::process::exit;

/// Represents an assembler phase (e.g., lexing, codegen)
pub trait Phase {

    type Input;
    type Output;

    fn run(input: Vec<Self::Input>) -> Reported<Vec<Self::Output>>;

    fn run_and_error(input: Vec<Self::Input>) -> Vec<Self::Output> {
        let reports = Self::run(input);

        for report in reports.reports.iter() {
            eprintln!("{}", report.format());
            eprintln!("");
        }

        if reports.has_errors() {
            eprintln!("aborted due to previous errors");
            exit(1);

        } else {
            reports.result.unwrap()
        }
    }
}