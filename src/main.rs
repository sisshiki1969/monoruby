#![feature(box_patterns)]
//extern crate ariadne;
use ariadne::*;
//extern crate chumsky;
use chumsky::prelude::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod codegen;
mod eval;
mod hir;
mod mcir;
mod parse;
pub use ast::*;
use codegen::*;
use eval::*;
use hir::*;
use mcir::*;
pub use parse::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f64),
}

#[derive(Clone, Copy, PartialEq)]
pub enum Type {
    Integer,
    Float,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Integer => "i32",
            Self::Float => "f64",
        };
        write!(f, "{}", s)
    }
}

impl Value {
    fn as_i(self) -> i32 {
        match self {
            Value::Integer(i) => i,
            _ => unreachable!(),
        }
    }

    fn as_f(self) -> f64 {
        match self {
            Value::Float(f) => f,
            _ => unreachable!(),
        }
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("monoruby> ");
        match readline {
            Ok(code) => {
                rl.add_history_entry(code.as_str());
                run(&code);
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn run(code: &str) {
    if code.len() == 0 {
        return;
    }
    match parser().parse(code) {
        Ok(expr) => {
            let mut hir = HIRContext::new();
            hir.from_ast(&expr);
            let eval_res = Evaluator::eval_hir(dbg!(&hir));
            let mut mcir_context = MachineIRContext::new();
            mcir_context.from_hir(&hir);
            let mut codegen = Codegen::new();
            let jit_res = codegen.compile_and_run(dbg!(&mcir_context));
            eprintln!("Evaluator output: {:?}", eval_res);
            assert_eq!(eval_res, jit_res);
            run_ruby(code);
        }
        Err(err) => {
            dbg!(&err);
            let mut rep = Report::build(ReportKind::Error, (), 0);
            for e in err {
                let expected: Vec<_> = e.expected().filter_map(|o| o.as_ref()).collect();
                rep = rep.with_label(Label::new(e.span()).with_message(format!(
                    "{:?} expected:{:?}",
                    e.reason(),
                    expected
                )));
            }
            rep.finish().print(Source::from(code)).unwrap();
        }
    };
}

fn run_ruby(code: &str) {
    use std::process::Command;
    let output = Command::new("ruby")
        .args(&["-e", &format!("puts({})", code)])
        .output();
    let res = match &output {
        Ok(output) => std::str::from_utf8(&output.stdout).unwrap().to_string(),
        Err(err) => err.to_string(),
    };
    eprintln!("Ruby output: {}", res);
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        run("4 * (2.9 + 7 / (1.15 - 6))");
        run("-4 * (2.9 + 7 / (-1.15 - 6))");
        run("1.5 + (2.0 + 3) + 1.1");
    }
}
