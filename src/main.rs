#![feature(box_patterns)]
use std::collections::HashMap;

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

#[derive(Clone, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f64),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}i32", n),
            Self::Float(n) => write!(f, "{}f64", n),
        }
    }
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
    fn as_i(&self) -> i32 {
        match self {
            Value::Integer(i) => *i,
            _ => unreachable!(),
        }
    }

    fn as_f(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            _ => unreachable!(),
        }
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    let mut locals = vec![];
    let mut eval_locals = vec![];
    let mut local_map = HashMap::new();
    loop {
        let readline = rl.readline("monoruby> ");
        match readline {
            Ok(code) => {
                rl.add_history_entry(code.as_str());
                run_with_locals(&code, &mut locals, &mut local_map, &mut eval_locals);
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

fn run_with_locals(
    code: &str,
    locals: &mut Vec<u64>,
    local_map: &mut HashMap<String, (usize, Type)>,
    eval_locals: &mut Vec<Value>,
) {
    if code.len() == 0 {
        return;
    }
    match parser().parse(code) {
        Ok(expr) => {
            let mut hir = HIRContext::new();
            if let Err(err) = hir.from_ast(local_map, &expr) {
                eprintln!("{:?}", err);
                return;
            };
            #[cfg(debug_assertions)]
            dbg!(&hir);
            let eval_res = Evaluator::eval_hir(&hir, local_map, eval_locals);
            let mcir_context = MachineIRContext::from_hir(&mut hir);
            let mut codegen = Codegen::new();
            #[cfg(debug_assertions)]
            dbg!(&mcir_context);
            let jit_res = codegen.compile_and_run(&mcir_context, locals, local_map);
            eprintln!("Evaluator output: {:?}", eval_res);
            assert_eq!(eval_res, jit_res);
            run_ruby(code);
        }
        Err(err) => {
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

pub fn run(code: &str) {
    let mut locals = vec![];
    let mut eval_locals = vec![];
    let mut local_map = HashMap::new();
    run_with_locals(code, &mut locals, &mut local_map, &mut eval_locals);
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
