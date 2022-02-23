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
    Bool(bool),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}i32", n),
            Self::Float(n) => write!(f, "{}f64", n),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Bool,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Integer => "i32",
            Self::Float => "f64",
            Self::Bool => "bool",
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
    let mut all_codes = vec![];
    loop {
        let readline = rl.readline("monoruby> ");
        match readline {
            Ok(code) => {
                rl.add_history_entry(code.as_str());
                run_with_locals(
                    &code,
                    &mut locals,
                    &mut local_map,
                    &mut eval_locals,
                    &mut all_codes,
                );
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
    all_codes: &mut Vec<String>,
) {
    if code.len() == 0 {
        return;
    }
    match parser().parse(code) {
        Ok(expr) => {
            //dbg!(&expr);
            let mut hir = HIRContext::new();
            let ret_ty = match hir.from_ast(local_map, &expr) {
                Ok((_, ty)) => ty,
                Err(err) => {
                    eprintln!("{:?}", err);
                    return;
                }
            };
            #[cfg(debug_assertions)]
            dbg!(&hir);
            let eval_res = Evaluator::eval_hir(&hir, local_map, eval_locals);
            all_codes.push(code.to_string());
            let mcir_context = McIrContext::from_hir(&mut hir);
            let mut codegen = Codegen::new();
            #[cfg(debug_assertions)]
            dbg!(&mcir_context);
            let jit_res = codegen.compile_and_run(&mcir_context, locals, local_map, ret_ty);
            eprintln!("JIT: {:?}", jit_res);
            eprintln!("Evaluator: {:?}", eval_res);
            eprintln!("Ruby output: {:?}", run_ruby(all_codes));
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

pub fn run_test(code: &str) {
    if code.len() == 0 {
        return;
    }
    let mut locals = vec![];
    let mut local_map = HashMap::default();
    let mut eval_locals = vec![];
    let all_codes = vec![code.to_string()];
    match parser().parse(code) {
        Ok(expr) => {
            //dbg!(&expr);
            let mut hir = HIRContext::new();
            let ret_ty = match hir.from_ast(&mut local_map, &expr) {
                Ok((_, ty)) => ty,
                Err(err) => panic!("Error in compiling AST. {:?}", err),
            };
            //#[cfg(debug_assertions)]
            //dbg!(&hir);
            let eval_res = Evaluator::eval_hir(&hir, &mut local_map, &mut eval_locals);
            let mcir_context = McIrContext::from_hir(&mut hir);
            let mut codegen = Codegen::new();
            //#[cfg(debug_assertions)]
            //dbg!(&mcir_context);
            let jit_res =
                codegen.compile_and_run(&mcir_context, &mut locals, &mut local_map, ret_ty);
            assert_eq!(dbg!(&jit_res), dbg!(&eval_res));
            let ruby_res = run_ruby(&all_codes);
            assert_eq!(&jit_res, dbg!(&ruby_res));
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
            rep.finish().eprint(Source::from(code)).unwrap();
            panic!()
        }
    };
}

fn run_ruby(code: &Vec<String>) -> Value {
    use std::process::Command;
    let code = code
        .iter()
        .map(|s| s.trim_matches('\n').to_owned())
        .collect::<Vec<String>>()
        .join(";");
    let output = Command::new("ruby")
        .args(&["-e", &format!("puts(eval\"{}\")", code)])
        .output();
    match &output {
        Ok(output) => {
            let res = std::str::from_utf8(&output.stdout)
                .unwrap()
                .trim_matches('\n');
            if let Ok(n) = res.parse::<i64>() {
                Value::Integer(n as i32)
            } else if let Ok(n) = res.parse::<f64>() {
                Value::Float(n)
            } else if res == "true" {
                Value::Bool(true)
            } else if res == "false" {
                Value::Bool(false)
            } else {
                unreachable!("{:?}", res)
            }
        }
        Err(err) => {
            eprintln!("{:?}", err);
            panic!();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        run_test("4 * (2.9 + 7 / (1.15 - 6))");
        run_test("-4 * (2.9 + 7 / (-1.15 - 6))");
        run_test("1.5 + (2.0 + 3) + 1.1");
        run_test("a = 55; a = a /5; a");
        run_test("a = 42; if a == 42 then 1.1 else 2.2 end");
    }
}
