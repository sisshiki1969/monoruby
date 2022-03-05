#![feature(box_patterns)]
use std::collections::HashMap;

use ariadne::*;
use chumsky::{prelude::*, Stream};
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
use parse::Span;
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
    let mut all_codes = vec![];
    loop {
        let readline = rl.readline("monoruby> ");
        match readline {
            Ok(code) => {
                rl.add_history_entry(code.as_str());
                run(&code, &mut all_codes);
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

fn run(code: &str, all_codes: &mut Vec<String>) {
    all_codes.push(code.to_string());
    let (ast, errs, parse_errs) = parse(&dbg!(all_codes.join(";")));
    if let Some(ast) = ast {
        if let Some(last_ast) = ast.last() {
            dbg!(last_ast);
        }
        let mut hir = HIRContext::new();
        match hir.from_ast(&ast) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error in compiling AST. {:?}", err);
                all_codes.pop();
                return;
            }
        };
        #[cfg(debug_assertions)]
        dbg!(&hir);
        let eval_res = Evaluator::eval_hir(&hir, 0, &[]);
        eprintln!("Evaluator: {:?}", eval_res);
        let mcir_context = McIrContext::from_hir(&mut hir);
        let mut codegen = Codegen::new();
        #[cfg(debug_assertions)]
        dbg!(&mcir_context);
        let jit_res = codegen.compile_and_run(&mcir_context);
        eprintln!("JIT: {:?}", jit_res);
        eprintln!("Evaluator: {:?}", eval_res);
        //eprintln!("Ruby output: {:?}", run_ruby(all_codes));
    } else {
        all_codes.pop();
    }
    show_err(errs, parse_errs, code);
}

pub fn run_test(code: &str) {
    let all_codes = vec![code.to_string()];
    let (ast, errs, parse_errs) = parse(code);
    if let Some(stmt) = ast {
        //dbg!(&stmt);
        let mut hir = HIRContext::new();
        match hir.from_ast(&stmt) {
            Ok(_) => {}
            Err(err) => panic!("Error in compiling AST. {:?}", err),
        };
        #[cfg(debug_assertions)]
        dbg!(&hir);
        let eval_res = Evaluator::eval_hir(&hir, 0, &[]);
        let mcir_context = dbg!(McIrContext::from_hir(&mut hir));
        let mut codegen = Codegen::new();
        //#[cfg(debug_assertions)]
        //dbg!(&mcir_context);
        let jit_res = codegen.compile_and_run(&mcir_context);
        assert_eq!(dbg!(&jit_res), dbg!(&eval_res));
        let ruby_res = run_ruby(&all_codes);
        assert_eq!(&jit_res, dbg!(&ruby_res));
    }
    show_err(errs, parse_errs, code);
}

fn parse(
    code: &str,
) -> (
    Option<Vec<(Stmt, Span)>>,
    Vec<Simple<char>>,
    Vec<Simple<Token>>,
) {
    let len = code.len();
    let (tokens, errs) = lexer().parse_recovery(code);
    let (ast, parse_errs) = if let Some(tokens) = tokens {
        parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()))
    } else {
        (None, vec![])
    };
    (ast, errs, parse_errs)
}

fn show_err(errs: Vec<Simple<char>>, parse_errs: Vec<Simple<Token>>, code: &str) {
    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let mut rep = Report::build(ReportKind::Error, (), e.span().start);
            let expected: Vec<_> = e.expected().filter_map(|o| o.as_ref()).collect();
            rep = rep.with_label(Label::new(e.span()).with_message(format!(
                "{:?} expected:{:?}",
                e.reason(),
                expected
            )));
            rep.finish().eprint(Source::from(code)).unwrap();
        });
}

fn run_ruby(code: &Vec<String>) -> Value {
    use std::process::Command;
    let code = code
        .iter()
        .map(|s| s.trim_matches('\n').to_owned())
        .collect::<Vec<String>>()
        .join(";");
    let output = Command::new("ruby")
        .args(&["-e", &format!("p(eval\"{}\")", code)])
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
                eprintln!("Ruby: {:?}", res);
                Value::Bool(false)
            }
        }
        Err(err) => {
            panic!("Error occured in executing Ruby. {:?}", err);
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
        run_test("-100/5");

        run_test("a = 55; a = a /5; a");
        run_test("1 < 2");
        run_test("1 <= 2");
        run_test("1 >= 2");
        run_test("1 > 2");
        run_test("1 == 2");
        run_test("1 != 2");
        run_test("10 < 2");
        run_test("10 <= 2");
        run_test("10 >= 2");
        run_test("10 > 2");
        run_test("10 == 2");
        run_test("10 != 2");

        run_test("1.9 < 2.1");
        run_test("1.9 <= 2.1");
        run_test("1.9 >= 2.1");
        run_test("1.9 > 2.1");
        run_test("1.9 == 2.1");
        run_test("1.9 != 2.1");
        run_test("10.3 < 2.1");
        run_test("10.3 <= 2.1");
        run_test("10.3 >= 2.1");
        run_test("10.3 > 2.1");
        run_test("10.3 == 2.1");
        run_test("10.3 != 2.1");
        run_test("a = 42; if a == 42 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a == 42.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a != 42.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a < 52.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a > 52.0 then 1.1 else 2.2 end");
        run_test("a = 42.0 > 52.0; if a then 1.1 else 2.2 end");
        run_test("a=42; b=35.0; c=7; def f(x) a=4; end; if a-b==c then 0 else 1 end");
    }

    #[test]
    fn test2() {
        run_test("def fib(x) if x < 3 then 1 else fib(x-1)+fib(x-2) end end; fib(10)");
    }
}
