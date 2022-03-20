#![feature(box_patterns)]
use std::collections::HashMap;
use std::time::*;

use ariadne::*;
use chumsky::{error::Cheap, prelude::*, Stream};
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod codegen;
mod eval;
mod hir;
mod mcir;
mod mir;
mod parse;
pub use ast::*;
use codegen::*;
use eval::*;
use hir::*;
use mcir::*;
use mir::*;
use parse::Span;
pub use parse::*;

#[derive(Clone)]
pub enum Value {
    Integer(i32),
    Float(f64),
    Bool(bool),
    Nil,
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl std::cmp::Eq for Value {}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}i32", n),
            Self::Float(n) => write!(f, "{}f64", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::Nil => Type::Nil,
        }
    }

    pub fn pack(&self) -> u64 {
        match self {
            Self::Integer(i) => *i as i64 as u64,
            Self::Float(f) => u64::from_ne_bytes(f.to_ne_bytes()),
            Self::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            Self::Nil => 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Nil,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Integer => "i32",
            Self::Float => "f64",
            Self::Bool => "bool",
            Self::Nil => "nil",
        };
        write!(f, "{}", s)
    }
}

impl Value {
    fn as_i(&self) -> i32 {
        match self {
            Value::Integer(i) => *i,
            _ => unreachable!("{:?}", self),
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
    let (ast, errs, parse_errs) = parse(&all_codes.join(";"));
    if let Some(ast) = ast {
        /*let mut mir = MirContext::new();
                match mir.from_ast(&ast) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("Error in compiling AST. {:?}", err);
                        all_codes.pop();
                        return;
                    }
                };
                #[cfg(debug_assertions)]
                dbg!(&mir);
        */
        let mut hir = HirContext::new();
        //        dbg!(&ast);
        match hir.from_ast(&ast) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error in compiling AST. {:?}", err);
                all_codes.pop();
                return;
            }
        };
        let eval_res = Evaluator::eval_toplevel(&hir);
        //eprintln!("Evaluator: {:?}", eval_res);
        /*let mcir_context = McIrContext::from_mir(&mut mir);
        let mut codegen = Codegen::new();
        #[cfg(debug_assertions)]
        dbg!(&mcir_context);
        let (func, ty) = codegen.compile(&mcir_context);
        let jit_res = codegen.run(func, ty);
        eprintln!("JIT: {:?}", jit_res);*/
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
        let mut hir = HirContext::new();
        match hir.from_ast(&stmt) {
            Ok(_) => {}
            Err(err) => panic!("Error in compiling AST. {:?}", err),
        };
        #[cfg(debug_assertions)]
        dbg!(&hir);
        let now = Instant::now();
        let eval_res = Evaluator::eval_toplevel(&hir);
        eprintln!("eval: {:?} elapsed:{:?}", eval_res, now.elapsed());
        let now = Instant::now();
        let ruby_res = run_ruby(&all_codes);
        eprintln!("ruby: {:?} elapsed:{:?}", ruby_res, now.elapsed());
        assert_eq!(eval_res, ruby_res);
    }
    show_err(errs, parse_errs, code);
}

fn parse(
    code: &str,
) -> (
    Option<Vec<(Stmt, Span)>>,
    Vec<Cheap<char>>,
    Vec<Cheap<Token>>,
) {
    let len = code.len();
    let (tokens, errs) = lexer().parse_recovery(code);
    //dbg!(&tokens);
    let (ast, parse_errs) = if let Some(tokens) = tokens {
        parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()))
    } else {
        (None, vec![])
    };
    (ast, errs, parse_errs)
}

fn show_err(errs: Vec<Cheap<char>>, parse_errs: Vec<Cheap<Token>>, code: &str) {
    errs.into_iter().for_each(|e| {
        let mut rep = Report::build(ReportKind::Error, (), e.span().start);
        rep = rep
            .with_label(Label::new(e.span()).with_message(format!("unexpected:{:?}", e.label())));
        rep.finish().eprint(Source::from(code)).unwrap();
    });
    parse_errs.into_iter().for_each(|e| {
        let mut rep = Report::build(ReportKind::Error, (), e.span().start);
        rep = rep.with_label(Label::new(e.span()).with_message(format!(
            "unexpected:{}",
            match e.label() {
                Some(s) => s,
                None => "",
            }
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
    fn test_fib() {
        run_test(
            r#"
            def fib(x)
                if x<3 then
                    1
                else
                    fib(x-1)+fib(x-2)
                end
            end;
            fib(35)"#,
        );
    }

    #[test]
    fn test_fibpoly() {
        run_test(
            r#"
            def fib(x)
                if x<3 then
                    1
                else
                    fib(x-1)+fib(x-2)
                end
            end;
            fib(32)
            "#,
        );
        run_test(
            r#"
            def fib(x)
                if x<3 then
                    1
                else
                    fib(x-1)+fib(x-2)
                end
            end;
            fib(32.0)
            "#,
        );
    }

    #[test]
    fn test2() {
        run_test(
            r#"
            a=1
            while a<25 do
                a=a+1
            end
            a
            "#,
        );
    }

    #[test]
    fn test3() {
        run_test(
            r#"
        a=3;
        if a==1;
          3
        else
          4
        end"#,
        );
    }

    #[test]
    fn test4() {
        run_test(
            r#"
        def f(a,b)
          a + b
        end
        f(5,7)
        f(4,9)
        "#,
        );
    }

    #[test]
    fn test5() {
        run_test(
            r#"
        def f(a,b)
          a + b
        end
        f(5.1, 7)
        "#,
        );
    }
}
