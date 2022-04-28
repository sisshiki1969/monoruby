#![feature(box_patterns)]
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::time::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod codegen;
mod eval;
mod hir;
mod interp;
mod mcir;
mod mir;
mod parse;
mod types;
mod value;
pub use ast::*;
use codegen::*;
use eval::*;
use hir::*;
use interp::*;
use mcir::*;
use mir::*;
use parse::Span;
pub use parse::*;
use types::*;
use value::*;

use clap;

/// Simple program to greet a person
#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct CLArgs {
    /// File name.
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let args = CLArgs::parse();

    match args.file {
        Some(file) => {
            let mut file = File::open(file).unwrap();
            let mut code = String::new();
            file.read_to_string(&mut code).unwrap();
            let _ = exec(&code);
        }
        None => {
            let mut rl = Editor::<()>::new();
            let mut all_codes = vec![];
            loop {
                let readline = rl.readline("monoruby> ");
                match readline {
                    Ok(code) => {
                        rl.add_history_entry(code.as_str());
                        run_repl(&code, &mut all_codes);
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
    }
}

fn exec(code: &str) -> Result<(), ()> {
    let ast = match MonorubyParser::parse(code) {
        Ok(ast) => ast,
        Err(_) => {
            eprintln!("Parse error.");
            return Err(());
        }
    };
    let gen = match FuncStore::new(&ast) {
        Ok(gen) => gen,
        Err(err) => {
            eprintln!("Error in compiling AST. {:?}", err);
            return Err(());
        }
    };
    let bccomp_val = BcCompiler::exec_toplevel(&gen);
    eprintln!("bccomp: {:?}", bccomp_val);
    Ok(())
}

fn repl_exec(code: &str) -> Result<(), ()> {
    let ast = match MonorubyParser::parse(code) {
        Ok(ast) => ast,
        Err(_) => {
            eprintln!("Parse error.");
            return Err(());
        }
    };
    let gen = match FuncStore::new(&ast) {
        Ok(gen) => gen,
        Err(err) => {
            eprintln!("Error in compiling AST. {:?}", err);
            return Err(());
        }
    };

    let interp_val = Interp::eval_toplevel(&gen);
    eprintln!("Interp: {:?}", interp_val);

    let bccomp_val = BcCompiler::exec_toplevel(&gen);
    eprintln!("bccomp: {:?}", bccomp_val);

    Ok(())
}

fn run_repl(code: &str, all_codes: &mut Vec<String>) {
    all_codes.push(code.to_string());
    if let Err(_) = repl_exec(&all_codes.join(";")) {
        all_codes.pop();
    };
}

pub fn run_test(code: &str) {
    dbg!(code);
    let all_codes = vec![code.to_string()];
    let ast = match MonorubyParser::parse(code) {
        Ok(ast) => ast,
        Err(_) => panic!("Parse error."),
    };
    let gen = match FuncStore::new(&ast) {
        Ok(gen) => gen,
        Err(err) => {
            panic!("Error in compiling AST. {:?}", err);
        }
    };
    //#[cfg(debug_assertions)]
    //dbg!(&gen);
    let now = Instant::now();
    let interp_val = Interp::eval_toplevel(&gen);
    eprintln!("interp: {:?} elapsed:{:?}", interp_val, now.elapsed());

    let now = Instant::now();
    let bccomp_val = BcCompiler::exec_toplevel(&gen);
    eprintln!("bccomp: {:?} elapsed:{:?}", bccomp_val, now.elapsed());

    assert_eq!(interp_val, bccomp_val);

    //dbg!(&stmt);
    let mut hir = HirContext::new();
    match hir.from_ast(&ast) {
        Ok(_) => {}
        Err(err) => panic!("Error in compiling AST. {:?}", err),
    };
    //#[cfg(debug_assertions)]
    //dbg!(&hir);
    let now = Instant::now();
    let eval_res = Evaluator::eval_toplevel(&hir);
    eprintln!("eval: {:?} elapsed:{:?}", eval_res, now.elapsed());

    let now = Instant::now();
    let ruby_res = run_ruby(&all_codes);
    eprintln!("ruby: {:?} elapsed:{:?}", ruby_res, now.elapsed());
    assert_eq!(interp_val.unpack(), ruby_res);
}

fn run_ruby(code: &Vec<String>) -> RV {
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
                RV::Integer(n as i32)
            } else if let Ok(n) = res.parse::<f64>() {
                RV::Float(n)
            } else if res == "true" {
                RV::Bool(true)
            } else if res == "false" {
                RV::Bool(false)
            } else if res == "nil" {
                RV::Nil
            } else {
                eprintln!("Ruby: {:?}", res);
                RV::Bool(false)
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
    fn test0() {
        run_test("");
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
    }

    #[test]
    fn test1() {
        run_test("a=42; b=35.0; c=7; def f(x) a=4; end; if a-b==c then 0 else 1 end");
        run_test("def fn(x) x*2 end; a=42; c=b=a+7; d=b-a; e=b*d; d=f=fn(e); f=d/a");
        run_test("a=42; b=-a");
        run_test("a=42; a; b=-a");
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
    #[ignore]
    fn bench_fibo() {
        run_test(
            r#"
            def fib(x)
                if x<3 then
                    1
                else
                    fib(x-1)+fib(x-2)
                end
            end;
            fib(40)
            "#,
        );
    }

    #[test]
    fn test2() {
        run_test(
            r#"
            a=1
            b=while a<25000000 do
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
