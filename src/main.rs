#![feature(box_patterns)]
pub use ruruby_parse::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
#[cfg(not(debug_assertions))]
use std::time::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod executor;
mod type_infer;
mod types;
mod value;
pub use ast::*;
use executor::*;
use value::*;

use clap;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None, trailing_var_arg = true)]
struct CommandLineArgs {
    /// one lineer. several -e's allowed. Omit [programfile]
    #[clap(short, multiple_occurrences = true)]
    exec: Vec<String>,
    /// print the version number, then turn on verbose mode
    #[clap(short)]
    verbose: bool,
    /// File name.
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let args = CommandLineArgs::parse();

    if !args.exec.is_empty() {
        for code in args.exec {
            let _ = exec(&code);
        }
        return;
    }

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

fn exec(code: &str) -> Result<(), MonorubyErr> {
    let res = match Parser::parse_program(code.to_string(), std::path::Path::new("REPL"), "eval") {
        Ok(ast) => ast,
        Err(err) => {
            err.source_info.show_loc(&err.loc);
            return Err(MonorubyErr::Parse(err));
        }
    };
    let mut store = Globals::new(res.id_store);
    store.compile_main(res.node)?;
    #[cfg(debug_assertions)]
    {
        let interp_val = Interp::eval_toplevel(&mut store);
        eprintln!("interp: {:?}", interp_val);
    }

    let _ = jitcompiler(&mut store);

    Ok(())
}

fn repl_exec(code: &str) -> Result<Value, MonorubyErr> {
    let res = match Parser::parse_program(code.to_string(), std::path::Path::new("REPL"), "eval") {
        Ok(ast) => ast,
        Err(err) => {
            err.source_info.show_loc(&err.loc);
            return Err(MonorubyErr::Parse(err));
        }
    };
    let mut store = Globals::new(res.id_store);
    store.compile_main(res.node)?;

    let interp_val = Interp::eval_toplevel(&mut store);
    eprintln!("interp: {:?}", interp_val);

    jitcompiler(&mut store)
}

fn run_repl(code: &str, all_codes: &mut Vec<String>) {
    all_codes.push(code.to_string());
    if let Err(err) = repl_exec(&all_codes.join(";")) {
        eprintln!("{:?}", err);
        all_codes.pop();
    };
}

pub fn run_test(code: &str) {
    #[cfg(debug_assertions)]
    dbg!(code);
    let all_codes = vec![code.to_string()];
    let res = Parser::parse_program(code.to_string(), std::path::Path::new(""), "")
        .unwrap_or_else(|err| panic!("Error in parsing. {:?}", err));
    let mut store = Globals::new(res.id_store);
    store
        .compile_main(res.node)
        .unwrap_or_else(|err| panic!("Error in compiling AST. {:?}", err));
    #[cfg(not(debug_assertions))]
    let now = Instant::now();
    let interp_val = Interp::eval_toplevel(&mut store.clone());
    #[cfg(not(debug_assertions))]
    eprintln!("interp: {:?} elapsed:{:?}", interp_val, now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("interp: {:?}", interp_val);

    let jit_val = jitcompiler(&mut store);

    let interp_val = interp_val.unwrap();
    let jit_val = jit_val.unwrap();

    assert_eq!(interp_val, jit_val);

    //dbg!(&stmt);
    /*let mut hir = HirContext::new();
    match hir.from_ast(&ast) {
        Ok(_) => {}
        Err(err) => panic!("Error in compiling AST. {:?}", err),
    };
    let now = Instant::now();
    let eval_res = Evaluator::eval_toplevel(&hir);
    eprintln!("eval: {:?} elapsed:{:?}", eval_res, now.elapsed());
    */

    let ruby_res = run_ruby(&all_codes);

    assert_eq!(interp_val.unpack(), ruby_res);
}

fn jitcompiler(gen: &mut Globals) -> Result<Value, MonorubyErr> {
    #[cfg(not(debug_assertions))]
    let now = Instant::now();
    let jit_val = Interp::jit_exec_toplevel(gen);
    #[cfg(not(debug_assertions))]
    eprintln!("jit: {:?} elapsed:{:?}", jit_val, now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("jit: {:?}", jit_val);
    jit_val
}

fn run_ruby(code: &Vec<String>) -> RV {
    use std::process::Command;
    let code = code
        .iter()
        .map(|s| s.trim_matches('\n').to_owned())
        .collect::<Vec<String>>()
        .join(";");

    #[cfg(not(debug_assertions))]
    let now = Instant::now();
    let output = Command::new("ruby")
        .args(&[
            "-e",
            &format!("x=(eval\"def f() {} end; f\");puts;p(x)", code),
        ])
        .output();

    let res = match &output {
        Ok(output) => {
            let res = std::str::from_utf8(&output.stdout)
                .unwrap()
                .trim_end()
                .split('\n')
                .last()
                .unwrap();
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
    };
    #[cfg(not(debug_assertions))]
    eprintln!("ruby: {} elapsed:{:?}", res, now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("ruby: {}", res);
    res
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

        run_test("true != true");
        run_test("true != false");
        run_test("false != false");
        run_test("false != true");

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
                    fib(x-1) + fib(x-2)
                end
            end;
            fib 40
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_loop() {
        run_test(
            r#"
            i = 0
            while i < 1000000000
              i = i + 1
            end
            i
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

    #[test]
    fn test6() {
        run_test("return 5");
        run_test("a=5; return a");
        run_test("a=5; b=6; return a+b");
    }

    #[test]
    fn test7() {
        run_test(
            r#"
        def f
          1
        end
        a = 0
        i = 0
        while i < 1000000
          a = a + f()
          if i == 500
            def f
              0
            end
          end
          i = i + 1
        end
        a 
        "#,
        );
    }

    #[test]
    fn test8() {
        run_test(
            r#"
        def f(x)
          x * 2
        end
        def g(x)
          x + 2
        end
        def h(x)
          x * x
        end
        h g f 7
        "#,
        );
    }

    #[test]
    fn test9() {
        run_test(
            r#"
            puts 100
        "#,
        );
    }
}
