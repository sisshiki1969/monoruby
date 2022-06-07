#![feature(box_patterns)]
#![feature(int_roundings)]
pub use alloc::*;
pub use fxhash::FxHashMap as HashMap;
pub use monoasm::CodePtr;
use num::BigInt;
pub use ruruby_parse::*;
//use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
#[cfg(not(debug_assertions))]
use std::time::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod alloc;
mod executor;
mod rvalue;
mod value;
use executor::*;
use rvalue::*;
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
    /// switch JIT compilation.
    #[clap(short, long)]
    jit: bool,
    /// File name.
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let args = CommandLineArgs::parse();

    if !args.exec.is_empty() {
        for code in args.exec {
            let _ = exec(&code, args.jit).unwrap();
        }
        return;
    }

    match args.file {
        Some(file) => {
            let mut file = File::open(file).unwrap();
            let mut code = String::new();
            file.read_to_string(&mut code).unwrap();
            let _ = exec(&code, args.jit).unwrap();
        }
        None => {
            let mut rl = Editor::<()>::new();
            let mut all_codes = vec![];
            loop {
                let readline = rl.readline("monoruby> ");
                match readline {
                    Ok(code) => {
                        rl.add_history_entry(code.as_str());
                        run_repl(&code, &mut all_codes, args.jit);
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

fn exec(code: &str, jit: bool) -> Result<(), MonorubyErr> {
    let mut globals = Globals::new();
    globals.compile_script(code.to_string(), std::path::Path::new("REPL"), "eval")?;
    if !jit {
        let interp_val = Interp::eval_toplevel(&mut globals);
        eprintln!("interp: {:?}", interp_val);
    } else {
        let _ = jitcompiler(&mut globals);
    }
    Ok(())
}

fn repl_exec(code: &str, jit_flag: bool) -> Result<Value, MonorubyErr> {
    let mut globals = Globals::new();
    globals.compile_script(code.to_string(), std::path::Path::new("REPL"), "eval")?;

    if !jit_flag {
        let interp_val = Interp::eval_toplevel(&mut globals.clone());
        eprintln!("interp: {:?}", interp_val);
    }

    jitcompiler(&mut globals)
}

fn run_repl(code: &str, all_codes: &mut Vec<String>, jit_flag: bool) {
    all_codes.push(code.to_string());
    if let Err(err) = repl_exec(&all_codes.join(";"), jit_flag) {
        eprintln!("{:?}", err);
        err.show_loc();
        all_codes.pop();
    };
}

pub fn run_test(code: &str) {
    #[cfg(debug_assertions)]
    dbg!(code);
    let all_codes = vec![code.to_string()];
    let mut globals = Globals::new();
    globals
        .compile_script(code.to_string(), std::path::Path::new(""), "")
        .unwrap_or_else(|err| {
            err.show_loc();
            panic!("Error in compiling AST. {:?}", err)
        });
    #[cfg(not(debug_assertions))]
    let now = Instant::now();
    let interp_val = Interp::eval_toplevel(&mut globals.clone());
    #[cfg(not(debug_assertions))]
    eprintln!("interp: {:?} elapsed:{:?}", interp_val, now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("interp: {:?}", interp_val);

    let jit_val = jitcompiler(&mut globals);

    let interp_val = interp_val.unwrap();
    let jit_val = jit_val.unwrap();

    assert!(Value::eq(interp_val, jit_val));

    let ruby_res = run_ruby(&all_codes);

    assert!(Value::eq(jit_val, ruby_res));
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

fn run_ruby(code: &Vec<String>) -> Value {
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
                Value::integer(n)
            } else if let Ok(n) = res.parse::<BigInt>() {
                Value::bigint(n)
            } else if let Ok(n) = res.parse::<f64>() {
                Value::float(n)
            } else if res == "true" {
                Value::bool(true)
            } else if res == "false" {
                Value::bool(false)
            } else if res == "nil" {
                Value::nil()
            } else if res.starts_with('"') {
                let s = res.trim_matches('"').to_string();
                Value::string(s.into_bytes())
            } else {
                eprintln!("Ruby: {:?}", res);
                Value::bool(false)
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
    fn test_bigint() {
        for lhs in [
            "0",
            "53785",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ] {
            for rhs in [
                "17",
                "3454",
                "25084",
                "234234645",
                "2352354645657876868978696835652452546462456245646",
            ] {
                for op in ["+", "-", "*", "/", "&", "|", "^"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    #[ignore]
    fn test_call() {
        run_test("print 1"); // max number of 63bit signed int.
    }

    #[test]
    fn test_int_bigint() {
        run_test("4611686018427387903"); // max number of 63bit signed int.
        run_test("4611686018427387903 + 1");
        run_test("4611686018400000000 + 27387904");
        run_test("-4611686018427387904"); // min number of 63bit signed int.
        run_test("-4611686018427387904 - 1");
        run_test("-4611686018400000001 - 27387904");
    }

    #[test]
    fn test_shift() {
        for lhs in ["157"] {
            for rhs in ["1", "54", "64"] {
                for op in ["<<", ">>"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    fn test_assign_op() {
        run_test("a=3; a+=7; a");
        run_test("a=3; a-=7; a");
        run_test("a=3; a*=7; a");
        run_test("a=300; a/=7; a");
        run_test("a=30; a<<=7; a");
        run_test("a=3000; a>>=7; a");
        run_test("a=36; a|=77; a");
        run_test("a=36; a&=77; a");
        run_test("a=36; a^=77; a");
    }

    #[test]
    fn test1() {
        run_test("a=42; b=35.0; c=7; def f(x) a=4; end; if a-b==c then 0 else 1 end");
        run_test("def fn(x) x*2 end; a=42; c=b=a+7; d=b-a; e=b*d; d=f=fn(e); f=d/a");
        run_test("a=42; b=-a");
        run_test("a=42; a; b=-a");
    }

    #[test]
    fn test_assign() {
        run_test("a=8; b=2; a,b=b,a; b/a");
        run_test("a,b,c=1,2,3; a-b-c");
        run_test("a=b=c=7; a+b+c");
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
    fn bench_factorial() {
        run_test(
            r#"
            def fact(x)
                if x <= 1 then
                    1
                else
                    x * fact(x-1)
                end
            end;
            fact 4000
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_while() {
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
    #[ignore]
    fn bench_for() {
        run_test(
            r#"
            j = 0
            for i in 0..1000000000
              j = j + 1
            end
            j
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_redefine() {
        run_test(
            r#"
            def f; 1; end
            a = 0; i = 0
            while i < 200000000
              a = a + f
              if i == 500
                def f; 0; end
              end
              i = i + 1
            end
            a
            "#,
        );
    }

    #[test]
    fn test_while1() {
        run_test(
            r#"
            a=1
            b=while a<2500 do
                a=a+1
            end
            a
            "#,
        );
    }

    #[test]
    fn test_while2() {
        run_test(
            r#"
            a=1
            b=while a<2500 do
                a=a+1
                if a == 100 then break a end
            end
            b
            "#,
        );
    }

    /*#[test]
    fn test_for1() {
        run_test(
            r#"
            a=1
            b = for a in 0..300 do
            end
            b # => 0..300
            "#,
        );
    }*/

    #[test]
    fn test_for2() {
        run_test(
            r#"
            b = for a in 0..300 do
                if a == 77 then break a/7 end
            end
            b
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
    fn test5a() {
        run_test(
            r#"
        def f(a)
          a
        end
        f(7)
        "#,
        );
    }

    #[test]
    fn test5b() {
        run_test(
            r#"
        def f(a); a; end
        f(7)
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

    #[test]
    fn test9a() {
        run_test(
            r#"
            64.chr
        "#,
        );
    }

    #[test]
    fn test10() {
        run_test(
            r#"
            if nil then 2*5/3 else 5 end
        "#,
        );
    }
}
