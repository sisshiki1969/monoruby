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
mod interp;
mod mcir;
mod mir;
mod parse;
pub use ast::*;
use codegen::*;
use eval::*;
use hir::*;
use interp::*;
use mcir::*;
use mir::*;
use parse::Span;
pub use parse::*;

//const UNINITIALIZED: u64 = 0x04; // 0000_0100
const FALSE_VALUE: u64 = 0x14; // 0001_0100
const NIL_VALUE: u64 = 0x24; // 0010_0100
const TRUE_VALUE: u64 = 0x1c; // 0001_1100
                              //const TAG_SYMBOL: u64 = 0x0c; // 0000_1100
                              //const BOOL_MASK1: u64 = 0b0011_0000;
                              //const BOOL_MASK2: u64 = 0xffff_ffff_ffff_ffcf;
const FLOAT_MASK1: u64 = !(0b0110u64 << 60);
const FLOAT_MASK2: u64 = 0b0100u64 << 60;

const ZERO: u64 = (0b1000 << 60) | 0b10;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value(std::num::NonZeroU64);

#[derive(Debug, Clone, PartialEq)]
pub enum RV {
    Nil,
    Bool(bool),
    Integer(i32),
    Float(f64),
}

impl RV {
    pub fn pack(&self) -> u64 {
        match self {
            Self::Integer(i) => *i as i64 as u64,
            Self::Float(f) => f64::to_bits(*f),
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

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.unpack() {
            RV::Integer(n) => write!(f, "{}i32", n),
            RV::Float(n) => write!(f, "{}f64", n),
            RV::Bool(b) => write!(f, "{}", b),
            RV::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    #[inline(always)]
    fn from(id: u64) -> Self {
        Value(std::num::NonZeroU64::new(id).unwrap())
    }

    #[inline(always)]
    fn from_unchecked(id: u64) -> Self {
        unsafe { Value(std::num::NonZeroU64::new_unchecked(id)) }
    }

    #[inline(always)]
    fn get(&self) -> u64 {
        self.0.get()
    }

    #[inline(always)]
    const fn nil() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(NIL_VALUE) })
    }

    /*#[inline(always)]
    const fn true_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(TRUE_VALUE) })
    }

    #[inline(always)]
    const fn false_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(FALSE_VALUE) })
    }*/

    #[inline(always)]
    fn bool(b: bool) -> Self {
        if b {
            Value::from(TRUE_VALUE)
        } else {
            Value::from(FALSE_VALUE)
        }
    }

    pub fn bool_fromu64(num: u64) -> Self {
        Value::bool(num as u8 != 0)
    }

    #[inline(always)]
    fn fixnum(num: i64) -> Self {
        Value::from((num << 1) as u64 | 0b1)
    }

    /*#[inline(always)]
    fn is_i63(num: i64) -> bool {
        let top = (num as u64) >> 62 ^ (num as u64) >> 63;
        top & 0b1 == 0
    }*/

    pub fn integer(num: i32) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn integer_fromu64(num: u64) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn float(num: f64) -> Self {
        if num == 0.0 {
            return Value::from(ZERO);
        }
        let unum = f64::to_bits(num);
        let exp = ((unum >> 60) & 0b111) + 1;
        if (exp & 0b0110) == 0b0100 {
            Value::from((unum & FLOAT_MASK1 | FLOAT_MASK2).rotate_left(3))
        } else {
            panic!()
        }
    }

    pub fn float_fromu64(num: u64) -> Self {
        Value::float(f64::from_bits(num))
    }

    pub fn unpack(&self) -> RV {
        if let Some(i) = self.as_fixnum() {
            RV::Integer(i)
        } else if let Some(f) = self.as_flonum() {
            RV::Float(f)
        } else if !self.is_packed_value() {
            panic!()
        } else {
            match self.0.get() {
                NIL_VALUE => RV::Nil,
                TRUE_VALUE => RV::Bool(true),
                FALSE_VALUE => RV::Bool(false),
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }

    pub fn pack(&self) -> u64 {
        if !self.is_packed_value() {
            panic!()
        } else if let Some(i) = self.as_fixnum() {
            i as i64 as u64
        } else if let Some(f) = self.as_flonum() {
            f64::to_bits(f)
        } else {
            match self.0.get() {
                NIL_VALUE => 0,
                TRUE_VALUE => 1,
                FALSE_VALUE => 0,
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }
}

impl Value {
    /*#[inline(always)]
    fn is_nil(&self) -> bool {
        self.0.get() == NIL_VALUE
    }*/

    #[inline(always)]
    fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    #[inline(always)]
    fn as_fnum(&self) -> i64 {
        (self.0.get() as i64) >> 1
    }

    #[inline(always)]
    fn is_fnum(&self) -> bool {
        self.0.get() & 0b1 == 1
    }

    #[inline(always)]
    fn as_fixnum(&self) -> Option<i32> {
        if self.is_fnum() {
            Some(self.as_fnum() as i32)
        } else {
            None
        }
    }

    #[inline(always)]
    fn as_flonum(&self) -> Option<f64> {
        let u = self.0.get();
        if u & 0b11 == 2 {
            if u == ZERO {
                return Some(0.0);
            }
            let bit = 0b10 - ((u >> 63) & 0b1);
            let num = ((u & !(0b0011u64)) | bit).rotate_right(3);
            //eprintln!("after  unpack:{:064b}", num);
            Some(f64::from_bits(num))
        } else {
            None
        }
    }

    /*#[inline(always)]
    fn is_packed_num(&self) -> bool {
        self.0.get() & 0b11 != 0
    }*/

    pub fn ty(&self) -> Type {
        match self.unpack() {
            RV::Integer(_) => Type::Integer,
            RV::Float(_) => Type::Float,
            RV::Bool(_) => Type::Bool,
            RV::Nil => Type::Nil,
        }
    }

    /*pub fn pack(&self) -> u64 {
        self.0.get()
    }*/
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
        match self.unpack() {
            RV::Integer(i) => i,
            _ => unreachable!("{:?}", self),
        }
    }

    fn as_f(&self) -> f64 {
        match self.unpack() {
            RV::Float(f) => f,
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
        let gen = match BcGen::new_bc(&ast) {
            Ok(gen) => gen,
            Err(err) => {
                eprintln!("Error in compiling AST. {:?}", err);
                all_codes.pop();
                return;
            }
        };
        //#[cfg(debug_assertions)]
        //dbg!(&gen);
        let interp_val = Interp::eval_toplevel(&gen);
        eprintln!("Interp: {:?}", interp_val);

        let mut hir = HirContext::new();
        match hir.from_ast(&ast) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error in compiling AST. {:?}", err);
                all_codes.pop();
                return;
            }
        };
        //dbg!(&hir);
    } else {
        all_codes.pop();
    }
    show_err(errs, parse_errs, code);
}

pub fn run_test(code: &str) {
    dbg!(code);
    let all_codes = vec![code.to_string()];
    let (ast, errs, parse_errs) = parse(code);
    if let Some(stmt) = ast {
        let gen = match BcGen::new_bc(&stmt) {
            Ok(gen) => gen,
            Err(err) => {
                eprintln!("Error in compiling AST. {:?}", err);
                return;
            }
        };
        //#[cfg(debug_assertions)]
        //dbg!(&gen);
        let now = Instant::now();
        let interp_val = Interp::eval_toplevel(&gen);
        eprintln!("interp: {:?} elapsed:{:?}", interp_val, now.elapsed());

        //dbg!(&stmt);
        let mut hir = HirContext::new();
        match hir.from_ast(&stmt) {
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
