#![feature(box_patterns)]
extern crate ariadne;
use ariadne::*;
extern crate chumsky;
use chumsky::prelude::*;

mod ast;
mod eval;
mod hir;
mod parse;
pub use ast::*;
use eval::*;
use hir::*;
pub use parse::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Integer,
    Float,
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
    let code = "-42 + 3.1 * (5 + 1.1)";
    match parser().parse(code) {
        Ok(expr) => {
            let hir = HIR::from_ast(dbg!(&expr));
            dbg!(Evaluator::eval(dbg!(&hir)));
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
