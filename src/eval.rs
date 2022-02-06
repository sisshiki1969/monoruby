use super::*;

pub struct Evaluator {}

impl Evaluator {
    pub fn eval(hir: &HIR) -> Value {
        eval(hir)
    }
}

fn eval(hir: &HIR) -> Value {
    match hir.kind() {
        HIROp::Integer(i) => Value::Integer(*i),
        HIROp::Float(f) => Value::Float(*f),
        HIROp::IntAsFloat(box hir) => Value::Float(eval(hir).as_i() as f64),
        HIROp::INeg(box lhs) => Value::Integer(-eval(lhs).as_i()),
        HIROp::FNeg(box lhs) => Value::Float(-eval(lhs).as_f()),
        HIROp::IAdd(box lhs, box rhs) => Value::Integer(eval(lhs).as_i() + eval(rhs).as_i()),
        HIROp::FAdd(box lhs, box rhs) => Value::Float(eval(lhs).as_f() + eval(rhs).as_f()),
        HIROp::ISub(box lhs, box rhs) => Value::Integer(eval(lhs).as_i() - eval(rhs).as_i()),
        HIROp::FSub(box lhs, box rhs) => Value::Float(eval(lhs).as_f() - eval(rhs).as_f()),
        HIROp::IMul(box lhs, box rhs) => Value::Integer(eval(lhs).as_i() * eval(rhs).as_i()),
        HIROp::FMul(box lhs, box rhs) => Value::Float(eval(lhs).as_f() * eval(rhs).as_f()),
        HIROp::IDiv(box lhs, box rhs) => Value::Integer(eval(lhs).as_i() / eval(rhs).as_i()),
        HIROp::FDiv(box lhs, box rhs) => Value::Float(eval(lhs).as_f() / eval(rhs).as_f()),
    }
}
