use super::*;

mod bcgen;
mod bcinst;
mod stack;
pub use bcgen::BcGen;
use bcgen::*;
use bcinst::*;
use stack::*;

#[derive(Debug, Clone)]
pub enum BcErr {
    UndefinedLocal(String),
    //UndefinedMethod(String),
}

///
/// ID of instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(pub usize);

impl std::fmt::Debug for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

///
/// ID of register.
///
#[derive(Clone, Copy, PartialEq, Eq)]
enum BcReg {
    Local(BcLocal),
    Temp(BcTemp),
}

impl std::fmt::Debug for BcReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(local) => write!(f, "{:?}", local),
            Self::Temp(temp) => write!(f, "{:?}", temp),
        }
    }
}

impl std::convert::From<BcLocal> for BcReg {
    fn from(local: BcLocal) -> Self {
        BcReg::Local(local)
    }
}

impl std::convert::From<BcTemp> for BcReg {
    fn from(temp: BcTemp) -> Self {
        BcReg::Temp(temp)
    }
}

///
/// ID of temporary register.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BcTemp(pub u16);

impl std::fmt::Debug for BcTemp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

///
/// ID of local variable.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BcLocal(pub u16);

impl std::fmt::Debug for BcLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: BcFuncId,
    pc: usize,
    call_stack: Stack,
}

impl std::ops::Index<u16> for Interp {
    type Output = Value;
    fn index(&self, index: u16) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<u16> for Interp {
    fn index_mut(&mut self, index: u16) -> &mut Value {
        &mut self.call_stack[index]
    }
}

macro_rules! cmp_op {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match ($lhs.unpack(), $rhs.unpack()) {
            (RV::Integer($lhs), RV::Integer($rhs)) => $lhs.$op(&$rhs),
            (RV::Integer($lhs), RV::Float($rhs)) => ($lhs as f64).$op(&$rhs),
            (RV::Float($lhs), RV::Integer($rhs)) => $lhs.$op(&($rhs as f64)),
            (RV::Float($lhs), RV::Float($rhs)) => $lhs.$op(&$rhs),
            _ => unreachable!(),
        }
    }};
}

macro_rules! cmp_op_ri {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match $lhs.unpack() {
            RV::Integer($lhs) => $lhs.$op(&(*$rhs as i32)),
            RV::Float($lhs) => $lhs.$op(&(*$rhs as f64)),
            _ => unreachable!(),
        }
    }};
}

impl Interp {
    fn new() -> Self {
        Self {
            cur_fn: BcFuncId(0),
            pc: 0,
            call_stack: Stack::new(),
        }
    }

    fn push_frame(&mut self, args: u16, len: usize, bc_func: &BcFunc, ret: Option<u16>) {
        self.call_stack
            .push_frame(args, len, bc_func, self.cur_fn, self.pc, ret);
        self.pc = 0;
        self.cur_fn = bc_func.id;
    }

    fn pop_frame(&mut self, val: Value) -> bool {
        let (b, func, pc, ret) = self.call_stack.pop_frame();
        if b {
            return true;
        };
        self.cur_fn = func;
        self.pc = pc;
        if let Some(ret) = ret {
            self[ret] = val;
        }
        false
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new();
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(0, 0, hir_func, None);
        eval.eval_loop(bc_context)
    }

    fn eval_loop(&mut self, bc_context: &BcGen) -> Value {
        loop {
            let op = &bc_context[self.cur_fn].bc[self.pc];
            self.pc += 1;
            match op {
                BcOp::Integer(ret, i) => {
                    self[*ret] = Value::integer(*i);
                }
                BcOp::Float(ret, f) => {
                    self[*ret] = Value::float(*f);
                }
                BcOp::Nil(ret) => {
                    self[*ret] = Value::nil();
                }
                BcOp::Neg(dst, src) => {
                    self[*dst] = match self[*src].unpack() {
                        RV::Integer(i) => Value::integer(-i),
                        RV::Float(f) => Value::float(-f),
                        _ => unreachable!(),
                    };
                }
                BcOp::Add(ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    let rhs = self[*rhs];
                    self[*ret] = if lhs.is_fnum() && rhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() + rhs.as_fnum())
                    } else {
                        match (lhs.unpack(), rhs.unpack()) {
                            (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
                            (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
                            (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
                            (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    self[*ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() + *rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs + *rhs as i32),
                            RV::Float(lhs) => Value::float(lhs + *rhs as f64),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    let rhs = self[*rhs];
                    self[*ret] = if lhs.is_fnum() && rhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() - rhs.as_fnum())
                    } else {
                        match (lhs.unpack(), rhs.unpack()) {
                            (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
                            (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
                            (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
                            (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    self[*ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() - *rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs - *rhs as i32),
                            RV::Float(lhs) => Value::float(lhs - *rhs as f64),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Mul(ret, lhs, rhs) => {
                    self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                        (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                        _ => unreachable!(),
                    };
                }
                BcOp::Div(ret, lhs, rhs) => {
                    self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                        (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                        _ => unreachable!(),
                    };
                }
                BcOp::Cmp(kind, ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    let rhs = self[*rhs];
                    self[*ret] = Value::bool(match kind {
                        CmpKind::Eq => cmp_op!(lhs, rhs, eq),
                        CmpKind::Ne => cmp_op!(lhs, rhs, ne),
                        CmpKind::Lt => cmp_op!(lhs, rhs, lt),
                        CmpKind::Gt => cmp_op!(lhs, rhs, gt),
                        CmpKind::Le => cmp_op!(lhs, rhs, le),
                        CmpKind::Ge => cmp_op!(lhs, rhs, ge),
                    });
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let lhs = self[*lhs];
                    self[*ret] = Value::bool(match kind {
                        CmpKind::Eq => cmp_op_ri!(lhs, rhs, eq),
                        CmpKind::Ne => cmp_op_ri!(lhs, rhs, ne),
                        CmpKind::Lt => cmp_op_ri!(lhs, rhs, lt),
                        CmpKind::Gt => cmp_op_ri!(lhs, rhs, gt),
                        CmpKind::Le => cmp_op_ri!(lhs, rhs, le),
                        CmpKind::Ge => cmp_op_ri!(lhs, rhs, ge),
                    });
                }
                BcOp::Ret(lhs) => {
                    let val = self[*lhs];
                    if self.pop_frame(val) {
                        return val;
                    };
                }
                BcOp::Mov(dst, local) => {
                    self[*dst] = self[*local];
                }
                BcOp::Call(id, ret, args, len) => {
                    let bc_func = &bc_context[*id];
                    self.push_frame(*args, *len, bc_func, *ret);
                }
                BcOp::Br(next_pc) => {
                    self.pc = next_pc.0;
                }
                BcOp::CondBr(cond_, then_) => {
                    if self[*cond_] != Value::bool(false) {
                        self.pc = then_.0;
                    };
                }
                BcOp::CondNotBr(cond_, else_) => {
                    if self[*cond_] == Value::bool(false) {
                        self.pc = else_.0;
                    };
                }
            }
        }
    }
}
