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
struct BcTemp(pub usize);

impl std::fmt::Debug for BcTemp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

///
/// ID of local variable.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BcLocal(pub usize);

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

impl std::ops::Index<BcReg> for Interp {
    type Output = Value;
    fn index(&self, index: BcReg) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<BcReg> for Interp {
    fn index_mut(&mut self, index: BcReg) -> &mut Value {
        &mut self.call_stack[index]
    }
}

impl Interp {
    fn new() -> Self {
        Self {
            cur_fn: BcFuncId(0),
            pc: 0,
            call_stack: Stack::new(),
        }
    }

    fn push_frame(&mut self, args: BcTemp, len: usize, bc_func: &BcFunc, ret: Option<BcReg>) {
        self.call_stack
            .push_frame(args, len, bc_func, self.cur_fn, self.pc, ret);
        self.pc = 0;
        self.cur_fn = bc_func.id;
    }

    fn pop_frame(&mut self, bc_context: &BcGen, val: Value) -> bool {
        let (b, func, pc, ret) = self.call_stack.pop_frame();
        if b {
            return true;
        };
        self.cur_fn = func;
        self.pc = pc;
        self.call_stack.set_reg_base(bc_context[func].local_num());
        if let Some(ret) = ret {
            self.call_stack[ret] = val;
        }
        false
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new();
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(BcTemp(0), 0, hir_func, None);
        eval.eval_loop(bc_context)
    }

    fn eval_loop(&mut self, bc_context: &BcGen) -> Value {
        loop {
            let inst = &bc_context[self.cur_fn].insts()[self.pc];
            //eprintln!("{:?}", &inst);
            self.pc += 1;
            if let Some(val) = self.eval(bc_context, inst) {
                return val;
            }
        }
    }
}

macro_rules! value_op {
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

impl Interp {
    fn eval(&mut self, bc_context: &BcGen, inst: &Inst) -> Option<Value> {
        match inst {
            Inst::Integer(ret, i) => {
                self[*ret] = Value::integer(*i);
            }
            Inst::Float(ret, f) => {
                self[*ret] = Value::float(*f);
            }
            Inst::Nil(ret) => {
                self[*ret] = Value::nil();
            }
            Inst::Neg(dst, src) => {
                self[*dst] = match self[*src].unpack() {
                    RV::Integer(i) => Value::integer(-i),
                    RV::Float(f) => Value::float(-f),
                    _ => unreachable!(),
                };
            }
            Inst::Add(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Sub(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Mul(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Div(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Cmp(kind, ret, lhs, rhs) => {
                let lhs = self[*lhs];
                let rhs = self[*rhs];
                self[*ret] = Value::bool(match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                });
            }
            Inst::Ret(lhs) => {
                let val = self[*lhs];
                if self.pop_frame(bc_context, val) {
                    return Some(val);
                };
            }
            Inst::Mov(dst, local) => {
                self[*dst] = self[*local];
            }
            Inst::Call(id, ret, args, len) => {
                let bc_func = &bc_context[*id];
                self.push_frame(*args, *len, bc_func, *ret);
            }
            Inst::Br(next_pc) => {
                self.pc = bc_context[self.cur_fn].labels()[*next_pc].unwrap().0;
            }
            Inst::CondBr(cond_, then_) => {
                if self[*cond_] != Value::bool(false) {
                    self.pc = bc_context[self.cur_fn].labels()[*then_].unwrap().0;
                };
            }
            Inst::CondNotBr(cond_, else_) => {
                if self[*cond_] == Value::bool(false) {
                    self.pc = bc_context[self.cur_fn].labels()[*else_].unwrap().0;
                };
            }
        }
        None
    }
}
