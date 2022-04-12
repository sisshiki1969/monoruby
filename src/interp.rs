use super::*;

mod bcgen;
mod bcinst;
pub use bcgen::BcGen;
use bcgen::*;
use bcinst::*;

#[derive(Debug, Clone)]
pub enum BcErr {
    UndefinedLocal(String),
    //UndefinedMethod(String),
}

///
/// ID of instruction.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(pub usize);

///
/// ID of temporary register.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Temp(pub usize);

///
/// ID of local variable.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Local(pub usize);

pub struct Interp {
    cur_fn: BcFuncId,
    pc: usize,
    call_stack: Stack,
}

impl std::ops::Index<Temp> for Interp {
    type Output = Value;
    fn index(&self, index: Temp) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<Temp> for Interp {
    fn index_mut(&mut self, index: Temp) -> &mut Value {
        &mut self.call_stack[index]
    }
}

impl std::ops::Index<Local> for Interp {
    type Output = Value;
    fn index(&self, index: Local) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<Local> for Interp {
    fn index_mut(&mut self, index: Local) -> &mut Value {
        &mut self.call_stack[index]
    }
}

impl std::ops::Index<Reg> for Interp {
    type Output = Value;
    fn index(&self, index: Reg) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<Reg> for Interp {
    fn index_mut(&mut self, index: Reg) -> &mut Value {
        &mut self.call_stack[index]
    }
}

#[derive(Debug, Clone)]
struct Stack {
    stack: Vec<Value>,
    bp: usize,
    reg_base: usize,
    args_len: usize,
}

impl std::ops::Index<Temp> for Stack {
    type Output = Value;
    fn index(&self, index: Temp) -> &Value {
        &self.stack[self.reg_base + index.0 as usize]
    }
}

impl std::ops::IndexMut<Temp> for Stack {
    fn index_mut(&mut self, index: Temp) -> &mut Value {
        &mut self.stack[self.reg_base + index.0 as usize]
    }
}

impl std::ops::Index<Local> for Stack {
    type Output = Value;
    fn index(&self, index: Local) -> &Value {
        &self.stack[self.bp + index.0]
    }
}

impl std::ops::IndexMut<Local> for Stack {
    fn index_mut(&mut self, index: Local) -> &mut Value {
        &mut self.stack[self.bp + index.0]
    }
}

impl std::ops::Index<Reg> for Stack {
    type Output = Value;
    fn index(&self, reg: Reg) -> &Value {
        let i = self.get_index(reg);
        &self.stack[i]
    }
}

impl std::ops::IndexMut<Reg> for Stack {
    fn index_mut(&mut self, reg: Reg) -> &mut Value {
        let i = self.get_index(reg);
        &mut self.stack[i]
    }
}

impl Stack {
    fn new() -> Self {
        Self {
            stack: Vec::with_capacity(4096),
            bp: 0,
            reg_base: 0,
            args_len: 0,
        }
    }

    fn get_index(&self, reg: Reg) -> usize {
        match reg {
            Reg::Temp(i) => self.reg_base + i.0 as usize,
            Reg::Local(i) => self.bp + i.0 as usize,
        }
    }

    /*fn args(&self) -> &[Value] {
        &self.stack[self.bp..self.bp + self.args_len]
    }*/

    fn reg_slice(&self, reg: Temp, len: usize) -> std::ops::Range<usize> {
        let start = self.reg_base + reg.0 as usize;
        start..start + len
    }

    fn push_frame(
        &mut self,
        args: Temp,
        args_len: usize,
        bc_func: &BcFunc,
        cur_fn: BcFuncId,
        pc: usize,
    ) {
        let args = self.reg_slice(args, args_len);
        let local_num = bc_func.local_num();
        let reg_num = bc_func.reg_num;
        self.stack.push(Value::from_unchecked(cur_fn.0 as u64));
        self.stack.push(Value::from_unchecked(pc as u64));
        self.stack.push(Value::from_unchecked(self.args_len as u64));
        self.stack.push(Value::from_unchecked(self.reg_base as u64));
        self.stack.push(Value::from_unchecked(self.bp as u64));
        self.bp = self.stack.len();
        self.reg_base = self.bp + local_num;
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num as usize;
        self.stack.extend_from_within(args);
        self.stack.resize(new_len, Value::nil());
    }

    fn pop_frame(&mut self) -> (bool, BcFuncId, usize) {
        let old_bp = self.bp;
        let cur_fn = self.stack[old_bp - 5].get() as usize;
        let pc = self.stack[old_bp - 4].get() as usize;
        self.args_len = self.stack[old_bp - 3].get() as usize;
        self.reg_base = self.stack[old_bp - 2].get() as usize;
        self.bp = self.stack[old_bp - 1].get() as usize;
        self.stack.truncate(old_bp - 5);
        (self.bp == 0, BcFuncId(cur_fn), pc)
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

    fn push_frame(&mut self, args: Temp, len: usize, bc_func: &BcFunc) {
        self.call_stack
            .push_frame(args, len, bc_func, self.cur_fn, self.pc);
    }

    fn pop_frame(&mut self) -> bool {
        let (b, func, pc) = self.call_stack.pop_frame();
        self.cur_fn = func;
        self.pc = pc;
        b
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new();
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(Temp(0), 0, hir_func);
        eval.eval_function(bc_context)
    }

    fn eval_function(&mut self, bc_context: &BcGen) -> Value {
        self.pc = 0;
        loop {
            let inst = &bc_context[self.cur_fn].insts()[self.pc];
            //eprintln!("{:?}", &inst);
            self.pc += 1;
            if let Some(val) = self.eval(bc_context, inst) {
                let _ = self.pop_frame();
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
            Inst::Ret(lhs) => return Some(self[*lhs]),
            Inst::Mov(dst, local) => {
                self[*dst] = self[*local];
            }
            Inst::Call(id, ret, args, len) => {
                let bc_func = &bc_context[*id];
                self.push_frame(*args, *len, bc_func);
                self.cur_fn = *id;
                let res = self.eval_function(bc_context);
                if let Some(ret) = *ret {
                    self[ret] = res;
                }
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
