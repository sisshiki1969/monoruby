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
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Temp(pub usize);

impl std::fmt::Debug for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

///
/// ID of local variable.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Local(pub usize);

impl std::fmt::Debug for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

pub struct Interp {
    cur_fn: BcFuncId,
    pc: usize,
    call_stack: Stack,
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
        ret: Option<Reg>,
    ) {
        let args = self.reg_slice(args, args_len);
        let local_num = bc_func.local_num();
        let reg_num = bc_func.reg_num;
        let ret = match ret {
            Some(r) => self.get_index(r) + 1,
            None => 0,
        };
        self.stack.push(Value::from_unchecked(ret as u64));
        self.stack.push(Value::from_unchecked(
            (cur_fn.0 << 32) as u64 | (pc as u32 as u64),
        ));
        self.stack.push(Value::from_unchecked(self.args_len as u64));
        self.stack.push(Value::from_unchecked(self.bp as u64));
        self.bp = self.stack.len();
        self.reg_base = self.bp + local_num;
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num as usize;
        self.stack.extend_from_within(args);
        self.stack.resize(new_len, Value::nil());
    }

    fn pop_frame(&mut self) -> (bool, BcFuncId, usize, Option<usize>) {
        let old_bp = self.bp;
        let ret = match self.stack[old_bp - 4].get() as usize {
            0 => None,
            r => Some(r - 1),
        };
        let fn_pc = self.stack[old_bp - 3].get() as usize;
        let cur_fn = fn_pc >> 32;
        let pc = fn_pc as u32 as usize;
        self.args_len = self.stack[old_bp - 2].get() as usize;
        self.bp = self.stack[old_bp - 1].get() as usize;
        self.stack.truncate(old_bp - 4);
        (self.bp == 0, BcFuncId(cur_fn), pc, ret)
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

    fn push_frame(&mut self, args: Temp, len: usize, bc_func: &BcFunc, ret: Option<Reg>) {
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
        self.call_stack.reg_base = self.call_stack.bp + bc_context[func].local_num();
        if let Some(ret) = ret {
            self.call_stack.stack[ret] = val;
        }
        false
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new();
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(Temp(0), 0, hir_func, None);
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
