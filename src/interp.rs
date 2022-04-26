use super::*;

mod bccomp;
mod bcgen;
mod bcinst;
mod op;
mod stack;
pub use bccomp::*;
pub use bcgen::BcGen;
use bcgen::*;
use bcinst::*;
use op::*;
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
struct InstId(pub u32);

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
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BcPcBase(*const BcOp);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs as isize) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    fn new(func: &BcFunc) -> Self {
        BcPcBase(&func.bc[0] as *const _)
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BcPc(*const BcOp);

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::AddAssign<usize> for BcPc {
    fn add_assign(&mut self, offset: usize) {
        unsafe {
            *self = BcPc(self.0.add(offset));
        }
    }
}

impl BcPc {
    fn get(&self) -> BcOp {
        unsafe { (*(self.0)).clone() }
    }
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: BcFuncId,
    pc: BcPc,
    pc_top: BcPcBase,
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

macro_rules! cmp_op_ri {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match $lhs.unpack() {
            RV::Integer($lhs) => $lhs.$op(&($rhs as i32)),
            RV::Float($lhs) => $lhs.$op(&($rhs as f64)),
            _ => unreachable!(),
        }
    }};
}

impl Interp {
    fn new(bc_context: &BcGen) -> Self {
        let cur_fn = BcFuncId(0);
        let pc_top = BcPcBase::new(&bc_context[cur_fn]);
        Self {
            cur_fn,
            pc: pc_top + 0,
            pc_top,
            call_stack: Stack::new(),
        }
    }

    fn get_op(&mut self) -> BcOp {
        let op = self.pc.get();
        self.pc += 1;
        op
    }

    fn push_frame(&mut self, args: u16, len: u16, bc_func: &BcFunc, ret: Option<u16>) {
        let pc = self.pc - self.pc_top;
        self.call_stack
            .push_frame(args, len, bc_func, self.cur_fn, pc, ret);
        let pc = BcPcBase::new(bc_func);
        self.pc_top = pc;
        self.pc = pc + 0;
        self.cur_fn = bc_func.id;
    }

    fn pop_frame(&mut self, val: Value, bc_context: &BcGen) -> bool {
        let (b, func, pc, ret) = self.call_stack.pop_frame();
        if b {
            return true;
        };
        self.cur_fn = func;
        self.pc_top = BcPcBase::new(&bc_context[func]);
        self.pc = self.pc_top + pc;
        if let Some(ret) = ret {
            self[ret] = val;
        }
        false
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new(bc_context);
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(0, 0, hir_func, None);
        eval.eval_loop(bc_context)
    }

    fn eval_loop(&mut self, bc_context: &BcGen) -> Value {
        loop {
            let op = self.get_op();
            match op {
                BcOp::Integer(ret, i) => {
                    self[ret] = Value::integer(i);
                }
                BcOp::Const(ret, id) => {
                    self[ret] = bc_context[self.cur_fn].get_constant(id);
                }
                BcOp::Nil(ret) => {
                    self[ret] = Value::nil();
                }
                BcOp::Neg(dst, src) => {
                    self[dst] = match self[src].unpack() {
                        RV::Integer(i) => Value::integer(-i),
                        RV::Float(f) => Value::float(-f),
                        _ => unreachable!(),
                    };
                }
                BcOp::Add(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = add_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() + rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs + rhs as i32),
                            RV::Float(lhs) => Value::float(lhs + rhs as f64),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = sub_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() - rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs - rhs as i32),
                            RV::Float(lhs) => Value::float(lhs - rhs as f64),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Mul(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = mul_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Div(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = div_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Cmp(kind, ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    self[ret] = match kind {
                        CmpKind::Eq => cmp_eq_values(lhs, rhs),
                        CmpKind::Ne => cmp_ne_values(lhs, rhs),
                        CmpKind::Lt => cmp_lt_values(lhs, rhs),
                        CmpKind::Gt => cmp_gt_values(lhs, rhs),
                        CmpKind::Le => cmp_le_values(lhs, rhs),
                        CmpKind::Ge => cmp_ge_values(lhs, rhs),
                    };
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = Value::bool(match kind {
                        CmpKind::Eq => cmp_op_ri!(lhs, rhs, eq),
                        CmpKind::Ne => cmp_op_ri!(lhs, rhs, ne),
                        CmpKind::Lt => cmp_op_ri!(lhs, rhs, lt),
                        CmpKind::Gt => cmp_op_ri!(lhs, rhs, gt),
                        CmpKind::Le => cmp_op_ri!(lhs, rhs, le),
                        CmpKind::Ge => cmp_op_ri!(lhs, rhs, ge),
                    });
                }
                BcOp::Ret(lhs) => {
                    let val = self[lhs];
                    if self.pop_frame(val, bc_context) {
                        return val;
                    };
                }
                BcOp::Mov(dst, local) => {
                    self[dst] = self[local];
                }
                BcOp::Call(id, ret, args, len) => {
                    let bc_func = &bc_context[id];
                    let ret = if ret == u16::MAX { None } else { Some(ret) };
                    self.push_frame(args, len, bc_func, ret);
                }
                BcOp::Br(next_pc) => {
                    self.pc = self.pc_top + next_pc;
                }
                BcOp::CondBr(cond_, then_) => {
                    if self[cond_] != Value::bool(false) {
                        self.pc = self.pc_top + then_;
                    };
                }
                BcOp::CondNotBr(cond_, else_) => {
                    if self[cond_] == Value::bool(false) {
                        self.pc = self.pc_top + else_;
                    };
                }
            }
        }
    }
}
