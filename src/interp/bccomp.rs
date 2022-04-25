use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

///
/// Code generator
///
/// This generates x86-64 machine code from McIR into heap memory .
///
pub struct BcComp {
    jit: JitMemory,
    cur_fn: BcFuncId,
    pc: BcPc,
    pc_top: BcPcBase,
    call_stack: Stack,
}

impl std::ops::Index<u16> for BcComp {
    type Output = Value;
    fn index(&self, index: u16) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<u16> for BcComp {
    fn index_mut(&mut self, index: u16) -> &mut Value {
        &mut self.call_stack[index]
    }
}

extern "C" fn add_values(lhs: Value, rhs: Value) -> Value {
    if lhs.is_fnum() && rhs.is_fnum() {
        Value::fixnum(lhs.as_fnum() + rhs.as_fnum())
    } else {
        match (lhs.unpack(), rhs.unpack()) {
            (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
            (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
            (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
            (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
            _ => unreachable!(),
        }
    }
}

extern "C" fn sub_values(lhs: Value, rhs: Value) -> Value {
    if lhs.is_fnum() && rhs.is_fnum() {
        Value::fixnum(lhs.as_fnum() - rhs.as_fnum())
    } else {
        match (lhs.unpack(), rhs.unpack()) {
            (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
            (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
            (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
            (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
            _ => unreachable!(),
        }
    }
}

use paste::paste;

macro_rules! cmp_values {
    ($op:ident) => {
        paste! {
          extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
              let b = if lhs.is_fnum() && rhs.is_fnum() {
                  lhs.as_fnum().$op(&rhs.as_fnum())
              } else {
                  match (lhs.unpack(), rhs.unpack()) {
                      (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                      (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                      (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                      (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                      _ => unreachable!(),
                  }
              };
              Value::bool(b)
          }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_values!($op1);
        cmp_values!($($op2),+);
    };
}

cmp_values!(eq, ne, ge, gt, le, lt);

impl BcComp {
    pub fn new(bcgen: &BcGen) -> Self {
        let cur_fn = BcFuncId(0);
        let pc_top = BcPcBase::new(&bcgen[cur_fn]);
        Self {
            jit: JitMemory::new(),
            cur_fn,
            pc: pc_top + 0,
            pc_top,
            call_stack: Stack::new(),
        }
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

    pub fn exec_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new(bc_context);
        let func = &bc_context[BcFuncId(0)];
        eval.push_frame(0, 0, func, None);
        let v = eval.compile_bc(func)(&mut eval[0u16]);
        assert!(eval.pop_frame(v, bc_context));
        v
    }

    fn compile_bc(&mut self, func: &BcFunc) -> extern "C" fn(*mut Value) -> Value {
        let fn_start = self.jit.label();
        self.jit.bind_label(fn_start);
        monoasm!(self.jit,
          pushq rbx;
          movq rbx, rdi;
        );
        let mut labels = vec![];
        for _ in &func.bc {
            labels.push(self.jit.label());
        }
        for (idx, op) in func.bc.iter().enumerate() {
            self.jit.bind_label(labels[idx]);
            match op {
                BcOp::Integer(ret, i) => {
                    let i = Value::integer(*i).get();
                    let ret = *ret as u64 * 8;
                    monoasm!(self.jit,
                      movq [rbx + (ret)], (i);
                    );
                }
                BcOp::Const(ret, id) => {
                    let ret = *ret as u64 * 8;
                    let v = func.get_constant(*id).get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Nil(ret) => {
                    let ret = *ret as u64 * 8;
                    let v = Value::nil().get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbx + (ret)], rax;
                    );
                }
                /*
                BcOp::Neg(dst, src) => {
                    /*self[dst] = match self[src].unpack() {
                        RV::Integer(i) => Value::integer(-i),
                        RV::Float(f) => Value::float(-f),
                        _ => unreachable!(),
                    };*/
                }*/
                BcOp::Add(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, [rbx + (rhs)];
                      movq rax, (add_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = Value::integer(*rhs as i32).get();
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, (rhs);
                      movq rax, (add_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, [rbx + (rhs)];
                      movq rax, (sub_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = Value::integer(*rhs as i32).get();
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, (rhs);
                      movq rax, (sub_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                /*
                BcOp::Mul(ret, lhs, rhs) => {
                    /*self[ret] = match (self[lhs].unpack(), self[rhs].unpack()) {
                        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                        (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                        _ => unreachable!(),
                    };*/
                }
                BcOp::Div(ret, lhs, rhs) => {
                    /*self[ret] = match (self[lhs].unpack(), self[rhs].unpack()) {
                        (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                        (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                        (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                        (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                        _ => unreachable!(),
                    };*/
                  }
                  */
                BcOp::Cmp(kind, ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    let func = match kind {
                        CmpKind::Eq => cmp_eq_values,
                        CmpKind::Ne => cmp_ne_values,
                        CmpKind::Lt => cmp_lt_values,
                        CmpKind::Gt => cmp_gt_values,
                        CmpKind::Le => cmp_le_values,
                        CmpKind::Ge => cmp_ge_values,
                    };
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, [rbx + (rhs)];
                      movq rax, (func);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = Value::integer(*rhs as i32).get();
                    let func = match kind {
                        CmpKind::Eq => cmp_eq_values,
                        CmpKind::Ne => cmp_ne_values,
                        CmpKind::Lt => cmp_lt_values,
                        CmpKind::Gt => cmp_gt_values,
                        CmpKind::Le => cmp_le_values,
                        CmpKind::Ge => cmp_ge_values,
                    };
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, (rhs);
                      movq rax, (func);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Ret(lhs) => {
                    let lhs = *lhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rax, [rbx + (lhs)];
                      popq rbx;
                      ret;
                    );
                }
                BcOp::Mov(dst, local) => {
                    let dst = *dst as u64 * 8;
                    let local = *local as u64 * 8;
                    monoasm!(self.jit,
                      movq rax, [rbx + (local)];
                      movq [rbx + (dst)], rax;
                    );
                }
                /*
                BcOp::Call(id, ret, args, len) => {
                    /*let bc_func = &bc_context[id];
                    let ret = if ret == u16::MAX { None } else { Some(ret) };
                    self.push_frame(args, len, bc_func, ret);*/
                }
                */
                BcOp::Br(next_pc) => {
                    let dest = labels[next_pc.0 as usize];
                    monoasm!(self.jit,
                      jmp dest;
                    );
                }
                BcOp::CondBr(cond_, then_) => {
                    let cond_ = *cond_ as u64 * 8;
                    let dest = labels[then_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbx + (cond_)], (false_val);
                      jne dest;
                    );
                }
                BcOp::CondNotBr(cond_, else_) => {
                    let cond_ = *cond_ as u64 * 8;
                    let dest = labels[else_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbx + (cond_)], (false_val);
                      jeq dest;
                    );
                }
                _ => unimplemented!(),
            }
        }
        self.jit.finalize();
        self.jit.get_label_addr(fn_start)
    }
}
