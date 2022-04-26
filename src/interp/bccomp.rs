use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

type EntryPtr = extern "C" fn(*mut Value, *mut BcComp, *const BcGen) -> Value;

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
    method_label: Vec<DestLabel>,
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

extern "C" fn unwind_call(eval: &mut BcComp, val: Value, bc_context: &BcGen) -> *mut Value {
    let _ = eval.pop_frame(val, bc_context);
    eval.call_stack.get_bp()
}

extern "C" fn prepare_call(
    eval: &mut BcComp,
    bc_context: &BcGen,
    id: BcFuncId,
    ret: u16,
    args: u16,
    len: u16,
) -> *mut Value {
    let bc_func = &bc_context[id];
    let ret = if ret == u16::MAX { None } else { Some(ret) };
    eval.push_frame(args, len, bc_func, ret);
    eval.call_stack.get_bp()
}

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
            method_label: vec![],
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
        for _ in &bc_context.functions {
            eval.method_label.push(eval.jit.label());
        }
        for func in &bc_context.functions {
            eval.compile_func_bc(func);
        }
        let entry = eval.jit.label();
        eval.jit.bind_label(entry);
        let main = eval.method_label[0];
        // arguments
        // RDI <- bp: &mut Value
        // RSI <- self: &mut BcComp
        // RDX <- bc_context: &BcGen
        // callee save registers
        // RBX <- bp: &mut Value
        // R12 <- self: &mut BcComp
        // R13 <- bc_context: &BcGen
        monoasm!(eval.jit,
          pushq rbx;
          pushq r12;
          pushq r13;
          movq rbx, rdi;
          movq r12, rsi;
          movq r13, rdx;
          call main;
          popq r13;
          popq r12;
          popq rbx;
          ret;
        );
        eval.jit.finalize();
        let entry_pount: EntryPtr = unsafe { std::mem::transmute(eval.jit.get_label_u64(entry)) };
        prepare_call(&mut eval, bc_context, BcFuncId(0), u16::MAX, 0, 0);
        let bp = eval.call_stack.get_bp();
        let v = entry_pount(bp, &mut eval, bc_context);
        //assert!(eval.pop_frame(v, bc_context));
        v
    }

    fn compile_func_bc(&mut self, func: &BcFunc) {
        self.jit.bind_label(self.method_label[func.id.0 as usize]);
        let mut labels = vec![];
        for _ in &func.bc {
            labels.push(self.jit.label());
        }
        monoasm!(self.jit,
          pushq rbp;
        );
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
                BcOp::Neg(dst, src) => {
                    let dst = *dst as u64 * 8;
                    let src = *src as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbx + (src)];
                      movq rax, (neg_value);
                      call rax;
                      movq [rbx + (dst)], rax;
                    );
                }
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

                BcOp::Mul(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, [rbx + (rhs)];
                      movq rax, (mul_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
                BcOp::Div(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbx + (lhs)];
                      movq rsi, [rbx + (rhs)];
                      movq rax, (div_values);
                      call rax;
                      movq [rbx + (ret)], rax;
                    );
                }
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
                BcOp::Mov(dst, local) => {
                    let dst = *dst as u64 * 8;
                    let local = *local as u64 * 8;
                    monoasm!(self.jit,
                      movq rax, [rbx + (local)];
                      movq [rbx + (dst)], rax;
                    );
                }
                BcOp::Ret(lhs) => {
                    let lhs = *lhs as u64 * 8;
                    monoasm!(self.jit,
                        movq rax, [rbx + (lhs)];
                        popq rbp;
                        ret;
                    );
                }
                BcOp::Call(id, ret, args, len) => {
                    let func_ptr = self.method_label[id.0 as usize];
                    monoasm!(self.jit,
                      movq rdi, r12;
                      movq rsi, r13;
                      movq rdx, (id.0);
                      movq rcx, (*ret);
                      movq r8, (*args);
                      movq r9, (*len);
                      movq rax, (prepare_call);
                      call rax;
                      movq rbx, rax;
                      call func_ptr;
                      movq rdi, r12;
                      movq rsi, rax;
                      movq rdx, r13;
                      movq rax, (unwind_call);
                      call rax;
                      movq rbx, rax;
                    );
                }
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
            }
        }
    }
}
