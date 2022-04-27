use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

type EntryPtr = extern "C" fn(*mut Value, *mut BcCompiler, *const FuncStore) -> Value;

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct BcCompiler {
    jit: JitMemory,
    call_stack: Stack,
    method_label: Vec<DestLabel>,
}

impl std::ops::Index<u16> for BcCompiler {
    type Output = Value;
    fn index(&self, index: u16) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<u16> for BcCompiler {
    fn index_mut(&mut self, index: u16) -> &mut Value {
        &mut self.call_stack[index]
    }
}

extern "C" fn pop_frame(eval: &mut BcCompiler, val: Value) -> *mut Value {
    let (b, _func, _pc, ret) = eval.call_stack.pop_frame();
    if !b {
        if let Some(ret) = ret {
            eval[ret] = val;
        }
    }
    eval.call_stack.get_bp()
}

extern "C" fn push_frame(
    eval: &mut BcCompiler,
    bc_context: &FuncStore,
    id: FuncId,
    ret: u16,
    args: u16,
    len: u16,
) -> *mut Value {
    let bc_func = &bc_context[id];
    let ret = if ret == u16::MAX { None } else { Some(ret) };
    let pc = 0;
    eval.call_stack
        .push_frame(args, len, bc_func, FuncId(0), pc, ret);
    eval.call_stack.get_bp()
}

impl BcCompiler {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            call_stack: Stack::new(),
            method_label: vec![],
        }
    }

    pub fn exec_toplevel(fn_store: &FuncStore) -> Value {
        let mut eval = Self::new();
        for _ in &fn_store.functions {
            eval.method_label.push(eval.jit.label());
        }
        for func in &fn_store.functions {
            eval.compile_func_bc(func);
        }
        let entry = eval.jit.label();
        eval.jit.bind_label(entry);
        let main = eval.method_label[0];
        // arguments
        // rdi <- bp: &mut Value
        // rsi <- self: &mut BcCompiler
        // rdx <- fn_store: &FuncStore
        // callee save registers
        // rbp <- bp: &mut Value
        // r12 <- self: &mut BcCompiler
        // r13 <- fn_store: &FuncStore
        monoasm!(eval.jit,
          pushq rbp;
          pushq r12;
          pushq r13;
          movq rbp, rdi;
          movq r12, rsi;
          movq r13, rdx;
          call main;
          popq r13;
          popq r12;
          popq rbp;
          ret;
        );
        eval.jit.finalize();
        let entry_pount: EntryPtr = unsafe { std::mem::transmute(eval.jit.get_label_u64(entry)) };
        push_frame(&mut eval, fn_store, FuncId(0), u16::MAX, 0, 0);
        let bp = eval.call_stack.get_bp();
        let v = entry_pount(bp, &mut eval, fn_store);
        //assert!(eval.pop_frame(v, bc_context));
        v
    }

    fn compile_func_bc(&mut self, func: &FuncInfo) {
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
                      movq [rbp + (ret)], (i);
                    );
                }
                BcOp::Const(ret, id) => {
                    let ret = *ret as u64 * 8;
                    let v = func.get_constant(*id).get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Nil(ret) => {
                    let ret = *ret as u64 * 8;
                    let v = Value::nil().get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Neg(dst, src) => {
                    let dst = *dst as u64 * 8;
                    let src = *src as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (src)];
                      movq rax, (neg_value);
                      call rax;
                      movq [rbp + (dst)], rax;
                    );
                }
                BcOp::Add(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, [rbp + (rhs)];
                      movq rax, (add_values);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, (*rhs as i64);
                      movq rax, (add_ri_values);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, [rbp + (rhs)];
                      movq rax, (sub_values);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, (*rhs as i64);
                      movq rax, (sub_ri_values);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }

                BcOp::Mul(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, [rbp + (rhs)];
                      movq rax, (mul_values);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Div(ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let rhs = *rhs as u64 * 8;
                    monoasm!(self.jit,
                      movq rdi, [rbp + (lhs)];
                      movq rsi, [rbp + (rhs)];
                      movq rax, (div_values);
                      call rax;
                      movq [rbp + (ret)], rax;
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
                      movq rdi, [rbp + (lhs)];
                      movq rsi, [rbp + (rhs)];
                      movq rax, (func);
                      call rax;
                      movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let ret = *ret as u64 * 8;
                    let lhs = *lhs as u64 * 8;
                    let func = match kind {
                        CmpKind::Eq => cmp_eq_ri_values,
                        CmpKind::Ne => cmp_ne_ri_values,
                        CmpKind::Lt => cmp_lt_ri_values,
                        CmpKind::Gt => cmp_gt_ri_values,
                        CmpKind::Le => cmp_le_ri_values,
                        CmpKind::Ge => cmp_ge_ri_values,
                    };
                    monoasm!(self.jit,
                        movq rdi, [rbp + (lhs)];
                        movq rsi, (*rhs as i64);
                        movq rax, (func);
                        call rax;
                        movq [rbp + (ret)], rax;
                    );
                }
                BcOp::Mov(dst, local) => {
                    let dst = *dst as u64 * 8;
                    let local = *local as u64 * 8;
                    monoasm!(self.jit,
                      movq rax, [rbp + (local)];
                      movq [rbp + (dst)], rax;
                    );
                }
                BcOp::Ret(lhs) => {
                    let lhs = *lhs as u64 * 8;
                    monoasm!(self.jit,
                        movq rax, [rbp + (lhs)];
                        popq rbp;
                        ret;
                    );
                }
                BcOp::Call(id, ret, args, len) => {
                    let dest = self.method_label[id.0 as usize];
                    monoasm!(self.jit,
                      movq rdi, r12;
                      movq rsi, r13;
                      movq rdx, (id.0);
                      movq rcx, (*ret);
                      movq r8, (*args);
                      movq r9, (*len);
                      movq rax, (push_frame);
                      call rax;
                      movq rbp, rax;
                      call dest;
                      movq rdi, r12;
                      movq rsi, rax;
                      movq rax, (pop_frame);
                      call rax;
                      movq rbp, rax;
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
                      cmpq [rbp + (cond_)], (false_val);
                      jne dest;
                    );
                }
                BcOp::CondNotBr(cond_, else_) => {
                    let cond_ = *cond_ as u64 * 8;
                    let dest = labels[else_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbp + (cond_)], (false_val);
                      jeq dest;
                    );
                }
            }
        }
    }
}
