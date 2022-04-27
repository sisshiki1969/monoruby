use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct BcCompiler {
    jit: JitMemory,
    method_label: Vec<DestLabel>,
}

fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 8
}

impl BcCompiler {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            method_label: vec![],
        }
    }

    fn prologue(&mut self, locals: usize) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, ((locals + locals % 2) * 8);
        );
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            movq rsp, rbp;
            popq rbp;
            ret;
        );
    }

    pub fn exec_toplevel(fn_store: &FuncStore) -> Value {
        let mut eval = Self::new();
        for _ in &fn_store.functions {
            eval.method_label.push(eval.jit.label());
        }
        for func in &fn_store.functions {
            eval.compile_func_bc(func);
        }
        let main = eval.method_label[0];
        eval.jit.finalize();
        let entry_point: extern "C" fn(()) -> Value = eval.jit.get_label_addr(main);
        entry_point(())
    }

    fn compile_func_bc(&mut self, func: &FuncInfo) {
        self.jit.bind_label(self.method_label[func.id.0 as usize]);
        let mut labels = vec![];
        for _ in &func.bc {
            labels.push(self.jit.label());
        }
        self.prologue(func.local_num() + func.reg_num as usize);
        for (idx, op) in func.bc.iter().enumerate() {
            self.jit.bind_label(labels[idx]);
            match op {
                BcOp::Integer(ret, i) => {
                    let i = Value::integer(*i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(*ret))], (i);
                    );
                }
                BcOp::Const(ret, id) => {
                    let v = func.get_constant(*id).get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Nil(ret) => {
                    let v = Value::nil().get();
                    monoasm!(self.jit,
                      movq rax, (v);
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Neg(dst, src) => {
                    monoasm!(self.jit,
                      movq rdi, [rbp - (conv(*src))];
                      movq rax, (neg_value);
                      call rax;
                      movq [rbp - (conv(*dst))], rax;
                    );
                }
                BcOp::Add(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, [rbp - (conv(*rhs))];
                        movq rax, rdi;
                        andq rax, 0x1;
                        jeq generic;
                        movq rax, rsi;
                        andq rax, 0x1;
                        jeq generic;
                        subq rdi, 1;
                        addq rdi, rsi;
                        movq rax, rdi;
                        jmp exit;
                    generic:
                        movq rax, (add_values);
                        call rax;
                    exit:
                        movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (*rhs as i64);
                        movq rax, rdi;
                        andq rax, 0x1;
                        jeq generic;
                        shlq rsi, 1;
                        addq rdi, rsi;
                        movq rax, rdi;
                        jmp exit;
                    generic:
                        movq rax, (add_ri_values);
                        call rax;
                    exit:
                        movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, [rbp - (conv(*rhs))];
                        movq rax, rdi;
                        andq rax, 0x1;
                        jeq generic;
                        movq rax, rsi;
                        andq rax, 0x1;
                        jeq generic;
                        subq rdi, rsi;
                        addq rdi, 1;
                        movq rax, rdi;
                        jmp exit;
                    generic:
                        movq rax, (sub_values);
                        call rax;
                    exit:
                        movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (*rhs as i64);
                        movq rax, rdi;
                        andq rax, 0x1;
                        jeq generic;
                        shlq rsi, 1;
                        subq rdi, rsi;
                        movq rax, rdi;
                        jmp exit;
                    generic:
                        movq rax, (sub_ri_values);
                        call rax;
                    exit:
                        movq [rbp - (conv(*ret))], rax;
                    );
                }

                BcOp::Mul(ret, lhs, rhs) => {
                    monoasm!(self.jit,
                      movq rdi, [rbp - (conv(*lhs))];
                      movq rsi, [rbp - (conv(*rhs))];
                      movq rax, (mul_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Div(ret, lhs, rhs) => {
                    monoasm!(self.jit,
                      movq rdi, [rbp - (conv(*lhs))];
                      movq rsi, [rbp - (conv(*rhs))];
                      movq rax, (div_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Cmp(kind, ret, lhs, rhs) => {
                    let func = match kind {
                        CmpKind::Eq => cmp_eq_values,
                        CmpKind::Ne => cmp_ne_values,
                        CmpKind::Lt => cmp_lt_values,
                        CmpKind::Gt => cmp_gt_values,
                        CmpKind::Le => cmp_le_values,
                        CmpKind::Ge => cmp_ge_values,
                    };
                    monoasm!(self.jit,
                      movq rdi, [rbp - (conv(*lhs))];
                      movq rsi, [rbp - (conv(*rhs))];
                      movq rax, (func);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let func = match kind {
                        CmpKind::Eq => cmp_eq_ri_values,
                        CmpKind::Ne => cmp_ne_ri_values,
                        CmpKind::Lt => cmp_lt_ri_values,
                        CmpKind::Gt => cmp_gt_ri_values,
                        CmpKind::Le => cmp_le_ri_values,
                        CmpKind::Ge => cmp_ge_ri_values,
                    };
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (*rhs as i64);
                        movq rax, (func);
                        call rax;
                        movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Mov(dst, src) => {
                    monoasm!(self.jit,
                      movq rax, [rbp - (conv(*src))];
                      movq [rbp - (conv(*dst))], rax;
                    );
                }
                BcOp::Ret(lhs) => {
                    let lhs = *lhs as i64 * 8 + 8;
                    monoasm!(self.jit,
                        movq rax, [rbp - (lhs)];
                    );
                    self.epilogue();
                }
                BcOp::Call(id, ret, args, len) => {
                    let dest = self.method_label[id.0 as usize];
                    let args = *args as i64 * 8 + 8;
                    // set arguments to a callee stack.
                    for i in 0..*len {
                        let offset = i as i64 * 8;
                        monoasm!(self.jit,
                            movq rax, [rbp - (args + offset)];
                            movq [rsp - (offset + 24)], rax;
                        );
                    }
                    monoasm!(self.jit,
                        call dest;
                    );
                    if *ret != u16::MAX {
                        let ret = *ret as i64 * 8 + 8;
                        monoasm!(self.jit,
                            movq [rbp - (ret)], rax;
                        );
                    }
                }
                BcOp::Br(next_pc) => {
                    let dest = labels[next_pc.0 as usize];
                    monoasm!(self.jit,
                      jmp dest;
                    );
                }
                BcOp::CondBr(cond_, then_) => {
                    let cond_ = *cond_ as i64 * 8 + 8;
                    let dest = labels[then_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbp - (cond_)], (false_val);
                      jne dest;
                    );
                }
                BcOp::CondNotBr(cond_, else_) => {
                    let cond_ = *cond_ as i64 * 8 + 8;
                    let dest = labels[else_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbp - (cond_)], (false_val);
                      jeq dest;
                    );
                }
            }
        }
    }
}
