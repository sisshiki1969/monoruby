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
    method_label: MethodDestTable,
}

struct MethodDestTable(Vec<DestLabel>);

impl MethodDestTable {
    fn new() -> Self {
        Self(vec![])
    }

    fn push(&mut self, dest: DestLabel) {
        self.0.push(dest);
    }
}

impl std::ops::Index<FuncId> for MethodDestTable {
    type Output = DestLabel;
    fn index(&self, index: FuncId) -> &DestLabel {
        &self.0[index.0 as usize]
    }
}

fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 8
}

//
// Runtime functions.
//

extern "C" fn get_func_absolute_address(
    bc_comp: &mut BcCompiler,
    fn_store: &FuncStore,
    func_name: IdentId,
    args_len: usize,
) -> *const u8 {
    let func_id = match fn_store.get_method(func_name) {
        Some(fid) => *fid,
        None => panic!("undefined method {:?}.", func_name),
    };

    let arity = fn_store[func_id].arity();
    if arity != args_len {
        panic!(
            "number of arguments mismatch. expected:{} actual:{}",
            arity, args_len
        );
    }

    let dest = bc_comp.method_label[func_id];
    #[cfg(debug_assertions)]
    eprintln!("method {:?} resolved.", fn_store.get_ident_name(func_name));
    bc_comp.jit.get_label_absolute_address(dest)
}

impl BcCompiler {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            method_label: MethodDestTable::new(),
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

    fn load_binary_args(&mut self, lhs: u16, rhs: u16) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
    }

    fn guard_rdi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether lhs is fixnum.
            movq rax, rdi;
            andq rax, 0x1;
            jeq generic;
        );
    }

    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether rhs is fixnum.
            movq rax, rsi;
            andq rax, 0x1;
            jeq generic;
        );
    }

    pub fn exec_toplevel(fn_store: &FuncStore) -> Value {
        let now = Instant::now();
        let mut eval = Self::new();
        for _ in &fn_store.functions {
            eval.method_label.push(eval.jit.label());
        }
        for func in &fn_store.functions {
            eval.jit.bind_label(eval.method_label[func.id]);
            match &func.kind {
                FuncKind::Normal(info) => eval.compile_func_bc(info),
                FuncKind::Builtin { abs_address, arity } => {
                    eval.compile_builtin_func(*abs_address, *arity)
                }
            }
        }
        let main = eval.method_label[FuncId(0)];
        let entry = eval.jit.label();
        let bccomp_ptr = &eval as *const _;
        let funcstore_ptr = fn_store as *const _;
        monoasm!(eval.jit,
        entry:
            pushq rbp;
            pushq rbx;
            pushq r12;
            movq rbx, (bccomp_ptr);
            movq r12, (funcstore_ptr);
            call main;
            popq r12;
            popq rbx;
            popq rbp;
            ret;
        );
        eval.jit.finalize();
        let entry_point: extern "C" fn(()) -> Value = eval.jit.get_label_addr(entry);
        eprintln!("jit compile elapsed:{:?}", now.elapsed());
        entry_point(())
    }

    fn compile_builtin_func(&mut self, abs_address: u64, arity: usize) {
        //
        // generate a wrapper for a builtin function which has C ABI.
        // stack layout at the point of just after execution of call instruction.
        //
        //       +-------------+
        // +0x00 | return addr | <- rsp
        //       +-------------+
        // +0x08 |             |
        //       +-------------+
        // +0x10 |    arg 0    |
        //       +-------------+
        // +0x18 |    arg 1    |
        //       +-------------+
        // +0x20 |      :      |
        //
        match arity {
            0 => {}
            1 => {
                monoasm!(self.jit,
                    movq rdi, [rsp - 16];
                );
            }
            2 => {
                monoasm!(self.jit,
                    movq rdi, [rsp - 16];
                    movq rsi, [rsp - 24];
                );
            }
            _ => unimplemented!(),
        }
        monoasm!(self.jit,
            movq rax, (abs_address);
            jmp rax;
        );
    }

    fn compile_func_bc(&mut self, func: &NormalFuncInfo) {
        let mut labels = vec![];
        for _ in func.bytecode() {
            labels.push(self.jit.label());
        }
        self.prologue(func.total_reg_num());
        for (idx, op) in func.bytecode().iter().enumerate() {
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
                    self.load_binary_args(*lhs, *rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    monoasm!(self.jit,
                        // fastpath
                        subq rdi, 1;
                        addq rdi, rsi;
                        // store the result to return reg.
                        movq [rbp - (conv(*ret))], rdi;
                        jmp exit;
                    generic:
                        // generic path
                        movq rax, (add_values);
                        call rax;
                        // store the result to return reg.
                        movq [rbp - (conv(*ret))], rax;
                    exit:
                    );
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (*rhs as i64);
                    );
                    self.guard_rdi_fixnum(generic);
                    monoasm!(self.jit,
                        // fastpath
                        shlq rsi, 1;
                        addq rdi, rsi;
                        movq [rbp - (conv(*ret))], rdi;
                        jmp exit;
                    generic:
                        // generic path
                        movq rax, (add_ri_values);
                        call rax;
                        movq [rbp - (conv(*ret))], rax;
                    exit:
                    );
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    self.load_binary_args(*lhs, *rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    monoasm!(self.jit,
                        // fastpath
                        subq rdi, rsi;
                        addq rdi, 1;
                        movq [rbp - (conv(*ret))], rdi;
                        jmp exit;
                    generic:
                        // generic path
                        movq rax, (sub_values);
                        call rax;
                        movq [rbp - (conv(*ret))], rax;
                    exit:
                    );
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (*rhs as i64);
                    );
                    self.guard_rdi_fixnum(generic);
                    monoasm!(self.jit,
                        // fastpath
                        shlq rsi, 1;
                        subq rdi, rsi;
                        movq [rbp - (conv(*ret))], rdi;
                        jmp exit;
                    generic:
                        // generic path
                        movq rax, (sub_ri_values);
                        call rax;
                        movq [rbp - (conv(*ret))], rax;
                    exit:
                    );
                }

                BcOp::Mul(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (mul_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Div(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
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
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (func);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Cmpri(kind, ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
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
                    );
                    self.guard_rdi_fixnum(generic);
                    monoasm!(self.jit,
                        movq rax, (Value::bool(false).get());
                        movq rsi, (Value::integer(*rhs as i32).get());
                        cmpq rdi, rsi;
                    );
                    match kind {
                        CmpKind::Eq => {
                            monoasm!(self.jit, seteq rdi; );
                        }
                        CmpKind::Ne => {
                            monoasm!(self.jit, setne rdi; );
                        }
                        CmpKind::Lt => {
                            monoasm!(self.jit, setlt rdi; );
                        }
                        CmpKind::Gt => {
                            monoasm!(self.jit, setgt rdi; );
                        }
                        CmpKind::Le => {
                            monoasm!(self.jit, setle rdi; );
                        }
                        CmpKind::Ge => {
                            monoasm!(self.jit, setge rdi; );
                        }
                    }
                    monoasm!(self.jit,
                        andq rdi, 0xff;
                        shlq rdi, 3;
                        orq rax, rdi;
                        jmp exit;
                    generic:
                        movq rsi, (*rhs as i64);
                        movq rax, (func);
                        call rax;
                    exit:
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
                BcOp::FnCall(id, ret, args, len) => {
                    let args = *args as i64 * 8 + 8;
                    // set arguments to a callee stack.
                    //
                    //       +-------------+
                    // +0x00 |             | <- rsp
                    //       +-------------+
                    // +0x08 | return addr |
                    //       +-------------+
                    // +0x10 |   old rbp   |
                    //       +-------------+
                    // +0x18 |    arg 0    |
                    //       +-------------+
                    // +0x20 |    arg 1    |
                    //       +-------------+
                    // +0x28 |      :      |
                    //
                    for i in 0..*len {
                        let offset = i as i64 * 8;
                        monoasm!(self.jit,
                            movq rax, [rbp - (args + offset)];
                            movq [rsp - (offset + 24)], rax;
                        );
                    }
                    let func = self.jit.label();
                    let l1 = self.jit.label();
                    monoasm!(self.jit,
                        jmp l1;
                    func:
                        // call site stub code.
                        // push down sp to avoid destroying arguments area.
                        subq rsp, 168;
                        movq rdi, rbx; // &mut BcCmpiler
                        movq rsi, r12; // &FuncStore
                        movq rdx, (id.0 as u64); // IdentId
                        movq rcx, (*len as usize); // args_len: usize
                        movq rax, (get_func_absolute_address);
                        call rax;
                        addq rsp, 168;
                        // absolute address was returned to rax.
                        // set patch point address (= return address - 4) to rdi.
                        movq rdi, [rsp];
                        subq rdi, 4;
                        // calculate a new displacement to a function address.
                        movq rsi, rax;
                        subq rsi, [rsp];
                        // apply patch.
                        movl [rdi], rsi;
                        jmp rax;
                    l1:
                        // patch point
                        call func;
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
