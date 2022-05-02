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
    reg as i64 * 8 + 16
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
    let func_id = fn_store.get_method_or_panic(func_name);

    let arity = fn_store[func_id].arity();
    if arity != args_len {
        panic!(
            "number of arguments mismatch. expected:{} actual:{}",
            arity, args_len
        );
    }

    let dest = bc_comp.method_label[func_id];
    bc_comp.jit.get_label_absolute_address(dest)
}

impl BcCompiler {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            method_label: MethodDestTable::new(),
        }
    }

    fn prologue(&mut self, regs: usize) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, ((regs + regs % 2) * 8);
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

    fn compri_pre(&mut self, lhs: u16, rhs: i16, generic: DestLabel) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
        );
        self.guard_rdi_fixnum(generic);
        monoasm!(self.jit,
            xorq rax, rax;
            movq rsi, (Value::integer(rhs as i32).get());
            cmpq rdi, rsi;
        );
    }

    fn compri_post(
        &mut self,
        func: extern "C" fn(Value, i64) -> Value,
        ret: u16,
        rhs: i16,
        generic: DestLabel,
        exit: DestLabel,
    ) {
        monoasm!(self.jit,
            shlq rax, 3;
            orq rax, (Value::bool(false).get());
            jmp exit;
        generic:
            movq rsi, (rhs as i64);
            movq rax, (func);
            call rax;
        exit:
            movq [rbp - (conv(ret))], rax;
        );
    }

    //
    // stack layout for jit-ed code.
    //
    //       +-------------+
    // +0x08 | return addr |
    //       +-------------+
    //  0x00 |  prev rbp   | <- rbp
    //       +-------------+
    // -0x08 |    meta     |
    //       +-------------+
    // -0x10 |     %0      |
    //       +-------------+
    // -0x18 |     %1      |
    //       +-------------+
    //       |      :      |
    //       +-------------+
    // -0xxx |    %(n-1)   | <- rsp
    //       +-------------+
    //       |      :      |
    //

    pub fn exec_toplevel(fn_store: &mut FuncStore) -> Value {
        let now = Instant::now();
        let mut eval = Self::new();
        for _ in &fn_store.functions {
            eval.method_label.push(eval.jit.label());
        }
        for func in &fn_store.functions {
            eval.jit.bind_label(eval.method_label[func.id]);
            match &func.kind {
                FuncKind::Normal(info) => eval.compile_func_bc(info),
                FuncKind::Builtin { abs_address } => {
                    eval.compile_builtin_func(*abs_address, func.arity())
                }
            }
        }
        let main = eval.method_label[fn_store.get_main_func()];
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
        //  0x00 | return addr | <- rsp
        //       +-------------+
        // -0x08 |             |
        //       +-------------+
        // -0x10 |    meta     |
        //       +-------------+
        // -0x18 |  %0 (self)  |
        //       +-------------+
        // -0x20 | %1(1st arg) |
        //       +-------------+
        //
        match arity {
            0 => {}
            1 => {
                monoasm!(self.jit,
                    movq rdi, [rsp - 0x20];
                );
            }
            2 => {
                monoasm!(self.jit,
                    movq rdi, [rsp - 0x20];
                    movq rsi, [rsp - 0x28];
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
                BcOp::Eq(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_eq_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Ne(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_ne_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Ge(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_ge_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Gt(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_gt_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Le(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_le_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Lt(ret, lhs, rhs) => {
                    self.load_binary_args(*lhs, *rhs);
                    monoasm!(self.jit,
                      movq rax, (cmp_lt_values);
                      call rax;
                      movq [rbp - (conv(*ret))], rax;
                    );
                }
                BcOp::Eqri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_eq_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, seteq rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Neri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_ne_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setne rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Geri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_ge_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setge rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Gtri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_gt_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setgt rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Leri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_le_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setle rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Ltri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_lt_ri_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setlt rax; );
                    self.compri_post(func, *ret, *rhs, generic, exit);
                }
                BcOp::Mov(dst, src) => {
                    monoasm!(self.jit,
                      movq rax, [rbp - (conv(*src))];
                      movq [rbp - (conv(*dst))], rax;
                    );
                }
                BcOp::Ret(lhs) => {
                    let lhs = conv(*lhs);
                    monoasm!(self.jit,
                        movq rax, [rbp - (lhs)];
                    );
                    self.epilogue();
                }
                BcOp::FnCall(id, ret, args, len) => {
                    // set arguments to a callee stack.
                    //
                    //       +-------------+
                    //  0x00 |             | <- rsp
                    //       +-------------+
                    // -0x08 | return addr |
                    //       +-------------+
                    // -0x10 |   old rbp   |
                    //       +-------------+
                    // -0x18 |    meta     |
                    //       +-------------+
                    // -0x20 |     %0      |
                    //       +-------------+
                    // -0x28 | %1(1st arg) |
                    //       +-------------+
                    //       |             |
                    //
                    for i in 0..*len {
                        let reg = *args + i;
                        monoasm!(self.jit,
                            movq rax, [rbp - (conv(reg))];
                            movq [rsp - ((0x28 + i * 8) as i64)], rax;
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
                        movq rdx, (u32::from(*id)); // IdentId
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
                        monoasm!(self.jit,
                            movq [rbp - (conv(*ret))], rax;
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
                    let cond_ = conv(*cond_);
                    let dest = labels[then_.0 as usize];
                    let false_val = Value::bool(false).get();
                    monoasm!(self.jit,
                      cmpq [rbp - (cond_)], (false_val);
                      jne dest;
                    );
                }
                BcOp::CondNotBr(cond_, else_) => {
                    let cond_ = conv(*cond_);
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
