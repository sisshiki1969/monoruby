use std::time::Instant;

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
    class_version: DestLabel,
}

fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}

//
// Runtime functions.
//

extern "C" fn get_func_absolute_address(
    bc_comp: &mut BcCompiler,
    globals: &mut Globals,
    func_name: IdentId,
    args_len: usize,
) -> *const u8 {
    let func_id = match globals.get_method(func_name) {
        Some(id) => *id,
        None => panic!("undefined method {:?}.", globals.get_ident_name(func_name)),
    };

    let info = &mut globals.func[func_id];
    let arity = info.arity();
    if arity != args_len {
        panic!(
            "number of arguments mismatch. expected:{} actual:{}",
            arity, args_len
        );
    }
    let jit_label = match info.jit_label() {
        Some(dest) => dest,
        None => bc_comp.jit_compile(info),
    };
    bc_comp.jit.get_label_absolute_address(jit_label)
}

extern "C" fn define_method(
    _bc_comp: &mut BcCompiler,
    globals: &mut Globals,
    func_name: IdentId,
    func_id: FuncId,
) {
    globals.func.insert(func_name, func_id);
}

impl BcCompiler {
    pub fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.const_i64(0);
        Self { jit, class_version }
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
            leave;
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
            testq rdi, 0x1;
            jeq generic;
        );
    }

    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether rhs is fixnum.
            testq rsi, 0x1;
            jeq generic;
        );
    }

    fn generic_add(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            subq rdi, 1;
            addq rdi, rsi;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
            jmp exit;
        generic:
            // generic path
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        exit:
        );
    }

    fn generic_sub(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            subq rdi, rsi;
            addq rdi, 1;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
            jmp exit;
        generic:
            // generic path
            movq rax, (sub_values);
            call rax;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        exit:
        );
    }

    fn compri_pre(&mut self, lhs: u16, rhs: i16, generic: DestLabel) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, (Value::integer(rhs as i32).get());
        );
        self.guard_rdi_fixnum(generic);
        monoasm!(self.jit,
            xorq rax, rax;
            cmpq rdi, rsi;
        );
    }

    fn compri_post(
        &mut self,
        func: extern "C" fn(Value, Value) -> Value,
        ret: u16,
        generic: DestLabel,
        exit: DestLabel,
    ) {
        monoasm!(self.jit,
            shlq rax, 3;
            orq rax, (Value::bool(false).get());
            jmp exit;
        generic:
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

    pub fn exec_toplevel(&mut self, globals: &mut Globals) -> Value {
        let main = globals.get_main_func();
        self.jit_compile(&mut globals.func[main]);

        let main = globals.func[main].jit_label().unwrap();
        let val = self.exec(globals, main);
        val
    }

    // # ABI of JIT-compiled code.
    //
    //  ## registers which used globally
    //
    //  - rbx: &mut BcCompiler
    //  - r12: &mut Globals
    //
    //  ## stack layout when jut after the code is called
    //
    //  - meta and arguments is set by caller.
    //
    //       +-------------+
    // -0x00 | return addr | <- rsp
    //       +-------------+
    // -0x08 |  (old rbp)  |
    //       +-------------+
    // -0x10 |    meta     |
    //       +-------------+
    // -0x18 |     %0      |
    //       +-------------+
    // -0x20 | %1(1st arg) |
    //       +-------------+
    //       |             |
    //
    //  - (old rbp) is to be set by callee.
    //
    fn exec(&mut self, fn_store: &mut Globals, func: DestLabel) -> Value {
        let entry = self.jit.label();
        monoasm!(self.jit,
        entry:
            pushq rbp;
            pushq rbx;
            pushq r12;
            movq rbx, rdi;
            movq r12, rsi;
            call func;
            popq r12;
            popq rbx;
            popq rbp;
            ret;
        );
        self.jit.finalize();

        let func = self.jit.get_label_addr2(entry);
        func(self, fn_store)
    }

    fn jit_compile(&mut self, func: &mut FuncInfo) -> DestLabel {
        let now = Instant::now();
        let label = self.jit.label();
        func.set_jit_label(label);
        self.jit.bind_label(label);
        match &func.kind {
            FuncKind::Normal(info) => self.jit_compile_normal(info),
            FuncKind::Builtin { abs_address } => {
                self.jit_compile_builtin(*abs_address, func.arity())
            }
        };
        self.jit.finalize();
        #[cfg(feature = "emit-asm")]
        {
            eprintln!("jit compile: {:?}", func.id());
            let (start, code_end, end) = self.jit.code_block.last().unwrap();
            eprintln!(
                "offset:{:?} code: {} bytes  data: {} bytes",
                start,
                *code_end - *start,
                *end - *code_end
            );
            eprintln!("{}", self.jit.dump_code().unwrap());
            eprintln!("jit compile elapsed:{:?}", now.elapsed());
        }
        label
    }

    fn jit_compile_builtin(&mut self, abs_address: u64, arity: usize) {
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
        let offset = (arity + arity % 2) * 8 + 16;
        monoasm!(self.jit,
            pushq rbp;
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rsp - 0x18]; // 1st argument: *const Value
            movq rcx, (arity); // 2nd arguments: length of arguments:usize
            movq rax, (abs_address);
            movq rbp, rsp;
            subq rsp, (offset);
            call rax;
            leave;
            ret;
        );
    }

    fn jit_compile_normal(&mut self, func: &NormalFuncInfo) {
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
                    self.generic_add(generic, exit, *ret);
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let rhs = Value::integer(*rhs as i32).get();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (rhs);
                    );
                    self.guard_rdi_fixnum(generic);
                    self.generic_add(generic, exit, *ret);
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    self.load_binary_args(*lhs, *rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    self.generic_sub(generic, exit, *ret);
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let rhs = Value::integer(*rhs as i32).get();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(*lhs))];
                        movq rsi, (rhs);
                    );
                    self.guard_rdi_fixnum(generic);
                    self.generic_sub(generic, exit, *ret);
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
                    let func = cmp_eq_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, seteq rax; );
                    self.compri_post(func, *ret, generic, exit);
                }
                BcOp::Neri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_ne_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setne rax; );
                    self.compri_post(func, *ret, generic, exit);
                }
                BcOp::Geri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_ge_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setge rax; );
                    self.compri_post(func, *ret, generic, exit);
                }
                BcOp::Gtri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_gt_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setgt rax; );
                    self.compri_post(func, *ret, generic, exit);
                }
                BcOp::Leri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_le_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setle rax; );
                    self.compri_post(func, *ret, generic, exit);
                }
                BcOp::Ltri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let func = cmp_lt_values;
                    self.compri_pre(*lhs, *rhs, generic);
                    monoasm!(self.jit, setlt rax; );
                    self.compri_post(func, *ret, generic, exit);
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
                    let exit = self.jit.label();
                    let saved_class_version = self.jit.const_i64(0);
                    let class_version = self.class_version;
                    monoasm!(self.jit,
                        movq rax, [rip + class_version];
                        cmpq [rip + saved_class_version], rax;
                        jeq l1;
                        movq [rip + saved_class_version], rax;
                        lea rax, [rip + exit];
                        pushq rax;
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
                        //movq rsi, rax;
                        subq rax, [rsp];
                        // apply patch.
                        movl [rdi], rax;
                        popq rax;
                    l1:
                        // patch point
                        call func;
                    exit:
                    );
                    if *ret != u16::MAX {
                        monoasm!(self.jit,
                            movq [rbp - (conv(*ret))], rax;
                        );
                    }
                }
                BcOp::MethodDef(id, fid) => {
                    let class_version = self.class_version;
                    monoasm!(self.jit,
                        addq [rip + class_version], 1;
                        movq rdi, rbx; // &mut BcCmpiler
                        movq rsi, r12; // &FuncStore
                        movq rdx, (u32::from(*id)); // IdentId
                        movq rcx, (u32::from(*fid)); // FuncId
                        movq rax, (define_method);
                        call rax;
                    );
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
