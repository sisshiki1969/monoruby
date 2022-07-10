use monoasm_macro::monoasm;
use paste::paste;

use super::*;

impl Codegen {
    pub(super) fn jit_compile_normal(&mut self, func: &NormalFuncInfo, store: &FnStore) -> CodePtr {
        macro_rules! cmp {
            ($lhs:ident, $rhs:ident, $ret:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    self.load_binary_args($lhs, $rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    self.[<cmp_ $op>](generic);
                    monoasm! { self.jit,
                        movq [rbp - (conv($ret))], rax;
                    };
                }
            }};
        }

        macro_rules! cmp_ri {
            ($lhs:ident, $rhs:ident, $ret:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv($lhs))];
                        movq rsi, (Value::new_integer($rhs as i64).get());
                    );
                    self.guard_rdi_fixnum(generic);
                    self.[<cmp_ $op>](generic);
                    monoasm! { self.jit,
                        movq [rbp - (conv($ret))], rax;
                    }
                }
            }};
        }

        macro_rules! cmp_o {
            ($lhs:ident, $rhs:ident, $dest:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    self.load_binary_args($lhs, $rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    self.[<cmp_opt_ $op>]($dest, generic, true);
                }
            }};
        }

        macro_rules! cmp_o_not {
            ($lhs:ident, $rhs:ident, $dest:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    self.load_binary_args($lhs, $rhs);
                    self.guard_rdi_fixnum(generic);
                    self.guard_rsi_fixnum(generic);
                    self.[<cmp_opt_ $op>]($dest, generic, false);
                }
            }};
        }

        macro_rules! cmp_o_ri {
            ($lhs:ident, $rhs:ident, $dest:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv($lhs))];
                        movq rsi, (Value::new_integer($rhs as i64).get());
                    );
                    self.guard_rdi_fixnum(generic);
                    self.[<cmp_opt_ $op>]($dest, generic, true);
                }
            }};
        }

        macro_rules! cmp_o_not_ri {
            ($lhs:ident, $rhs:ident, $dest:ident, $op:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv($lhs))];
                        movq rsi, (Value::new_integer($rhs as i64).get());
                    );
                    self.guard_rdi_fixnum(generic);
                    self.[<cmp_opt_ $op>]($dest, generic, false);
                }
            }};
        }

        macro_rules! bin_ops {
            ($op:ident, $ret:ident, $lhs:ident, $rhs:ident) => {{
                paste! {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    self.load_binary_args($lhs, $rhs);
                    self.guard_rdi_rsi_fixnum(generic);
                    self.[<generic_ $op>](generic, exit, $ret);
                }
            }};
        }

        let label = self.jit.get_current_address();
        let mut labels = vec![];
        for _ in func.bytecode() {
            labels.push(self.jit.label());
        }
        self.prologue(func.total_reg_num());
        let mut skip = false;
        for (idx, op) in func.bytecode().iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            self.jit.bind_label(labels[idx]);
            let ops = BcOp1::from_bc(*op);
            match ops {
                BcOp1::Integer(ret, i) => {
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp1::Symbol(ret, id) => {
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                      movq [rbp - (conv(ret))], rax;
                    );
                }
                BcOp1::Literal(ret, id) => {
                    let v = store.get_literal(id);
                    if v.is_packed_value() {
                        monoasm!(self.jit,
                          movq rax, (v.get());
                          movq [rbp - (conv(ret))], rax;
                        );
                    } else {
                        monoasm!(self.jit,
                          movq rdi, (v.get());
                          movq rax, (Value::dup);
                          call rax;
                          movq [rbp - (conv(ret))], rax;
                        );
                    }
                }
                BcOp1::LoadConst(ret, id) => {
                    let jit_return = self.vm_return;
                    let cached_const_version = self.jit.const_i64(-1);
                    let cached_value = self.jit.const_i64(0);
                    let global_const_version = self.const_version;
                    let slow_path = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rax, [rip + global_const_version];
                        cmpq rax, [rip + cached_const_version];
                        jne  slow_path;
                        movq rax, [rip + cached_value];
                    exit:
                        movq [rbp - (conv(ret))], rax;
                    );
                    self.jit.select(1);
                    monoasm!(self.jit,
                    slow_path:
                        movq rdx, (id.get());  // name: ConstSiteId
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        movq rax, (get_constant);
                        call rax;
                        testq rax, rax;
                        jeq  jit_return;
                        movq [rip + cached_value], rax;
                        movq rdi, [rip + global_const_version];
                        movq [rip + cached_const_version], rdi;
                        jmp  exit;
                    );
                    self.jit.select(0);
                }
                BcOp1::StoreConst(ret, id) => {
                    let const_version = self.const_version;
                    monoasm!(self.jit,
                      movq rdx, (id.get());  // name: IdentId
                      movq rcx, [rbp - (conv(ret))];  // val: Value
                      lea  r8, [rip + const_version];
                      movq rdi, rbx;  // &mut Interp
                      movq rsi, r12;  // &mut Globals
                      movq rax, (set_constant);
                      call rax;
                    );
                }
                BcOp1::Nil(ret) => {
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp1::Neg(dst, src) => {
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(src))];
                    );
                    self.call_unop(neg_value as _, self.vm_return);
                    monoasm!(self.jit,
                        movq [rbp - (conv(dst))], rax;
                    );
                }
                BcOp1::BinOp(kind, ret, lhs, rhs) => match kind {
                    BinOpK::Add => {
                        let generic = self.jit.label();
                        let exit = self.jit.label();
                        self.load_binary_args(lhs, rhs);
                        self.guard_rdi_rsi_fixnum(generic);
                        self.fast_add(exit, generic, ret);
                        self.side_generic_op(generic, exit, ret, add_values as _);
                    }
                    BinOpK::Sub => {
                        let generic = self.jit.label();
                        let exit = self.jit.label();
                        self.load_binary_args(lhs, rhs);
                        self.guard_rdi_rsi_fixnum(generic);
                        self.fast_sub(exit, generic, ret);
                        self.side_generic_op(generic, exit, ret, sub_values as _);
                    }
                    BinOpK::Mul => {
                        self.load_binary_args(lhs, rhs);
                        self.generic_op(ret, mul_values as _);
                    }
                    BinOpK::Div => {
                        self.load_binary_args(lhs, rhs);
                        self.generic_op(ret, div_values as _);
                    }
                    BinOpK::BitOr => bin_ops!(bit_or, ret, lhs, rhs),
                    BinOpK::BitAnd => bin_ops!(bit_and, ret, lhs, rhs),
                    BinOpK::BitXor => bin_ops!(bit_xor, ret, lhs, rhs),
                    BinOpK::Shr => bin_ops!(shr, ret, lhs, rhs),
                    BinOpK::Shl => bin_ops!(shl, ret, lhs, rhs),
                },

                BcOp1::Addri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let rhs = Value::int32(rhs as i32).get();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(lhs))];
                        movq rsi, (rhs);
                    );
                    self.guard_rdi_fixnum(generic);
                    self.fast_add(exit, generic, ret);
                    self.side_generic_op(generic, exit, ret, add_values as _);
                }
                BcOp1::Subri(ret, lhs, rhs) => {
                    let generic = self.jit.label();
                    let exit = self.jit.label();
                    let rhs = Value::int32(rhs as i32).get();
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(lhs))];
                        movq rsi, (rhs);
                    );
                    self.guard_rdi_fixnum(generic);
                    self.fast_sub(exit, generic, ret);
                    self.side_generic_op(generic, exit, ret, sub_values as _);
                }
                BcOp1::Cmp(kind, ret, lhs, rhs, false) => match kind {
                    CmpKind::Eq => cmp!(lhs, rhs, ret, eq),
                    CmpKind::Ne => cmp!(lhs, rhs, ret, ne),
                    CmpKind::Ge => cmp!(lhs, rhs, ret, ge),
                    CmpKind::Gt => cmp!(lhs, rhs, ret, gt),
                    CmpKind::Le => cmp!(lhs, rhs, ret, le),
                    CmpKind::Lt => cmp!(lhs, rhs, ret, lt),
                    _ => unimplemented!(),
                },
                BcOp1::Cmp(_, _, _, _, true) | BcOp1::Cmpri(_, _, _, _, true) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(ops);
                }
                BcOp1::Cmpri(kind, ret, lhs, rhs, false) => match kind {
                    CmpKind::Eq => cmp_ri!(lhs, rhs, ret, eq),
                    CmpKind::Ne => cmp_ri!(lhs, rhs, ret, ne),
                    CmpKind::Ge => cmp_ri!(lhs, rhs, ret, ge),
                    CmpKind::Gt => cmp_ri!(lhs, rhs, ret, gt),
                    CmpKind::Le => cmp_ri!(lhs, rhs, ret, le),
                    CmpKind::Lt => cmp_ri!(lhs, rhs, ret, lt),
                    _ => unimplemented!(),
                },
                BcOp1::Mov(dst, src) => {
                    monoasm!(self.jit,
                      movq rax, [rbp - (conv(src))];
                      movq [rbp - (conv(dst))], rax;
                    );
                }
                BcOp1::Ret(lhs) => {
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                }
                BcOp1::ConcatStr(ret, arg, len) => {
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
                        call rax;
                    );
                    if ret != 0 {
                        monoasm!(self.jit,
                            movq [rbp - (conv(ret))], rax;
                        );
                    }
                }
                BcOp1::MethodCall(..) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(ops);
                }
                BcOp1::MethodArgs(ret, args, len) => {
                    match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::MethodCall(recv, name) => {
                            self.jit_method_call(recv, name, ret, args, len)
                        }
                        _ => unreachable!(),
                    }
                    skip = true;
                }
                BcOp1::MethodDef(id) => {
                    let MethodDefInfo { name, func } = store[id];
                    let class_version = self.class_version;
                    monoasm!(self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func)); // FuncId
                        movq rax, (define_method);
                        call rax;
                        addl [rip + class_version], 1;
                    );
                }
                BcOp1::Br(disp) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    monoasm!(self.jit,
                        jmp dest;
                    );
                }
                BcOp1::CondBr(cond_, disp, false) => {
                    let cond_ = conv(cond_);
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    monoasm!(self.jit,
                        movq rax, [rbp - (cond_)];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                        jne dest;
                    );
                }
                BcOp1::CondNotBr(cond_, disp, false) => {
                    let cond_ = conv(cond_);
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    monoasm!(self.jit,
                        movq rax, [rbp - (cond_)];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                        jeq dest;
                    );
                }
                BcOp1::CondBr(_, disp, true) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => match kind {
                            CmpKind::Eq => cmp_o!(lhs, rhs, dest, eq),
                            CmpKind::Ne => cmp_o!(lhs, rhs, dest, ne),
                            CmpKind::Ge => cmp_o!(lhs, rhs, dest, ge),
                            CmpKind::Gt => cmp_o!(lhs, rhs, dest, gt),
                            CmpKind::Le => cmp_o!(lhs, rhs, dest, le),
                            CmpKind::Lt => cmp_o!(lhs, rhs, dest, lt),
                            _ => unimplemented!(),
                        },
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => match kind {
                            CmpKind::Eq => cmp_o_ri!(lhs, rhs, dest, eq),
                            CmpKind::Ne => cmp_o_ri!(lhs, rhs, dest, ne),
                            CmpKind::Ge => cmp_o_ri!(lhs, rhs, dest, ge),
                            CmpKind::Gt => cmp_o_ri!(lhs, rhs, dest, gt),
                            CmpKind::Le => cmp_o_ri!(lhs, rhs, dest, le),
                            CmpKind::Lt => cmp_o_ri!(lhs, rhs, dest, lt),
                            _ => unimplemented!(),
                        },
                        _ => unreachable!(),
                    }
                }
                BcOp1::CondNotBr(_, disp, true) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => match kind {
                            CmpKind::Eq => cmp_o_not!(lhs, rhs, dest, eq),
                            CmpKind::Ne => cmp_o_not!(lhs, rhs, dest, ne),
                            CmpKind::Ge => cmp_o_not!(lhs, rhs, dest, ge),
                            CmpKind::Gt => cmp_o_not!(lhs, rhs, dest, gt),
                            CmpKind::Le => cmp_o_not!(lhs, rhs, dest, le),
                            CmpKind::Lt => cmp_o_not!(lhs, rhs, dest, lt),
                            _ => unimplemented!(),
                        },
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => match kind {
                            CmpKind::Eq => cmp_o_not_ri!(lhs, rhs, dest, eq),
                            CmpKind::Ne => cmp_o_not_ri!(lhs, rhs, dest, ne),
                            CmpKind::Ge => cmp_o_not_ri!(lhs, rhs, dest, ge),
                            CmpKind::Gt => cmp_o_not_ri!(lhs, rhs, dest, gt),
                            CmpKind::Le => cmp_o_not_ri!(lhs, rhs, dest, le),
                            CmpKind::Lt => cmp_o_not_ri!(lhs, rhs, dest, lt),
                            _ => unimplemented!(),
                        },
                        _ => unreachable!(),
                    }
                }
            }
        }
        label
    }

    fn prologue(&mut self, regs: usize) {
        let offset = (regs + regs % 2) * 8 + 16;
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (offset);
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

    fn fast_add(&mut self, exit: DestLabel, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        exit:
        );
    }

    fn fast_sub(&mut self, exit: DestLabel, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        exit:
        );
    }

    fn generic_bit_or(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        exit:
        );
        self.side_generic_op(generic, exit, ret, bitor_values as _);
    }

    fn generic_bit_and(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        exit:
        );
        self.side_generic_op(generic, exit, ret, bitand_values as _);
    }

    fn generic_bit_xor(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        exit:
        );
        self.side_generic_op(generic, exit, ret, bitxor_values as _);
    }

    fn shift_under(&mut self, under: DestLabel, after: DestLabel) {
        self.jit.select(1);
        let zero = self.jit.label();
        monoasm!(self.jit,
        under:
            testq rdi, rdi;
            jns zero;
            xorq rdi, rdi;
            subq rdi, 1;
            jmp after;
        zero:
            xorq rdi, rdi;
            jmp after;
        );
        self.jit.select(0);
    }

    fn generic_shr(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shl;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
        after:
            orq rdi, 1;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        exit:
        );
        self.jit.select(1);
        monoasm!(self.jit,
        shl:
            negq rcx;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
            jmp after;
        );
        self.jit.select(0);
        self.shift_under(under, after);
        self.side_generic_op(generic, exit, ret, shr_values as _);
    }

    fn generic_shl(&mut self, generic: DestLabel, exit: DestLabel, ret: u16) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shr;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
        after:
            orq rdi, 1;
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        exit:
        );
        self.jit.select(1);
        monoasm!(self.jit,
        shr:
            negq rcx;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
            jmp after;
        );
        self.jit.select(0);
        self.shift_under(under, after);
        self.side_generic_op(generic, exit, ret, shl_values as _);
    }

    fn side_generic_op(&mut self, generic: DestLabel, exit: DestLabel, ret: u16, func: u64) {
        self.jit.select(1);
        self.jit.bind_label(generic);
        self.call_binop(func, self.vm_return);
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn generic_op(&mut self, ret: u16, func: u64) {
        self.call_binop(func, self.vm_return);
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        );
    }

    fn jit_method_call(&mut self, recv: u16, name: IdentId, ret: u16, args: u16, len: u16) {
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
        // argument registers:
        //   rdi: args len
        //
        if recv != 0 {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(recv))];
                movq rax, (Value::get_class);
                call rax;
                movq r15, rax;  // r15: receiver class_id
            );
        }

        let sp_max = 0x40 + (len as u64 + (len % 2) as u64) * 8;
        monoasm!(self.jit,
            // set self
            movq rax, [rbp - (conv(recv))];
            movq [rsp - 0x20], rax;
        );
        // set arguments
        for i in 0..len {
            let reg = args + i;
            monoasm!(self.jit,
                movq rax, [rbp - (conv(reg))];
                movq [rsp - ((0x28 + i * 8) as i64)], rax;
            );
        }
        let exit = self.jit.label();
        let patch_meta = self.jit.label();
        let patch_adr = self.jit.label();
        let patch_pc = self.jit.label();
        let slow_path = self.jit.label();
        let cached_class_version = self.jit.const_i32(-1);
        let cached_recv_class = self.jit.const_i32(0);
        let global_class_version = self.class_version;
        let entry_find_method = self.entry_find_method;
        let entry_panic = self.entry_panic;
        let entry_return = self.vm_return;
        if recv != 0 {
            monoasm!(self.jit,
                cmpl r15, [rip + cached_recv_class];
                jne slow_path;
            );
        }
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl [rip + cached_class_version], rax;
            jne slow_path;
        exit:
            movq rdi, (len);
            // set meta.
            movq rax, 0x8000_0000_0000_0000;
        patch_meta:
            movq [rsp - 0x18], rax;
            movq r13, 0x8000_0000_0000_0000;
        patch_pc:
            // patch point
            call entry_panic;
        patch_adr:
            testq rax, rax;
            jeq entry_return;
        );
        if ret != 0 {
            monoasm!(self.jit,
                movq [rbp - (conv(ret))], rax;
            );
        }

        self.jit.select(1);
        // call site stub code.
        // push down sp to avoid destroying arguments area.
        monoasm!(self.jit,
        slow_path:
            subq rsp, (sp_max);
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (len as usize); // args_len: usize
            movq r8, [rbp - (conv(recv))]; // receiver: Value
            call entry_find_method;
            // absolute address was returned to rax.
            addq rsp, (sp_max);
            testq rax, rax;
            jeq entry_return;

            lea rdi, [rip + patch_meta];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_META)];
            movq [rdi], rcx;

            lea rdi, [rip + patch_pc];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_PC)];
            movq [rdi], rcx;

            movq rax, [rax + (FUNCDATA_OFFSET_CODEPTR)];
            lea rdi, [rip + patch_adr];
            // calculate a displacement to the function address.
            subq rax, rdi;
            // set patch point address (= return address - 4) to rdi.
            subq rdi, 4;
            // apply patch.
            movl [rdi], rax;

            movl rax, [rip + global_class_version];
            movl [rip + cached_class_version], rax;
            movl [rip + cached_recv_class], r15;
            jmp exit;
        );
        self.jit.select(0);
    }
}
