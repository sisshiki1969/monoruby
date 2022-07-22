use monoasm_macro::monoasm;

use super::*;

struct FloatContext {
    float_reg: usize,
    reg_info: Vec<Option<usize>>,
}

impl FloatContext {
    fn new(reg_num: usize) -> Self {
        Self {
            float_reg: 0,
            reg_info: vec![None; reg_num],
        }
    }

    fn unset(&mut self, reg: u16) {
        self.reg_info[reg as usize] = None;
    }

    fn write_back(&mut self, reg: u16) {
        if let Some(freg) = self.reg_info[reg as usize] {
            eprintln!("     %{} = F{}", reg, freg);
            self.unset(reg);
        }
    }

    fn set(&mut self, reg: u16) -> usize {
        let flhs = self.float_reg;
        self.reg_info[reg as usize] = Some(flhs);
        self.float_reg += 1;
        flhs
    }

    fn get_float(&mut self, reg: u16) -> usize {
        if let Some(freg) = self.reg_info[reg as usize] {
            freg
        } else {
            let freg = self.set(reg);
            eprintln!("     F{} = %{}", freg, reg);
            freg
        }
    }
}

impl Codegen {
    pub(super) fn jit_compile_normal(
        &mut self,
        func: &NormalFuncInfo,
        store: &FnStore,
        position: Option<usize>,
    ) -> DestLabel {
        let start_pos = position.unwrap_or_default();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            eprintln!(
                "--> start {} compile: {} {:?} position={}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                match func.name() {
                    Some(name) => name,
                    None => "<unnamed>",
                },
                func.id,
                start_pos
            );
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = Instant::now();

        let entry = self.jit.label();
        let mut labels = vec![];
        for _ in &func.bytecode()[start_pos..] {
            labels.push(self.jit.label());
        }
        self.jit.bind_label(entry);

        if position.is_none() {
            self.prologue(func.total_reg_num());
        }

        let mut skip = false;
        let mut info = vec![false; func.bytecode().len()];
        for (idx, op) in func.bytecode().iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            let ops = BcOp1::from_bc(*op);
            match ops {
                BcOp1::MethodArgs(..) => {
                    skip = true;
                }
                BcOp1::Br(disp) | BcOp1::CondBr(_, disp, _) | BcOp1::CondNotBr(_, disp, _) => {
                    info[((idx + 1) as i32 + disp) as usize] = true;
                }
                _ => {}
            }
        }

        let mut skip = false;
        let mut loop_count = 0;
        let mut ctx = FloatContext::new(func.total_reg_num());
        for (idx, op) in func.bytecode()[start_pos..].iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            eprint!(
                "{} {:05} {:?}",
                if info[start_pos + idx] { "=>" } else { "  " },
                start_pos + idx,
                op
            );
            self.jit.bind_label(labels[idx]);
            let ops = BcOp1::from_bc(*op);
            match ops {
                BcOp1::LoopStart(_) => {
                    loop_count += 1;
                }
                BcOp1::LoopEnd => {
                    assert_ne!(0, loop_count);
                    loop_count -= 1;
                    if position.is_some() && loop_count == 0 {
                        let fetch = self.vm_fetch;
                        let pc = func.get_bytecode_address(start_pos + idx + 1);

                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("    return pc:[{}] {:?}", start_pos + idx + 1, pc);

                        monoasm!(self.jit,
                            movq r13, (pc.0);
                            jmp fetch;
                        );
                        break;
                    }
                }
                BcOp1::Integer(ret, i) => {
                    ctx.unset(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp1::Symbol(ret, id) => {
                    ctx.unset(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                BcOp1::Literal(ret, id) => {
                    ctx.unset(ret);
                    let v = store.get_literal(id);
                    if v.is_packed_value() {
                        monoasm!(self.jit,
                          movq rax, (v.get());
                        );
                    } else {
                        monoasm!(self.jit,
                          movq rdi, (v.get());
                          movq rax, (Value::dup);
                          call rax;
                        );
                    }
                    self.store_rax(ret);
                }
                BcOp1::LoadConst(ret, id) => {
                    ctx.unset(ret);
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
                    );
                    self.store_rax(ret);
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
                    ctx.unset(ret);
                    let const_version = self.const_version;
                    monoasm!(self.jit,
                      movq rdx, (id.get());  // name: IdentId
                      movq rcx, [rbp - (conv(ret))];  // val: Value
                      movq rdi, rbx;  // &mut Interp
                      movq rsi, r12;  // &mut Globals
                      addq [rip + const_version], 1;
                      movq rax, (set_constant);
                      call rax;
                    );
                }
                BcOp1::Nil(ret) => {
                    ctx.unset(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp1::Neg(dst, src) => {
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(src))];
                    );
                    self.call_unop(neg_value as _);
                    self.store_rax(dst);
                }
                BcOp1::BinOp(kind, ret, lhs, rhs) => {
                    if Bc2::from_bc_classid(*op) == FLOAT_CLASS {
                        let flhs = ctx.get_float(lhs);
                        let frhs = ctx.get_float(rhs);
                        let fret = ctx.set(ret);
                        eprintln!("     F{} = F{} {} F{}", fret, flhs, kind, frhs);
                    } else {
                        ctx.write_back(lhs);
                        ctx.write_back(rhs);
                        ctx.unset(ret);
                    }
                    self.load_binary_args(lhs, rhs);
                    match kind {
                        BinOpK::Add => match Bc2::from_bc_classid(*op) {
                            INTEGER_CLASS => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_fixnum(generic);
                                self.gen_add(generic, ret);
                            }
                            FLOAT_CLASS => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_flonum(generic);
                                monoasm!(self.jit,
                                    addsd xmm0, xmm1;
                                );
                                self.gen_f64_to_val();
                                self.store_rax(ret);
                                self.side_generic_op(generic, ret, add_values as _);
                            }
                            _ => self.generic_binop(ret, add_values as _),
                        },
                        BinOpK::Sub => match Bc2::from_bc_classid(*op) {
                            INTEGER_CLASS => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_fixnum(generic);
                                self.gen_sub(generic, ret);
                            }
                            FLOAT_CLASS => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_flonum(generic);
                                monoasm!(self.jit,
                                    subsd xmm0, xmm1;
                                );
                                self.gen_f64_to_val();
                                self.store_rax(ret);
                                self.side_generic_op(generic, ret, sub_values as _);
                            }
                            _ => self.generic_binop(ret, sub_values as _),
                        },
                        BinOpK::Mul => match Bc2::from_bc_classid(*op) {
                            FLOAT_CLASS => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_flonum(generic);
                                monoasm!(self.jit,
                                    mulsd xmm0, xmm1;
                                );
                                self.gen_f64_to_val();
                                self.store_rax(ret);
                                self.side_generic_op(generic, ret, mul_values as _);
                            }
                            _ => self.generic_binop(ret, mul_values as _),
                        },
                        BinOpK::Div => {
                            self.generic_binop(ret, div_values as _);
                        }
                        _ => {
                            let generic = self.jit.label();
                            self.guard_rdi_rsi_fixnum(generic);
                            match kind {
                                BinOpK::BitOr => self.gen_bit_or(generic, ret),
                                BinOpK::BitAnd => self.gen_bit_and(generic, ret),
                                BinOpK::BitXor => self.gen_bit_xor(generic, ret),
                                BinOpK::Shr => self.gen_shr(generic, ret),
                                BinOpK::Shl => self.gen_shl(generic, ret),
                                _ => unimplemented!(),
                            }
                        }
                    }
                }

                BcOp1::BinOpRi(kind, ret, lhs, rhs) => {
                    if Bc2::from_bc_classid(*op) == FLOAT_CLASS {
                        let flhs = ctx.get_float(lhs);
                        let fret = ctx.set(ret);
                        eprintln!("     F{} = F{} {} {}: i16", fret, flhs, kind, rhs);
                    } else {
                        ctx.write_back(lhs);
                        ctx.unset(ret);
                    }
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(lhs))];
                    );
                    let imm = self.jit.const_f64(rhs as f64);
                    match kind {
                        BinOpK::Add => match Bc2::from_bc_classid(*op) {
                            INTEGER_CLASS => {
                                let generic = self.jit.label();
                                monoasm!(self.jit,
                                    movq rsi, (Value::int32(rhs as i32).get());
                                );
                                self.guard_rdi_fixnum(generic);
                                self.gen_add(generic, ret);
                            }
                            FLOAT_CLASS => {
                                let exit = self.jit.label();
                                let generic = self.jit.label();
                                self.guard_rdi_flonum(generic);
                                monoasm!(self.jit,
                                    movq xmm1, [rip + imm];
                                    addsd xmm0, xmm1;
                                );
                                self.gen_f64_to_val();
                                self.store_rax(ret);
                                self.jit.bind_label(exit);
                                self.side_generic_op_with_rhs_integer(
                                    rhs,
                                    generic,
                                    exit,
                                    ret,
                                    add_values as _,
                                );
                            }
                            _ => {
                                monoasm!(self.jit,
                                    movq rsi, (Value::int32(rhs as i32).get());
                                );
                                self.generic_binop(ret, add_values as _);
                            }
                        },
                        BinOpK::Sub => match Bc2::from_bc_classid(*op) {
                            INTEGER_CLASS => {
                                let generic = self.jit.label();
                                monoasm!(self.jit,
                                    movq rsi, (Value::int32(rhs as i32).get());
                                );
                                self.guard_rdi_fixnum(generic);
                                self.gen_sub(generic, ret);
                            }
                            FLOAT_CLASS => {
                                let exit = self.jit.label();
                                let generic = self.jit.label();
                                self.guard_rdi_flonum(generic);
                                monoasm!(self.jit,
                                    subsd xmm0, [rip + imm];
                                );
                                self.gen_f64_to_val();
                                self.store_rax(ret);
                                self.jit.bind_label(exit);
                                self.side_generic_op_with_rhs_integer(
                                    rhs,
                                    generic,
                                    exit,
                                    ret,
                                    sub_values as _,
                                );
                            }
                            _ => {
                                monoasm!(self.jit,
                                    movq rsi, (Value::int32(rhs as i32).get());
                                );
                                self.generic_binop(ret, sub_values as _);
                            }
                        },
                        _ => unimplemented!(),
                    }
                }

                BcOp1::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if Bc2::from_bc_classid(*op) == FLOAT_CLASS {
                        let flhs = ctx.get_float(lhs);
                        let frhs = ctx.get_float(rhs);
                        ctx.unset(ret);
                        eprintln!("     %{} = F{} {:?} F{}", ret, flhs, kind, frhs);
                    } else {
                        ctx.write_back(lhs);
                        ctx.write_back(rhs);
                        ctx.unset(ret);
                    }
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(ops);
                    } else {
                        let generic = self.jit.label();
                        self.load_binary_args(lhs, rhs);
                        self.guard_rdi_rsi_fixnum(generic);
                        match kind {
                            CmpKind::Eq => self.cmp_eq(generic),
                            CmpKind::Ne => self.cmp_ne(generic),
                            CmpKind::Ge => self.cmp_ge(generic),
                            CmpKind::Gt => self.cmp_gt(generic),
                            CmpKind::Le => self.cmp_le(generic),
                            CmpKind::Lt => self.cmp_lt(generic),
                            _ => unimplemented!(),
                        }
                        self.store_rax(ret);
                    }
                }
                BcOp1::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if Bc2::from_bc_classid(*op) == FLOAT_CLASS {
                        let flhs = ctx.get_float(lhs);
                        ctx.unset(ret);
                        eprintln!("     %{} = F{} {:?} {}: i16", ret, flhs, kind, rhs);
                    } else {
                        ctx.write_back(lhs);
                        ctx.unset(ret);
                    }
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(ops);
                    } else {
                        let generic = self.jit.label();
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::new_integer(rhs as i64).get());
                        );
                        self.guard_rdi_fixnum(generic);
                        match kind {
                            CmpKind::Eq => self.cmp_eq(generic),
                            CmpKind::Ne => self.cmp_ne(generic),
                            CmpKind::Ge => self.cmp_ge(generic),
                            CmpKind::Gt => self.cmp_gt(generic),
                            CmpKind::Le => self.cmp_le(generic),
                            CmpKind::Lt => self.cmp_lt(generic),
                            _ => unimplemented!(),
                        }
                        self.store_rax(ret);
                    }
                }
                BcOp1::Mov(dst, src) => {
                    ctx.reg_info[dst as usize] = ctx.reg_info[src as usize];
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
                    ctx.unset(ret);
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
                        call rax;
                    );
                    if ret != 0 {
                        self.store_rax(ret);
                    }
                }
                BcOp1::MethodCall(..) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(ops);
                }
                BcOp1::MethodArgs(recv, args, len) => {
                    for r in args..args + len {
                        ctx.unset(r);
                    }
                    match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::MethodCall(ret, name) => {
                            ctx.unset(ret);
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
                    let generic = self.jit.label();
                    let kind = match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => {
                            self.load_binary_args(lhs, rhs);
                            self.guard_rdi_rsi_fixnum(generic);
                            kind
                        }
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => {
                            monoasm!(self.jit,
                                movq rdi, [rbp - (conv(lhs))];
                                movq rsi, (Value::new_integer(rhs as i64).get());
                            );
                            self.guard_rdi_fixnum(generic);
                            kind
                        }
                        _ => unreachable!(),
                    };
                    match kind {
                        CmpKind::Eq => self.cmp_opt_eq(dest, generic, true),
                        CmpKind::Ne => self.cmp_opt_ne(dest, generic, true),
                        CmpKind::Ge => self.cmp_opt_ge(dest, generic, true),
                        CmpKind::Gt => self.cmp_opt_gt(dest, generic, true),
                        CmpKind::Le => self.cmp_opt_le(dest, generic, true),
                        CmpKind::Lt => self.cmp_opt_lt(dest, generic, true),
                        _ => unimplemented!(),
                    }
                }
                BcOp1::CondNotBr(_, disp, true) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let generic = self.jit.label();
                    let kind = match std::mem::take(&mut self.opt_buf).unwrap() {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => {
                            self.load_binary_args(lhs, rhs);
                            self.guard_rdi_rsi_fixnum(generic);
                            kind
                        }
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => {
                            monoasm!(self.jit,
                                movq rdi, [rbp - (conv(lhs))];
                                movq rsi, (Value::new_integer(rhs as i64).get());
                            );
                            self.guard_rdi_fixnum(generic);
                            kind
                        }
                        _ => unreachable!(),
                    };
                    match kind {
                        CmpKind::Eq => self.cmp_opt_eq(dest, generic, false),
                        CmpKind::Ne => self.cmp_opt_ne(dest, generic, false),
                        CmpKind::Ge => self.cmp_opt_ge(dest, generic, false),
                        CmpKind::Gt => self.cmp_opt_gt(dest, generic, false),
                        CmpKind::Le => self.cmp_opt_le(dest, generic, false),
                        CmpKind::Lt => self.cmp_opt_lt(dest, generic, false),
                        _ => unimplemented!(),
                    }
                }
            }
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let elapsed = now.elapsed();
        #[cfg(any(feature = "emit-asm"))]
        {
            let (start, code_end, end) = self.jit.code_block.last().unwrap();
            eprintln!(
                "offset:{:?} code: {} bytes  data: {} bytes",
                start,
                *code_end - *start,
                *end - *code_end
            );
            self.jit.select(0);
            eprintln!("{}", self.jit.dump_code().unwrap());
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("<-- finished compile. elapsed:{:?}", elapsed);

        entry
    }

    fn prologue(&mut self, regs: usize) {
        let offset = (regs + regs % 2) * 8 + 16;
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            movq rax, (regs - 1);
            subq rax, rdi;
            jeq  loop_exit;
            lea  rcx, [rsp - ((regs as i32) * 8 + 16)];
            //
            // rax: counter, regs - 1 - args_len
            // rcx: pointer, the next quad word of the last register.
            //       +-------------+
            //       | return addr |
            //       +-------------+
            //       |   old rbp   |
            //       +-------------+
            //       |    meta     |
            //       +-------------+
            //       |     %0      |
            //       +-------------+
            //       |     %1      |
            //       +-------------+
            //       |     %2      |
            //       +-------------+
            // rcx-> |             |
            //       +-------------+
        loop_:
            movq [rcx + rax * 8], (FALSE_VALUE);
            subq rax, 1;
            jne  loop_;
        loop_exit:
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

    fn gen_add(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, add_values as _);
    }

    fn gen_sub(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, sub_values as _);
    }

    fn gen_bit_or(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _);
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

    fn gen_shr(&mut self, generic: DestLabel, ret: u16) {
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
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, shr_values as _);
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
    }

    fn gen_shl(&mut self, generic: DestLabel, ret: u16) {
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
        );
        self.store_rdi(ret);

        self.side_generic_op(generic, ret, shl_values as _);
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
    }

    fn side_generic_op(&mut self, generic: DestLabel, ret: u16, func: u64) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn side_generic_op_with_rhs_integer(
        &mut self,
        rhs: i16,
        generic: DestLabel,
        exit: DestLabel,
        ret: u16,
        func: u64,
    ) {
        self.jit.select(1);
        self.jit.bind_label(generic);
        monoasm!(self.jit,
            movq rsi, (Value::int32(rhs as i32).get());
        );
        self.generic_binop(ret, func);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn generic_binop(&mut self, ret: u16, func: u64) {
        self.call_binop(func);
        self.store_rax(ret);
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
                movq rdi, [rbp - (conv(recv))];
                movq rax, (Value::get_class);
                call rax;
                movl r15, rax;  // r15: receiver class_id
                cmpl r15, [rip + cached_recv_class];
                jne slow_path;
            );
        }
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl [rip + cached_class_version], rax;
            jne slow_path;
        exit:
        );

        // set self
        monoasm!(self.jit,
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

        monoasm!(self.jit,
            // set meta.
            movq rax, 0x8000_0000_0000_0000;
        patch_meta:
            movq [rsp - 0x18], rax;

            movq r13, 0x8000_0000_0000_0000;
        patch_pc:
            movq rdi, (len);
            // patch point
            call entry_panic;
        patch_adr:
            testq rax, rax;
            jeq entry_return;
        );
        if ret != 0 {
            self.store_rax(ret);
        }

        self.jit.select(1);
        // call site stub code.
        monoasm!(self.jit,
        slow_path:
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (len as usize); // args_len: usize
            movq r8, [rbp - (conv(recv))]; // receiver: Value
            call entry_find_method;
            // absolute address was returned to rax.
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
            //subq rdi, 4;
            // apply patch.
            movl [rdi - 4], rax;

            movl rax, [rip + global_class_version];
            movl [rip + cached_class_version], rax;
        );
        if recv != 0 {
            monoasm!(self.jit,
                movl [rip + cached_recv_class], r15;
            );
        }
        monoasm!(self.jit,
            jmp exit;
        );
        self.jit.select(0);
    }
}
