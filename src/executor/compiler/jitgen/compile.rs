use super::*;

impl Codegen {
    pub(super) fn compile_bb(
        &mut self,
        func: &NormalFuncInfo,
        cc: &mut CompileContext,
    ) -> Option<usize> {
        let mut skip = false;
        self.gen_side_write_back(cc, cc.bb_pos);
        let mut ctx = BBContext::new(func.total_reg_num());
        for (ofs, pc) in func.bytecode()[cc.bb_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            match pc.op1() {
                BcOp::CondBr(_, _, true, _) => {}
                _ => {
                    cc.push(TIr::Label(cc.bb_pos + ofs, pc));
                    self.jit.bind_label(cc.label(ofs, 0));
                }
            }
            match pc.op1() {
                BcOp::LoopStart(_) => {
                    cc.push(TIr::LoopStart);
                    cc.loop_count += 1;
                }
                BcOp::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.push(TIr::LoopEnd);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.bb_pos + ofs);
                        let wb = ctx.get_write_back();
                        cc.push(TIr::Deopt(pc, wb.clone()));
                        self.deopt(pc, wb);
                        break;
                    }
                }
                BcOp::Integer(ret, i) => {
                    cc.push(TIr::Integer(ret, i));
                    ctx.dealloc_xmm(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp::Symbol(ret, id) => {
                    cc.push(TIr::Symbol(ret, id));
                    ctx.dealloc_xmm(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                BcOp::Literal(dst, val) => {
                    ctx.dealloc_xmm(dst);
                    if let RV::Float(f) = val.unpack() {
                        let fdst = ctx.xmm_write(dst);
                        cc.push(TIr::FLiteral(fdst, f));
                        let imm = self.jit.const_f64(f);
                        monoasm!(self.jit,
                            movq xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        if val.is_packed_value() {
                            cc.push(TIr::LiteralPacked(dst, val));
                            monoasm!(self.jit,
                                movq rax, (val.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            cc.push(TIr::Literal(dst, val, xmm_using.clone()));
                            self.xmm_save(&xmm_using);
                            monoasm!(self.jit,
                              movq rdi, (val.get());
                              movq rax, (Value::dup);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(dst);
                    }
                }
                BcOp::Array(ret, src, len) => {
                    ctx.write_back_range(self, cc, src, len);
                    ctx.dealloc_xmm(ret);
                    cc.push(TIr::Array(ret, src, len));
                    monoasm!(self.jit,
                        lea  rdi, [rbp - (conv(src))];
                        movq rsi, (len);
                        movq rax, (gen_array);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                BcOp::Index(ret, base, idx) => {
                    ctx.read_slot(self, cc, base);
                    ctx.read_slot(self, cc, idx);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    cc.push(TIr::Index(ret, base, idx, xmm_using.clone()));
                    self.jit_get_index(ret, base, idx, pc, xmm_using);
                }
                BcOp::IndexAssign(src, base, idx) => {
                    ctx.read_slot(self, cc, base);
                    ctx.read_slot(self, cc, idx);
                    ctx.read_slot(self, cc, src);
                    let xmm_using = ctx.get_xmm_using();
                    cc.push(TIr::IndexAssign(src, base, idx, xmm_using.clone()));
                    self.jit_index_assign(src, base, idx, pc, xmm_using);
                }
                BcOp::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    let xmm_using = ctx.get_xmm_using();

                    if pc.value().is_none() || pc.value().unwrap().class_id() != FLOAT_CLASS {
                        cc.push(TIr::LoadConst(dst, id, xmm_using.clone()));
                        self.load_constant(dst, id, pc, xmm_using);
                    } else {
                        let wb = ctx.get_write_back();
                        let fdst = ctx.xmm_read(dst);
                        cc.push(TIr::FLoadConst(
                            fdst,
                            dst,
                            id,
                            xmm_using.clone(),
                            wb.clone(),
                        ));

                        self.load_float_constant(dst, fdst, id, pc, xmm_using, wb);
                    }
                }
                BcOp::StoreConst(src, id) => {
                    let xmm_using = ctx.get_xmm_using();
                    cc.push(TIr::StoreConst(src, id, xmm_using.clone()));
                    ctx.read_slot(self, cc, src);
                    self.jit_store_constant(id, src, xmm_using);
                }
                BcOp::Nil(ret) => {
                    cc.push(TIr::Nil(ret));
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = ctx.xmm_read_assume_float(self, cc, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        cc.push(TIr::FNeg(fdst, fsrc));
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.read_slot(self, cc, src);
                        ctx.dealloc_xmm(dst);
                        cc.push(TIr::Neg(dst, src));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.store_rax(dst);
                    }
                }
                BcOp::BinOp(kind, ret, lhs, rhs) => {
                    let wb = ctx.get_write_back();
                    let xmm_using = ctx.get_xmm_using();
                    if pc.is_binary_integer() {
                        ctx.read_slot(self, cc, lhs);
                        ctx.read_slot(self, cc, rhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOp(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::RR(lhs, rhs),
                            wb,
                            xmm_using,
                        );
                    } else if pc.is_binary_float() {
                        let (flhs, frhs) = ctx.xmm_read_binary(self, cc, lhs, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        cc.push(TIr::FBinOp(kind, fret, flhs, frhs));
                        self.gen_binop_float(kind, fret, flhs, frhs);
                    } else {
                        ctx.read_slot(self, cc, lhs);
                        ctx.read_slot(self, cc, rhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOp(kind, ret, lhs, rhs, pc.classid1()));
                        self.load_binary_args(lhs, rhs);
                        self.gen_binop_kind(ctx.get_xmm_using(), pc, kind, ret);
                    }
                }

                BcOp::BinOpRi(kind, ret, lhs, rhs) => {
                    let wb = ctx.get_write_back();
                    let xmm_using = ctx.get_xmm_using();
                    if pc.is_integer1() {
                        ctx.read_slot(self, cc, lhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOpRi(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::RI(lhs, rhs),
                            wb,
                            xmm_using,
                        );
                    } else if pc.is_float1() {
                        let flhs = ctx.xmm_read_assume_float(self, cc, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        cc.push(TIr::FBinOpRf(kind, fret, flhs, rhs as f64));
                        self.gen_binop_float_ri(kind, fret, flhs, rhs);
                    } else {
                        ctx.read_slot(self, cc, lhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOpRi(kind, ret, lhs, rhs, pc.classid1()));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_binop_kind(ctx.get_xmm_using(), pc, kind, ret);
                    }
                }

                BcOp::BinOpIr(kind, ret, lhs, rhs) => {
                    let wb = ctx.get_write_back();
                    let xmm_using = ctx.get_xmm_using();
                    if pc.is_integer2() {
                        ctx.read_slot(self, cc, rhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOpIr(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::IR(lhs, rhs),
                            wb,
                            xmm_using,
                        );
                    } else if pc.is_float2() {
                        let frhs = ctx.xmm_read_assume_float(self, cc, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        cc.push(TIr::FBinOpFr(kind, fret as u16, lhs as f64, frhs as u16));
                        self.gen_binop_float_ir(kind, fret, lhs, frhs);
                    } else {
                        ctx.read_slot(self, cc, rhs);
                        ctx.dealloc_xmm(ret);
                        cc.push(TIr::BinOpIr(kind, ret, lhs, rhs, pc.classid1()));
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_binop_kind(ctx.get_xmm_using(), pc, kind, ret);
                    }
                }

                BcOp::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(pc);
                    } else {
                        if pc.is_binary_float() {
                            let (flhs, frhs) = ctx.xmm_read_binary(self, cc, lhs, rhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                            cc.push(TIr::FCmp(kind, ret, flhs as u16, frhs as u16, false));
                        } else {
                            let generic = self.jit.label();
                            ctx.read_slot(self, cc, lhs);
                            ctx.read_slot(self, cc, rhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmp_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, ctx.get_xmm_using());
                            cc.push(TIr::Cmp(kind, ret, lhs, rhs, optimizable, pc.classid1()));
                        }
                    }
                }
                BcOp::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(pc);
                    } else {
                        if pc.is_float1() {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = ctx.xmm_read_assume_float(self, cc, lhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                            cc.push(TIr::FCmpRf(kind, ret, flhs as u16, rhs as f64, false));
                        } else {
                            let generic = self.jit.label();
                            ctx.read_slot(self, cc, lhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmpri_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, ctx.get_xmm_using());
                            cc.push(TIr::Cmpri(kind, ret, lhs, rhs, optimizable, pc.classid1()));
                        }
                    }
                }
                BcOp::Mov(dst, src) => {
                    ctx.copy_slot(self, cc, src, dst);
                }
                BcOp::ConcatStr(ret, arg, len) => {
                    ctx.write_back_range(self, cc, arg, len);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    cc.push(TIr::ConcatStr(ret, arg, len, xmm_using.clone()));
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                BcOp::MethodCall(..) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(pc);
                }
                BcOp::MethodArgs(recv, args, len) => {
                    ctx.read_slot(self, cc, recv);
                    ctx.write_back_range(self, cc, args, len);

                    match std::mem::take(&mut self.opt_buf).unwrap().op1() {
                        BcOp::MethodCall(ret, name) => {
                            cc.push(TIr::MethodCall(ret, name, recv, args, len));
                            ctx.dealloc_xmm(ret);
                            self.jit_method_call(recv, name, ret, args, len, &ctx, pc + 2);
                        }
                        _ => unreachable!(),
                    }
                    skip = true;
                }
                BcOp::MethodDef(name, func) => {
                    cc.push(TIr::MethodDef(name, func));
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
                BcOp::Ret(lhs) => {
                    ctx.read_slot(self, cc, lhs);
                    cc.push(TIr::Ret(lhs));
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    let next_idx = cc.bb_pos + ofs + 1;
                    return Some(next_idx);
                }
                BcOp::Br(disp) => {
                    let next_idx = cc.bb_pos + ofs + 1;
                    let dest_idx = (next_idx as i64 + disp as i64) as usize;
                    let wb = ctx.get_write_back();
                    cc.push(TIr::Br(dest_idx, wb.clone()));

                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx, branch_dest);
                    monoasm!(self.jit,
                        jmp branch_dest;
                    );
                    return Some(next_idx);
                }
                BcOp::CondBr(cond_, disp, false, kind) => {
                    let wb = ctx.get_write_back();
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    cc.push(TIr::CondBr(cond_, dest_idx, kind, wb.clone()));
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!(self.jit, jne branch_dest;),
                        BrKind::BrIfNot => monoasm!(self.jit, jeq branch_dest;),
                    }
                }
                BcOp::CondBr(_, disp, true, brkind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let generic = self.jit.label();
                    let pc = std::mem::take(&mut self.opt_buf).unwrap();
                    if pc.is_binary_float() {
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, ret, lhs, rhs, true) => {
                                let (flhs, frhs) = ctx.xmm_read_binary(self, cc, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                cc.push(TIr::FCmp(kind, ret, flhs, frhs, true));
                                kind
                            }
                            BcOp::Cmpri(kind, ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = ctx.xmm_read_assume_float(self, cc, lhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                                };
                                cc.push(TIr::FCmpRf(kind, ret, flhs, rhs as f64, true));
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let wb = ctx.get_write_back_if_another_bb(cc, ofs, disp);
                        let xmm_using = ctx.get_xmm_using();
                        cc.push(TIr::CondBrOpt(
                            ((cc.bb_pos + ofs + 1) as i32 + disp) as usize,
                            kind,
                            brkind,
                            wb.clone(),
                            xmm_using.clone(),
                        ));
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_float_opt(kind, branch_dest, generic, brkind, xmm_using);
                    } else {
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, ret, lhs, rhs, true) => {
                                ctx.read_slot(self, cc, lhs);
                                ctx.read_slot(self, cc, rhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmp_prep(lhs, rhs, generic);
                                cc.push(TIr::Cmp(kind, ret, lhs, rhs, true, pc.classid1()));
                                kind
                            }
                            BcOp::Cmpri(kind, ret, lhs, rhs, true) => {
                                ctx.read_slot(self, cc, lhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmpri_prep(lhs, rhs, generic);
                                cc.push(TIr::Cmpri(kind, ret, lhs, rhs, true, pc.classid1()));
                                kind
                            }
                            _ => unreachable!(),
                        };

                        let wb = ctx.get_write_back_if_another_bb(cc, ofs, disp);
                        let xmm_using = ctx.get_xmm_using();
                        cc.push(TIr::CondBrOpt(
                            ((cc.bb_pos + ofs + 1) as i32 + disp) as usize,
                            kind,
                            brkind,
                            wb.clone(),
                            xmm_using.clone(),
                        ));
                        monoasm! { self.jit,
                            cmpq rdi, rsi;
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_int_opt(kind, branch_dest, generic, brkind, xmm_using);
                    }
                }
            }

            let next_idx = cc.bb_pos + ofs + 1;
            if let Some(_) = cc.bb_info[next_idx] {
                let wb = ctx.get_write_back();
                cc.push(TIr::Br(cc.bb_pos + ofs + 1, wb));
                let branch_dest = self.jit.label();
                cc.new_branch(cc.bb_pos + ofs, next_idx, ctx.clone(), branch_dest);
                monoasm!(self.jit,
                    jmp branch_dest;
                );
                return Some(next_idx);
            }
        }
        None
    }
}
