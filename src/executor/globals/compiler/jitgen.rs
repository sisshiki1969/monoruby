use monoasm_macro::monoasm;

use super::*;

mod analysis;
mod binary_op;
mod compile;
mod constants;
mod method_call;

//
// Just-in-time compiler module.
//

type UsingXmm = Vec<usize>;

#[derive(PartialEq)]
enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _interp: &mut Executor,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Value,
) {
    let name = match globals.func[func_id].as_ruby_func().name() {
        Some(name) => name.to_string(),
        None => "<unnamed>".to_string(),
    };
    let bc_begin = globals.func[func_id].as_ruby_func().get_bytecode_address(0);
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let BcOp::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else if let BcOp::ClassDef { .. } = pc.op1() {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {} caused by {:?}", index, fmt, v);
    }
}

impl Codegen {
    pub(super) fn compile_bb(
        &mut self,
        fnstore: &FnStore,
        func: &ISeqInfo,
        cc: &mut CompileContext,
    ) -> bool {
        let mut skip = false;
        let is_loop = matches!(func.get_pc(cc.bb_pos).op1(), BcOp::LoopStart(_));
        self.jit.bind_label(cc.labels[&cc.bb_pos]);
        let mut ctx = if is_loop {
            self.gen_merging_branches_loop(func, cc, cc.bb_pos)
        } else {
            self.gen_merging_branches(func, cc, cc.bb_pos)
        };
        for (ofs, pc) in func.bytecode()[cc.bb_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            #[cfg(feature = "emit-asm")]
            cc.sourcemap
                .push((cc.bb_pos + ofs, self.jit.get_current() - cc.start_codepos));
            match pc.op1() {
                BcOp::LoopStart(_) => {
                    cc.loop_count += 1;
                }
                BcOp::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.bb_pos + ofs);
                        self.deopt(&ctx, pc);
                        break;
                    }
                }
                BcOp::Integer(ret, i) => {
                    ctx.dealloc_xmm(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp::Symbol(ret, id) => {
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
                        let imm = self.jit.const_f64(f);
                        monoasm!(self.jit,
                            movq xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        if val.is_packed_value() {
                            monoasm!(self.jit,
                                movq rax, (val.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            monoasm!(self.jit,
                              movq rdi, (val.get());
                              movq rax, (Value::deep_copy);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(dst);
                    }
                }
                BcOp::Array(ret, src, len) => {
                    ctx.write_back_range(self, src, len);
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        lea  rdi, [rbp - (conv(src))];
                        movq rsi, (len);
                        movq rax, (gen_array);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                BcOp::Index(ret, base, idx) => {
                    ctx.write_back_slot(self, base);
                    ctx.write_back_slot(self, idx);
                    ctx.dealloc_xmm(ret);
                    self.jit_get_index(ret, base, idx, pc, &ctx);
                }
                BcOp::IndexAssign(src, base, idx) => {
                    ctx.write_back_slot(self, base);
                    ctx.write_back_slot(self, idx);
                    ctx.write_back_slot(self, src);
                    self.jit_index_assign(src, base, idx, pc, &ctx);
                }
                BcOp::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    self.jit_load_constant(&mut ctx, dst, id, pc);
                }
                BcOp::StoreConst(src, id) => {
                    ctx.write_back_slot(self, src);
                    self.jit_store_constant(id, src, &ctx);
                }
                BcOp::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                    ctx.dealloc_xmm(ret);
                    self.jit_load_ivar(&ctx, id, ret, cached_class, cached_ivarid);
                }
                BcOp::StoreIvar(src, id, cached_class, cached_ivarid) => {
                    ctx.write_back_slot(self, src);
                    self.jit_store_ivar(&ctx, id, src, pc, cached_class, cached_ivarid);
                }
                BcOp::LoadDynVar(ret, src) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..src.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    monoasm!(self.jit,
                        lea  rax, [rax + (OFFSET_OUTER)];
                        movq rax, [rax - (conv(src.reg))];
                    );
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                BcOp::StoreDynVar(dst, src) => {
                    ctx.write_back_slot(self, src);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..dst.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    monoasm!(self.jit,
                        lea  rax, [rax + (OFFSET_OUTER)];
                        movq rdi, [rbp - (conv(src))];
                        movq [rax - (conv(dst.reg))], rdi;
                    );
                }
                BcOp::Nil(ret) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = self.xmm_read_assume_float(&mut ctx, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.write_back_slot(self, src);
                        ctx.dealloc_xmm(dst);
                        let xmm_using = ctx.get_xmm_using();
                        self.xmm_save(&xmm_using);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.xmm_restore(&xmm_using);
                        self.handle_error(pc);
                        self.store_rax(dst);
                    }
                }
                BcOp::BinOp(kind, ret, lhs, rhs) => {
                    if pc.is_binary_integer() {
                        ctx.write_back_slot(self, lhs);
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::RR(lhs, rhs), &ctx);
                    } else if pc.is_binary_float() {
                        let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float(kind, &ctx, fret, flhs, frhs);
                    } else {
                        ctx.write_back_slot(self, lhs);
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.load_binary_args(lhs, rhs);
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::BinOpRi(kind, ret, lhs, rhs) => {
                    if pc.is_integer1() {
                        ctx.write_back_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::RI(lhs, rhs), &ctx);
                    } else if pc.is_float1() {
                        let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ri(kind, &ctx, fret, flhs, rhs);
                    } else {
                        ctx.write_back_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::BinOpIr(kind, ret, lhs, rhs) => {
                    if pc.is_integer2() {
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::IR(lhs, rhs), &ctx);
                    } else if pc.is_float2() {
                        let frhs = self.xmm_read_assume_float(&mut ctx, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ir(kind, &ctx, fret, lhs, frhs);
                    } else {
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if !optimizable {
                        if pc.is_binary_float() {
                            let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                        } else {
                            let generic = self.jit.label();
                            ctx.write_back_slot(self, lhs);
                            ctx.write_back_slot(self, rhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmp_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                        }
                    }
                }
                BcOp::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if !optimizable {
                        if pc.is_float1() {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                        } else {
                            let generic = self.jit.label();
                            ctx.write_back_slot(self, lhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmpri_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                        }
                    }
                }
                BcOp::Mov(dst, src) => {
                    ctx.copy_slot(self, src, dst);
                }
                BcOp::ConcatStr(ret, arg, len) => {
                    ctx.write_back_range(self, arg, len);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
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
                BcOp::MethodCall(..) => {}
                BcOp::MethodCallBlock(..) => {}
                BcOp::MethodArgs(method_info) => {
                    self.gen_method_call(fnstore, &mut ctx, method_info, pc);
                    skip = true;
                }
                BcOp::MethodDef(name, func) => {
                    let class_version = self.class_version;
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func)); // FuncId
                        movq rax, (define_method);
                        call rax;
                        addl [rip + class_version], 1;
                    );
                    self.xmm_restore(&xmm_using);
                }
                BcOp::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    self.jit_class_def(&ctx, ret, superclass, name, func_id);
                }
                BcOp::Ret(lhs) => {
                    ctx.write_back_slot(self, lhs);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    return false;
                }
                BcOp::Br(disp) => {
                    let next_idx = cc.bb_pos + ofs + 1;
                    let dest_idx = (next_idx as i64 + disp as i64) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx, branch_dest);
                    monoasm!(self.jit,
                        jmp branch_dest;
                    );
                    return false;
                }
                BcOp::CondBr(cond_, disp, false, kind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
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
                    let pc = pc - 1;
                    if pc.is_binary_float() {
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, _ret, lhs, rhs, true) => {
                                let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                kind
                            }
                            BcOp::Cmpri(kind, _ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                                };
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_float_opt(kind, branch_dest, brkind);
                    } else {
                        let generic = self.jit.label();
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, ret, lhs, rhs, true) => {
                                ctx.write_back_slot(self, lhs);
                                ctx.write_back_slot(self, rhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmp_prep(lhs, rhs, generic);
                                kind
                            }
                            BcOp::Cmpri(kind, ret, lhs, rhs, true) => {
                                ctx.write_back_slot(self, lhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmpri_prep(lhs, rhs, generic);
                                kind
                            }
                            _ => unreachable!(),
                        };

                        let xmm_using = ctx.get_xmm_using();
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
            if cc.bb_info[next_idx].is_some() {
                let branch_dest = self.jit.label();
                cc.new_branch(cc.bb_pos + ofs, next_idx, ctx.clone(), branch_dest);
                monoasm!(self.jit,
                    jmp branch_dest;
                );
                return false;
            }
        }
        true
    }

    pub(super) fn gen_backedge_branch(
        &mut self,
        cc: &mut CompileContext,
        func: &ISeqInfo,
        bb_pos: usize,
    ) {
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let (target_label, target_slot_info, unused) = cc.get_backedge(bb_pos);
            let target_ctx = BBContext::from(&target_slot_info, cc.self_class);
            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  backedge_write_back {_src_idx}->{bb_pos}");
                bbctx.remove_unused(&unused);
                let pc = func.get_pc(bb_pos);
                self.gen_write_back_for_target(bbctx, &target_ctx, dest_label, target_label, pc);
            }
        }
    }
}

impl Codegen {
    fn xmm_read_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    fn xmm_read_assume_float(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_float(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_integer(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (u16, u16) {
        if lhs != rhs {
            (
                self.xmm_read_assume(ctx, lhs, pc.classid1(), pc),
                self.xmm_read_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.xmm_read_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }
}

impl Codegen {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - rdi: Value
    ///
    /// ### registers destroyed
    ///
    /// - rax
    ///
    fn guard_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz side_exit;
                    testq rdi, 0b010;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (NIL_VALUE);
                    jnz side_exit;
                );
            }
            SYMBOL_CLASS => {
                monoasm!(self.jit,
                    cmpb rdi, (TAG_SYMBOL);
                    jnz side_exit;
                );
            }
            TRUE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (TRUE_VALUE);
                    jnz side_exit;
                );
            }
            FALSE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (FALSE_VALUE);
                    jnz side_exit;
                );
            }
            _ => self.guard_unpacked_class(class_id, side_exit),
        }
    }

    fn guard_unpacked_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        monoasm!(self.jit,
            testq rdi, 0b111;
            jnz side_exit;
            cmpl [rdi + 4], (class_id.0);
            jne side_exit;
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;

        for (class, value) in [
            (INTEGER_CLASS, Value::new_integer(-2558)),
            (INTEGER_CLASS, Value::new_integer(i64::MAX)),
            (INTEGER_CLASS, Value::new_integer(i64::MIN)),
            (FLOAT_CLASS, Value::new_float(1.44e-17)),
            (FLOAT_CLASS, Value::new_float(0.0)),
            (FLOAT_CLASS, Value::new_float(f64::MAX)),
            (FLOAT_CLASS, Value::new_float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
        ] {
            let entry_point = gen.jit.get_current_address();
            gen.guard_class(class, side_exit);
            monoasm!(gen.jit,
                xorq rax, rax;
                ret;
            );
            gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
    }
}

impl Codegen {
    fn handle_error(&mut self, pc: BcPc) {
        let jit_return = self.vm_return;
        monoasm!(self.jit,
            movq r13, ((pc + 1).get_u64());
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn gen_write_back(&mut self, wb: WriteBack) {
        for (freg, v) in wb {
            self.gen_write_back_single(freg, v);
        }
    }

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        entry: DestLabel,
        exit: DestLabel,
        pc: BcPc,
    ) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.stack_slot);
            eprintln!("      target: {:?}", target_ctx.stack_slot);
        }
        let len = src_ctx.stack_slot.0.len();

        self.jit.select_page(1);
        self.jit.bind_label(entry);
        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx.stack_slot[reg] == LinkMode::None {
                match src_ctx.stack_slot[reg] {
                    LinkMode::XmmRW(freg) => {
                        let v = src_ctx.xmm[freg as usize].clone();
                        for i in &v {
                            src_ctx.stack_slot[*i] = LinkMode::XmmR(freg);
                        }
                        src_ctx.dealloc_xmm(reg);
                        self.gen_write_back_single(freg, v);
                    }
                    LinkMode::XmmR(_) => {
                        src_ctx.dealloc_xmm(reg);
                    }
                    _ => {}
                }
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (LinkMode::XmmRW(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                    guard_list.push(reg);
                }
                (_, LinkMode::None) => {}
                (LinkMode::XmmRW(l), LinkMode::XmmR(r)) => {
                    self.gen_write_back_single(l, vec![reg]);
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmR(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::None, LinkMode::XmmR(r)) => {
                    src_ctx.link_r_xmm(reg, r);
                    conv_list.push((reg, r));
                }
                _ => unreachable!(),
            }
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      src_end:   {:?}", src_ctx.stack_slot);

        let side_exit = self.jit.label();
        for (reg, freg) in conv_list {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(reg))];
            );
            self.gen_val_to_f64(freg as u64 + 2, side_exit);
            #[cfg(feature = "emit-tir")]
            eprintln!("      conv: {:?}->{:?}", reg, freg);
        }
        for reg in guard_list {
            self.gen_assume_float(reg, side_exit);
        }
        monoasm!(self.jit,
            jmp exit;
        );
        self.jit.select_page(0);
        let side_label = self.gen_side_deopt_dest(pc + 1, &src_ctx);
        self.jit.select_page(1);
        monoasm!(self.jit,
        side_exit:
            jmp side_label;
        );
        self.jit.select_page(0);
    }

    fn gen_write_back_single(&mut self, freg: u16, v: Vec<SlotId>) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!(self.jit,
            movq xmm0, xmm(freg as u64 + 2);
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(reg);
        }
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    fn gen_side_deopt_dest(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let wb = ctx.get_write_back();
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let entry = self.jit.label();
        self.jit.bind_label(entry);
        if !wb.is_empty() {
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt");
            self.gen_write_back(wb);
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt end");
        }
        let fetch = self.vm_fetch;
        monoasm!(self.jit,
            movq r13, (pc.get_u64());
        );
        #[cfg(feature = "log-jit")]
        monoasm!(self.jit,
            movq r8, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - (OFFSET_META)];
            movq rcx, r13;
            movq rax, (log_deoptimize);
            call rax;
        );
        monoasm!(self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_deopt_dest(pc, ctx);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    pub(super) fn prologue(&mut self, regs: usize, args: usize) {
        let offset = (regs * 8 + OFFSET_SELF as usize + 15) & !0xf;
        let clear_len = regs - args;
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (offset);
        );
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], (NIL_VALUE);
                );
            }
        }
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            leave;
            ret;
        );
    }

    fn load_binary_args(&mut self, lhs: SlotId, rhs: SlotId) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
    }

    fn xmm_save(&mut self, xmm_using: &[usize]) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        monoasm!(self.jit,
            subq rsp, (sp_offset);
        );
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
                movq [rsp + (8 * i)], xmm(*freg as u64 + 2);
            );
        }
    }

    fn xmm_restore(&mut self, xmm_using: &[usize]) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
                movq xmm(*freg as u64 + 2), [rsp + (8 * i)];
            );
        }
        monoasm!(self.jit,
            addq rsp, (sp_offset);
        );
    }

    fn gen_merging_branches_loop(
        &mut self,
        func: &ISeqInfo,
        cc: &mut CompileContext,
        bb_pos: usize,
    ) -> BBContext {
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);
            #[cfg(feature = "emit-tir")]
            eprintln!("gen_merge bb(loop): {bb_pos}");
            let (use_set, unused) = analysis::LoopAnalysis::analyse(func, cc.bb_pos);
            let cur_label = cc.labels[&bb_pos];

            #[cfg(feature = "emit-tir")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
            }

            let target_slot_info = StackSlotInfo::merge_entries(&entries);
            let mut ctx = BBContext::new(func.total_reg_num(), cc.self_class);
            for (reg, class) in use_set {
                match target_slot_info[reg] {
                    LinkMode::None => {}
                    LinkMode::XmmRW(_) if class => {
                        let freg = ctx.alloc_xmm();
                        ctx.link_rw_xmm(reg, freg);
                    }
                    _ => {
                        let freg = ctx.alloc_xmm();
                        ctx.link_r_xmm(reg, freg);
                    }
                };
            }
            #[cfg(feature = "emit-tir")]
            eprintln!("  merged target:   {:?}", ctx.stack_slot);

            for BranchEntry {
                src_idx: _src_idx,
                mut bbctx,
                dest_label,
            } in entries
            {
                bbctx.remove_unused(&unused);
                #[cfg(feature = "emit-tir")]
                eprintln!("  write_back {_src_idx}->{bb_pos} {:?}", bbctx.stack_slot);
                self.gen_write_back_for_target(bbctx, &ctx, dest_label, cur_label, pc + 1);
            }

            cc.new_backedge(cc.bb_pos, cur_label, ctx.stack_slot.clone(), unused);
            #[cfg(feature = "emit-tir")]
            eprintln!("merge_end");
            ctx
        } else {
            unreachable!()
        }
    }

    fn gen_merging_branches(
        &mut self,
        func: &ISeqInfo,
        cc: &mut CompileContext,
        bb_pos: usize,
    ) -> BBContext {
        if let Some(mut entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);

            if entries.len() == 1 {
                let entry = entries.remove(0);
                #[cfg(feature = "emit-tir")]
                eprintln!("gen_merge bb: {bb_pos}<-{}", entry.src_idx);
                self.jit.bind_label(entry.dest_label);
                return entry.bbctx;
            }

            #[cfg(feature = "emit-tir")]
            eprintln!("gen_merge bb: {bb_pos}");

            let target_slot_info = StackSlotInfo::merge_entries(&entries);
            #[cfg(feature = "emit-tir")]
            eprintln!("  target: {:?}", target_slot_info);

            let cur_label = cc.labels[&bb_pos];
            let target_ctx = BBContext::from(&target_slot_info, cc.self_class);
            for BranchEntry {
                src_idx: _src_idx,
                bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  write_back {_src_idx}->{bb_pos}",);
                self.gen_write_back_for_target(bbctx, &target_ctx, dest_label, cur_label, pc);
            }

            #[cfg(feature = "emit-tir")]
            eprintln!("merge_end");

            target_ctx
        } else {
            unreachable!()
        }
    }
}
