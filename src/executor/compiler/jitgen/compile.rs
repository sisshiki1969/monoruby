use super::analysis::*;
use super::*;

impl Codegen {
    fn gen_merging_branches_loop(
        &mut self,
        func: &RubyFuncInfo,
        cc: &mut CompileContext,
        bb_pos: usize,
    ) -> BBContext {
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let pc = func.get_pc(bb_pos);
            #[cfg(feature = "emit-tir")]
            eprintln!("gen_merge bb(loop): {bb_pos}");
            let (backedge_info, unused) = LoopAnalysis::analyse(func, cc.bb_pos);
            let use_set = backedge_info.get_loop_used_as_float();
            let cur_label = cc.labels[&bb_pos];

            #[cfg(feature = "emit-tir")]
            {
                eprintln!("  use set:  {:?}", use_set);
                eprintln!("  not used: {:?}", unused);
            }

            let target_slot_info = StackSlotInfo::merge_entries(&entries);
            let mut ctx = BBContext::new(func.total_reg_num());
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
                self.jit.select_page(1);
                self.jit.bind_label(dest_label);
                self.gen_write_back_for_target(bbctx, &ctx, pc + 1);
                monoasm!(self.jit,
                    jmp cur_label;
                );
                self.jit.select_page(0);
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
        func: &RubyFuncInfo,
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
            let target_ctx = BBContext::from(&target_slot_info);
            for BranchEntry {
                src_idx: _src_idx,
                bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "emit-tir")]
                eprintln!("  write_back {_src_idx}->{bb_pos}",);
                self.jit.select_page(1);
                self.jit.bind_label(dest_label);
                self.gen_write_back_for_target(bbctx, &target_ctx, pc);
                monoasm!(self.jit,
                    jmp cur_label;
                );
                self.jit.select_page(0);
            }

            #[cfg(feature = "emit-tir")]
            eprintln!("merge_end");

            target_ctx
        } else {
            unreachable!()
        }
    }

    pub(super) fn gen_backedge_branch(
        &mut self,
        cc: &mut CompileContext,
        func: &RubyFuncInfo,
        bb_pos: usize,
    ) {
        if let Some(entries) = cc.branch_map.remove(&bb_pos) {
            let (target_label, target_slot_info, unused) = cc.get_backedge(bb_pos);
            let target_ctx = BBContext::from(&target_slot_info);
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
                self.jit.select_page(1);
                self.jit.bind_label(dest_label);
                self.gen_write_back_for_target(bbctx, &target_ctx, pc);
                monoasm!(self.jit,
                    jmp target_label;
                );
                self.jit.select_page(0);
            }
        }
    }

    pub(super) fn compile_bb(
        &mut self,
        globals: &Globals,
        func: &RubyFuncInfo,
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
                              movq rax, (Value::dup);
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
                    ctx.read_slot(self, base);
                    ctx.read_slot(self, idx);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.jit_get_index(ret, base, idx, pc, xmm_using);
                }
                BcOp::IndexAssign(src, base, idx) => {
                    ctx.read_slot(self, base);
                    ctx.read_slot(self, idx);
                    ctx.read_slot(self, src);
                    let xmm_using = ctx.get_xmm_using();
                    self.jit_index_assign(src, base, idx, pc, xmm_using);
                }
                BcOp::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    let xmm_using = ctx.get_xmm_using();

                    if pc.value().is_none() || pc.value().unwrap().class_id() != FLOAT_CLASS {
                        self.load_constant(dst, id, pc, xmm_using);
                    } else {
                        let wb = ctx.get_write_back();
                        let fdst = ctx.alloc_xmm_read(dst);
                        self.load_float_constant(dst, fdst, id, pc, xmm_using, wb);
                    }
                }
                BcOp::StoreConst(src, id) => {
                    let xmm_using = ctx.get_xmm_using();
                    ctx.read_slot(self, src);
                    self.jit_store_constant(id, src, xmm_using);
                }
                BcOp::LoadIvar(ret, id) => {
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.jit_load_ivar(id, ret, xmm_using);
                }
                BcOp::StoreIvar(src, id) => {
                    ctx.read_slot(self, src);
                    let xmm_using = ctx.get_xmm_using();
                    self.jit_store_ivar(id, src, xmm_using);
                }
                BcOp::Nil(ret) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = ctx.xmm_read_assume_float(self, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.read_slot(self, src);
                        ctx.dealloc_xmm(dst);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.store_rax(dst);
                    }
                }
                BcOp::BinOp(kind, ret, lhs, rhs) => {
                    if pc.is_binary_integer() {
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::RR(lhs, rhs),
                            ctx.get_write_back(),
                            ctx.get_xmm_using(),
                        );
                    } else if pc.is_binary_float() {
                        let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float(kind, fret, flhs, frhs);
                    } else {
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.load_binary_args(lhs, rhs);
                        self.gen_binop_kind(ctx.get_xmm_using(), pc, kind, ret);
                    }
                }

                BcOp::BinOpRi(kind, ret, lhs, rhs) => {
                    let wb = ctx.get_write_back();
                    let xmm_using = ctx.get_xmm_using();
                    if pc.is_integer1() {
                        ctx.read_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::RI(lhs, rhs),
                            wb,
                            xmm_using,
                        );
                    } else if pc.is_float1() {
                        let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ri(kind, fret, flhs, rhs);
                    } else {
                        ctx.read_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
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
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::IR(lhs, rhs),
                            wb,
                            xmm_using,
                        );
                    } else if pc.is_float2() {
                        let frhs = ctx.xmm_read_assume_float(self, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ir(kind, fret, lhs, frhs);
                    } else {
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
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
                    } else if pc.is_binary_float() {
                        let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
                        ctx.dealloc_xmm(ret);
                        monoasm! { self.jit,
                            xorq rax, rax;
                            ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                        };
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else {
                        let generic = self.jit.label();
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmp_prep(lhs, rhs, generic);
                        self.gen_cmp_kind(kind, generic, ret, ctx.get_xmm_using());
                    }
                }
                BcOp::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(pc);
                    } else if pc.is_float1() {
                        let rhs_label = self.jit.const_f64(rhs as f64);
                        let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
                        ctx.dealloc_xmm(ret);
                        monoasm! { self.jit,
                            xorq rax, rax;
                            ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                        };
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else {
                        let generic = self.jit.label();
                        ctx.read_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmpri_prep(lhs, rhs, generic);
                        self.gen_cmp_kind(kind, generic, ret, ctx.get_xmm_using());
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
                BcOp::MethodCall(..) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(pc);
                }
                BcOp::MethodArgs(recv, args, len) => {
                    ctx.read_slot(self, recv);
                    ctx.write_back_range(self, args, len);

                    match std::mem::take(&mut self.opt_buf).unwrap().op1() {
                        BcOp::MethodCall(ret, name) => {
                            ctx.dealloc_xmm(ret);
                            let callee_codeptr = pc.codeptr();
                            if let Some(id) = callee_codeptr {
                                let meta = (pc + 1).meta();
                                let pc = (pc + 1).pc();
                                /*eprintln!(
                                    "{:?} {:?} {:?}",
                                    globals.func[meta.func_id()].kind,
                                    id,
                                    pc
                                );*/
                                match globals.func[meta.func_id()].kind {
                                    FuncKind::AttrReader { ivar_name } => {
                                        assert_eq!(0, len);
                                        let xmm_using = ctx.get_xmm_using();
                                        self.xmm_save(&xmm_using);
                                        monoasm!(self.jit,
                                            movq rdi, [rbp - (conv(recv))];  // recv: Value
                                            movq rsi, (ivar_name.get()); // name: IdentId
                                            movq rax, (get_instance_var);
                                            call rax;
                                        );
                                        if !ret.is_zero() {
                                            self.store_rax(ret);
                                        }
                                        self.xmm_restore(&xmm_using);
                                    }
                                    FuncKind::AttrWriter { ivar_name } => {
                                        assert_eq!(1, len);
                                        let xmm_using = ctx.get_xmm_using();
                                        self.xmm_save(&xmm_using);
                                        monoasm!(self.jit,
                                            movq rdi, [rbp - (conv(recv))];  // recv: Value
                                            movq rsi, (ivar_name.get()); // name: IdentId
                                            movq rdx, [rbp - (conv(args))];  //val: Value
                                            movq rax, (set_instance_var);
                                            call rax;
                                        );
                                        if !ret.is_zero() {
                                            self.store_rax(args);
                                        }
                                        self.xmm_restore(&xmm_using);
                                    }
                                    _ => self.jit_method_call(
                                        recv,
                                        name,
                                        ret,
                                        args,
                                        len,
                                        &ctx,
                                        pc + 2,
                                    ),
                                };
                            } else {
                                self.jit_method_call(recv, name, ret, args, len, &ctx, pc + 2);
                            }
                        }
                        _ => unreachable!(),
                    }
                    skip = true;
                }
                BcOp::MethodDef(name, func) => {
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
                BcOp::ClassDef(_ret, _name, _func) => {
                    let wb = ctx.get_write_back();
                    let side_exit = self.gen_side_deopt_dest(pc, wb);
                    monoasm!(self.jit,
                        jmp side_exit;
                    );
                }
                BcOp::Ret(lhs) => {
                    ctx.read_slot(self, lhs);
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
                    let pc = std::mem::take(&mut self.opt_buf).unwrap();
                    if pc.is_binary_float() {
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, _ret, lhs, rhs, true) => {
                                let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                kind
                            }
                            BcOp::Cmpri(kind, _ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
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
                                ctx.read_slot(self, lhs);
                                ctx.read_slot(self, rhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmp_prep(lhs, rhs, generic);
                                kind
                            }
                            BcOp::Cmpri(kind, ret, lhs, rhs, true) => {
                                ctx.read_slot(self, lhs);
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
            if let Some(_) = cc.bb_info[next_idx] {
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
}
