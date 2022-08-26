use super::*;

impl Codegen {
    fn gen_merging_branches(
        &mut self,
        func: &NormalFuncInfo,
        cc: &mut CompileContext,
        pos: usize,
        is_loop: bool,
    ) -> BBContext {
        if let Some(mut entries) = cc.branch_map.remove(&pos) {
            let regnum = func.total_reg_num();
            if is_loop {
                #[cfg(feature = "log-jit")]
                eprintln!("bb(loop): {pos}");
                let cur_label = cc.labels[pos - cc.start_pos];
                for BranchEntry {
                    src_idx,
                    bbctx,
                    dest_label,
                } in entries
                {
                    #[cfg(feature = "log-jit")]
                    eprintln!(
                        "write_back_all from:{src_idx} to:{pos} {:?}",
                        bbctx.stack_slot
                    );
                    let wb = bbctx.get_write_back();
                    self.jit.select(1);
                    self.jit.bind_label(dest_label);
                    self.gen_write_back(wb);
                    monoasm!(self.jit,
                        jmp cur_label;
                    );
                    self.jit.select(0);
                }
                BBContext::new(func.total_reg_num())
            } else {
                #[cfg(feature = "log-jit")]
                {
                    eprintln!("bb: {pos}");
                    eprintln!("scan: {:?}", ScanContext::scan_non_loop(func, pos));
                }
                if entries.len() == 1 {
                    let entry = entries.remove(0);
                    self.jit.bind_label(entry.dest_label);
                    return entry.bbctx;
                }

                let cur_label = cc.labels[pos - cc.start_pos];
                for BranchEntry {
                    src_idx,
                    bbctx,
                    dest_label,
                } in entries
                {
                    #[cfg(feature = "log-jit")]
                    eprintln!(
                        "write_back_all from:{src_idx} to:{pos} {:?}",
                        bbctx.stack_slot
                    );
                    let wb = bbctx.get_write_back();
                    self.jit.select(1);
                    self.jit.bind_label(dest_label);
                    self.gen_write_back(wb);
                    monoasm!(self.jit,
                        jmp cur_label;
                    );
                    self.jit.select(0);
                }
                BBContext::new(func.total_reg_num())
            }
        } else {
            #[cfg(feature = "log-jit")]
            eprintln!("orphan bb: {pos}");
            BBContext::new(func.total_reg_num())
        }
    }

    pub(super) fn gen_backedge_branch(&mut self, cc: &mut CompileContext, pos: usize) {
        if let Some(entries) = cc.branch_map.remove(&pos) {
            let cur_label = cc.labels[pos - cc.start_pos];
            for BranchEntry {
                src_idx,
                bbctx,
                dest_label,
            } in entries
            {
                #[cfg(feature = "log-jit")]
                eprintln!(
                    "backedge_write_back_all from:{src_idx} to:{pos} {:?}",
                    bbctx.stack_slot
                );
                let wb = bbctx.get_write_back();
                self.jit.select(1);
                self.jit.bind_label(dest_label);
                self.gen_write_back(wb);
                monoasm!(self.jit,
                    jmp cur_label;
                );
                self.jit.select(0);
            }
        }
    }

    pub(super) fn compile_bb(&mut self, func: &NormalFuncInfo, cc: &mut CompileContext) -> bool {
        let mut skip = false;
        let is_loop = matches!(
            BcOp::from_bc(&func.bytecode()[cc.bb_pos]),
            BcOp::LoopStart(_)
        );
        let mut ctx = self.gen_merging_branches(func, cc, cc.bb_pos, is_loop);
        for (ofs, pc) in func.bytecode()[cc.bb_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            match pc.op1() {
                BcOp::CondBr(_, _, true, _) => {}
                _ => {
                    self.jit.bind_label(cc.label(ofs, 0));
                }
            }
            match pc.op1() {
                BcOp::LoopStart(_) => {
                    let use_set = ScanContext::scan_loop(func, cc.bb_pos + ofs);
                    //eprintln!("{:?}", use_set);
                    for (reg, class) in use_set {
                        match class {
                            FLOAT_CLASS => {
                                ctx.xmm_read_assume_float(self, reg, pc);
                            }
                            INTEGER_CLASS => {
                                ctx.xmm_read_assume_integer(self, reg, pc);
                            }
                            _ => {
                                ctx.xmm_read_without_assumption(self, reg, pc);
                            }
                        }
                    }
                    cc.loop_count += 1;
                }
                BcOp::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.bb_pos + ofs);
                        let wb = ctx.get_write_back();
                        self.deopt(pc, wb);
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
                    let wb = ctx.get_write_back();
                    let xmm_using = ctx.get_xmm_using();
                    if pc.is_binary_integer() {
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(
                            pc,
                            kind,
                            ret,
                            BinOpMode::RR(lhs, rhs),
                            wb,
                            xmm_using,
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
                            self.jit_method_call(recv, name, ret, args, len, &ctx, pc + 2);
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
                    let generic = self.jit.label();
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
                        let xmm_using = ctx.get_xmm_using();
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_float_opt(kind, branch_dest, generic, brkind, xmm_using);
                    } else {
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum XmmLink {
    ND,
    Linked,
    NotLinked,
}

impl XmmLink {
    fn merge(&mut self, other: &Self) {
        *self = match (*self, *other) {
            (XmmLink::Linked, XmmLink::Linked) => XmmLink::Linked,
            (XmmLink::NotLinked, _) => XmmLink::NotLinked,
            (_, XmmLink::NotLinked) => XmmLink::NotLinked,
            (XmmLink::ND, r) => r,
            (l, XmmLink::ND) => l,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum IsUsed {
    ND,
    Used(bool, ClassId),
    NotUsed,
}

impl IsUsed {
    fn merge(&mut self, other: &Self) {
        *self = match (*self, *other) {
            (IsUsed::Used(..), _) => *self,
            (_, IsUsed::Used(..)) => *other,
            (IsUsed::ND, r) => r,
            (l, IsUsed::ND) => l,
            (IsUsed::NotUsed, IsUsed::NotUsed) => IsUsed::NotUsed,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum RegClass {
    ND,
    Any,
    Class(ClassId),
}

impl RegClass {
    fn merge(&mut self, other: &Self) {
        *self = match (*self, *other) {
            (RegClass::ND, r) => r,
            (l, RegClass::ND) => l,
            (RegClass::Any, _) => RegClass::Any,
            (_, RegClass::Any) => RegClass::Any,
            (RegClass::Class(l), RegClass::Class(r)) => {
                if l == r {
                    RegClass::Class(l)
                } else {
                    RegClass::Any
                }
            }
        };
    }
}

#[derive(Debug, Clone)]
struct RegDetail {
    xmm_link: XmmLink,
    is_used: IsUsed,
    class: RegClass,
}

impl RegDetail {
    fn new() -> Self {
        Self {
            xmm_link: XmmLink::ND,
            is_used: IsUsed::ND,
            class: RegClass::ND,
        }
    }

    fn is_float(&self) -> bool {
        self.xmm_link == XmmLink::Linked
    }
}

#[derive(Debug, Clone)]
struct RegInfo {
    info: Vec<RegDetail>,
}

impl RegInfo {
    fn get_loop_used_as_float(&self) -> Vec<(SlotId, ClassId)> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, b)| match (b.xmm_link, b.is_used) {
                (XmmLink::Linked, IsUsed::Used(true, class)) => Some((SlotId(i as u16), class)),
                _ => None,
            })
            .collect()
    }

    fn get_used_as_float(&self) -> Vec<(SlotId, ClassId)> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, b)| match b.is_used {
                IsUsed::Used(true, class) => Some((SlotId(i as u16), class)),
                _ => None,
            })
            .collect()
    }
}

impl RegInfo {
    fn new(reg_num: usize) -> Self {
        Self {
            info: vec![RegDetail::new(); reg_num],
        }
    }

    fn merge(&mut self, other: &Self) {
        for (i, detail) in &mut self.info.iter_mut().enumerate() {
            detail.xmm_link.merge(&other[i].xmm_link);
            detail.is_used.merge(&other[i].is_used);
            detail.class.merge(&other[i].class);
        }
    }

    fn use_as(&mut self, slot: SlotId, is_float: bool, class: ClassId) {
        self[slot].xmm_link = if is_float {
            XmmLink::Linked
        } else {
            XmmLink::NotLinked
        };
        self[slot].class = RegClass::Class(class);
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::Used(is_float, class);
        }
    }

    fn def_as(&mut self, slot: SlotId, is_float: bool) {
        self[slot].xmm_link = if is_float {
            XmmLink::Linked
        } else {
            XmmLink::NotLinked
        };
        self[slot].class = if is_float {
            RegClass::Class(FLOAT_CLASS)
        } else {
            RegClass::Any
        };
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::NotUsed;
        }
    }

    fn copy(&mut self, dst: SlotId, src: SlotId) {
        let mut is_used = self[dst].is_used;
        let class = self[src].class;
        is_used.merge(&self[src].is_used);
        self[dst] = RegDetail {
            xmm_link: self[src].xmm_link,
            is_used,
            class,
        };
    }
}

impl std::ops::Index<SlotId> for RegInfo {
    type Output = RegDetail;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.info[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for RegInfo {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.info[i.0 as usize]
    }
}

impl std::ops::Index<usize> for RegInfo {
    type Output = RegDetail;
    fn index(&self, i: usize) -> &Self::Output {
        &self.info[i]
    }
}

impl std::ops::IndexMut<usize> for RegInfo {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.info[i]
    }
}

struct ScanContext {
    /// key: dest_idx, value Vec<(src_idx, reginfo)>
    branch_map: HashMap<usize, Vec<(usize, RegInfo)>>,
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    loop_level: usize,
    back_info: RegInfo,
    pc: BcPc,
}

impl ScanContext {
    fn new(func: &NormalFuncInfo) -> Self {
        Self {
            branch_map: HashMap::default(),
            bb_info: func.get_bb_info(),
            loop_level: 0,
            back_info: RegInfo::new(func.total_reg_num()),
            pc: BcPc::default(),
        }
    }

    fn add_branch(&mut self, src_idx: usize, reg_info: RegInfo, dest_idx: usize) {
        match self.branch_map.get_mut(&dest_idx) {
            Some(entry) => {
                entry.push((src_idx, reg_info));
            }
            None => {
                self.branch_map.insert(dest_idx, vec![(src_idx, reg_info)]);
            }
        }
    }

    fn get_branches(&self, dest_idx: usize) -> Vec<(usize, RegInfo)> {
        match self.branch_map.get(&dest_idx) {
            Some(v) => v.clone(),
            None => vec![],
        }
    }
}

impl ScanContext {
    fn scan_loop(func: &NormalFuncInfo, bb_pos: usize) -> Vec<(SlotId, ClassId)> {
        let mut ctx = ScanContext::new(func);
        let regnum = func.total_reg_num();
        ctx.add_branch(0, RegInfo::new(regnum), bb_pos);
        let bb_start_vec: Vec<usize> = ctx
            .bb_info
            .iter()
            .enumerate()
            .flat_map(|(idx, info)| match info {
                Some(_) => {
                    if idx >= bb_pos {
                        Some(idx)
                    } else {
                        None
                    }
                }
                None => None,
            })
            .collect();
        for bb_pos in bb_start_vec {
            let branches = ctx.get_branches(bb_pos);
            let reg_info0 = RegInfo::new(regnum);
            let reg_info = branches.into_iter().fold(reg_info0, |mut acc, (_, info)| {
                acc.merge(&info);
                acc
            });
            if ctx.scan_bb(func, reg_info, bb_pos, true) {
                break;
            };
        }
        //ctx.back_info.info.truncate(func.total_local_num());
        ctx.back_info.get_loop_used_as_float()
    }

    fn scan_non_loop(func: &NormalFuncInfo, bb_pos: usize) -> Vec<(SlotId, ClassId)> {
        let mut ctx = ScanContext::new(func);
        let regnum = func.total_reg_num();
        ctx.add_branch(0, RegInfo::new(regnum), bb_pos);
        let bb_start_vec: Vec<usize> = ctx
            .bb_info
            .iter()
            .enumerate()
            .flat_map(|(idx, info)| match info {
                Some(_) => {
                    if idx >= bb_pos {
                        Some(idx)
                    } else {
                        None
                    }
                }
                None => None,
            })
            .collect();
        for bb_pos in bb_start_vec {
            let branches = ctx.get_branches(bb_pos);
            let reg_info0 = RegInfo::new(regnum);
            let reg_info = branches.into_iter().fold(reg_info0, |mut acc, (_, info)| {
                acc.merge(&info);
                acc
            });
            //eprintln!("{} {:?}", bb_pos, reg_info.get_used_as_float());
            if ctx.scan_bb(func, reg_info, bb_pos, false) {
                break;
            };
        }
        let used = ctx.back_info.get_used_as_float();
        used
    }

    fn scan_bb(
        &mut self,
        func: &NormalFuncInfo,
        mut reg_info: RegInfo,
        bb_pos: usize,
        loop_mode: bool,
    ) -> bool {
        let mut skip = false;
        let mut method_buf = None;
        for (ofs, pc) in func.bytecode()[bb_pos..].iter().enumerate() {
            self.pc = BcPc::from(pc);
            let idx = bb_pos + ofs;
            if skip {
                skip = false;
                continue;
            }

            match BcOp::from_bc(pc) {
                BcOp::LoopStart(_) => {
                    self.loop_level += 1;
                }
                BcOp::LoopEnd => {
                    self.loop_level -= 1;
                    if loop_mode && self.loop_level == 0 {
                        return true;
                    }
                }
                BcOp::Integer(ret, _val) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Symbol(ret, _id) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Array(ret, _src, _len) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Index(ret, _base, _idx) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Nil(ret) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Literal(dst, val) => {
                    reg_info.def_as(dst, val.class_id() == FLOAT_CLASS);
                }
                BcOp::IndexAssign(_src, _base, _idx) => {}
                BcOp::LoadConst(dst, _const_id) => {
                    let is_float =
                        pc.value().is_some() && pc.value().unwrap().class_id() == FLOAT_CLASS;
                    reg_info.def_as(dst, is_float);
                }
                BcOp::StoreConst(_dst, _id) => {}
                BcOp::Neg(dst, src) => {
                    let is_float = pc.is_float1();
                    reg_info.def_as(dst, is_float);
                    reg_info.use_as(src, is_float, pc.classid1());
                }
                BcOp::BinOp(_kind, dst, lhs, rhs) => {
                    let is_float = pc.is_binary_float();
                    reg_info.def_as(dst, is_float);
                    reg_info.use_as(lhs, is_float, pc.classid1());
                    reg_info.use_as(rhs, is_float, pc.classid2());
                }
                BcOp::BinOpRi(_kind, dst, lhs, _rhs) => {
                    let is_float = pc.is_float1();
                    reg_info.def_as(dst, is_float);
                    reg_info.use_as(lhs, is_float, pc.classid1());
                }
                BcOp::BinOpIr(_kind, dst, _lhs, rhs) => {
                    let is_float = pc.is_float2();
                    reg_info.def_as(dst, is_float);
                    reg_info.use_as(rhs, is_float, pc.classid2());
                }
                BcOp::Cmp(_kind, dst, lhs, rhs, _opt) => {
                    let is_float = pc.is_binary_float();
                    reg_info.def_as(dst, false);
                    reg_info.use_as(lhs, is_float, pc.classid1());
                    reg_info.use_as(rhs, is_float, pc.classid2());
                }
                BcOp::Cmpri(_kind, dst, lhs, _rhs, _opt) => {
                    let is_float = pc.is_float1();
                    reg_info.def_as(dst, false);
                    reg_info.use_as(lhs, is_float, pc.classid1());
                }
                BcOp::Mov(dst, src) => {
                    reg_info.copy(dst, src);
                }
                BcOp::ConcatStr(dst, arg, len) => {
                    reg_info.def_as(dst, false);
                    for r in arg.0..arg.0 + len {
                        reg_info.use_as(SlotId(r), false, STRING_CLASS);
                    }
                }
                BcOp::MethodCall(ret, id) => {
                    assert!(method_buf.is_none());
                    method_buf = Some((ret, pc.classid1(), id));
                }
                BcOp::MethodArgs(recv, _args, _len) => {
                    match std::mem::take(&mut method_buf) {
                        Some((ret, class, _id)) => {
                            reg_info.def_as(ret, false);
                            reg_info.use_as(recv, class == FLOAT_CLASS, class);
                        }
                        None => unreachable!(),
                    }
                    skip = true;
                }
                BcOp::MethodDef(_id, _func_id) => {}
                BcOp::Ret(_ret) => {
                    if !loop_mode {
                        self.back_info.merge(&reg_info);
                    }
                    return false;
                }
                BcOp::Br(disp) => {
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
                        //eprintln!("{}->{} {:?}", idx, dest_idx, reg_info.get_used_as_float());
                        self.add_branch(idx, reg_info, dest_idx);
                    } else {
                        self.back_info.merge(&reg_info);
                    }
                    return false;
                }
                BcOp::CondBr(cond_, disp, _opt, _brkind) => {
                    reg_info.use_as(cond_, false, TRUE_CLASS);
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
                        //eprintln!("{}->{} {:?}", idx, dest_idx, reg_info.get_used_as_float());
                        self.add_branch(idx, reg_info.clone(), dest_idx);
                    } else {
                        self.back_info.merge(&reg_info);
                    }
                }
            }

            let next_idx = idx + 1;
            if self.bb_info[next_idx].is_some() {
                self.add_branch(idx, reg_info, next_idx);
                return false;
            }
        }
        if !loop_mode {
            self.back_info.merge(&reg_info);
        }
        true
    }
}
