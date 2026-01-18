use crate::jitgen::BasicBlockInfo;

use super::*;

struct IncomingBranches {
    branches: Vec<Vec<BcIndex>>,
}

impl std::ops::Index<BcIndex> for IncomingBranches {
    type Output = Vec<BcIndex>;
    fn index(&self, index: BcIndex) -> &Self::Output {
        &self.branches[index.to_usize()]
    }
}

impl std::ops::IndexMut<BcIndex> for IncomingBranches {
    fn index_mut(&mut self, index: BcIndex) -> &mut Self::Output {
        &mut self.branches[index.to_usize()]
    }
}

impl IncomingBranches {
    fn new(len: usize) -> Self {
        let branches = vec![vec![]; len + 1];
        Self { branches }
    }

    fn push(&mut self, src: BcIndex, dst: BcIndex) {
        if !self[dst].contains(&src) {
            self[dst].push(src);
        }
    }

    fn pop(&mut self) -> Option<Vec<BcIndex>> {
        self.branches.pop()
    }
}

impl<'a> BytecodeGen<'a> {
    pub(super) fn into_bytecode(mut self) -> Result<()> {
        let func_id = self.func_id;
        for (dst, (dst_sp, src)) in std::mem::take(&mut self.merge_info) {
            let dst_idx = self[dst];
            let dst_sp = dst_sp.unwrap();
            for MergeSourceInfo {
                idx: src_idx,
                sp: src_sp,
            } in src
            {
                if dst_sp != src_sp {
                    let name = self.iseq().name();
                    eprintln!(
                        "warning: sp mismatch: {name} {:?}:{:?} <- {:?}:{:?}",
                        dst_idx, dst_sp, src_idx, src_sp
                    );
                    panic!();
                }
            }
        }
        let temp_num = self.temp_num;
        let non_temp_num = self.labels.non_temp_num;

        self.ir_to_bc()?;

        for ExceptionEntry {
            range,
            rescue,
            ensure,
            err_reg,
        } in std::mem::take(&mut self.exception_table)
        {
            if rescue.is_none() && ensure.is_none() {
                continue;
            }
            let start = self[range.start];
            let end = self[range.end];
            let rescue = rescue.map(|l| self[l]);
            let ensure = ensure.map(|l| self[l]);
            let err_reg = err_reg.map(|reg| self.slot_id(&reg));
            self.iseq_mut()
                .exception_push(start..end, rescue, ensure, err_reg);
        }

        let sp: Vec<_> = std::mem::take(&mut self.sp)
            .into_iter()
            .map(|r| self.slot_id(&BcReg::from(r)))
            .collect();

        let info = self.iseq_mut();
        info.temp_num = temp_num;
        info.non_temp_num = non_temp_num;
        info.sp = sp;
        self.store.set_func_data(func_id);
        Ok(())
    }

    fn ir_to_bc(&mut self) -> Result<()> {
        let mut ops = vec![];
        let ir = std::mem::take(&mut self.ir);
        let mut incoming = IncomingBranches::new(ir.len());
        for (idx, (inst, loc)) in ir.iter().enumerate() {
            let idx = BcIndex::from(idx);
            let op = self.inst_to_bc(&mut incoming, inst.clone(), idx)?;
            ops.push(op);
            self.iseq_mut().sourcemap.push(*loc);
        }
        for (idx, (inst, _)) in ir.iter().enumerate() {
            let idx = BcIndex::from(idx);
            if !incoming[idx + 1].is_empty() && !inst.is_terminal() {
                incoming.push(idx, idx + 1);
            }
        }
        incoming.pop();

        self.iseq_mut().bb_info = BasicBlockInfo::new(incoming.branches, &BytecodeIr::new(ir));
        self.iseq_mut().set_bytecode(ops);

        Ok(())
    }

    fn inst_to_bc(
        &mut self,
        incoming: &mut IncomingBranches,
        inst: BytecodeInst,
        bc_pos: BcIndex,
    ) -> Result<Bytecode> {
        let bc = match inst {
            BytecodeInst::Br(dst) => {
                // 3
                let dst = self[dst];
                incoming.push(bc_pos, dst);
                let op1 = dst - bc_pos - 1;
                // terminal inst.
                Bytecode::from(enc_l(3, op1 as u32))
            }
            BytecodeInst::CondBr(reg, dst, optimizable, kind) => {
                // 4, 5, 12, 13
                let dst = self[dst];
                incoming.push(bc_pos, dst);
                incoming.push(bc_pos, bc_pos + 1);
                let op1 = self.slot_id(&reg);
                let op2 = dst - bc_pos - 1;
                let kind = kind as u16;
                let op = enc_wl(
                    if optimizable { 12 + kind } else { 4 + kind },
                    op1.0,
                    op2 as u32,
                );
                Bytecode::from(op)
            }
            BytecodeInst::NilBr(reg, dst) => {
                // 37
                let dst = self[dst];
                incoming.push(bc_pos, dst);
                incoming.push(bc_pos, bc_pos + 1);
                let op1 = self.slot_id(&reg);
                let op2 = dst - bc_pos - 1;
                let op = enc_wl(37, op1.0, op2 as u32);
                Bytecode::from(op)
            }
            BytecodeInst::CheckLocal(local, dst) => {
                // 20
                let op1 = self.slot_id(&local);
                let dst = self[dst];
                incoming.push(bc_pos, dst);
                incoming.push(bc_pos, bc_pos + 1);
                let op2 = dst - bc_pos - 1;
                Bytecode::from(enc_wl(20, op1.0, op2 as u32))
            }
            BytecodeInst::CheckKwRest(local) => {
                // 19
                let op1 = self.slot_id(&local);
                Bytecode::from(enc_wl(19, op1.0, 0))
            }
            BytecodeInst::OptCase {
                reg,
                min,
                max,
                table,
                labels,
            } => {
                // 36
                let else_ = labels[0];
                let else_ofs = u32::try_from(self[else_] - bc_pos - 1).unwrap();
                let mut branch_table = vec![else_ofs; (max - min + 1) as usize];
                for (i, label) in table {
                    branch_table[i as usize] = u32::try_from(self[label] - bc_pos - 1).unwrap();
                }
                for label in &labels {
                    incoming.push(bc_pos, self[*label]);
                }
                let offsets = labels
                    .iter()
                    .map(|label| u32::try_from(self[*label] - bc_pos - 1).unwrap())
                    .collect::<Vec<_>>();
                let id = self.store.new_optcase(min, max, branch_table, offsets);
                let op1 = self.slot_id(&reg);
                // terminal inst.
                Bytecode::from(enc_wl(36, op1.0, id.get()))
            }
            BytecodeInst::Ret(reg) => {
                // 80
                let op1 = self.slot_id(&reg);
                // terminal inst.
                Bytecode::from(enc_w(80, op1.0))
            }
            BytecodeInst::MethodRet(reg) => {
                // 81
                let op1 = self.slot_id(&reg);
                // terminal inst.
                Bytecode::from(enc_w(81, op1.0))
            }
            BytecodeInst::BlockBreak(reg) => {
                // 82
                let op1 = self.slot_id(&reg);
                // terminal inst.
                Bytecode::from(enc_w(82, op1.0))
            }
            BytecodeInst::Raise(reg) => {
                // 83
                let op1 = self.slot_id(&reg);
                // terminal inst.
                Bytecode::from(enc_w(83, op1.0))
            }
            BytecodeInst::LoopStart => {
                // 14
                Bytecode::from(enc_l(14, 0))
            }
            BytecodeInst::LoopEnd => {
                // 15
                incoming.push(bc_pos, bc_pos + 1);
                Bytecode::from(enc_l(15, 0))
            }
            BytecodeInst::SingletonMethodDef { obj, name, func_id } => {
                // 1
                let op1 = self.slot_id(&obj);
                Bytecode::from_with_func_name_id(enc_wl(1, op1.0, 0), Some(name), func_id)
            }
            BytecodeInst::MethodDef { name, func_id } => {
                // 2
                Bytecode::from_with_func_name_id(enc_l(2, 0), Some(name), func_id)
            }
            BytecodeInst::Lambda { dst, func_id } => {
                // 38
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(38, op1.0, func_id.get()))
            }
            BytecodeInst::Immediate(dst, val) => {
                // 6
                let op1 = self.slot_id(&dst);
                Bytecode::from_with_value(enc_wl(6, op1.0, 0), val)
            }
            BytecodeInst::Literal(reg, val) => {
                // 7
                let op1 = self.slot_id(&reg);
                Bytecode::from_with_value(enc_wl(7, op1.0, 0), val)
            }
            BytecodeInst::LoadConst {
                dst,
                base,
                toplevel,
                prefix,
                name,
            } => {
                // 10
                let op1 = self.slot_id(&dst);
                let base = base.map(|base| self.slot_id(&base));
                let op2 = self.store.new_constsite(base, name, prefix, toplevel);
                Bytecode::from(enc_wl(10, op1.0, op2.0))
            }
            BytecodeInst::StoreConst {
                src,
                toplevel,
                base: parent,
                prefix,
                name,
            } => {
                // 11
                let op1 = self.slot_id(&src);
                let base = parent.map(|base| self.slot_id(&base));
                let op2 = self.store.new_constsite(base, name, prefix, toplevel);
                Bytecode::from(enc_wl(11, op1.0, op2.0))
            }
            BytecodeInst::LoadIvar(reg, name) => {
                // 16
                let op1 = self.slot_id(&reg);
                Bytecode::from(enc_wl(16, op1.0, name.get()))
            }
            BytecodeInst::StoreIvar(reg, name) => {
                // 17
                let op1 = self.slot_id(&reg);
                Bytecode::from(enc_wl(17, op1.0, name.get()))
            }
            BytecodeInst::CheckConst {
                dst,
                base,
                toplevel,
                prefix,
                name,
            } => {
                // 18
                let op1 = self.slot_id(&dst);
                let base = base.map(|base| self.slot_id(&base));
                let op2 = self.store.new_constsite(base, name, prefix, toplevel);
                Bytecode::from(enc_wl(18, op1.0, op2.0))
            }
            BytecodeInst::ClassDef {
                ret,
                base,
                superclass,
                name,
                func_id,
            } => {
                // 70
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(r) => self.slot_id(&r),
                };
                let op2 = match base {
                    None => SlotId::new(0),
                    Some(r) => self.slot_id(&r),
                };
                let op3 = match superclass {
                    None => SlotId::new(0),
                    Some(r) => self.slot_id(&r),
                };
                Bytecode::from_with_func_name_id(
                    enc_www(70, op1.0, op2.0, op3.0),
                    Some(name),
                    func_id,
                )
            }
            BytecodeInst::ModuleDef {
                ret,
                base,
                name,
                func_id,
            } => {
                // 71
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.slot_id(&ret),
                };
                let op2 = match base {
                    None => SlotId::new(0),
                    Some(r) => self.slot_id(&r),
                };
                Bytecode::from_with_func_name_id(enc_www(71, op1.0, op2.0, 0), Some(name), func_id)
            }
            BytecodeInst::BlockArgProxy(dst, outer) => {
                // 21
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(21, op1.0, outer as u32))
            }
            BytecodeInst::SingletonClassDef { ret, base, func_id } => {
                // 22
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.slot_id(&ret),
                };
                let op2 = self.slot_id(&base);
                Bytecode::from_with_func_name_id(enc_wl(22, op1.0, op2.0 as u32), None, func_id)
            }
            BytecodeInst::BlockArg(dst, outer) => {
                // 23
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(23, op1.0, outer as u32))
            }
            BytecodeInst::CheckCvar { dst, name } => {
                // 24
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(24, op1.0, name.get()))
            }
            BytecodeInst::LoadGvar { dst, name } => {
                // 25
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(25, op1.0, name.get()))
            }
            BytecodeInst::StoreGvar { val, name } => {
                // 26
                let op1 = self.slot_id(&val);
                Bytecode::from(enc_wl(26, op1.0, name.get()))
            }
            BytecodeInst::LoadCvar { dst, name } => {
                // 27
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_wl(27, op1.0, name.get()))
            }
            BytecodeInst::LoadSvar { ret, id } => {
                // 28
                let op1 = self.slot_id(&ret);
                Bytecode::from(enc_wl(28, op1.0, id))
            }
            BytecodeInst::StoreCvar { val, name } => {
                // 29
                let op1 = self.slot_id(&val);
                Bytecode::from(enc_wl(29, op1.0, name.get()))
            }
            BytecodeInst::MethodCall(box callsite) => {
                // 30, 31
                let opcode = if callsite.is_simple() { 30 } else { 31 };
                self.encode_call(opcode, callsite, bc_pos)?
            }
            BytecodeInst::MethodCallBlock(box callsite) => {
                // 32, 33
                let opcode = if callsite.is_simple() { 32 } else { 33 };
                self.encode_call(opcode, callsite, bc_pos)?
            }
            BytecodeInst::Yield(box callsite) => {
                // 34, 35
                let opcode = if callsite.is_simple() { 34 } else { 35 };
                self.encode_call(opcode, callsite, bc_pos)?
            }
            BytecodeInst::InlineCache(box callsite) => self.encode_cache(130, callsite)?,
            BytecodeInst::Not { ret, src } => {
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&src);
                Bytecode::from(enc_ww(120, op1.0, op2.0))
            }
            BytecodeInst::UnOp {
                kind,
                dst: ret,
                recv: src,
            } => {
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&src);
                let callid =
                    self.new_callsite(CallSite::unary(Some(kind.into()), src, Some(ret)), bc_pos)?;
                self.store
                    .new_callsite_map_entry(self.iseq_id, bc_pos, callid);
                Bytecode::from_with_class_and_version(
                    enc_ww(121 + kind as u16, op1.0, op2.0),
                    None,
                    -1i32 as u32,
                )
            }
            BytecodeInst::BinOp(kind, ret, (lhs, rhs)) => {
                let op1 = ret.map_or(SlotId::self_(), |ret| self.slot_id(&ret));
                let op2 = self.slot_id(&lhs);
                let op3 = self.slot_id(&rhs);
                let name = kind.into();
                let callid =
                    self.new_callsite(CallSite::binary(Some(name), lhs, rhs, ret), bc_pos)?;
                self.store
                    .new_callsite_map_entry(self.iseq_id, bc_pos, callid);
                Bytecode::from_with_class2(enc_www(160 + kind as u16, op1.0, op2.0, op3.0))
            }
            BytecodeInst::Cmp(kind, ret, (lhs, rhs), optimizable) => {
                let op1 = ret.map_or(SlotId::self_(), |ret| self.slot_id(&ret));
                let op2 = self.slot_id(&lhs);
                let op3 = self.slot_id(&rhs);
                let op = if optimizable {
                    enc_www(150 + kind as u16, op1.0, op2.0, op3.0)
                } else {
                    enc_www(140 + kind as u16, op1.0, op2.0, op3.0)
                };
                let name = kind.into();
                let callid =
                    self.new_callsite(CallSite::binary(Some(name), lhs, rhs, ret), bc_pos)?;
                self.store
                    .new_callsite_map_entry(self.iseq_id, bc_pos, callid);
                Bytecode::from_with_class2(op)
            }
            BytecodeInst::Index(ret, base, idx) => {
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&base);
                let op3 = self.slot_id(&idx);
                let callid = self.new_callsite(
                    CallSite::binary(Some(IdentId::_INDEX), base, idx, Some(ret)),
                    bc_pos,
                )?;
                self.store
                    .new_callsite_map_entry(self.iseq_id, bc_pos, callid);
                Bytecode::from_with_class2(enc_www(132, op1.0, op2.0, op3.0))
            }
            BytecodeInst::StoreIndex { base, index, src } => {
                let op1 = self.slot_id(&src);
                let op2 = self.slot_id(&base);
                let op3 = self.slot_id(&index);
                let callid = self.new_callsite(
                    CallSite::ternary(Some(IdentId::_INDEX_ASSIGN), base, index, None),
                    bc_pos,
                )?;
                self.store
                    .new_callsite_map_entry(self.iseq_id, bc_pos, callid);
                Bytecode::from_with_class2(enc_www(133, op1.0, op2.0, op3.0))
            }
            BytecodeInst::Array(ret, box callsite) => {
                // 39
                let op1 = self.slot_id(&ret);
                let callid = self.new_callsite(callsite, bc_pos)?;
                Bytecode::from(enc_wl(39, op1.0, callid.get()))
            }
            BytecodeInst::DefinedYield { dst } => {
                // 64
                let op1 = self.slot_id(&dst);
                Bytecode::from(enc_www(64, op1.0, 0, 0))
            }
            BytecodeInst::DefinedConst {
                ret,
                toplevel,
                prefix,
                name,
            } => {
                // 65
                let op1 = self.slot_id(&ret);
                let op2 = self.store.new_constsite(None, name, prefix, toplevel);
                Bytecode::from_u32(enc_www(65, op1.0, 0, 0), op2.0)
            }
            BytecodeInst::DefinedMethod { ret, recv, name } => {
                // 66
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&recv);
                Bytecode::from_u32(enc_www(66, op1.0, op2.0, 0), name.get())
            }
            BytecodeInst::DefinedGvar { dst, name } => {
                // 67
                let op1 = self.slot_id(&dst);
                Bytecode::from_u32(enc_www(67, op1.0, 0, 0), name.get())
            }
            BytecodeInst::DefinedIvar { ret, name } => {
                // 68
                let op1 = self.slot_id(&ret);
                Bytecode::from_u32(enc_www(68, op1.0, 0, 0), name.get())
            }
            BytecodeInst::DefinedSuper { dst } => {
                // 69
                let op1 = self.slot_id(&dst);
                Bytecode::from_u32(enc_www(69, op1.0, 0, 0), 0)
            }
            BytecodeInst::EnsureEnd => Bytecode::from(enc_w(85, 0)),
            BytecodeInst::ConcatRegexp(ret, arg, len) => {
                let op1 = ret.map_or(SlotId::self_(), |ret| self.slot_id(&ret));
                let op2 = self.slot_id(&BcReg::from(arg));
                Bytecode::from(enc_www(86, op1.0, op2.0, len as u16))
            }

            BytecodeInst::ArrayTEq { lhs, rhs } => {
                let op1 = self.slot_id(&lhs);
                let op2 = self.slot_id(&rhs);
                Bytecode::from(enc_www(40, 0, op1.0, op2.0))
            }
            BytecodeInst::LoadDynVar { dst, src, outer } => {
                let op1 = self.slot_id(&dst);
                let op2 = self.slot_id(&src);
                let op3 = outer as u16;
                Bytecode::from(enc_www(148, op1.0, op2.0, op3))
            }
            BytecodeInst::StoreDynVar { dst, outer, src } => {
                let op1 = self.slot_id(&dst);
                let op2 = outer as u16;
                let op3 = self.slot_id(&src);
                Bytecode::from(enc_www(149, op1.0, op2, op3.0))
            }
            BytecodeInst::InitMethod(fn_info) => Bytecode::from(enc_www_fn_info(170, &fn_info)),
            BytecodeInst::ExpandArray(src, dst, len, rest_pos) => {
                let op1 = self.slot_id(&src);
                let op2 = self.slot_id(&dst);
                let rest = if let Some(rest) = rest_pos {
                    rest + 1
                } else {
                    0
                };
                Bytecode::from_u16(enc_www(171, op1.0, op2.0, len), rest)
            }
            BytecodeInst::UndefMethod { undef } => Bytecode::from(enc_wl(172, 0, undef.get())),
            BytecodeInst::AliasMethod { new, old } => {
                Bytecode::from_with_ident2(enc_www(173, 0, 0, 0), new, old)
            }
            BytecodeInst::Hash { ret, args, len } => {
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&args);
                Bytecode::from(enc_www(174, op1.0, op2.0, len))
            }
            BytecodeInst::ToA { dst, src } => {
                let op1 = self.slot_id(&dst);
                let op2 = self.slot_id(&src);
                Bytecode::from(enc_ww(175, op1.0, op2.0))
            }
            BytecodeInst::Mov(dst, src) => {
                let op1 = self.slot_id(&dst);
                let op2 = self.slot_id(&src);
                Bytecode::from(enc_ww(176, op1.0, op2.0))
            }
            BytecodeInst::Range {
                ret,
                start,
                end,
                exclude_end,
            } => {
                let op1 = self.slot_id(&ret);
                let op2 = self.slot_id(&start);
                let op3 = self.slot_id(&end);
                Bytecode::from(enc_www(177 + u16::from(exclude_end), op1.0, op2.0, op3.0))
            }
            BytecodeInst::ConcatStr(ret, arg, len) => {
                let op1 = ret.map_or(SlotId::self_(), |ret| self.slot_id(&ret));
                let op2 = self.slot_id(&BcReg::from(arg));
                Bytecode::from(enc_www(179, op1.0, op2.0, len as u16))
            }
        };
        Ok(bc)
    }

    fn encode_call(
        &mut self,
        opcode: u16,
        callsite: CallSite,
        bc_pos: BcIndex,
    ) -> Result<Bytecode> {
        let CallSite { dst, .. } = callsite;
        let ret = match dst {
            None => 0,
            Some(ret) => self.slot_id(&ret).0,
        };
        let callid = self.new_callsite(callsite, bc_pos)?;
        Ok(Bytecode::from(enc_wl(opcode, ret, callid.get())))
    }

    fn encode_cache(&self, opcode: u16, callsite: CallSite) -> Result<Bytecode> {
        let CallSite {
            args,
            pos_num,
            recv,
            ..
        } = callsite;
        Ok(Bytecode::from_with_class_and_version(
            enc_www(
                opcode,
                self.slot_id(&recv).0,
                self.slot_id(&args).0,
                pos_num as _,
            ),
            None,
            -1i32 as u32,
        ))
    }

    fn new_callsite(&mut self, callsite: CallSite, bc_pos: BcIndex) -> Result<CallSiteId> {
        let CallSite {
            name,
            pos_num,
            kw,
            splat_pos,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
            forwarding,
        } = callsite;

        let args = self.slot_id(&args);
        let recv = self.slot_id(&recv);
        let block_arg = block_arg.map(|r| self.slot_id(&r));
        let dst = dst.map(|r| self.slot_id(&r));
        let (kw_pos, kw_args, hash_splat_pos) = if let Some(KeywordArgs {
            kw_start,
            kw_args,
            hash_splat_pos,
        }) = kw
        {
            let kw_pos = self.slot_id(&kw_start);
            let hash_splat_pos = hash_splat_pos
                .into_iter()
                .map(|r| self.slot_id(&r))
                .collect();
            (kw_pos, kw_args, hash_splat_pos)
        } else {
            (SlotId(0), indexmap::IndexMap::default(), vec![])
        };
        Ok(self.store.new_callsite(
            name,
            bc_pos,
            pos_num,
            kw_pos,
            kw_args,
            splat_pos,
            hash_splat_pos,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
            forwarding,
        ))
    }
}

fn enc_w(opcode: u16, op1: u16) -> u64 {
    enc_www(opcode, op1, 0, 0)
}

fn enc_wl(opcode: u16, op1: u16, op2: u32) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + (op2 as u64)
}

fn enc_l(opcode: u16, op1: u32) -> u64 {
    enc_wl(opcode, 0, op1)
}

fn enc_ww(opcode: u16, op1: u16, op2: u16) -> u64 {
    enc_www(opcode, op1, op2, 0)
}

fn enc_www(opcode: u16, op1: u16, op2: u16, op3: u16) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + ((op2 as u64) << 16) + (op3 as u64)
}

fn enc_www_fn_info(opcode: u16, fn_info: &FnInitInfo) -> u64 {
    let FnInitInfo {
        reg_num,
        arg_num,
        stack_offset,
        ..
    } = fn_info;
    enc_www(
        opcode,
        *reg_num as u16,
        *arg_num as u16,
        *stack_offset as u16,
    )
}
