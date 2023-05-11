use super::*;

impl BytecodeGen {
    pub(super) fn into_bytecode(mut self, store: &mut Store, func_id: FuncId) -> Result<()> {
        let mut ops = vec![];
        let mut locs = vec![];
        for (idx, (inst, loc)) in self.ir.iter().enumerate() {
            let op = self.inst_to_bc(store, inst, idx);
            ops.push(op);
            locs.push(*loc);
        }
        for CallSite {
            name,
            arg_num,
            kw,
            splat_pos,
        } in std::mem::take(&mut self.callsites)
        {
            if let Some(KeywordArgs {
                kw_pos,
                kw_args,
                hash_splat_pos,
            }) = kw
            {
                let pos = self.get_index(&kw_pos);
                let hash_splat_pos = hash_splat_pos
                    .into_iter()
                    .map(|r| self.get_index(&r))
                    .collect();
                store.add_callsite(name, arg_num, pos, kw_args, splat_pos, hash_splat_pos);
            } else {
                store.add_callsite(
                    name,
                    arg_num,
                    SlotId(0),
                    HashMap::default(),
                    splat_pos,
                    vec![],
                );
            }
        }
        for f in std::mem::take(&mut self.functions) {
            let sourceinfo = self.sourceinfo.clone();
            match f {
                Functions::Method { name, info } => {
                    store.add_method(name, info, sourceinfo)?;
                }
                Functions::ClassDef { name, info } => {
                    store.add_classdef(name, info, sourceinfo)?;
                }
                Functions::Block {
                    mother,
                    outer,
                    optional_params,
                    info,
                } => {
                    store.add_block(mother, outer, optional_params, info, sourceinfo)?;
                }
            }
        }
        let info = store[func_id].as_ruby_func_mut();
        info.temp_num = self.temp_num;
        info.non_temp_num = self.non_temp_num;
        info.literals = std::mem::take(&mut self.literals);
        info.set_bytecode(ops);
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
            let start = self.get_pc(info, range.start);
            let end = self.get_pc(info, range.end);
            let rescue = rescue.map(|l| self.get_pc(info, l));
            let ensure = ensure.map(|l| self.get_pc(info, l));
            let err_reg = err_reg.map(|reg| self.get_index(&reg));
            info.exception_push(start..end, rescue, ensure, err_reg);
        }

        info.sourcemap = locs;
        Ok(())
    }

    fn get_pc(&self, info: &ISeqInfo, label: Label) -> BcPc {
        info.get_pc(self[label])
    }

    fn inst_to_bc(&self, store: &mut Store, inst: &BcIr, idx: usize) -> Bc {
        match inst {
            BcIr::SingletonMethodDef { obj, name, func_id } => {
                let op1 = self.get_index(obj);
                Bc::from_with_func_name_id(enc_wl(1, op1.0, 0), Some(*name), *func_id)
            }
            BcIr::MethodDef { name, func_id } => {
                Bc::from_with_func_name_id(enc_l(2, 0), Some(*name), *func_id)
            }
            BcIr::Br(dst) => {
                let dst = self[*dst].to_usize();
                let op1 = dst as isize - idx as isize - 1;
                Bc::from(enc_l(3, op1 as u32))
            }
            BcIr::CondBr(reg, dst, optimizable, kind) => {
                let dst = self[*dst].to_usize();
                let op1 = self.get_index(reg);
                let op2 = dst as isize - idx as isize - 1;
                let kind = *kind as u16;
                let op = enc_wl(
                    if *optimizable { 12 + kind } else { 4 + kind },
                    op1.0,
                    op2 as u32,
                );
                Bc::from(op)
            }
            BcIr::Integer(reg, num) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_wl(6, op1.0, *num as u32))
            }
            BcIr::Literal(reg, val) => {
                let op1 = self.get_index(reg);
                Bc::from_with_value(enc_wl(7, op1.0, 0), *val)
            }
            BcIr::Nil(reg) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_w(8, op1.0))
            }
            BcIr::Symbol(reg, name) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_wl(9, op1.0, name.get()))
            }
            BcIr::LoadConst {
                ret,
                toplevel,
                prefix,
                name,
            } => {
                let op1 = self.get_index(ret);
                let op2 = store.add_constsite(*name, prefix.clone(), *toplevel);
                Bc::from(enc_wl(10, op1.0, op2.0))
            }
            BcIr::StoreConst(reg, name) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_wl(11, op1.0, name.get()))
            }
            BcIr::LoopStart => Bc::from(enc_l(14, 0)),
            BcIr::LoopEnd => Bc::from(enc_l(15, 0)),
            BcIr::LoadIvar(reg, name) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_wl(16, op1.0, name.get()))
            }
            BcIr::StoreIvar(reg, name) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_wl(17, op1.0, name.get()))
            }
            BcIr::ClassDef {
                ret,
                superclass,
                name,
                func_id,
            } => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                let op2 = match superclass {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                Bc::from_with_func_name_id(enc_wl(18, op1.0, op2.0 as u32), Some(*name), *func_id)
            }
            BcIr::ModuleDef { ret, name, func_id } => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                Bc::from_with_func_name_id(enc_wl(19, op1.0, 0), Some(*name), *func_id)
            }
            BcIr::CheckLocal(local, dst) => {
                let op1 = self.get_index(local);
                let dst = self[*dst].to_usize();
                let op2 = dst as isize - idx as isize - 1;
                Bc::from(enc_wl(20, op1.0, op2 as u32))
            }
            BcIr::BlockArgProxy(dst, outer) => {
                let op1 = self.get_index(dst);
                Bc::from(enc_wl(21, op1.0, *outer as u32))
            }
            BcIr::SingletonClassDef { ret, base, func_id } => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                let op2 = self.get_index(base);
                Bc::from_with_func_name_id(enc_wl(22, op1.0, op2.0 as u32), None, *func_id)
            }
            BcIr::LoadGvar { ret, name } => {
                let op1 = self.get_index(ret);
                Bc::from(enc_wl(25, op1.0, name.get()))
            }
            BcIr::StoreGvar { val, name } => {
                let op1 = self.get_index(val);
                Bc::from(enc_wl(26, op1.0, name.get()))
            }
            BcIr::LoadSvar { ret, id } => {
                let op1 = self.get_index(ret);
                Bc::from(enc_wl(28, op1.0, *id))
            }
            BcIr::MethodCall(ret, callid, has_splat) => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                Bc::from_with_class_and_version(
                    enc_wl(if *has_splat { 30 } else { 31 }, op1.0, callid.get()),
                    ClassId::new(0),
                    -1i32 as u32,
                )
            }
            BcIr::MethodCallBlock(ret, callid, has_splat) => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                Bc::from_with_class_and_version(
                    enc_wl(if *has_splat { 32 } else { 33 }, op1.0, callid.get()),
                    ClassId::new(0),
                    -1i32 as u32,
                )
            }
            BcIr::Super(ret, callid) => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                Bc::from_with_class_and_version(
                    enc_wl(34, op1.0, callid.get()),
                    ClassId::new(0),
                    -1i32 as u32,
                )
            }
            BcIr::DefinedYield { ret } => {
                let op1 = self.get_index(ret);
                Bc::from(enc_www(64, op1.0, 0, 0))
            }
            BcIr::DefinedConst {
                ret,
                toplevel,
                prefix,
                name,
            } => {
                let op1 = self.get_index(ret);
                let op2 = store.add_constsite(*name, prefix.clone(), *toplevel);
                Bc::from_u32(enc_www(65, op1.0, 0, 0), op2.0)
            }
            BcIr::DefinedMethod { ret, recv, name } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(recv);
                Bc::from_u32(enc_www(66, op1.0, op2.0, 0), name.get())
            }
            BcIr::DefinedGvar { ret, name } => {
                let op1 = self.get_index(ret);
                Bc::from_u32(enc_www(67, op1.0, 0, 0), name.get())
            }
            BcIr::DefinedIvar { ret, name } => {
                let op1 = self.get_index(ret);
                Bc::from_u32(enc_www(68, op1.0, 0, 0), name.get())
            }
            BcIr::Ret(reg) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_w(80, op1.0))
            }
            BcIr::MethodRet(reg) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_w(81, op1.0))
            }
            BcIr::Break(reg) => {
                let op1 = self.get_index(reg);
                Bc::from(enc_w(82, op1.0))
            }
            BcIr::EnsureEnd => Bc::from(enc_w(85, 0)),
            BcIr::Pos { ret, src } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                Bc::from_with_class_and_version(
                    enc_ww(126, op1.0, op2.0),
                    ClassId::default(),
                    -1i32 as u32,
                )
            }
            BcIr::BitNot { ret, src } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                Bc::from(enc_ww(127, op1.0, op2.0))
            }
            BcIr::Not { ret, src } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                Bc::from(enc_ww(128, op1.0, op2.0))
            }
            BcIr::Neg { ret, src } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                Bc::from_with_class_and_version(
                    enc_ww(129, op1.0, op2.0),
                    ClassId::default(),
                    -1i32 as u32,
                )
            }
            BcIr::MethodArgs(recv, args, len) => {
                let op1 = self.get_index(recv);
                let op2 = self.get_index(args);
                Bc::from(enc_www(130, op1.0, op2.0, *len as u16))
            }
            BcIr::Array(ret, src, len) => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                Bc::from(enc_www(131, op1.0, op2.0, *len))
            }
            BcIr::Index(ret, base, idx) => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(base);
                let op3 = self.get_index(idx);
                Bc::from_with_class2(enc_www(132, op1.0, op2.0, op3.0))
            }
            BcIr::StoreIndex(src, base, idx) => {
                let op1 = self.get_index(src);
                let op2 = self.get_index(base);
                let op3 = self.get_index(idx);
                Bc::from_with_class2(enc_www(133, op1.0, op2.0, op3.0))
            }
            BcIr::Cmp(kind, dst, mode, optimizable) => {
                let op1 = self.get_index(dst);
                match mode {
                    BinopMode::RR(lhs, rhs) => {
                        let op2 = self.get_index(lhs);
                        let op3 = self.get_index(rhs);
                        let op = if *optimizable {
                            enc_www(154 + *kind as u16, op1.0, op2.0, op3.0)
                        } else {
                            enc_www(134 + *kind as u16, op1.0, op2.0, op3.0)
                        };
                        Bc::from_with_class2(op)
                    }
                    BinopMode::RI(lhs, rhs) => {
                        let op2 = self.get_index(lhs);
                        let op = if *optimizable {
                            enc_wwsw(162 + *kind as u16, op1.0, op2.0, *rhs)
                        } else {
                            enc_wwsw(142 + *kind as u16, op1.0, op2.0, *rhs)
                        };
                        Bc::from_with_class2(op)
                    }
                    _ => unreachable!(),
                }
            }
            BcIr::LoadDynVar { ret, src, outer } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(src);
                let op3 = *outer as u16;
                Bc::from(enc_www(150, op1.0, op2.0, op3))
            }
            BcIr::StoreDynVar { dst, outer, src } => {
                let op1 = self.get_index(dst);
                let op2 = *outer as u16;
                let op3 = self.get_index(src);
                Bc::from(enc_www(151, op1.0, op2, op3.0))
            }
            BcIr::Yield {
                ret,
                args,
                len,
                callid,
            } => {
                let op1 = match ret {
                    None => SlotId::new(0),
                    Some(ret) => self.get_index(ret),
                };
                let op2 = self.get_index(args);
                Bc::from_with_callid(enc_www(152, op1.0, op2.0, *len as u16), *callid)
            }

            BcIr::InitMethod(fn_info) => Bc::from_fn_info(enc_www_fn_info(170, fn_info), fn_info),
            BcIr::ExpandArray(src, dst, len) => {
                let op1 = self.get_index(src);
                let op2 = self.get_index(dst);
                Bc::from(enc_www(171, op1.0, op2.0, *len))
            }
            BcIr::InitBlock(fn_info) => Bc::from_fn_info(enc_www_fn_info(172, fn_info), fn_info),
            BcIr::AliasMethod { new, old } => {
                let op1 = self.get_index(new);
                let op2 = self.get_index(old);
                Bc::from(enc_www(173, 0, op1.0, op2.0))
            }
            BcIr::Hash { ret, args, len } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(args);
                Bc::from(enc_www(174, op1.0, op2.0, *len))
            }
            BcIr::Mov(dst, src) => {
                let op1 = self.get_index(dst);
                let op2 = self.get_index(src);
                Bc::from(enc_ww(176, op1.0, op2.0))
            }
            BcIr::Range {
                ret,
                start,
                end,
                exclude_end,
            } => {
                let op1 = self.get_index(ret);
                let op2 = self.get_index(start);
                let op3 = self.get_index(end);
                Bc::from(enc_www(177 + u16::from(*exclude_end), op1.0, op2.0, op3.0))
            }
            BcIr::ConcatStr(ret, arg, len) => {
                let op1 = ret.map_or(SlotId::self_(), |ret| self.get_index(&ret));
                let op2 = self.get_index(&BcReg::from(*arg));
                Bc::from(enc_www(179, op1.0, op2.0, *len as u16))
            }
            BcIr::BinOp(kind, dst, mode) => {
                let op1 = self.get_index(dst);
                match mode {
                    BinopMode::RR(lhs, rhs) => {
                        let op2 = self.get_index(lhs);
                        let op3 = self.get_index(rhs);
                        Bc::from_with_class2(enc_www(200 + *kind as u16, op1.0, op2.0, op3.0))
                    }
                    BinopMode::IR(lhs, rhs) => {
                        let op3 = self.get_index(rhs);
                        Bc::from_with_class2(enc_wsww(180 + *kind as u16, op1.0, *lhs, op3.0))
                    }
                    BinopMode::RI(lhs, rhs) => {
                        let op2 = self.get_index(lhs);
                        Bc::from_with_class2(enc_wwsw(220 + *kind as u16, op1.0, op2.0, *rhs))
                    }
                }
            }
        }
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
        reqopt_num,
        stack_offset,
        ..
    } = fn_info;
    enc_www(
        opcode,
        *reg_num as u16,
        *reqopt_num as u16,
        *stack_offset as u16,
    )
}

fn enc_wsww(opcode: u16, op1: u16, op2: i16, op3: u16) -> u64 {
    enc_www(opcode, op1, op2 as u16, op3)
}

fn enc_wwsw(opcode: u16, op1: u16, op2: u16, op3: i16) -> u64 {
    enc_www(opcode, op1, op2, op3 as u16)
}
