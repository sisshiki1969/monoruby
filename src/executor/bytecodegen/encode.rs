use super::*;

impl IrContext {
    pub(super) fn ir_to_bytecode(&mut self, info: &mut ISeqInfo, store: &mut FnStore) {
        let mut ops = vec![];
        let mut locs = vec![];
        for (idx, (inst, loc)) in self.ir.iter().enumerate() {
            let op = match inst {
                BcIr::MethodCall(ret, name) => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_class_and_version(
                        enc_wl(1, op1.0, name.get()),
                        ClassId::new(0),
                        -1i32 as u32,
                    )
                }
                BcIr::Br(dst) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let op1 = dst - idx as i32 - 1;
                    Bc::from(enc_l(3, op1 as u32))
                }
                BcIr::Integer(reg, num) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(6, op1.0, *num as u32))
                }
                BcIr::Literal(reg, val) => {
                    let op1 = info.get_index(reg);
                    Bc::from_with_value(enc_wl(7, op1.0, 0), *val)
                }
                BcIr::Nil(reg) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_w(8, op1.0))
                }
                BcIr::Symbol(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(9, op1.0, name.get()))
                }
                BcIr::LoadConst(reg, toplevel, prefix, name) => {
                    let op1 = info.get_index(reg);
                    let op2 = info.add_constsite(store, *name, prefix.clone(), *toplevel);
                    Bc::from(enc_wl(10, op1.0, op2.0))
                }
                BcIr::StoreConst(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(11, op1.0, name.get()))
                }
                BcIr::CondBr(reg, dst, optimizable, kind) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let op1 = info.get_index(reg);
                    let op2 = dst - idx as i32 - 1;
                    let kind = *kind as u16;
                    let op = enc_wl(
                        if *optimizable { 12 + kind } else { 4 + kind },
                        op1.0,
                        op2 as u32,
                    );
                    Bc::from(op)
                }
                BcIr::LoopStart => Bc::from(enc_l(14, 0)),
                BcIr::LoopEnd => Bc::from(enc_l(15, 0)),
                BcIr::LoadIvar(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(16, op1.0, name.get()))
                }
                BcIr::StoreIvar(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(17, op1.0, name.get()))
                }
                BcIr::MethodCallBlock(ret, name) => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_class_and_version(
                        enc_wl(19, op1.0, name.get()),
                        ClassId::new(0),
                        -1i32 as u32,
                    )
                }
                BcIr::CheckLocal(local, dst) => {
                    let op1 = info.get_index(local);
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let op2 = dst - idx as i32 - 1;
                    Bc::from(enc_wl(20, op1.0, op2 as u32))
                }
                BcIr::BlockArgProxy(dst) => {
                    let op1 = info.get_index(dst);
                    Bc::from(enc_wl(21, op1.0, 0))
                }
                BcIr::LoadGvar { ret, name } => {
                    let op1 = info.get_index(ret);
                    Bc::from(enc_wl(25, op1.0, name.get()))
                }
                BcIr::StoreGvar { val, name } => {
                    let op1 = info.get_index(val);
                    Bc::from(enc_wl(26, op1.0, name.get()))
                }
                BcIr::Splat(src) => {
                    let op1 = info.get_index(src);
                    Bc::from(enc_wl(27, op1.0, 0))
                }
                BcIr::Array(ret, src, len) => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(src);
                    Bc::from(enc_www(131, op1.0, op2.0, *len))
                }
                BcIr::Index(ret, base, idx) => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(base);
                    let op3 = info.get_index(idx);
                    Bc::from_with_class2(enc_www(132, op1.0, op2.0, op3.0))
                }
                BcIr::StoreIndex(src, base, idx) => {
                    let op1 = info.get_index(src);
                    let op2 = info.get_index(base);
                    let op3 = info.get_index(idx);
                    Bc::from(enc_www(133, op1.0, op2.0, op3.0))
                }
                BcIr::LoadDynVar { ret, src, outer } => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(src);
                    let op3 = *outer as u16;
                    Bc::from(enc_www(150, op1.0, op2.0, op3))
                }
                BcIr::StoreDynVar { dst, outer, src } => {
                    let op1 = info.get_index(dst);
                    let op2 = *outer as u16;
                    let op3 = info.get_index(src);
                    Bc::from(enc_www(151, op1.0, op2, op3.0))
                }
                BcIr::Range {
                    ret,
                    start,
                    end,
                    exclude_end,
                } => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(start);
                    let op3 = info.get_index(end);
                    Bc::from(enc_www(
                        153 + if *exclude_end { 1 } else { 0 },
                        op1.0,
                        op2.0,
                        op3.0,
                    ))
                }
                BcIr::Neg(dst, src) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(src);
                    Bc::from_with_class_and_version(
                        enc_ww(129, op1.0, op2.0),
                        ClassId::default(),
                        -1i32 as u32,
                    )
                }
                BcIr::BinOpIr(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_wsww(180 + *kind as u16, op1.0, *lhs, op3.0))
                }
                BcIr::BinOp(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_www(200 + *kind as u16, op1.0, op2.0, op3.0))
                }
                BcIr::BinOpRi(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    Bc::from_with_class2(enc_wwsw(220 + *kind as u16, op1.0, op2.0, *rhs))
                }
                BcIr::Cmp(kind, dst, lhs, rhs, optimizable) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op3 = info.get_index(rhs);
                    let op = if *optimizable {
                        enc_www(156 + *kind as u16, op1.0, op2.0, op3.0)
                    } else {
                        enc_www(134 + *kind as u16, op1.0, op2.0, op3.0)
                    };
                    Bc::from_with_class2(op)
                }
                BcIr::Cmpri(kind, dst, lhs, rhs, optimizable) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op = if *optimizable {
                        enc_wwsw(162 + *kind as u16, op1.0, op2.0, *rhs)
                    } else {
                        enc_wwsw(142 + *kind as u16, op1.0, op2.0, *rhs)
                    };
                    Bc::from_with_class2(op)
                }
                BcIr::Ret(reg) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_w(148, op1.0))
                }
                BcIr::Mov(dst, src) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(src);
                    Bc::from(enc_ww(149, op1.0, op2.0))
                }
                BcIr::InitMethod(info) => {
                    let FnInitInfo {
                        reg_num,
                        arg_num,
                        pos_num,
                        req_num,
                        block_pos,
                        stack_offset,
                    } = info;
                    Bc::from_with_num(
                        enc_www(170, *reg_num as u16, *pos_num as u16, *stack_offset as u16),
                        *req_num as u16,
                        *arg_num as u16,
                        *block_pos as u16,
                    )
                }
                BcIr::InitBlock(info) => {
                    let FnInitInfo {
                        reg_num,
                        arg_num,
                        pos_num,
                        req_num,
                        block_pos,
                        stack_offset,
                    } = info;
                    Bc::from_with_num(
                        enc_www(172, *reg_num as u16, *pos_num as u16, *stack_offset as u16),
                        *req_num as u16,
                        *arg_num as u16,
                        *block_pos as u16,
                    )
                }
                BcIr::Yield { ret, args, len } => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    let op2 = info.get_index(args);
                    Bc::from(enc_www(152, op1.0, op2.0, *len as u16))
                }
                BcIr::MethodArgs(recv, args, len) => {
                    let op1 = info.get_index(recv);
                    let op2 = info.get_index(args);
                    Bc::from(enc_www(130, op1.0, op2.0, *len as u16))
                }
                BcIr::InlineCache => Bc::from(0),
                BcIr::MethodDef(name, func_id) => {
                    Bc::from_with_func_name_id(enc_l(2, 0), *name, *func_id)
                }
                BcIr::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    let op2 = match superclass {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_func_name_id(enc_wl(18, op1.0, op2.0 as u32), *name, *func_id)
                }
                BcIr::ConcatStr(ret, arg, len) => {
                    let op1 = ret.map_or(SlotId::self_(), |ret| info.get_index(&ret));
                    let op2 = info.get_index(&BcReg::from(*arg));
                    Bc::from(enc_www(155, op1.0, op2.0, *len as u16))
                }
                BcIr::ExpandArray(src, dst, len) => {
                    let op1 = info.get_index(src);
                    let op2 = info.get_index(dst);
                    Bc::from(enc_www(171, op1.0, op2.0, *len))
                }
            };
            ops.push(op);
            locs.push(*loc);
        }
        info.set_bytecode(ops);
        info.sourcemap = locs;
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

fn enc_wsww(opcode: u16, op1: u16, op2: i16, op3: u16) -> u64 {
    enc_www(opcode, op1, op2 as u16, op3)
}

fn enc_wwsw(opcode: u16, op1: u16, op2: u16, op3: i16) -> u64 {
    enc_www(opcode, op1, op2, op3 as u16)
}
