use super::*;

#[derive(Debug, Clone)]
pub(super) struct RegInfo {
    info: Vec<RegState>,
}

impl RegInfo {
    ///
    /// Extract a set of registers which will be used as Float in this loop,
    /// *and* xmm-linked on the back-edge.
    ///
    pub(super) fn get_loop_used_as_float(self) -> Vec<(SlotId, Option<ClassId>)> {
        self.info
            .into_iter()
            .enumerate()
            .flat_map(|(i, b)| match (b.xmm_link, b.is_used) {
                (XmmLink::Linked, IsUsed::Used(true, class)) => Some((SlotId(i as u16), class)),
                _ => None,
            })
            .collect()
    }
}

impl RegInfo {
    fn new(reg_num: usize) -> Self {
        Self {
            info: vec![RegState::new(); reg_num],
        }
    }

    fn merge(&mut self, other: &Self) {
        for (i, detail) in &mut self.info.iter_mut().enumerate() {
            detail.xmm_link.merge(&other[i].xmm_link);
            detail.is_used.merge(&other[i].is_used);
        }
    }

    fn use_as(&mut self, slot: SlotId, is_float: bool, class: ClassId) {
        self[slot].xmm_link = if is_float {
            XmmLink::Linked
        } else {
            XmmLink::NotLinked
        };
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::Used(is_float, Some(class));
        }
    }

    fn def_as(&mut self, slot: SlotId, is_float: bool) {
        self[slot].xmm_link = if is_float {
            XmmLink::Linked
        } else {
            XmmLink::NotLinked
        };
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::NotUsed;
        }
    }

    fn copy(&mut self, dst: SlotId, src: SlotId) {
        let is_used = match &self[dst].is_used {
            IsUsed::Used(b, c) => IsUsed::Used(*b, *c),
            _ => IsUsed::NotUsed,
        };
        self[dst] = RegState {
            xmm_link: self[src].xmm_link,
            is_used,
        };
    }
}

impl std::ops::Index<SlotId> for RegInfo {
    type Output = RegState;
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
    type Output = RegState;
    fn index(&self, i: usize) -> &Self::Output {
        &self.info[i]
    }
}

impl std::ops::IndexMut<usize> for RegInfo {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.info[i]
    }
}

#[derive(Debug, Clone)]
pub(super) struct RegState {
    xmm_link: XmmLink,
    is_used: IsUsed,
}

impl RegState {
    fn new() -> Self {
        Self {
            xmm_link: XmmLink::ND,
            is_used: IsUsed::ND,
        }
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
        *self = match (*self, other) {
            (XmmLink::Linked, XmmLink::Linked | XmmLink::ND) | (XmmLink::ND, XmmLink::Linked) => {
                XmmLink::Linked
            }
            (XmmLink::NotLinked, XmmLink::NotLinked) => XmmLink::NotLinked,
            _ => XmmLink::ND,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum IsUsed {
    ND,
    Used(bool, Option<ClassId>),
    MaybeUsed,
    NotUsed,
}

impl IsUsed {
    fn merge(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::ND, r) => r.clone(),
            (l, IsUsed::ND) => **l,
            (IsUsed::Used(b1, c1), IsUsed::Used(b2, c2)) => {
                IsUsed::Used(*b1 | *b2, if c1 == c2 { *c1 } else { None })
            }
            (IsUsed::NotUsed, IsUsed::NotUsed) => IsUsed::NotUsed,
            _ => IsUsed::MaybeUsed,
        };
    }
}

pub(super) struct ScanContext {
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
    pub(super) fn loop_analysis(func: &NormalFuncInfo, bb_pos: usize) -> RegInfo {
        let mut ctx = ScanContext::new(func);
        let regnum = func.total_reg_num();
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
            /*#[cfg(feature = "log-jit")]
            for (from, info) in &branches {
                eprintln!("{from}->{bb_pos}: {:?}", info.get_loop_used_as_float());
            }*/
            let reg_info0 = RegInfo::new(regnum);
            let reg_info = branches.into_iter().fold(reg_info0, |mut acc, (_, info)| {
                acc.merge(&info);
                acc
            });
            if ctx.scan_bb(func, reg_info, bb_pos) {
                break;
            };
        }
        ctx.back_info
    }

    fn scan_bb(&mut self, func: &NormalFuncInfo, mut reg_info: RegInfo, bb_pos: usize) -> bool {
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
                    if self.loop_level == 0 {
                        return true;
                    }
                }
                BcOp::Integer(ret, ..)
                | BcOp::Symbol(ret, ..)
                | BcOp::Array(ret, ..)
                | BcOp::Index(ret, ..)
                | BcOp::Nil(ret) => {
                    reg_info.def_as(ret, false);
                }
                BcOp::Literal(dst, val) => {
                    reg_info.def_as(dst, val.class_id() == FLOAT_CLASS);
                }
                BcOp::IndexAssign(..) => {}
                BcOp::MethodDef(..) => {}
                BcOp::StoreConst(..) => {}
                BcOp::LoadConst(dst, _const_id) => {
                    let is_float =
                        pc.value().is_some() && pc.value().unwrap().class_id() == FLOAT_CLASS;
                    reg_info.def_as(dst, is_float);
                }
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
                BcOp::Ret(_ret) => return false,
                BcOp::Br(disp) => {
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
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
        unreachable!();
    }
}
