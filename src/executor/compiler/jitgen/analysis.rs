use super::*;

pub(super) struct LoopAnalysis {
    /// key: dest_idx, value Vec<(src_idx, reginfo)>
    branch_map: HashMap<usize, Vec<(usize, RegInfo)>>,
    /// Basic block information
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    loop_level: usize,
    backedge_info: Option<RegInfo>,
    return_info: Option<RegInfo>,
    pc: BcPc,
}

impl LoopAnalysis {
    ///
    /// Liveness analysis for loops.
    ///
    pub(super) fn analyse(
        func: &ISeqInfo,
        fnstore: &FnStore,
        bb_pos: usize,
    ) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        let mut ctx = LoopAnalysis::new(func);
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
        let mut exit_info = RegInfo::new(regnum);
        for bb_pos in bb_start_vec {
            let branches = ctx.get_branches(bb_pos);
            let reg_info = if branches.is_empty() {
                RegInfo::new(regnum)
            } else {
                let reg_info0 = branches[0].1.clone();
                branches.into_iter().fold(reg_info0, |mut acc, (_, info)| {
                    acc.merge(&info);
                    acc
                })
            };
            if let Some(info) = ctx.scan_bb(func, fnstore, reg_info, bb_pos) {
                exit_info = info;
                break;
            };
        }
        let info = ctx.backedge_info.unwrap();
        exit_info.merge(&info);
        if let Some(info) = ctx.return_info {
            exit_info.merge(&info);
        }
        #[cfg(feature = "emit-tir")]
        eprintln!(
            "{:?}",
            info.info
                .iter()
                .enumerate()
                .flat_map(|(i, state)| {
                    if state.xmm_link == XmmLink::None || state.is_used != IsUsed::Used {
                        None
                    } else {
                        Some((i, state.xmm_link, state.is_used))
                    }
                })
                .collect::<Vec<_>>()
        );

        (info.get_loop_used_as_float(), exit_info.get_unused())
    }
}

impl LoopAnalysis {
    fn new(func: &ISeqInfo) -> Self {
        Self {
            branch_map: HashMap::default(),
            bb_info: func.get_bb_info(),
            loop_level: 0,
            backedge_info: None,
            return_info: None,
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

    fn add_backedge(&mut self, incoming_info: &RegInfo) {
        if let Some(info) = &mut self.backedge_info {
            info.merge(incoming_info);
        } else {
            self.backedge_info = Some(incoming_info.clone());
        }
    }

    fn add_return(&mut self, incoming_info: &RegInfo) {
        if let Some(info) = &mut self.return_info {
            info.merge(incoming_info);
        } else {
            self.return_info = Some(incoming_info.clone());
        }
    }

    fn scan_bb(
        &mut self,
        func: &ISeqInfo,
        fnstore: &FnStore,
        mut reg_info: RegInfo,
        bb_pos: usize,
    ) -> Option<RegInfo> {
        for (ofs, pc) in func.bytecode()[bb_pos..].iter().enumerate() {
            self.pc = BcPc::from(pc);
            let idx = bb_pos + ofs;
            match self.pc.get_ir(fnstore) {
                TraceIr::InitMethod { .. } => {}
                TraceIr::InitBlock { .. } => {}
                TraceIr::AliasMethod { .. } => {}
                TraceIr::MethodDef(..) => {}
                TraceIr::LoopStart(_) => {
                    self.loop_level += 1;
                }
                TraceIr::LoopEnd => {
                    self.loop_level -= 1;
                    if self.loop_level == 0 {
                        return Some(reg_info);
                    }
                }
                TraceIr::Integer(ret, ..) | TraceIr::Symbol(ret, ..) | TraceIr::Nil(ret) => {
                    reg_info.def_as(ret, false);
                }
                TraceIr::Literal(dst, val) => {
                    if val.class_id() == FLOAT_CLASS {
                        reg_info.def_float_const(dst);
                    } else {
                        reg_info.def_as(dst, false);
                    }
                }
                TraceIr::Array { ret, args, len } => {
                    for r in args.0..args.0 + len {
                        reg_info.use_non_float(SlotId(r));
                    }
                    reg_info.def_as(ret, false);
                }
                TraceIr::Hash { ret, args, len } => {
                    for r in args.0..args.0 + len * 2 {
                        reg_info.use_non_float(SlotId(r));
                    }
                    reg_info.def_as(ret, false);
                }
                TraceIr::Index { ret, base, idx } => {
                    reg_info.def_as(ret, false);
                    reg_info.use_non_float(base);
                    reg_info.use_non_float(idx);
                }
                TraceIr::Range {
                    ret, start, end, ..
                } => {
                    reg_info.def_as(ret, false);
                    reg_info.use_non_float(start);
                    reg_info.use_non_float(end);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    reg_info.use_non_float(src);
                    reg_info.use_non_float(base);
                    reg_info.use_non_float(idx);
                }
                TraceIr::ClassDef { ret, .. } => {
                    reg_info.def_as(ret, false);
                }
                TraceIr::LoadConst(dst, _const_id) => {
                    let is_float = if let Some(value) = pc.value() {
                        value.class_id() == FLOAT_CLASS
                    } else {
                        false
                    };
                    reg_info.def_as(dst, is_float);
                }
                TraceIr::BlockArgProxy(dst)
                | TraceIr::LoadDynVar(dst, ..)
                | TraceIr::LoadIvar(dst, ..)
                | TraceIr::LoadGvar { dst, .. } => {
                    reg_info.def_as(dst, false);
                }
                TraceIr::StoreConst(src, _)
                | TraceIr::StoreDynVar(_, src)
                | TraceIr::StoreIvar(src, ..)
                | TraceIr::StoreGvar { src, .. } => {
                    reg_info.use_non_float(src);
                }
                TraceIr::Neg(dst, src) => {
                    let is_float = pc.is_float1();
                    reg_info.use_as(src, is_float, pc.classid1());
                    reg_info.def_as(dst, is_float);
                }
                TraceIr::FloatBinOp { ret, mode, .. } => {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            reg_info.use_as(lhs, true, pc.classid1());
                            reg_info.use_as(rhs, true, pc.classid2());
                        }
                        OpMode::IR(_, rhs) => {
                            reg_info.use_as(rhs, true, pc.classid2());
                        }
                        OpMode::RI(lhs, _) => {
                            reg_info.use_as(lhs, true, pc.classid2());
                        }
                    }
                    reg_info.def_as(ret, true);
                }
                TraceIr::IntegerBinOp {
                    ret,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                } => {
                    reg_info.use_as(lhs, false, pc.classid1());
                    reg_info.use_as(rhs, false, pc.classid2());
                    reg_info.def_as(ret, false);
                }
                TraceIr::IntegerBinOp {
                    ret,
                    mode: OpMode::RI(lhs, _),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::RI(lhs, _),
                    ..
                } => {
                    reg_info.use_as(lhs, false, pc.classid1());
                    reg_info.def_as(ret, false);
                }
                TraceIr::IntegerBinOp {
                    ret,
                    mode: OpMode::IR(_, rhs),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::IR(_, rhs),
                    ..
                } => {
                    reg_info.use_as(rhs, false, pc.classid2());
                    reg_info.def_as(ret, false);
                }
                TraceIr::Cmp(_kind, dst, lhs, rhs, _opt) => {
                    let is_float = pc.is_float_binop();
                    reg_info.use_as(lhs, is_float, pc.classid1());
                    reg_info.use_as(rhs, is_float, pc.classid2());
                    reg_info.def_as(dst, false);
                }
                TraceIr::Cmpri(_kind, dst, lhs, _rhs, _opt) => {
                    let is_float = pc.is_float1();
                    reg_info.use_as(lhs, is_float, pc.classid1());
                    reg_info.def_as(dst, false);
                }
                TraceIr::Mov(dst, src) => {
                    reg_info.copy(dst, src);
                }
                TraceIr::ConcatStr(dst, arg, len) => {
                    for r in arg.0..arg.0 + len {
                        reg_info.use_as(SlotId(r), false, STRING_CLASS);
                    }
                    reg_info.def_as(dst, false);
                }
                TraceIr::ExpandArray(src, dst, len) => {
                    for r in dst.0..dst.0 + len {
                        reg_info.def_as(SlotId(r), false);
                    }
                    reg_info.use_as(src, false, NIL_CLASS);
                }
                TraceIr::Splat(src) => {
                    //reg_info.use_as(src, false, NIL_CLASS);
                    reg_info.def_as(src, false);
                }
                TraceIr::Yield { ret, args, len } => {
                    for i in 0..len {
                        reg_info.use_non_float(args + i);
                    }
                    reg_info.def_as(ret, false);
                }
                TraceIr::MethodCall { ret, info, .. } => {
                    let MethodInfo {
                        recv, args, len, ..
                    } = info;
                    reg_info.use_non_float(recv);
                    for i in 0..len {
                        reg_info.use_non_float(args + i);
                    }
                    for i in 1..1 + func.local_num() as u16 {
                        reg_info.unlink(SlotId(i));
                    }
                    reg_info.def_as(ret, false);
                }
                TraceIr::MethodCallBlock { ret, info, .. } => {
                    let MethodInfo {
                        recv, args, len, ..
                    } = info;
                    reg_info.use_non_float(recv);
                    for i in 0..len + 1 {
                        reg_info.use_non_float(args + i);
                    }
                    // unlink all local variables.
                    for i in 1..1 + func.local_num() as u16 {
                        reg_info.unlink(SlotId(i));
                    }
                    reg_info.def_as(ret, false);
                }
                TraceIr::MethodArgs(..) => {}
                TraceIr::InlineCall {
                    ret, method, info, ..
                } => {
                    let MethodInfo { recv, args, .. } = info;
                    match method {
                        InlineMethod::IntegerTof => {
                            reg_info.use_non_float(recv);
                            reg_info.def_as(ret, true);
                        }
                        InlineMethod::MathSqrt => {
                            reg_info.use_non_float(recv);
                            reg_info.use_as(args, true, FLOAT_CLASS);
                            reg_info.def_as(ret, true);
                        }
                        InlineMethod::MathCos => {
                            reg_info.use_non_float(recv);
                            reg_info.use_as(args, true, FLOAT_CLASS);
                            reg_info.def_as(ret, true);
                        }
                        InlineMethod::MathSin => {
                            reg_info.use_non_float(recv);
                            reg_info.use_as(args, true, FLOAT_CLASS);
                            reg_info.def_as(ret, true);
                        }
                    }
                }
                TraceIr::Ret(_ret) => {
                    self.add_return(&reg_info);
                    return None;
                }
                TraceIr::Br(disp) => {
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
                        self.add_branch(idx, reg_info, dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&reg_info);
                    }
                    return None;
                }
                TraceIr::CondBr(cond_, disp, _, _) => {
                    reg_info.use_as(cond_, false, TRUE_CLASS);
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
                        self.add_branch(idx, reg_info.clone(), dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&reg_info);
                    }
                }
                TraceIr::CheckLocal(_, disp) => {
                    let dest_idx = ((idx + 1) as i32 + disp) as usize;
                    if disp >= 0 {
                        self.add_branch(idx, reg_info.clone(), dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&reg_info);
                    }
                }
            }

            let next_idx = idx + 1;
            if self.bb_info[next_idx].is_some() {
                self.add_branch(idx, reg_info, next_idx);
                return None;
            }
        }
        unreachable!();
    }
}

#[derive(Debug, Clone)]
struct RegInfo {
    info: Vec<RegState>,
}

impl RegInfo {
    ///
    /// Extract a set of registers which will be used as Float in this loop,
    /// *and* xmm-linked on the back-edge.
    ///
    fn get_loop_used_as_float(&self) -> Vec<(SlotId, bool)> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, b)| match (b.xmm_link, b.is_used) {
                (XmmLink::R(true) | XmmLink::RW, IsUsed::Used) => Some((SlotId(i as u16), true)),
                (XmmLink::R(false), IsUsed::Used) => Some((SlotId(i as u16), false)),
                _ => None,
            })
            .collect()
    }

    fn get_unused(&self) -> Vec<SlotId> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, state)| {
                if state.is_used == IsUsed::NotUsed {
                    Some(SlotId(i as u16))
                } else {
                    None
                }
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

    fn use_as(&mut self, slot: SlotId, use_as_float: bool, class: ClassId) {
        self[slot].xmm_link = if use_as_float {
            match self[slot].xmm_link {
                XmmLink::None => XmmLink::R(class == FLOAT_CLASS),
                XmmLink::R(_) => XmmLink::R(class == FLOAT_CLASS),
                XmmLink::RW => XmmLink::RW,
            }
        } else {
            match self[slot].xmm_link {
                XmmLink::None => XmmLink::None,
                XmmLink::R(_) => XmmLink::R(class == FLOAT_CLASS),
                XmmLink::RW => XmmLink::R(true),
            }
        };
        if self[slot].is_used != IsUsed::NotUsed {
            self[slot].is_used = IsUsed::Used;
        }
    }

    fn use_non_float(&mut self, slot: SlotId) {
        self.use_as(slot, false, NIL_CLASS)
    }

    fn unlink(&mut self, slot: SlotId) {
        self[slot].xmm_link = XmmLink::None;
    }

    fn def_as(&mut self, slot: SlotId, is_float: bool) {
        if slot.is_zero() {
            return;
        }
        self[slot].xmm_link = if is_float { XmmLink::RW } else { XmmLink::None };
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::NotUsed;
        }
    }

    fn def_float_const(&mut self, slot: SlotId) {
        if slot.is_zero() {
            return;
        }
        self[slot].xmm_link = XmmLink::R(true);
        if self[slot].is_used == IsUsed::ND {
            self[slot].is_used = IsUsed::NotUsed;
        }
    }

    fn copy(&mut self, dst: SlotId, src: SlotId) {
        if self[src].is_used != IsUsed::NotUsed {
            self[src].is_used = IsUsed::Used;
        }
        if self[dst].is_used == IsUsed::ND {
            self[dst].is_used = IsUsed::NotUsed;
        }
        self[dst].xmm_link = self[src].xmm_link;
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
struct RegState {
    xmm_link: XmmLink,
    is_used: IsUsed,
}

impl RegState {
    fn new() -> Self {
        Self {
            xmm_link: XmmLink::None,
            is_used: IsUsed::ND,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum XmmLink {
    None,
    /// R(isFloat)
    R(bool),
    RW,
}

impl XmmLink {
    fn merge(&mut self, other: &Self) {
        *self = match (*self, other) {
            (XmmLink::RW, XmmLink::RW) => XmmLink::RW,
            (XmmLink::R(true), XmmLink::R(true))
            | (XmmLink::R(true), XmmLink::RW)
            | (XmmLink::RW, XmmLink::R(true)) => XmmLink::R(true),
            (_, XmmLink::R(_)) | (XmmLink::R(_), _) => XmmLink::R(false),
            _ => XmmLink::None,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum IsUsed {
    ND,
    Used,
    NotUsed,
}

impl IsUsed {
    fn merge(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used, _) | (_, IsUsed::Used) => IsUsed::Used,
            (IsUsed::NotUsed, IsUsed::NotUsed) => IsUsed::NotUsed,
            _ => IsUsed::ND,
        };
    }
}
