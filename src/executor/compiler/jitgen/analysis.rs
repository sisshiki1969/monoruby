use super::*;

pub(super) struct LoopAnalysis {
    /// key: dest_idx, value Vec<(src_idx, reginfo)>
    branch_map: HashMap<BcIndex, Vec<(BcIndex, SlotInfo)>>,
    /// Basic block information
    bb_info: BasicBlockInfo,
    loop_level: usize,
    ///
    /// Merged slot information of back edge.
    ///
    backedge_info: Option<SlotInfo>,
    ///
    /// Merged slot information of return edge.
    ///
    return_info: Option<SlotInfo>,
    pc: Option<BcPc>,
}

impl LoopAnalysis {
    ///
    /// Liveness analysis for loops.
    ///
    /// this module analyze the following:
    ///
    /// 1) Will the slots of incomming branch be used? or not-used?
    /// (or not determined)
    ///
    /// 2) Type information of a backedge branch of this loop.
    /// (Float? not Float? or a Value which is coerced into Float?)
    ///
    pub(super) fn analyse(
        cc: &JitContext,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        let mut ctx = LoopAnalysis::new(cc);
        let regnum = cc.total_reg_num;
        let bb_start_vec: Vec<_> = ctx
            .bb_info
            .iter()
            .enumerate()
            .flat_map(|(idx, info)| match info {
                Some(_) => {
                    let idx = BcIndex::from(idx);
                    if idx >= bb_pos {
                        Some(idx)
                    } else {
                        None
                    }
                }
                None => None,
            })
            .collect();
        let mut exit_info = SlotInfo::new(regnum);
        for bb_pos in bb_start_vec {
            let branches = ctx.get_branches(bb_pos);
            let reg_info = if branches.is_empty() {
                SlotInfo::new(regnum)
            } else {
                let reg_info0 = branches[0].1.clone();
                branches.into_iter().fold(reg_info0, |mut acc, (_, info)| {
                    acc.merge(&info);
                    acc
                })
            };
            if let Some(info) = ctx.scan_bb(func, reg_info, bb_pos) {
                exit_info = info;
                break;
            };
        }
        let backedge_info = ctx.backedge_info.unwrap();
        exit_info.merge(&backedge_info);
        if let Some(info) = ctx.return_info {
            exit_info.merge(&info);
        }

        (
            backedge_info.get_loop_used_as_float(),
            exit_info.get_unused(),
        )
    }
}

impl LoopAnalysis {
    fn new(cc: &JitContext) -> Self {
        Self {
            branch_map: HashMap::default(),
            bb_info: cc.bb_info.clone(),
            loop_level: 0,
            backedge_info: None,
            return_info: None,
            pc: None,
        }
    }

    fn add_branch(&mut self, src_idx: BcIndex, reg_info: SlotInfo, dest_idx: BcIndex) {
        match self.branch_map.get_mut(&dest_idx) {
            Some(entry) => {
                entry.push((src_idx, reg_info));
            }
            None => {
                self.branch_map.insert(dest_idx, vec![(src_idx, reg_info)]);
            }
        }
    }

    fn get_branches(&self, dest_idx: BcIndex) -> Vec<(BcIndex, SlotInfo)> {
        match self.branch_map.get(&dest_idx) {
            Some(v) => v.clone(),
            None => vec![],
        }
    }

    fn add_backedge(&mut self, incoming_info: &SlotInfo) {
        if let Some(info) = &mut self.backedge_info {
            info.merge(incoming_info);
        } else {
            self.backedge_info = Some(incoming_info.clone());
        }
    }

    fn add_return(&mut self, incoming_info: &SlotInfo) {
        if let Some(info) = &mut self.return_info {
            info.merge(incoming_info);
        } else {
            self.return_info = Some(incoming_info.clone());
        }
    }

    ///
    /// Scan a single basic block.
    ///
    /// ### arguments
    /// - reg_info: RegInfo  incoming register info.
    ///
    /// ### return
    /// - Some(RegInfo): this bb ends with LoopEnd.
    /// - None: this bb terminates with Br, Ret, MethodRet, Break.
    fn scan_bb(
        &mut self,
        func: &ISeqInfo,
        mut info: SlotInfo,
        bb_pos: BcIndex,
    ) -> Option<SlotInfo> {
        for (ofs, pc) in func.bytecode()[bb_pos.to_usize()..].iter().enumerate() {
            self.pc = Some(BcPc::from(pc));
            let idx = bb_pos + ofs;
            match self.pc.unwrap().get_ir() {
                TraceIr::InitMethod { .. } => {}
                TraceIr::AliasMethod { new, old } => {
                    info.use_non_float(new);
                    info.use_non_float(old);
                }
                TraceIr::MethodDef { .. } => {}
                TraceIr::SingletonMethodDef { .. } => {}
                TraceIr::EnsureEnd => {
                    info.unlink_locals(func);
                }
                TraceIr::LoopStart(_) => {
                    self.loop_level += 1;
                }
                TraceIr::LoopEnd => {
                    self.loop_level -= 1;
                    if self.loop_level == 0 {
                        return Some(info);
                    }
                }
                TraceIr::DefinedYield { ret }
                | TraceIr::DefinedConst { ret, .. }
                | TraceIr::DefinedGvar { ret, .. }
                | TraceIr::DefinedIvar { ret, .. }
                | TraceIr::Integer(ret, ..)
                | TraceIr::Symbol(ret, ..)
                | TraceIr::Nil(ret) => {
                    info.def_as(ret, false);
                }
                TraceIr::DefinedMethod { ret, recv, .. } => {
                    info.def_as(ret, false);
                    info.use_non_float(recv);
                }
                TraceIr::Literal(dst, val) => {
                    if val.class() == FLOAT_CLASS {
                        info.def_float_const(dst);
                    } else {
                        info.def_as(dst, false);
                    }
                }
                TraceIr::Array { ret, args, len } => {
                    for r in args.0..args.0 + len {
                        info.use_non_float(SlotId(r));
                    }
                    info.def_as(ret, false);
                }
                TraceIr::Hash { ret, args, len } => {
                    for r in args.0..args.0 + len * 2 {
                        info.use_non_float(SlotId(r));
                    }
                    info.def_as(ret, false);
                }
                TraceIr::Index { ret, base, idx } => {
                    info.def_as(ret, false);
                    info.use_non_float(base);
                    info.use_non_float(idx);
                }
                TraceIr::Range {
                    ret, start, end, ..
                } => {
                    info.def_as(ret, false);
                    info.use_non_float(start);
                    info.use_non_float(end);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    info.use_non_float(src);
                    info.use_non_float(base);
                    info.use_non_float(idx);
                }
                TraceIr::ClassDef {
                    ret,
                    superclass: base,
                    ..
                }
                | TraceIr::SingletonClassDef { ret, base, .. } => {
                    info.use_non_float(base);
                    info.def_as(ret, false);
                }
                TraceIr::ModuleDef { ret, .. } => {
                    info.def_as(ret, false);
                }
                TraceIr::LoadConst(dst, _const_id) => {
                    let is_float = if let Some(value) = pc.value() {
                        value.class() == FLOAT_CLASS
                    } else {
                        false
                    };
                    info.def_as(dst, is_float);
                }
                TraceIr::BlockArgProxy(dst, _)
                | TraceIr::LoadDynVar(dst, ..)
                | TraceIr::LoadIvar(dst, ..)
                | TraceIr::LoadGvar { dst, .. }
                | TraceIr::LoadSvar { dst, .. } => {
                    info.def_as(dst, false);
                }
                TraceIr::StoreConst(src, _)
                | TraceIr::StoreDynVar(_, src)
                | TraceIr::StoreIvar(src, ..)
                | TraceIr::StoreGvar { src, .. } => {
                    info.use_non_float(src);
                }
                TraceIr::BitNot { ret, src } => {
                    info.use_non_float(src);
                    info.def_as(ret, false);
                }
                TraceIr::Neg { ret, src } | TraceIr::Pos { ret, src } => {
                    let is_float = pc.is_float1();
                    info.use_as(src, is_float, pc.classid1());
                    info.def_as(ret, is_float);
                }
                TraceIr::Not { ret, src } => {
                    info.use_non_float(src);
                    info.def_as(ret, false);
                }
                TraceIr::FBinOp { ret, mode, .. } => {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as(lhs, true, pc.classid1());
                            info.use_as(rhs, true, pc.classid2());
                        }
                        OpMode::IR(_, reg) | OpMode::RI(reg, _) => {
                            info.use_as(reg, true, pc.classid2());
                        }
                    }
                    info.def_as(ret, true);
                }
                TraceIr::IBinOp {
                    ret,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                } => {
                    info.use_as(lhs, false, pc.classid1());
                    info.use_as(rhs, false, pc.classid2());
                    info.def_as(ret, false);
                }
                TraceIr::IBinOp {
                    ret,
                    mode: OpMode::RI(reg, _),
                    ..
                }
                | TraceIr::IBinOp {
                    ret,
                    mode: OpMode::IR(_, reg),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::RI(reg, _),
                    ..
                }
                | TraceIr::BinOp {
                    ret,
                    mode: OpMode::IR(_, reg),
                    ..
                } => {
                    info.use_as(reg, false, pc.classid2());
                    info.def_as(ret, false);
                }
                TraceIr::Cmp(_, dst, mode, _) => {
                    let is_float = mode.is_float_op(pc);
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as(lhs, is_float, pc.classid1());
                            info.use_as(rhs, is_float, pc.classid2());
                        }
                        OpMode::RI(lhs, _) => {
                            info.use_as(lhs, is_float, pc.classid1());
                        }
                        _ => unreachable!(),
                    }
                    info.def_as(dst, false);
                }
                TraceIr::Mov(dst, src) => {
                    info.copy(dst, src);
                }
                TraceIr::ConcatStr(dst, arg, len) => {
                    for r in arg.0..arg.0 + len {
                        info.use_as(SlotId(r), false, STRING_CLASS);
                    }
                    info.def_as(dst, false);
                }
                TraceIr::ExpandArray(src, dst, len) => {
                    for r in dst.0..dst.0 + len {
                        info.def_as(SlotId(r), false);
                    }
                    info.use_as(src, false, NIL_CLASS);
                }
                TraceIr::Yield { ret, args, len, .. } => {
                    for i in 0..len {
                        info.use_non_float(args + i);
                    }
                    info.def_as(ret, false);
                }
                TraceIr::MethodCall {
                    ret,
                    info: method_info,
                    ..
                }
                | TraceIr::Super {
                    ret,
                    info: method_info,
                    ..
                } => {
                    let MethodInfo {
                        recv, args, len, ..
                    } = method_info;
                    info.use_non_float(recv);
                    for i in 0..len {
                        info.use_non_float(args + i);
                    }
                    //reg_info.unlink_locals(func);
                    info.def_as(ret, false);
                }
                TraceIr::MethodCallBlock {
                    ret,
                    info: method_info,
                    ..
                } => {
                    let MethodInfo {
                        recv, args, len, ..
                    } = method_info;
                    info.use_non_float(recv);
                    for i in 0..len + 1 {
                        info.use_non_float(args + i);
                    }
                    info.unlink_locals(func);
                    info.def_as(ret, false);
                }
                TraceIr::MethodArgs(..) => {}
                TraceIr::InlineCall {
                    ret,
                    method,
                    info: method_info,
                    ..
                } => {
                    let MethodInfo { recv, args, .. } = method_info;
                    match method {
                        InlineMethod::IntegerTof => {
                            info.use_non_float(recv);
                            info.def_as(ret, true);
                        }
                        InlineMethod::MathSqrt => {
                            info.use_non_float(recv);
                            info.use_as(args, true, FLOAT_CLASS);
                            info.def_as(ret, true);
                        }
                        InlineMethod::MathCos => {
                            info.use_non_float(recv);
                            info.use_as(args, true, FLOAT_CLASS);
                            info.def_as(ret, true);
                        }
                        InlineMethod::MathSin => {
                            info.use_non_float(recv);
                            info.use_as(args, true, FLOAT_CLASS);
                            info.def_as(ret, true);
                        }
                        InlineMethod::ObjectNil => {
                            info.use_non_float(recv);
                            info.def_as(ret, false);
                        }
                    }
                }
                TraceIr::Ret(_ret) | TraceIr::MethodRet(_ret) | TraceIr::Break(_ret) => {
                    self.add_return(&info);
                    return None;
                }
                TraceIr::Br(disp) => {
                    let dest_idx = idx + 1 + disp;
                    if disp >= 0 {
                        self.add_branch(idx, info, dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&info);
                    }
                    return None;
                }
                TraceIr::CondBr(cond_, disp, _, _) => {
                    info.use_as(cond_, false, TRUE_CLASS);
                    let dest_idx = idx + 1 + disp;
                    if disp >= 0 {
                        self.add_branch(idx, info.clone(), dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&info);
                    }
                }
                TraceIr::CheckLocal(src, disp) => {
                    info.use_non_float(src);
                    let dest_idx = idx + 1 + disp;
                    if disp >= 0 {
                        self.add_branch(idx, info.clone(), dest_idx);
                    } else if self.loop_level == 1 {
                        self.add_backedge(&info);
                    }
                }
            }

            let next_idx = idx + 1;
            if self.bb_info[next_idx].is_some() {
                self.add_branch(idx, info, next_idx);
                return None;
            }
        }
        unreachable!();
    }
}

#[derive(Debug, Clone)]
struct SlotInfo {
    info: Vec<State>,
}

impl SlotInfo {
    ///
    /// Extract a set of registers which will be used as Float in this loop,
    /// *and* xmm-linked on the back-edge.
    ///
    fn get_loop_used_as_float(&self) -> Vec<(SlotId, bool)> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, b)| match (b.ty, b.used) {
                (Ty::Float, _) => Some((SlotId(i as u16), true)),
                (Ty::Both, _) => Some((SlotId(i as u16), false)),
                _ => None,
            })
            .collect()
    }

    ///
    /// Extract unsed slots.
    ///
    fn get_unused(&self) -> Vec<SlotId> {
        self.info
            .iter()
            .enumerate()
            .flat_map(|(i, state)| {
                if state.used == IsUsed::Killed {
                    Some(SlotId(i as u16))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl SlotInfo {
    fn new(reg_num: usize) -> Self {
        Self {
            info: vec![State::new(); reg_num],
        }
    }

    fn merge(&mut self, other: &Self) {
        for (i, detail) in &mut self.info.iter_mut().enumerate() {
            detail.ty.merge(&other[i].ty);
            detail.used.merge(&other[i].used);
        }
    }

    fn use_as(&mut self, slot: SlotId, use_as_float: bool, class: ClassId) {
        self[slot].ty = if use_as_float {
            if class == FLOAT_CLASS {
                match self[slot].ty {
                    Ty::Val | Ty::Both => Ty::Both,
                    Ty::Float => Ty::Float,
                }
            } else {
                match self[slot].ty {
                    Ty::Val => Ty::Val,
                    Ty::Both => Ty::Both,
                    Ty::Float => Ty::Both,
                }
            }
        } else {
            match self[slot].ty {
                Ty::Val => Ty::Val,
                Ty::Both | Ty::Float => Ty::Both,
            }
        };
        self.use_(slot);
    }

    fn use_non_float(&mut self, slot: SlotId) {
        self.use_as(slot, false, NIL_CLASS)
    }

    fn unlink(&mut self, slot: SlotId) {
        self[slot].ty = Ty::Val;
    }

    fn unlink_locals(&mut self, func: &ISeqInfo) {
        for i in 1..1 + func.local_num() as u16 {
            self.unlink(SlotId(i));
        }
    }

    fn def_as(&mut self, slot: SlotId, is_float: bool) {
        if slot.is_zero() {
            return;
        }
        self[slot].ty = if is_float { Ty::Float } else { Ty::Val };
        self.def_(slot);
    }

    fn def_float_const(&mut self, slot: SlotId) {
        if slot.is_zero() {
            return;
        }
        self[slot].ty = Ty::Float;
        self.def_(slot);
    }

    fn copy(&mut self, dst: SlotId, src: SlotId) {
        self.use_(src);
        self.def_(dst);
        self[dst].ty = self[src].ty;
    }

    fn use_(&mut self, slot: SlotId) {
        if self[slot].used != IsUsed::Killed {
            self[slot].used = IsUsed::Used;
        }
    }

    fn def_(&mut self, slot: SlotId) {
        if self[slot].used == IsUsed::ND {
            self[slot].used = IsUsed::Killed;
        }
    }
}

impl std::ops::Index<SlotId> for SlotInfo {
    type Output = State;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.info[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for SlotInfo {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.info[i.0 as usize]
    }
}

impl std::ops::Index<usize> for SlotInfo {
    type Output = State;
    fn index(&self, i: usize) -> &Self::Output {
        &self.info[i]
    }
}

impl std::ops::IndexMut<usize> for SlotInfo {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.info[i]
    }
}

///
/// The state of slots.
///
#[derive(Debug, Clone)]
struct State {
    /// Type information for a backedge branch.
    ty: Ty,
    /// Whether the slot is used or not.
    used: IsUsed,
}

impl State {
    fn new() -> Self {
        Self {
            ty: Ty::Val,
            used: IsUsed::ND,
        }
    }
}

///
/// LType status of slots.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum Ty {
    ///
    /// Value
    ///
    /// the slot is defined as non-f64.
    ///
    Val,
    ///
    /// Both
    ///
    /// the slot is used as f64 with coercion.
    ///
    Both,
    ///
    /// Float
    ///
    /// the slot is defined as f64 or used as f64 without coercion.
    ///
    Float,
}

impl Ty {
    fn merge(&mut self, other: &Self) {
        *self = match (*self, other) {
            (_, Ty::Val) | (Ty::Val, _) => Ty::Val,
            (_, Ty::Both) | (Ty::Both, _) => Ty::Both,
            (Ty::Float, Ty::Float) => Ty::Float,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum IsUsed {
    ///
    /// Not be used nor be killed.
    ///
    ND,
    ///
    /// Used in any of paths.
    ///
    Used,
    ///
    /// Guaranteed not to be used (= killed) in all paths.
    ///
    Killed,
}

impl IsUsed {
    fn merge(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used, _) | (_, IsUsed::Used) => IsUsed::Used,
            (IsUsed::Killed, IsUsed::Killed) => IsUsed::Killed,
            _ => IsUsed::ND,
        };
    }
}
