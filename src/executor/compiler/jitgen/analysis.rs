use super::*;

impl JitContext {
    ///
    /// Liveness analysis for loops.
    ///
    /// this module analyze the following:
    ///
    /// 1) Will the slots of incomming branch be used? or not-used?
    /// (or not touched)
    ///
    /// 2) Type information of a backedge branch of this loop.
    /// (Float? not Float? or a Value which is coerced into Float?)
    ///
    pub(super) fn analyse(&mut self, bb_pos: BcIndex) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        let entry_bb = self.bb_info.get_bb_id(bb_pos);
        let (begin, _) = self.bb_info.get_loop(entry_bb).unwrap();

        let backedge = self.loop_backedges.get(&begin).unwrap();
        let exit = &self.loop_exit.get(&begin).unwrap().1;

        (backedge.get_loop_used_as_float(), exit.get_unused())
    }

    pub(super) fn initialize_loop_info(&mut self) {
        for (loop_start, loop_end) in self.bb_info.loops.clone() {
            let (backedge, exit) = self.analyse_loop(loop_start, loop_end);
            self.loop_backedges.insert(loop_start, backedge);
            self.loop_exit.insert(loop_start, (loop_end, exit));
        }
    }

    fn analyse_loop(
        &self,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> (SlotInfo, SlotInfo) {
        let mut return_edge = SlotMerger::new();
        let mut back_edge = None;
        let mut exit_edge = None;
        let mut edges = HashMap::default();
        let mut nest_loop = None;
        for (i, (ty, info)) in self.bb_scan[loop_start.0..=loop_end.0].iter().enumerate() {
            let bb_id = loop_start + i;
            let mut slots = SlotMerger::new();
            let BasciBlockInfoEntry { pred, succ, .. } = &self.bb_info[bb_id];
            if let Some((end, _)) = &nest_loop {
                // In inner loops, we can skip scanning.
                if bb_id == *end {
                    // When reached end, make an exit edge to a successor basic block.
                    let info = std::mem::take(&mut nest_loop).unwrap().1;
                    let next_bb = bb_id + 1;
                    assert!(self.bb_info[bb_id].succ.contains(&next_bb));
                    assert!(self.bb_info[next_bb].pred.contains(&bb_id));
                    edges.insert((bb_id, next_bb), info);
                }
                continue;
            }
            if pred.is_empty() {
                // no predecessor.
                continue;
            }
            if i == 0 {
                assert_eq!(2, pred.len());
            }
            for src in pred {
                if *src < loop_start {
                    // entry edge
                    assert_eq!(0, i);
                    slots.merge(SlotInfo::new(self.total_reg_num));
                } else if (loop_start..=loop_end).contains(src) {
                    // inner
                    if bb_id <= *src {
                        // back edge
                        slots.merge(SlotInfo::new(self.total_reg_num));
                    } else if let Some(edge) = edges.remove(&(*src, bb_id)) {
                        // forward edge
                        slots.merge(edge);
                    } else {
                        // no information for an incoming edge
                    };
                } else {
                    unreachable!()
                }
            }
            let mut slots = if let Some(slots) = slots.0 {
                slots
            } else {
                // no incoming edge
                continue;
            };
            if i != 0 {
                if let Some((bb_end, loop_exit)) = self.loop_exit.get(&bb_id) {
                    slots.concat(loop_exit);
                    nest_loop = Some((*bb_end, slots.clone()));
                    continue;
                }
            }
            slots.concat(info);
            match ty {
                ExitType::Continue => {
                    for dst in succ {
                        if loop_end < *dst {
                            // exit edge
                            assert_eq!(bb_id, loop_end);
                            assert!(exit_edge.is_none());
                            exit_edge = Some((bb_id, slots.clone()));
                        } else if *dst < loop_start {
                            // backedge of an outer loop
                            // unreachable in "loop mode"
                        } else if bb_id < *dst {
                            // forward edge
                            edges.insert((bb_id, *dst), slots.clone());
                        } else {
                            if *dst == loop_start {
                                // backedge of a current loop
                                back_edge = Some((bb_id, slots.clone()));
                            } else {
                                // backedge of an inner loop
                                unreachable!("detect backedge of a inner loop.")
                            };
                        }
                    }
                }
                ExitType::Return => {
                    assert!(succ.is_empty());
                    let mut info = info.clone();
                    info.kill();
                    return_edge.merge(info);
                }
            }
        }
        if let Some((_, slots)) = &back_edge {
            return_edge.merge(slots.clone());
        }
        if let Some((_, slots)) = exit_edge {
            return_edge.merge(slots.clone());
        }
        let exit = match return_edge.0 {
            Some(slots) => slots.clone(),
            None => SlotInfo::new(self.total_reg_num),
        };

        let backedge = match back_edge {
            Some((_, slots)) => slots,
            None => SlotInfo::new(self.total_reg_num),
        };
        (backedge, exit)
    }
}

#[derive(Debug)]
struct SlotMerger(Option<SlotInfo>);

impl SlotMerger {
    fn new() -> Self {
        Self(None)
    }

    fn merge(&mut self, other: SlotInfo) {
        if let Some(me) = &mut self.0 {
            me.merge(&other);
        } else {
            *self = Self(Some(other));
        }
    }
}

impl JitContext {
    ///
    /// Scan a single basic block.
    ///
    pub(super) fn scan_bb(func: &ISeqInfo, entry: &BasciBlockInfoEntry) -> (ExitType, SlotInfo) {
        let mut info = SlotInfo::new(func.total_reg_num());
        let BasciBlockInfoEntry { begin, end, .. } = entry;
        for pc in func.bytecode()[begin.to_usize()..=end.to_usize()].iter() {
            let pc = BcPc::from(pc);
            match pc.get_ir() {
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
                TraceIr::LoopStart(_) => {}
                TraceIr::LoopEnd => {}
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
                    let is_float = mode.is_float_op(&pc);
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
                TraceIr::Ret(ret) | TraceIr::MethodRet(ret) | TraceIr::Break(ret) => {
                    info.def_as(ret, false);
                    return (ExitType::Return, info);
                }
                TraceIr::Br(_) => {
                    return (ExitType::Continue, info);
                }
                TraceIr::CondBr(cond_, _, _, _) => {
                    info.use_as(cond_, false, TRUE_CLASS);
                    return (ExitType::Continue, info);
                }
                TraceIr::CheckLocal(src, _) => {
                    info.use_non_float(src);
                    return (ExitType::Continue, info);
                }
            }
        }
        (ExitType::Continue, info)
    }
}

#[derive(Clone)]
pub(super) struct SlotInfo {
    info: Vec<State>,
}

impl std::fmt::Debug for SlotInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SlotInfo {{ ")?;
        for (i, s) in self.info.iter().enumerate() {
            if s.ty != Ty::Val || s.used != IsUsed::ND {
                write!(f, "%{i}:{:?} ", s)?;
            }
        }
        write!(f, "}}")
    }
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

    fn concat(&mut self, other: &Self) {
        for (i, detail) in &mut self.info.iter_mut().enumerate() {
            if other[i].used != IsUsed::ND {
                detail.ty = other[i].ty;
            }
            detail.used.concat(&other[i].used);
        }
    }

    fn kill(&mut self) {
        for detail in &mut self.info.iter_mut() {
            detail.used.kill();
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
#[derive(Clone)]
pub(super) struct State {
    /// Type information for a backedge branch.
    ty: Ty,
    /// Whether the slot is used or not.
    used: IsUsed,
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ty == Ty::Val && self.used == IsUsed::ND {
            write!(f, "[]")
        } else {
            write!(f, "[{:?}/{:?}]", self.ty, self.used)
        }
    }
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

    fn concat(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used, _) => IsUsed::Used,
            (IsUsed::Killed, _) => IsUsed::Killed,
            (IsUsed::ND, IsUsed::Used) => IsUsed::Used,
            (IsUsed::ND, IsUsed::Killed) => IsUsed::Killed,
            _ => IsUsed::ND,
        };
    }

    fn kill(&mut self) {
        *self = match &self {
            IsUsed::Used => IsUsed::Used,
            _ => IsUsed::Killed,
        };
    }
}

#[derive(Debug)]
pub(super) enum ExitType {
    Continue,
    Return,
}
