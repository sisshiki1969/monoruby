use super::*;

#[derive(Default)]
struct Edges(HashMap<BasicBlockId, Vec<SlotInfo>>);

impl Edges {
    fn insert(&mut self, dst_bb: BasicBlockId, info: SlotInfo) {
        match self.0.get_mut(&dst_bb) {
            Some(entry) => entry.push(info),
            None => {
                self.0.insert(dst_bb, vec![info]);
            }
        }
    }

    fn remove(&mut self, bb: &BasicBlockId) -> Option<Vec<SlotInfo>> {
        self.0.remove(bb)
    }
}

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
    pub(super) fn analyse(
        &mut self,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        let entry_bb = func.bb_info.get_bb_id(bb_pos);
        let (begin, _end) = func.bb_info.get_loop(entry_bb).unwrap();

        let backedge = self.loop_backedges.get(&begin).unwrap();
        let last = BasicBlockId(func.bb_info.len() - 1);
        let exit = self.analyse_loop(func, begin, last).1;

        (backedge.get_loop_used_as_float(), exit.get_unused())
    }

    pub(super) fn analyse_loop(
        &self,
        func: &ISeqInfo,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) -> (SlotInfo, SlotInfo) {
        let mut return_edge = SlotMerger::new();
        let mut back_edge = None;
        let mut exit_edge = None;
        let mut edges = Edges::default();
        let mut nest_loop = None;
        for (i, (ty, info)) in self.bb_scan[loop_start.0..=loop_end.0].iter().enumerate() {
            let bb_id = loop_start + i;
            let succ = &func.bb_info[bb_id].succ;
            if let Some(end) = &nest_loop {
                // In inner loops, we can skip scanning. (shortcut)
                if bb_id == *end {
                    // When reached loop's end, go next.
                    nest_loop = None;
                }
                continue;
            }
            let mut slots = SlotMerger::new();
            if let Some(src) = edges.remove(&bb_id) {
                for info in src {
                    slots.merge(info);
                }
            } else {
                if i == 0 {
                    // entry edge.
                    slots.merge(SlotInfo::new(func.total_reg_num()));
                } else {
                    // no incoming edge.
                    continue;
                }
            }
            let mut slots = slots.0.unwrap();
            if i != 0 {
                if let Some((bb_end, loop_exit)) = self.loop_exit.get(&bb_id) {
                    // if reached another loop's beginning, use shortcut.
                    slots.concat(loop_exit);
                    nest_loop = Some(*bb_end);
                    let next_bb = *bb_end + 1;
                    assert!(func.bb_info[*bb_end].succ.contains(&next_bb));
                    assert!(func.bb_info[next_bb].pred.contains(bb_end));
                    edges.insert(next_bb, slots);
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
                            let (end, exit) = self.loop_exit.get(dst).unwrap();
                            let mut slots = slots.clone();
                            let next = *end + 1;
                            slots.concat(exit);
                            assert!(func.bb_info[*end].succ.contains(&next));
                            assert!(func.bb_info[next].pred.contains(&end));
                            edges.insert(next, slots);
                        } else if bb_id < *dst {
                            // forward edge
                            edges.insert(*dst, slots.clone());
                        } else {
                            if *dst == loop_start {
                                // backedge of a current loop
                                back_edge = Some((bb_id, slots.clone()));
                            } else {
                                // backedge of an inner loop
                                unreachable!(
                                    "backedge of a inner loop detected. {:?}->{:?}",
                                    bb_id, dst
                                )
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
        assert!(edges.0.is_empty());

        if let Some((_, slots)) = &back_edge {
            return_edge.merge(slots.clone());
        }
        if let Some((_, slots)) = exit_edge {
            return_edge.merge(slots.clone());
        }
        let exit = match return_edge.0 {
            Some(slots) => slots.clone(),
            None => SlotInfo::new(func.total_reg_num()),
        };

        let backedge = match back_edge {
            Some((_, slots)) => slots,
            None => SlotInfo::new(func.total_reg_num()),
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
    pub(super) fn scan_bb(
        func: &ISeqInfo,
        store: &Store,
        entry: &BasciBlockInfoEntry,
    ) -> (ExitType, SlotInfo) {
        let mut info = SlotInfo::new(func.total_reg_num());
        let BasciBlockInfoEntry { begin, end, .. } = entry;
        for pc in func.bytecode()[begin.to_usize()..=end.to_usize()].iter() {
            let pc = BcPc::from(pc);
            match pc.get_ir() {
                TraceIr::InitMethod { .. } => {}
                TraceIr::AliasMethod { new, old } => {
                    info.r#use(new);
                    info.r#use(old);
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
                    info.def(ret);
                }
                TraceIr::DefinedMethod { ret, recv, .. } => {
                    info.def(ret);
                    info.r#use(recv);
                }
                TraceIr::Literal(dst, val) => {
                    if val.class() == FLOAT_CLASS {
                        info.def_as_float(dst);
                    } else {
                        info.def(dst);
                    }
                }
                TraceIr::Array { ret, callid } => {
                    let CallSiteInfo { args, len, .. } = store[callid];
                    info.use_range(args, len);
                    info.def(ret);
                }
                TraceIr::Hash { ret, args, len } => {
                    info.use_range(args, len * 2);
                    info.def(ret);
                }
                TraceIr::Index { ret, base, idx } => {
                    info.def(ret);
                    info.r#use(base);
                    info.r#use(idx);
                }
                TraceIr::Range {
                    ret, start, end, ..
                } => {
                    info.def(ret);
                    info.r#use(start);
                    info.r#use(end);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    info.r#use(src);
                    info.r#use(base);
                    info.r#use(idx);
                }
                TraceIr::ClassDef {
                    ret,
                    superclass: base,
                    ..
                }
                | TraceIr::SingletonClassDef { ret, base, .. } => {
                    info.r#use(base);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::ModuleDef { ret, .. } => {
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
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
                | TraceIr::BlockArg(dst, _)
                | TraceIr::LoadDynVar(dst, ..)
                | TraceIr::LoadIvar(dst, ..)
                | TraceIr::LoadGvar { dst, .. }
                | TraceIr::LoadSvar { dst, .. } => {
                    info.def(dst);
                }
                TraceIr::StoreConst(src, _)
                | TraceIr::StoreDynVar(_, src)
                | TraceIr::StoreIvar(src, ..)
                | TraceIr::StoreGvar { src, .. } => {
                    info.r#use(src);
                }
                TraceIr::BitNot { ret, src } => {
                    info.r#use(src);
                    info.def(ret);
                }
                TraceIr::Neg { ret, src } | TraceIr::Pos { ret, src } => {
                    let is_float = pc.is_float1();
                    info.use_as(src, is_float, pc.classid1() == FLOAT_CLASS);
                    info.def_as(ret, is_float);
                }
                TraceIr::Not { ret, src } => {
                    info.r#use(src);
                    info.def(ret);
                }
                TraceIr::FBinOp { ret, mode, .. } => {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as_float(lhs, pc.classid1() == FLOAT_CLASS);
                            info.use_as_float(rhs, pc.classid2() == FLOAT_CLASS);
                        }
                        OpMode::IR(_, reg) | OpMode::RI(reg, _) => {
                            info.use_as_float(reg, pc.classid2() == FLOAT_CLASS);
                        }
                    }
                    info.def_as_float(ret);
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
                    info.r#use(lhs);
                    info.r#use(rhs);
                    info.def(ret);
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
                    info.r#use(reg);
                    info.def(ret);
                }
                TraceIr::Cmp(_, dst, mode, _) => {
                    let is_float = mode.is_float_op(&pc);
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as(lhs, is_float, pc.classid1() == FLOAT_CLASS);
                            info.use_as(rhs, is_float, pc.classid2() == FLOAT_CLASS);
                        }
                        OpMode::RI(lhs, _) => {
                            info.use_as(lhs, is_float, pc.classid1() == FLOAT_CLASS);
                        }
                        _ => unreachable!(),
                    }
                    info.def(dst);
                }
                TraceIr::Mov(dst, src) => {
                    info.copy(dst, src);
                }
                TraceIr::ConcatStr(ret, args, len) => {
                    info.use_range(args, len);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::ConcatRegexp(ret, args, len) => {
                    info.use_range(args, len);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::ExpandArray(src, dst, len) => {
                    info.use_range(dst, len);
                    info.r#use(src);
                }
                TraceIr::Yield { ret, args, len, .. } => {
                    info.use_range(args, len);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::MethodCall { callid, .. } | TraceIr::Super { callid, .. } => {
                    let CallSiteInfo {
                        recv,
                        args,
                        len,
                        ret,
                        ..
                    } = store[callid];
                    info.r#use(recv);
                    info.use_range(args, len);
                    //reg_info.unlink_locals(func);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::MethodCallBlock { callid, .. } => {
                    let CallSiteInfo {
                        recv,
                        args,
                        len,
                        ret,
                        ..
                    } = store[callid];
                    info.r#use(recv);
                    info.use_range(args, len + 1);
                    info.unlink_locals(func);
                    if let Some(ret) = ret {
                        info.def(ret);
                    }
                }
                TraceIr::MethodArgs(..) => {}
                TraceIr::InlineCall {
                    inline_id,
                    callsite,
                    ..
                } => {
                    store.get_inline_info(inline_id).1(&mut info, &store[callsite]);
                }
                TraceIr::Ret(src)
                | TraceIr::MethodRet(src)
                | TraceIr::Break(src)
                | TraceIr::Raise(src) => {
                    info.r#use(src);
                    return (ExitType::Return, info);
                }
                TraceIr::Br(_) => {
                    return (ExitType::Continue, info);
                }
                TraceIr::CondBr(cond_, _, _, _) => {
                    info.r#use(cond_);
                    return (ExitType::Continue, info);
                }
                TraceIr::CheckLocal(src, _) => {
                    info.r#use(src);
                    return (ExitType::Continue, info);
                }
            }
        }
        (ExitType::Continue, info)
    }
}

#[derive(Clone)]
pub(in crate::executor) struct SlotInfo {
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

    ///
    /// Use `slot` as a Value.
    ///
    /// ### Arguments
    /// - `slot` - the slot to be used.
    ///
    pub(crate) fn r#use(&mut self, slot: SlotId) {
        self.use_as(slot, false, false);
    }

    pub(crate) fn use_range(&mut self, args: SlotId, len: u16) {
        for arg in args..(args + len) {
            self.r#use(arg);
        }
    }

    ///
    /// Use `slot` as a Float.
    ///
    /// ### Arguments
    /// - `slot` - the slot to be used.
    /// - `is_float` - whether the value in the slot is a Float or not.
    ///
    pub(crate) fn use_as_float(&mut self, slot: SlotId, is_float: bool) {
        self.use_as(slot, true, is_float);
    }

    ///
    /// Use `slot`.
    ///
    /// ### Arguments
    /// - `slot` - the slot to be used.
    /// - `use_as_float` - whether the slot is to be used as a Float or not.
    /// - `is_float` - whether the value in the slot is a Float or not.
    ///
    fn use_as(&mut self, slot: SlotId, use_as_float: bool, is_float: bool) {
        self[slot].ty = if use_as_float {
            if is_float {
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

    fn unlink(&mut self, slot: SlotId) {
        self[slot].ty = Ty::Val;
    }

    fn unlink_locals(&mut self, func: &ISeqInfo) {
        for i in 1..1 + func.local_num() as u16 {
            self.unlink(SlotId(i));
        }
    }

    pub(crate) fn def(&mut self, slot: SlotId) {
        self.def_as(slot, false)
    }

    pub(crate) fn def_as_float(&mut self, slot: SlotId) {
        self.def_as(slot, true)
    }

    pub(crate) fn def_as(&mut self, slot: SlotId, is_float: bool) {
        if slot.is_zero() {
            return;
        }
        self[slot].ty = if is_float { Ty::Float } else { Ty::Val };
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
pub(crate) struct State {
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
