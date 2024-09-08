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
    ///     (or not touched)
    ///
    /// 2) Type information of a backedge branch of this loop.
    ///     (Float? not Float? or a Value which is coerced into Float?)
    ///
    pub(super) fn analyse(
        &mut self,
        func: &ISeqInfo,
        entry_bb: BasicBlockId,
    ) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        let (begin, end) = func.bb_info.get_loop(entry_bb).unwrap();

        let backedge = self.loop_backedges.get(&begin).unwrap();
        let exit = self.analyse_loop(func, begin, end).1;

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
            } else if i == 0 {
                // entry edge.
                slots.merge(SlotInfo::new(func.total_reg_num()));
            } else {
                // no incoming edge.
                continue;
            }
            let mut slots = slots.0.unwrap();
            if i != 0 {
                if let Some((bb_end, loop_exit)) = self.loop_info.get(&bb_id) {
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
                            let (end, exit) = self.loop_info.get(dst).unwrap();
                            let mut slots = slots.clone();
                            let next = *end + 1;
                            slots.concat(exit);
                            assert!(func.bb_info[*end].succ.contains(&next));
                            assert!(func.bb_info[next].pred.contains(end));
                            edges.insert(next, slots);
                        } else if bb_id < *dst {
                            // forward edge
                            edges.insert(*dst, slots.clone());
                        } else if *dst == loop_start {
                            // backedge of a current loop
                            back_edge = Some((bb_id, slots.clone()));
                        } else {
                            // backedge of an inner loop
                            unreachable!(
                                "backedge of a inner loop detected. {:?}->{:?}",
                                bb_id, dst
                            )
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
            let pc = BytecodePtr::from_bc(pc);
            let ir = pc.trace_ir(store);
            let exit_type = ir.get_exit_type();
            match ir {
                TraceIr::InitMethod { .. } => {}
                TraceIr::AliasMethod { .. } => {}
                TraceIr::MethodDef { .. } => {}
                TraceIr::SingletonMethodDef { .. } => {}
                TraceIr::LoopStart(_) => {}
                TraceIr::LoopEnd => {}
                TraceIr::EnsureEnd => {
                    info.unlink_locals(func);
                }
                TraceIr::DefinedYield { dst }
                | TraceIr::DefinedConst { dst, .. }
                | TraceIr::DefinedGvar { dst, .. }
                | TraceIr::DefinedIvar { dst, .. }
                | TraceIr::Integer(dst, ..)
                | TraceIr::Symbol(dst, ..)
                | TraceIr::Nil(dst)
                | TraceIr::Lambda { dst, .. } => {
                    info.def(dst);
                }
                TraceIr::DefinedMethod { dst, recv, .. } => {
                    info.r#use(recv);
                    info.def(dst);
                }
                TraceIr::Literal(dst, val) => {
                    if val.class() == FLOAT_CLASS {
                        info.def_as_float(dst);
                    } else {
                        info.def(dst);
                    }
                }
                TraceIr::Array { dst, callid } => {
                    let CallSiteInfo { args, pos_num, .. } = store[callid];
                    info.use_range(args, pos_num as u16);
                    info.def(dst);
                }
                TraceIr::Hash { dst, args, len } => {
                    info.use_range(args, len * 2);
                    info.def(dst);
                }
                TraceIr::Index { dst, base, idx } => {
                    info.use_slots(&[base, idx]);
                    info.def(dst);
                }
                TraceIr::Range {
                    dst, start, end, ..
                } => {
                    info.use_slots(&[start, end]);
                    info.def(dst);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    info.use_slots(&[src, base, idx]);
                }
                TraceIr::ClassDef {
                    dst,
                    base,
                    superclass,
                    ..
                } => {
                    info.r#use(superclass);
                    info.r#use(base);
                    info.def(dst);
                }
                TraceIr::ModuleDef { dst, base, .. } => {
                    info.r#use(base);
                    info.def(dst);
                }
                TraceIr::SingletonClassDef { dst, base, .. } => {
                    info.r#use(base);
                    info.def(dst);
                }
                TraceIr::LoadConst(dst, const_id) => {
                    let is_float = if let Some(value) = store[const_id].cache.cached_value {
                        value.is_float()
                    } else {
                        false
                    };
                    info.def_as(dst, is_float);
                }
                TraceIr::BlockArgProxy(dst, _)
                | TraceIr::BlockArg(dst, _)
                | TraceIr::LoadDynVar(dst, ..)
                | TraceIr::LoadIvar(dst, ..)
                | TraceIr::LoadCvar { dst, .. }
                | TraceIr::CheckCvar { dst, .. }
                | TraceIr::LoadGvar { dst, .. }
                | TraceIr::LoadSvar { dst, .. } => {
                    info.def(dst);
                }
                TraceIr::StoreConst(src, _)
                | TraceIr::StoreDynVar(_, src)
                | TraceIr::StoreIvar(src, ..)
                | TraceIr::StoreCvar { src, .. }
                | TraceIr::StoreGvar { src, .. } => {
                    info.r#use(src);
                }
                TraceIr::BitNot { dst, src } => {
                    info.r#use(src);
                    info.def(dst);
                }
                TraceIr::FUnOp { kind: _, dst, src } => {
                    info.use_as(src, true, true);
                    info.def_as(dst, true);
                }
                TraceIr::IUnOp { kind: _, dst, src } | TraceIr::UnOp { kind: _, dst, src } => {
                    info.use_as(src, false, false);
                    info.def_as(dst, false);
                }
                TraceIr::Not { dst, src } => {
                    info.r#use(src);
                    info.def(dst);
                }
                TraceIr::FBinOp { dst, mode, .. } => {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as_float(lhs, pc.is_float1());
                            info.use_as_float(rhs, pc.is_float2());
                        }
                        OpMode::RI(lhs, _) => {
                            info.use_as_float(lhs, pc.is_float1());
                        }
                        OpMode::IR(_, rhs) => {
                            info.use_as_float(rhs, pc.is_float2());
                        }
                    }
                    if let Some(ret) = dst {
                        info.def_as_float(ret);
                    }
                }
                TraceIr::IBinOp {
                    dst,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                }
                | TraceIr::BinOp {
                    dst,
                    mode: OpMode::RR(lhs, rhs),
                    ..
                } => {
                    info.use_slots(&[lhs, rhs]);
                    info.def(dst);
                }
                TraceIr::IBinOp {
                    dst,
                    mode: OpMode::RI(reg, _),
                    ..
                }
                | TraceIr::IBinOp {
                    dst,
                    mode: OpMode::IR(_, reg),
                    ..
                }
                | TraceIr::BinOp {
                    dst,
                    mode: OpMode::RI(reg, _),
                    ..
                }
                | TraceIr::BinOp {
                    dst,
                    mode: OpMode::IR(_, reg),
                    ..
                } => {
                    info.r#use(reg);
                    info.def(dst);
                }
                TraceIr::Cmp(_, dst, mode, _) => {
                    let is_float = mode.is_float_op(&pc);
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            info.use_as(lhs, is_float, pc.is_float1());
                            info.use_as(rhs, is_float, pc.is_float2());
                        }
                        OpMode::RI(lhs, _) => {
                            info.use_as(lhs, is_float, pc.is_float1());
                        }
                        _ => unreachable!(),
                    }
                    info.def(dst);
                }
                TraceIr::Mov(dst, src) => {
                    info.copy(dst, src);
                }
                TraceIr::ConcatStr(dst, args, len) => {
                    info.use_range(args, len);
                    info.def(dst);
                }
                TraceIr::ConcatRegexp(dst, args, len) => {
                    info.use_range(args, len);
                    info.def(dst);
                }
                TraceIr::ExpandArray {
                    src,
                    dst: (dst, len),
                } => {
                    info.r#use(src);
                    info.def_range(dst, len);
                }
                TraceIr::Yield { callid } => {
                    let CallSiteInfo { dst, .. } = store[callid];
                    info.use_args(&store[callid]);
                    info.def(dst);
                }
                TraceIr::MethodCall { callid, .. } | TraceIr::MethodCallBlock { callid, .. } => {
                    let has_block = store[callid].block_fid.is_some();
                    let CallSiteInfo { recv, dst, .. } = store[callid];
                    info.r#use(recv);
                    info.use_args(&store[callid]);
                    if has_block {
                        info.unlink_locals(func);
                    }
                    info.def(dst);
                }
                TraceIr::InlineCall { inline_id, callid }
                | TraceIr::InlineObjectSend { inline_id, callid }
                | TraceIr::InlineObjectSendSplat { inline_id, callid } => {
                    (store.get_inline_info(inline_id).inline_analysis)(&mut info, &store[callid]);
                }
                TraceIr::InlineCache => {}
                TraceIr::Ret(src)
                | TraceIr::MethodRet(src)
                | TraceIr::Break(src)
                | TraceIr::Raise(src) => {
                    info.r#use(src);
                }
                TraceIr::Br(_) => {}
                TraceIr::CondBr(cond_, _, _, _) => {
                    info.r#use(cond_);
                }
                TraceIr::NilBr(cond_, _) => {
                    info.r#use(cond_);
                }
                TraceIr::CheckLocal(src, _) => {
                    info.r#use(src);
                }
                TraceIr::OptCase { cond, .. } => {
                    info.r#use(cond);
                }
            }
            if let Some(exit_type) = exit_type {
                return (exit_type, info);
            }
        }
        (ExitType::Continue, info)
    }
}

#[derive(Clone)]
pub(crate) struct SlotInfo {
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
                let i = SlotId(i as u16);
                if state.used == IsUsed::Killed {
                    Some(i)
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
    /// Use `slot` as a non-Float Value.
    ///
    /// ### Arguments
    /// - `slot` - the slot to be used
    ///
    fn r#use(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            self.use_as(slot, false, false);
        }
    }

    ///
    /// Use `slots` as a non-Float Value.
    ///
    /// ### Arguments
    /// - `slots` - the slots to be used
    ///
    fn use_slots(&mut self, slots: &[SlotId]) {
        for slot in slots {
            self.r#use(*slot);
        }
    }

    fn use_range(&mut self, args: SlotId, len: u16) {
        for arg in args..(args + len) {
            self.r#use(arg);
        }
    }

    fn use_args(&mut self, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            args,
            pos_num,
            block_arg,
            kw_pos,
            ..
        } = callsite;
        self.use_range(*args, *pos_num as u16);
        self.use_range(*kw_pos, callsite.kw_len() as u16);
        if let Some(block_arg) = block_arg {
            self.r#use(*block_arg);
        }
    }

    fn def_range(&mut self, args: SlotId, len: u16) {
        for arg in args..(args + len) {
            self.def(arg);
        }
    }

    ///
    /// Use `slot` as a Float.
    ///
    /// ### Arguments
    /// - `slot` - the slot to be used.
    /// - `is_float` - whether the value in the slot is a Float or not.
    ///
    fn use_as_float(&mut self, slot: SlotId, is_float: bool) {
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

    fn def(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            self.def_as(slot, false)
        }
    }

    fn def_as_float(&mut self, slot: SlotId) {
        self.def_as(slot, true)
    }

    fn def_as(&mut self, slot: SlotId, is_float: bool) {
        if slot.is_self() {
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
pub(crate) enum ExitType {
    Continue,
    Return,
}

///
/// <Value> = <Value>.method()
///
pub(crate) fn v_v(info: &mut SlotInfo, callsite: &CallSiteInfo) {
    info.r#use(callsite.recv);
    info.use_args(callsite);
    info.def(callsite.dst);
}

///
/// <Value> = <Value>.method(<Value>)
///
pub(crate) fn v_v_v(info: &mut SlotInfo, callsite: &CallSiteInfo) {
    info.r#use(callsite.recv);
    info.use_args(callsite);
    info.def(callsite.dst);
}

///
/// <Value> = <Value>.method(<Value>, ...)
///
pub(crate) fn v_v_vv(info: &mut SlotInfo, callsite: &CallSiteInfo) {
    info.r#use(callsite.recv);
    info.use_args(callsite);
    info.def(callsite.dst);
}

///
/// <Float> = <Value>.method()
///
pub(crate) fn f_v(info: &mut SlotInfo, callsite: &CallSiteInfo) {
    info.r#use(callsite.recv);
    info.use_args(callsite);
    if let Some(ret) = callsite.dst {
        info.def_as_float(ret);
    }
}

///
/// <Float> = <Value>.method(<Float>)
///
pub(crate) fn f_v_f(info: &mut SlotInfo, callsite: &CallSiteInfo) {
    info.r#use(callsite.recv);
    info.use_as_float(callsite.args, true);
    if let Some(ret) = callsite.dst {
        info.def_as_float(ret);
    }
}
