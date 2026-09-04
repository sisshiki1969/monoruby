//! Stage 1 of frame merging: can this callee run inside its caller's frame?
//!
//! [`frameless`](super::frameless) expands a callee into the caller's
//! *registers*. That caps it at bodies with no locals, no branches and one
//! accumulator. Merging instead gives the callee a **slot window inside the
//! caller's frame**, so locals, joins and loops all have somewhere to live —
//! while still pushing no frame, making no call, and running no prologue.
//!
//! # What the caller's frame has to say about the window
//!
//! The GC finds a frame's live slots by reading `reg_num` out of the frame's
//! own `LFP_META` word (`Lfp::mark_contents` iterates `meta.regs()`), so a
//! merged window is scanned exactly when the caller's `reg_num` covers it.
//! That is the whole of the GC story — the window is *inside* a frame that is
//! on the control-frame chain, not off it.
//!
//! # Why this stage forbids calls
//!
//! Eliding a frame makes the control-frame chain lie: a walk sees the
//! caller where the callee should be. Everything that consults the chain is
//! affected, not just `$~` — `caller`, `binding`, `__method__`,
//! backtraces, `super`. Marking the methods that would notice is a
//! blacklist over the whole Ruby method space, and a builtin added later
//! without the mark breaks it silently.
//!
//! So this stage takes the property that makes marking unnecessary:
//! **with no call in the body, nothing can observe the chain while the
//! window is live.** The GC walks it, but only wants the caller's slots,
//! which `reg_num` already covers. A guard's side exit hands the whole call
//! back to the interpreter before anything has happened. Nothing else runs.
//!
//! Admitting calls (stage 2) needs a real answer — a check at `Cfp::prev`,
//! the one door every chain walk goes through — and that is deliberately
//! not this stage's problem.
use super::*;

/// What a mergeable callee needs from its caller.
#[derive(Debug, Clone, Copy)]
pub(super) struct MergeInfo {
    /// Slots the callee needs — the size of the window to carve out of the
    /// caller's frame, and what its `reg_num` must grow by.
    pub reg_num: u16,
    /// Basic blocks in the body. One is what `frameless` already handles;
    /// more is what merging buys.
    pub bb_num: usize,
}

/// The most slots a callee may ask for. Every call site that merges this
/// body grows the caller's frame — and its GC scan range — by this much.
const MAX_WINDOW: u16 = 16;

///
/// Decide whether `iseq_id` can run inside its caller's frame.
///
/// The instruction test is an **allowlist**: anything not named here
/// declines. That is the same reasoning as the module doc's — a denylist
/// over an open set silently admits whatever is added to it next — and it
/// costs nothing here, since the point of the stage is a small, well-understood
/// body.
///
pub(super) fn mergeable_body(store: &Store, iseq_id: ISeqId) -> Option<MergeInfo> {
    mergeable_body_at(store, iseq_id, false)
}

///
/// The survey's upper bound: what would qualify if calls were admitted —
/// i.e. if stage 2 solved the control-frame-chain lie and the deopt
/// reconstruction that a guard *after* an effect needs. Only the frame
/// conditions and the constructs that can never be merged (globals, `eval`,
/// `defined?`, definitions, exception control flow) are still checked.
///
pub(super) fn mergeable_body_calls_ok(store: &Store, iseq_id: ISeqId) -> Option<MergeInfo> {
    mergeable_body_at(store, iseq_id, true)
}

/// Survey instrumentation: why a body was declined.
pub(super) fn decline_reason(store: &Store, iseq_id: ISeqId, calls_ok: bool) -> &'static str {
    let iseq = &store[iseq_id];
    if iseq.outer.is_some() {
        return "outer(block)";
    }
    let func = &store[iseq.func_id()];
    if !calls_ok && !func.meta().is_simple() {
        return "not-simple(params)";
    }
    if iseq.block_param().is_some() {
        return "block-param";
    }
    let reg_num = iseq.total_reg_num();
    if reg_num == 0 || reg_num > MAX_WINDOW as usize {
        return "window-too-big";
    }
    for bbid in 0..iseq.bb_info.len() {
        let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(bbid)];
        for idx in begin..=end {
            let ir = TraceIr::from_pc(iseq.get_pc(idx), store);
            if classify(ir, calls_ok).is_none() {
                return opcode_name(TraceIr::from_pc(iseq.get_pc(idx), store));
            }
        }
    }
    "ordering(guard-after-effect)"
}

fn opcode_name(ir: TraceIr) -> &'static str {
    use TraceIr::*;
    match ir {
        MethodCall { .. } => "call",
        Yield { .. } => "yield",
        LoadGvar { .. } | StoreGvar { .. } | DefinedGvar { .. } | AliasGvar { .. } => "gvar",
        LoadConst(..) | StoreConst(..) | DefinedConst { .. } => "const",
        LoadCvar { .. } | StoreCvar { .. } | CheckCvar { .. } | DefinedCvar { .. } => "cvar",
        LoadDynVar(..) | StoreDynVar(..) => "dynvar",
        Index { .. } | IndexAssign { .. } => "index",
        Array { .. } | ArrayAny { .. } | ArrayConcat { .. } | Hash { .. } | HashInsert { .. }
        | Range { .. } | Lambda | ConcatStr { .. } | ConcatRegexp { .. } => "literal-alloc",
        MethodDef { .. } | SingletonMethodDef { .. } | ClassDef { .. } | ModuleDef { .. }
        | SingletonClassDef { .. } | AliasMethod { .. } | UndefMethod { .. } => "definition",
        Raise(..) | EnsureEnd | Redo | Retry | MethodRet(..) | BlockBreak(..) => "control",
        BlockArg(..) | BlockArgProxy(..) => "block-arg",
        OptCase { .. } => "optcase",
        ExpandArray { .. } | ToA { .. } | ArrayTEq { .. } => "array-op",
        DefinedMethod { .. } | DefinedSuper { .. } | DefinedYield { .. } | DefinedIvar { .. } => {
            "defined?"
        }
        StringFreeze { .. } => "string-freeze",
        InlineCache { .. } => "inline-cache",
        _ => "other",
    }
}

fn mergeable_body_at(store: &Store, iseq_id: ISeqId, calls_ok: bool) -> Option<MergeInfo> {
    let iseq = &store[iseq_id];
    // An `outer` means the body reads locals through the frame chain, which
    // is exactly what merging perturbs.
    if iseq.outer.is_some() {
        return None;
    }
    let func = &store[iseq.func_id()];
    // `is_simple` (plain positional parameters only) is a *register*-form
    // restriction: without a window there is nowhere to bind an optional or
    // keyword argument. A merged callee has a window, so the survey's upper
    // bound drops it; stage 1 keeps it because binding is its own work.
    if (!calls_ok && !func.meta().is_simple()) || iseq.block_param().is_some() {
        return None;
    }
    let reg_num = iseq.total_reg_num();
    if reg_num == 0 || reg_num > MAX_WINDOW as usize {
        return None;
    }

    // `effect_bbs[b]` — an effect happens in block `b`. `guard_bbs[b]` — a
    // guarding instruction does. The rule is checked over the CFG below.
    let bb_num = iseq.bb_info.len();
    let mut effect_bbs = vec![false; bb_num];
    let mut guard_bbs = vec![false; bb_num];
    // Within one block the order is linear, so a guard that *follows* an
    // effect in the same block fails immediately.
    for bbid in 0..bb_num {
        let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(bbid)];
        let mut seen_effect = false;
        for idx in begin..=end {
            let (guards, effects) = classify(TraceIr::from_pc(iseq.get_pc(idx), store), calls_ok)?;
            if guards {
                // With calls admitted the ordering rule no longer holds
                // (stage 2 pays for that with deopt reconstruction), so the
                // survey's upper bound does not enforce it.
                if seen_effect && !calls_ok {
                    return None;
                }
                guard_bbs[bbid] = true;
            }
            if effects {
                seen_effect = true;
                effect_bbs[bbid] = true;
            }
        }
    }
    // Across blocks: no guard may be *reachable from* an effect. A loop
    // carrying both fails here, via the back edge.
    if !calls_ok && reaches_guard_from_effect(iseq, &effect_bbs, &guard_bbs) {
        return None;
    }

    Some(MergeInfo {
        reg_num: reg_num as u16,
        bb_num,
    })
}

///
/// `(can side-exit, has an effect)` for one instruction, or `None` when the
/// instruction is not on the allowlist.
///
/// "Effect" is anything observable after a side exit hands the call back:
/// an ivar store is one, writing a local is not (the window is discarded).
///
fn classify(ir: TraceIr, calls_ok: bool) -> Option<(bool, bool)> {
    use TraceIr::*;
    Some(match ir {
        // Prologue and control flow. `CondBr` reads the Value itself, so it
        // needs no operand class and cannot exit.
        InitMethod(..) | Br(..) | CondBr(..) | NilBr(..) | CheckLocal(..)
        | LoopStart { .. } | LoopEnd | Ret(..) => (false, false),
        // Moves and loads of things the frame already holds. `InlineCache`
        // is not an instruction at all — it is a cache slot in the bytecode
        // stream, which the compiler skips.
        Mov(..) | LoadIvar(..) | InlineCache => (false, false),
        // A literal that must be deep-copied allocates; that is fine (the
        // window is GC-scanned), and it cannot exit.
        FrozenLiteral(..) | Literal(..) => (false, false),
        // Arithmetic and comparison: guard their operands and, for `Div`,
        // the zero divisor.
        BinOp { .. } | BinCmp { .. } | BinCmpBr { .. } | UnOp { .. } => (true, false),
        // The one effect this stage admits. The frozen guard that precedes
        // it is a side exit, but it is hoisted ahead of every store.
        StoreIvar(..) => (false, true),
        // Survey only (see `mergeable_body_calls_ok`).
        MethodCall { .. } | Yield { .. } | Index { .. } | IndexAssign { .. }
        | Array { .. } | Hash { .. } | ConcatStr { .. } | LoadConst(..)
            if calls_ok =>
        {
            (true, true)
        }
        // Everything else — every call, `yield`, global / constant /
        // class-variable access, `defined?`, definitions, `eval`-adjacent
        // constructs, exception control flow, and the allocating literals
        // that take a call site.
        _ => return None,
    })
}

///
/// Whether any block containing a guard is reachable from any block
/// containing an effect.
///
/// Plain forward reachability over the successor graph, seeded with every
/// effect block's successors — a loop's back edge makes the loop's own guard
/// reachable from an effect inside it, which is the case worth catching.
///
fn reaches_guard_from_effect(iseq: &ISeqInfo, effect_bbs: &[bool], guard_bbs: &[bool]) -> bool {
    let bb_num = effect_bbs.len();
    let mut seen = vec![false; bb_num];
    let mut work: Vec<usize> = vec![];
    for (b, &has_effect) in effect_bbs.iter().enumerate() {
        if has_effect {
            for succ in successors(iseq, b) {
                if !seen[succ] {
                    seen[succ] = true;
                    work.push(succ);
                }
            }
        }
    }
    while let Some(b) = work.pop() {
        if guard_bbs[b] {
            return true;
        }
        for succ in successors(iseq, b) {
            if !seen[succ] {
                seen[succ] = true;
                work.push(succ);
            }
        }
    }
    false
}

fn successors(iseq: &ISeqInfo, bbid: usize) -> Vec<usize> {
    iseq.bb_info[BasicBlockId(bbid)]
        .succ
        .iter()
        .map(|b| b.0)
        .collect()
}
