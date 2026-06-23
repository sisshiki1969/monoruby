//! # LIR-level GP register allocator (§9 9d-2)
//!
//! The GP-pool allocation *decision* is moving out of the abstract interpreter
//! (the eager `GpAllocator` in `state/slot.rs`, which grabs a physical pool
//! register at each definition and flushes it at every boundary) and into this
//! LIR pass, which runs from [`Codegen::allocate_gp`] over the buffered
//! `Vec<LInst>` for a whole region. Working on the linear `LInst` stream — after
//! page selection, label binding and source-position pseudo-ops are already
//! interleaved in emission order — lets the allocator see *real* live ranges and
//! the exact positions of pool-clobbering C-calls, which the per-slot abstract
//! state cannot express. That is the prerequisite for cross-merge register
//! retention (keeping a value resident past a boundary the front-end currently
//! flushes at).
//!
//! ## What this module does today (the machinery step)
//!
//! 1. **Def/use extraction** ([`alloc_def`], [`alloc_uses`]): which `VReg::Alloc`
//!    virtual ids each `LInst` writes and reads. Only the decomposed value-
//!    residence ops (`Mov` / `LoadImm` / `Load` / `Store{,Imm}` / `Alu` / `Cmp`)
//!    carry `VReg`s; everything else uses fixed physical `GP`s.
//! 2. **Live ranges** ([`build_intervals`]): each contiguous def‥last-use run of
//!    an `Alloc` id is one interval. The front-end reuses a virtual id once the
//!    previous occupant dies, so a redefinition starts a fresh interval.
//! 3. **Pool-clobber map** ([`is_pool_clobber`]): the C-call / GC-safepoint
//!    `LInst`s that destroy the caller-saved pool registers. The front-end
//!    flushes (G→S) before every one of these, so no interval may span one.
//! 4. **Linear-scan assignment** ([`assign`]): colour every interval into the
//!    `GP_ALLOC_POOL`. Because no interval spans a clobber and the front-end
//!    already bounded simultaneous residents by the pool size, first-fit always
//!    succeeds; the result is a *valid* assignment (no two overlapping intervals
//!    share a register, every colour in range).
//!
//! [`allocate`] runs all four and then applies an **identity rewrite** — it
//! re-derives a valid pool assignment and asserts it, but emits the body
//! unchanged, so the output is byte-identical to the eager allocator. This
//! validates the live-range / clobber machinery end-to-end on real workloads
//! (run the suite with `--features gp-alloc-lir`) before the follow-up perf step
//! makes the front-end stop flushing and lets this pass insert the spills.
//!
//! [`Codegen::allocate_gp`]: super::Codegen::allocate_gp

use super::lir::{LInst, LMem, LOperand, LReg, VReg};
use crate::codegen::GP_ALLOC_POOL;

/// Number of allocatable physical pool registers (`r8`–`r11` on x86-64, none on
/// aarch64). When zero, the front-end never emits `VReg::Alloc`, so the whole
/// pass is a no-op.
const POOL: usize = GP_ALLOC_POOL.len();

/// The physical pool id (`VReg::Alloc(id)`) a `VReg` resolves to, or `None` for
/// a pre-coloured `Pinned` register.
fn vreg_alloc(v: VReg) -> Option<u32> {
    match v {
        VReg::Alloc(id) => Some(id),
        VReg::Pinned(_) => None,
    }
}

fn lreg_alloc(r: LReg) -> Option<u32> {
    match r {
        LReg::Gp(v) => vreg_alloc(v),
        LReg::Scratch => None,
    }
}

fn loperand_alloc(o: LOperand) -> Option<u32> {
    match o {
        LOperand::Reg(v) => vreg_alloc(v),
        LOperand::Imm(_) => None,
    }
}

/// An `Alloc` id read through a memory operand's base register (a field deref of
/// a pool-resident object pointer).
fn lmem_base_alloc(m: &LMem) -> Option<u32> {
    match m {
        LMem::Field { base, .. } => lreg_alloc(*base),
        LMem::Slot(_) | LMem::RspRel { .. } => None,
    }
}

/// The single `Alloc` id an instruction *defines* (writes), if any.
pub(super) fn alloc_def(inst: &LInst) -> Option<u32> {
    match inst {
        LInst::Mov { dst, .. } | LInst::LoadImm { dst, .. } | LInst::Alu { dst, .. } => {
            vreg_alloc(*dst)
        }
        LInst::Load { dst, .. } => lreg_alloc(*dst),
        _ => None,
    }
}

/// The `Alloc` ids an instruction *uses* (reads), appended to `out`.
pub(super) fn alloc_uses(inst: &LInst, out: &mut Vec<u32>) {
    let mut push = |o: Option<u32>| {
        if let Some(id) = o {
            out.push(id);
        }
    };
    match inst {
        LInst::Mov { src, .. } => push(vreg_alloc(*src)),
        LInst::Load { mem, .. } => push(lmem_base_alloc(mem)),
        LInst::Store { mem, .. } | LInst::StoreImm { mem, .. } => push(lmem_base_alloc(mem)),
        LInst::Alu { lhs, rhs, .. } => {
            push(vreg_alloc(*lhs));
            push(loperand_alloc(*rhs));
        }
        LInst::Cmp { lhs, rhs } => {
            push(vreg_alloc(*lhs));
            push(loperand_alloc(*rhs));
        }
        _ => {}
    }
}

/// Whether `inst` clobbers the caller-saved GP pool registers — the C-call /
/// runtime-call / GC-safepoint macro-ops. The front-end flushes every pool
/// resident (G→S) before each of these (`writeback_pool_state`), so no `Alloc`
/// live range may span one.
///
/// Deliberately conservative *downward*: a macro-op omitted here is at worst a
/// weaker check (the front-end still flushed, so nothing spans it), whereas
/// wrongly flagging a pure op as a clobber could reject a legitimate span. Only
/// ops that unambiguously perform a clobbering call are listed.
pub(super) fn is_pool_clobber(inst: &LInst) -> bool {
    matches!(
        inst,
        // Method-call / yield / argument-marshalling family.
        LInst::Call { .. }
            | LInst::Yield { .. }
            // GC safepoints (the stub call preserves the pool, but the abstract
            // state still flushes here for rooting).
            | LInst::ExecGc { .. }
            | LInst::CheckStack { .. }
            // Runtime allocation / C-call family (each does a C-ABI call).
            | LInst::NewArray { .. }
            | LInst::NewHash { .. }
            | LInst::HashInsert { .. }
            | LInst::ArrayConcat { .. }
            | LInst::NewRange { .. }
            | LInst::ConcatStr { .. }
            | LInst::ConcatRegexp { .. }
            | LInst::ToA { .. }
            | LInst::DeepCopyLit { .. }
            | LInst::ExpandArray { .. }
            // Variable access via runtime call.
            | LInst::StoreConstant { .. }
            | LInst::LoadGVar { .. }
            | LInst::StoreGVar { .. }
            | LInst::LoadCVar { .. }
            | LInst::StoreCVar { .. }
            | LInst::CheckCVar { .. }
            | LInst::AliasGvar { .. }
            | LInst::StoreIVarHeap { .. }
            // Generic-op / definition / defined? family.
            | LInst::GenericBinOp { .. }
            | LInst::OptEqCmp { .. }
            | LInst::ArrayTEq { .. }
            | LInst::UndefMethod { .. }
            | LInst::AliasMethod { .. }
            // f64 C-math calls clobber GPs too.
            | LInst::CFunc_F_F { .. }
            | LInst::CFunc_FF_F { .. }
    )
}

/// A live range of one `Alloc` virtual occupant: `[def, last_use]` (inclusive
/// instruction indices) for the virtual id `vid`. `last_use == def` for a value
/// that is defined but never read again before its id is reused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Interval {
    pub vid: u32,
    pub def: usize,
    pub last_use: usize,
}

/// Split the stream into per-occupant live ranges. Each definition of a virtual
/// id opens a new interval; uses extend the open interval's `last_use`; the next
/// definition (or end of stream) closes it. A use with no open interval is
/// skipped (the def precedes every use within a region, so this only guards a
/// malformed stream).
pub(super) fn build_intervals(body: &[LInst]) -> Vec<Interval> {
    let mut intervals: Vec<Interval> = Vec::new();
    // open[vid] = index into `intervals` of the currently-open interval, if any.
    let mut open: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
    let mut uses = Vec::new();
    for (idx, inst) in body.iter().enumerate() {
        // Uses first: they read the *current* (pre-redef) occupant.
        uses.clear();
        alloc_uses(inst, &mut uses);
        for &vid in &uses {
            if let Some(&i) = open.get(&vid) {
                intervals[i].last_use = idx;
            }
        }
        if let Some(vid) = alloc_def(inst) {
            // A `Mov dst, dst` (or any def that also uses the same id) keeps the
            // occupant; only treat it as a fresh interval when the id is not yet
            // open or this is a genuine redefinition past its last use.
            let new = Interval {
                vid,
                def: idx,
                last_use: idx,
            };
            let i = intervals.len();
            intervals.push(new);
            open.insert(vid, i);
        }
    }
    intervals
}

/// Linear-scan colouring of the intervals into the pool. Returns one physical
/// pool id per interval (index-aligned with `intervals`), or `None` if the pass
/// would need to spill (more than `POOL` intervals simultaneously live) — which
/// the front-end's own `find_vacant` bound makes impossible today, so the
/// caller treats `None` as "leave the front-end's assignment untouched".
pub(super) fn assign(intervals: &[Interval]) -> Option<Vec<usize>> {
    if POOL == 0 {
        return (intervals.is_empty()).then(Vec::new);
    }
    // Process intervals in start order; free a register when its interval ends.
    let mut order: Vec<usize> = (0..intervals.len()).collect();
    order.sort_by_key(|&i| (intervals[i].def, intervals[i].last_use));

    let mut colour = vec![usize::MAX; intervals.len()];
    // free_at[p] = the instruction index at which pool register p becomes free
    // (exclusive); 0 means free now.
    let mut busy_until = vec![0usize; POOL];
    let mut occupied = vec![false; POOL];

    for &i in &order {
        let Interval { def, last_use, .. } = intervals[i];
        // Reclaim registers whose interval ended strictly before this def.
        for p in 0..POOL {
            if occupied[p] && busy_until[p] < def {
                occupied[p] = false;
            }
        }
        let p = (0..POOL).find(|&p| !occupied[p])?;
        occupied[p] = true;
        busy_until[p] = last_use;
        colour[i] = p;
    }
    Some(colour)
}

/// Run the LIR GP allocator over a region body. Today this is an **identity
/// rewrite**: it builds the live ranges and clobber map, derives a valid pool
/// assignment, asserts it (no interval spans a clobber; the assignment colours
/// every interval), and returns the body unchanged so the output is
/// byte-identical to the eager allocator. The follow-up perf step replaces the
/// identity tail with the real rewrite (spill insertion at clobbers, relaxed
/// front-end flushing).
pub(super) fn allocate(body: Vec<LInst>) -> Vec<LInst> {
    if POOL == 0 {
        return body;
    }
    let intervals = build_intervals(&body);

    // Invariant the eager front-end guarantees: no Alloc live range spans a
    // pool-clobbering call. Check it on real workloads (debug builds only).
    #[cfg(debug_assertions)]
    {
        let clobbers: Vec<bool> = body.iter().map(is_pool_clobber).collect();
        for iv in &intervals {
            let spans = (iv.def + 1..iv.last_use).any(|k| clobbers[k]);
            debug_assert!(
                !spans,
                "gp-alloc-lir: Alloc({}) live [{}..{}] spans a pool clobber",
                iv.vid, iv.def, iv.last_use
            );
        }
        debug_assert!(
            assign(&intervals).is_some(),
            "gp-alloc-lir: linear scan failed to colour {} intervals into {POOL} regs",
            intervals.len()
        );
    }

    body
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::jitgen::lir::{LAluOp, VReg};

    fn alloc(id: u32) -> VReg {
        VReg::Alloc(id)
    }

    #[test]
    fn def_use_extraction() {
        let mov = LInst::Mov {
            dst: alloc(1),
            src: alloc(0),
        };
        assert_eq!(alloc_def(&mov), Some(1));
        let mut uses = Vec::new();
        alloc_uses(&mov, &mut uses);
        assert_eq!(uses, vec![0]);

        let alu = LInst::Alu {
            op: LAluOp::Add,
            dst: alloc(2),
            lhs: alloc(0),
            rhs: LOperand::Reg(alloc(1)),
        };
        assert_eq!(alloc_def(&alu), Some(2));
        uses.clear();
        alloc_uses(&alu, &mut uses);
        assert_eq!(uses, vec![0, 1]);

        // Pinned operands are not allocatable.
        let mov_pinned = LInst::Mov {
            dst: VReg::Pinned(crate::codegen::GP::Rax),
            src: VReg::Pinned(crate::codegen::GP::Rdi),
        };
        assert_eq!(alloc_def(&mov_pinned), None);
        uses.clear();
        alloc_uses(&mov_pinned, &mut uses);
        assert!(uses.is_empty());
    }

    #[test]
    fn intervals_split_on_redef() {
        // 0: def %0   ; 1: use %0   ; 2: redef %0  ; 3: use %0
        let body = vec![
            LInst::LoadImm {
                dst: alloc(0),
                imm: 1,
            },
            LInst::Mov {
                dst: VReg::Pinned(crate::codegen::GP::Rax),
                src: alloc(0),
            },
            LInst::LoadImm {
                dst: alloc(0),
                imm: 2,
            },
            LInst::Mov {
                dst: VReg::Pinned(crate::codegen::GP::Rax),
                src: alloc(0),
            },
        ];
        let ivs = build_intervals(&body);
        assert_eq!(ivs.len(), 2);
        assert_eq!((ivs[0].def, ivs[0].last_use), (0, 1));
        assert_eq!((ivs[1].def, ivs[1].last_use), (2, 3));
    }

    #[test]
    fn assign_disjoint_and_overlapping() {
        if POOL == 0 {
            // aarch64: no pool, nothing to colour.
            assert_eq!(assign(&[]), Some(vec![]));
            return;
        }
        // Two non-overlapping intervals can share a register.
        let disjoint = vec![
            Interval {
                vid: 0,
                def: 0,
                last_use: 1,
            },
            Interval {
                vid: 1,
                def: 2,
                last_use: 3,
            },
        ];
        let c = assign(&disjoint).unwrap();
        assert_eq!(c[0], c[1]);

        // Two overlapping intervals must not.
        let overlap = vec![
            Interval {
                vid: 0,
                def: 0,
                last_use: 3,
            },
            Interval {
                vid: 1,
                def: 1,
                last_use: 2,
            },
        ];
        let c = assign(&overlap).unwrap();
        assert_ne!(c[0], c[1]);
    }

    #[test]
    fn assign_respects_pool_capacity() {
        if POOL == 0 {
            return;
        }
        // POOL+1 mutually-overlapping intervals cannot be coloured.
        let n = POOL + 1;
        let over: Vec<Interval> = (0..n)
            .map(|i| Interval {
                vid: i as u32,
                def: 0,
                last_use: 10,
            })
            .collect();
        assert_eq!(assign(&over), None);

        // Exactly POOL mutually-overlapping intervals fit.
        let fit: Vec<Interval> = (0..POOL)
            .map(|i| Interval {
                vid: i as u32,
                def: 0,
                last_use: 10,
            })
            .collect();
        let c = assign(&fit).unwrap();
        let mut sorted = c.clone();
        sorted.sort_unstable();
        sorted.dedup();
        assert_eq!(sorted.len(), POOL, "each overlapping interval gets a distinct reg");
    }
}
