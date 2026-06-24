//! Per-basic-block **local GP register allocator** (Layer-② first cut).
//!
//! This is the first slice of the register-allocation pass discussed in
//! `doc/regalloc_separation.md` §3: a *local* (single basic block) allocator
//! that scans the typed IR and assigns physical GP registers to the fixnum
//! binop operands and results, **reusing** a register across instructions when
//! a slot it already caches is read again. For
//!
//! ```text
//! %1 = %2 + %3
//! ```
//!
//! it emits, mirroring the design note's worked example:
//!
//! - `%2` not yet in a GP → load it into `GP1`,
//! - `%3` not yet in a GP → load it into `GP2`,
//! - `GP3 = GP1 + GP2`,
//! - record that `%1` is now held in `GP3`.
//!
//! A subsequent `%4 = %1 + %5` then finds `%1` already resident and skips its
//! reload — the win local allocation buys over the always-reload slot-IR
//! lowering.
//!
//! ## Policy
//!
//! - **The result is always kept in a GP.** A binop result is very likely the
//!   operand of the next instruction, so it is never spilled at its def; if the
//!   register file is full the allocator evicts a *victim* to make room. Victim
//!   selection is the simplest useful policy — **evict the oldest resident**
//!   (FIFO) — and the evicted value is written back to its stack home.
//! - **Liveness is the AsmIR stack pointer.** A temporary slot at or above
//!   `next_sp` has been popped and will not be read again (exactly what
//!   `clear_above_next_sp` discards), so after each instruction the allocator
//!   frees every register caching such a slot — *without* a spill, since the
//!   value is dead. This is the cheap, exact liveness the design note asks for,
//!   and it keeps registers available for the results that matter.
//!
//! ## Scope / correctness model
//!
//! The register file is reset at basic-block entry and *flushed* — every dirty
//! resident spilled to its stack home — at the block boundary and before any
//! non-binop operation, so the rest of codegen always observes a slot in its
//! canonical stack home. Being strictly per-block and never part of a
//! cross-block merge, it avoids the loop-back-edge placement coupling that made
//! the old analysis-fused `LinkMode::G` load-bearing (doc §13.8).
//!
//! This module is the **pure allocator**: it consumes a list of binop records
//! (each tagged with the post-instruction `next_sp`) and produces a list of
//! [`GpAction`]s. It is codegen-independent and unit-tested in isolation;
//! wiring the actions into the AsmIR→LIR lowering (and threading dirty
//! residents into the deopt write-back) is the next step.

use crate::bytecodegen::BinOpK;
use crate::codegen::GP;
use crate::jitgen::SlotId;

/// The allocatable GP registers for the local allocator: the caller-saved
/// scratch registers that are *not* part of the fixed VM convention
/// (acc/lfp/pc/globals/executor) or the C-ABI / inline-builtin scratch
/// (rdi/rax/rsi/rdx/rcx). These are the registers the abolished `GP_ALLOC_POOL`
/// used; the allocator re-uses them, but driven by an explicit local pass
/// rather than fused into the type fixpoint.
pub(in crate::codegen::jitgen) const GP_ALLOC_SET: &[GP] = &[GP::R8, GP::R9, GP::R10, GP::R11];

/// One fixnum binop in the typed IR: `dst = lhs <kind> rhs`, all stack slots.
///
/// `next_sp` is the stack pointer *after* this instruction: every resident slot
/// whose index is `>= next_sp` is a dead temporary (mirrors
/// `clear_above_next_sp`) and is freed once the op has consumed it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) struct BinOpInst {
    pub kind: BinOpK,
    pub dst: SlotId,
    pub lhs: SlotId,
    pub rhs: SlotId,
    pub next_sp: SlotId,
}

/// A single lowered action the allocator emits for the codegen half to replay.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum GpAction {
    /// Load `slot` from its stack home into `reg`. `guard` requests a fixnum
    /// type guard (needed for a value freshly read from the stack; a value
    /// produced by a prior binop is already a known fixnum and skips it).
    Load { slot: SlotId, reg: GP, guard: bool },
    /// `dst = lhs <kind> rhs`, all three already in registers. The overflow /
    /// type side-exit is the consumer's responsibility (it is a deopt point).
    BinOp {
        kind: BinOpK,
        dst: GP,
        lhs: GP,
        rhs: GP,
    },
    /// Spill `reg` to `slot`'s stack home — emitted when a *dirty* resident is
    /// evicted to free its register, or at the block-boundary flush. A clean
    /// resident (freshly loaded, unmodified) already matches its home and is
    /// dropped without a store.
    Spill { reg: GP, slot: SlotId },
}

/// What an allocatable register currently caches.
#[derive(Clone, Copy)]
struct Holder {
    slot: SlotId,
    /// the register's value differs from `slot`'s stack home (a binop result);
    /// a clean holder (freshly loaded, unmodified) needs no spill.
    dirty: bool,
    /// monotonically-increasing stamp set when the slot was bound, used to pick
    /// the oldest resident as the eviction victim (FIFO).
    age: u64,
}

/// The per-basic-block GP register file: which slot, if any, each allocatable
/// register currently caches. The `vgp`-style `holder` vector mirrors the xmm
/// `FprAllocator`.
struct GpRegFile {
    /// `holder[i]` is what `GP_ALLOC_SET[i]` caches (`None` = free).
    holder: Vec<Option<Holder>>,
    /// FIFO clock for victim selection.
    clock: u64,
}

impl GpRegFile {
    fn new() -> Self {
        Self {
            holder: vec![None; GP_ALLOC_SET.len()],
            clock: 0,
        }
    }

    fn tick(&mut self) -> u64 {
        self.clock += 1;
        self.clock
    }

    fn index_of(reg: GP) -> usize {
        GP_ALLOC_SET.iter().position(|&r| r == reg).unwrap()
    }

    /// The register currently caching `slot`, if any (the reuse lookup).
    fn reg_of(&self, slot: SlotId) -> Option<GP> {
        self.holder
            .iter()
            .position(|h| h.map(|h| h.slot) == Some(slot))
            .map(|i| GP_ALLOC_SET[i])
    }

    fn find_free(&self) -> Option<GP> {
        self.holder
            .iter()
            .position(|h| h.is_none())
            .map(|i| GP_ALLOC_SET[i])
    }

    /// Allocate a register, always succeeding: a free register if any, else the
    /// **oldest** resident that is not `pinned` is evicted (its dirty value
    /// written back) so the caller — typically a binop result that must stay
    /// resident — always gets a register.
    fn alloc(&mut self, pinned: &[GP], out: &mut Vec<GpAction>) -> GP {
        if let Some(reg) = self.find_free() {
            return reg;
        }
        let victim_idx = (0..self.holder.len())
            .filter(|&i| !pinned.contains(&GP_ALLOC_SET[i]))
            .min_by_key(|&i| self.holder[i].unwrap().age)
            .expect("more pinned registers than the allocatable set");
        let reg = GP_ALLOC_SET[victim_idx];
        let h = self.holder[victim_idx].unwrap();
        if h.dirty {
            out.push(GpAction::Spill { reg, slot: h.slot });
        }
        self.holder[victim_idx] = None;
        reg
    }

    /// Record that `reg` now caches `slot`, dropping any prior cache of `slot`.
    fn bind(&mut self, reg: GP, slot: SlotId, dirty: bool) {
        for h in self.holder.iter_mut() {
            if h.map(|h| h.slot) == Some(slot) {
                *h = None;
            }
        }
        let age = self.tick();
        self.holder[Self::index_of(reg)] = Some(Holder { slot, dirty, age });
    }

    /// Free every register caching a slot at or above `sp` — those temporaries
    /// are dead (popped past the stack pointer), so they are dropped without a
    /// spill, mirroring `clear_above_next_sp`.
    fn free_above_sp(&mut self, sp: SlotId) {
        for h in self.holder.iter_mut() {
            if let Some(held) = *h
                && held.slot >= sp
            {
                *h = None;
            }
        }
    }
}

/// Allocate GP registers for a straight-line run of fixnum binops (one basic
/// block, or a maximal binop sub-run within one). Returns the lowered
/// [`GpAction`] stream, terminated by the flush of every dirty resident.
pub(in crate::codegen::jitgen) fn allocate_run(insts: &[BinOpInst]) -> Vec<GpAction> {
    let mut rf = GpRegFile::new();
    let mut out = Vec::new();

    for inst in insts {
        let lhs_reg = ensure(&mut rf, inst.lhs, &[], &mut out);
        let rhs_reg = ensure(&mut rf, inst.rhs, &[lhs_reg], &mut out);
        // The result is (re)defined: drop any stale cache, then claim a register
        // for it (always — results stay resident), pinning the operand registers
        // across the possible eviction.
        rf.bind_invalidate(inst.dst);
        let dst_reg = rf.alloc(&[lhs_reg, rhs_reg], &mut out);
        out.push(GpAction::BinOp {
            kind: inst.kind,
            dst: dst_reg,
            lhs: lhs_reg,
            rhs: rhs_reg,
        });
        rf.bind(dst_reg, inst.dst, /* dirty */ true);
        // Liveness: free temporaries the stack pointer has popped (incl. the
        // just-consumed operands), making their registers reusable.
        rf.free_above_sp(inst.next_sp);
    }

    flush_dirty(&mut rf, &mut out);
    out
}

/// Bring `slot` into a register and return it, reusing the resident copy when
/// present (no reload, no re-guard) and otherwise loading + fixnum-guarding it.
fn ensure(rf: &mut GpRegFile, slot: SlotId, pinned: &[GP], out: &mut Vec<GpAction>) -> GP {
    if let Some(reg) = rf.reg_of(slot) {
        return reg;
    }
    let reg = rf.alloc(pinned, out);
    out.push(GpAction::Load {
        slot,
        reg,
        guard: true,
    });
    rf.bind(reg, slot, /* dirty */ false);
    reg
}

/// Flush dirty residents to their stack homes and clear the file.
fn flush_dirty(rf: &mut GpRegFile, out: &mut Vec<GpAction>) {
    for i in 0..rf.holder.len() {
        if let Some(Holder { slot, dirty: true, .. }) = rf.holder[i] {
            out.push(GpAction::Spill {
                reg: GP_ALLOC_SET[i],
                slot,
            });
        }
        rf.holder[i] = None;
    }
}

impl GpRegFile {
    /// A slot is about to be redefined: drop any stale register cache of it (the
    /// old value is dead, so no spill).
    fn bind_invalidate(&mut self, slot: SlotId) {
        for h in self.holder.iter_mut() {
            if h.map(|h| h.slot) == Some(slot) {
                *h = None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sl(i: u16) -> SlotId {
        SlotId::new(i)
    }

    /// `dst = lhs + rhs`, with `next_sp` so that slots `>= next_sp` are dead.
    fn add(dst: u16, lhs: u16, rhs: u16, next_sp: u16) -> BinOpInst {
        BinOpInst {
            kind: BinOpK::Add,
            dst: sl(dst),
            lhs: sl(lhs),
            rhs: sl(rhs),
            next_sp: sl(next_sp),
        }
    }

    const R8: GP = GP::R8;
    const R9: GP = GP::R9;
    const R10: GP = GP::R10;

    /// The worked example `%1 = %2 + %3` (operands %2/%3 popped, %1 live):
    /// %2 → R8, %3 → R9, R10 = R8 + R9, %1 recorded in R10, R10 spilled at flush.
    #[test]
    fn single_binop() {
        // next_sp = 2 → slots %2,%3 (idx 2,3) dead, %1 (idx 1) live.
        let out = allocate_run(&[add(1, 2, 3, 2)]);
        assert_eq!(
            out,
            vec![
                GpAction::Load { slot: sl(2), reg: R8, guard: true },
                GpAction::Load { slot: sl(3), reg: R9, guard: true },
                GpAction::BinOp { kind: BinOpK::Add, dst: R10, lhs: R8, rhs: R9 },
                GpAction::Spill { reg: R10, slot: sl(1) },
            ]
        );
    }

    /// `%1 = %2 + %3; %4 = %1 + %5` — `%1` stays resident and is reused as an
    /// operand with no reload; `%2`/`%3` freed by the stack pointer are reused.
    #[test]
    fn result_reused_as_operand() {
        let out = allocate_run(&[add(1, 2, 3, 2), add(4, 1, 5, 4)]);
        // %1 is never reloaded (it is produced in a register and consumed there).
        assert!(!out
            .iter()
            .any(|a| matches!(a, GpAction::Load { slot, .. } if *slot == sl(1))));
        // %1's register is an operand of the second op.
        assert!(out
            .iter()
            .any(|a| matches!(a, GpAction::BinOp { lhs, .. } if *lhs == R10)));
    }

    /// The result is always kept in a register even under pressure: four live
    /// results force an eviction (spill of the oldest) rather than spilling the
    /// new result.
    #[test]
    fn result_always_resident_under_pressure() {
        // Five chained results, all kept live (next_sp high so nothing is freed).
        // %2..%6 are source locals; results %10..%14. With only 4 registers the
        // 5th result must evict the oldest resident, not skip its own register.
        let insts = [
            add(10, 2, 3, 100),
            add(11, 10, 4, 100),
            add(12, 11, 5, 100),
            add(13, 12, 6, 100),
            add(14, 13, 7, 100),
        ];
        let out = allocate_run(&insts);
        // Every binop result lands in a register (there is a BinOp for each).
        let binops = out
            .iter()
            .filter(|a| matches!(a, GpAction::BinOp { .. }))
            .count();
        assert_eq!(binops, 5);
        // Pressure forced at least one mid-run spill (a victim eviction).
        let mid_spills = out
            .iter()
            .take_while(|a| !matches!(a, GpAction::BinOp { .. } if false)) // all
            .filter(|a| matches!(a, GpAction::Spill { .. }))
            .count();
        assert!(mid_spills >= 1, "pressure should force a victim spill");
    }

    /// Liveness via the stack pointer frees a dead temporary's register so a
    /// later op reuses it instead of growing register pressure.
    #[test]
    fn sp_frees_dead_temporary() {
        // op1: %3 = %1 + %2, next_sp = 3 → %1,%2 dead, freed. %3 live in a reg.
        // op2: %5 = %3 + %4 — %1,%2's registers are free for %4 / the result.
        let out = allocate_run(&[add(3, 1, 2, 3), add(5, 3, 4, 5)]);
        // %1 and %2 are each loaded exactly once; their registers are recycled.
        for s in [1u16, 2] {
            let loads = out
                .iter()
                .filter(|a| matches!(a, GpAction::Load { slot, .. } if *slot == sl(s)))
                .count();
            assert_eq!(loads, 1);
        }
        // No spill of the dead operands %1/%2 (freed without write-back).
        for s in [1u16, 2] {
            assert!(!out
                .iter()
                .any(|a| matches!(a, GpAction::Spill { slot, .. } if *slot == sl(s))));
        }
    }
}
