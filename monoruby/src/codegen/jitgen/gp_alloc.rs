//! Per-basic-block **local GP register allocator** (Layer-②).
//!
//! The register-allocation pass discussed in `doc/regalloc_separation.md` §3:
//! a *local* (single basic block) allocator that assigns physical GP registers
//! to fixnum operands and results, **reusing** a register across instructions
//! when a slot it already caches is read again. The GP-aware operations that
//! drive it online are the integer binops (`binop_integer_gp`), the integer
//! compares / flag-setting compares (`gen_cmp_integer_gp` / `gen_cmp_integer_flags`),
//! concrete-literal defs (`def_lit2gp`); generic slot reads and write-backs consult the residents,
//! and dirty residents are threaded into deopt / GC write-backs. For
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
//! non-GP-aware operation, so the rest of codegen always observes a slot in
//! its canonical stack home. Being strictly per-block and never part of a
//! cross-block merge, it avoids the loop-back-edge placement coupling that made
//! the old analysis-fused `LinkMode::G` load-bearing (doc §13.8).
//!
//! This module holds the register file ([`GpRegFile`]), driven online by the
//! codegen paths above; the guard on a fresh load is decided by the driver
//! from the slot's abstract type (`is_fixnum`, see `gp_ensure`).

use crate::codegen::GP;
use crate::jitgen::SlotId;

/// The allocatable GP registers for the local allocator: the caller-saved
/// scratch registers that are *not* part of the fixed VM convention
/// (acc/lfp/pc/globals/executor) or the C-ABI / inline-builtin scratch
/// (rdi/rax/rsi/rdx/rcx). These are the registers the abolished `GP_ALLOC_POOL`
/// used; the allocator re-uses them, but driven by an explicit local pass
/// rather than fused into the type fixpoint.
pub(in crate::codegen::jitgen) const GP_ALLOC_SET: &[GP] = &[GP::R8, GP::R9, GP::R10, GP::R11];

/// One slot an allocatable register currently caches.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Holder {
    slot: SlotId,
    /// the register's value differs from `slot`'s stack home (a binop result,
    /// or a copy that has not been stored yet); a clean holder (freshly loaded,
    /// unmodified) needs no spill.
    dirty: bool,
}

/// The per-basic-block GP register file: which slots each allocatable register
/// currently caches. The `vgp`-style `holders` vector mirrors the xmm
/// `FprAllocator`, including its many-slots-per-register shape: a register
/// holds a *set* of slots that all carry the same value, each with its own
/// dirty bit. That is what makes a slot copy free — `%dst = %src` adds `dst` to
/// `src`'s register as a dirty holder and emits nothing; the store to `dst`'s
/// home is owed only when the register is evicted or the file is flushed, and
/// is dropped entirely if `dst` dies first (a `ret`, a popped temporary).
///
/// Held in the abstract state and driven online during the basic-block walk:
/// the GP-aware operations (integer binops, integer compares/compare-branches,
/// call/yield-result parking, concrete-literal defs, slot copies)
/// reuse/allocate registers through it, while every other instruction flushes
/// the live GP residents back to their stack homes up front (via `flush_gp` →
/// [`Self::take_dirty_spills`]).
///
/// It is flushed empty at every basic-block boundary, so although it rides
/// inside the cloned/merged `SlotState` it never actually carries state across a
/// block merge (the per-block-locality the design requires).
#[derive(Clone)]
pub(in crate::codegen::jitgen) struct GpRegFile {
    /// `holders[i]` is the set of slots `GP_ALLOC_SET[i]` caches (empty = free).
    holders: Vec<Vec<Holder>>,
    /// `age[i]`: monotonically-increasing stamp set when a slot was last bound
    /// to `GP_ALLOC_SET[i]`, used to pick the oldest register as the eviction
    /// victim (FIFO).
    age: Vec<u64>,
    /// FIFO clock for victim selection.
    clock: u64,
    /// The loop pin (see [`LoopPin`]), when this block lies in a loop that
    /// keeps one of its locals in a register across its blocks.
    pin: Option<LoopPin>,
}

/// A local an innermost loop keeps in a fixed register across the loop's
/// blocks, so that reading it costs no reload at each block entry.
///
/// The register is taken out of the allocatable set for the whole loop (it
/// is never a victim and never computed into), and holds a **clean** copy:
/// `valid` says the register equals the slot's current (boxed) value. It is
/// invalidated by a redefinition of the slot and by every full flush (the
/// operation that follows may clobber the caller-saved registers), and
/// re-established lazily by the next read of the slot (`gp_ensure`) or on
/// the edge into the next block of the loop, whose entry state requires it
/// valid. Since the copy is clean, no write-back ever looks at it.
///
/// It is not part of the state's identity (`PartialEq` / `Hash`): the
/// loop fixpoint and the memo compare abstract states, and the pin only
/// changes which register a read finds the value in.
#[derive(Clone, Copy, Debug)]
pub(in crate::codegen::jitgen) struct LoopPin {
    pub reg: GP,
    pub slot: SlotId,
    pub valid: bool,
}

/// The register a loop pin lives in: the last of the allocatable set, the
/// one `find_free` hands out last.
pub(in crate::codegen::jitgen) const PIN_REG: GP = GP::R11;

impl PartialEq for GpRegFile {
    fn eq(&self, other: &Self) -> bool {
        self.holders == other.holders && self.age == other.age && self.clock == other.clock
    }
}

impl Eq for GpRegFile {}

impl std::hash::Hash for GpRegFile {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.holders.hash(state);
        self.age.hash(state);
        self.clock.hash(state);
    }
}

impl Default for GpRegFile {
    fn default() -> Self {
        Self::new()
    }
}

impl GpRegFile {
    pub(in crate::codegen::jitgen) fn new() -> Self {
        Self {
            holders: vec![vec![]; GP_ALLOC_SET.len()],
            age: vec![0; GP_ALLOC_SET.len()],
            clock: 0,
            pin: None,
        }
    }

    /// The loop pin, if any.
    pub(in crate::codegen::jitgen) fn pin(&self) -> Option<LoopPin> {
        self.pin
    }

    /// Install (or drop) the loop pin. A pin whose register still caches
    /// slots would hand the register out twice, so the file must hold
    /// nothing in it.
    pub(in crate::codegen::jitgen) fn set_pin(&mut self, pin: Option<LoopPin>) {
        if let Some(p) = pin {
            debug_assert!(self.holders[Self::index_of(p.reg)].is_empty());
        }
        self.pin = pin;
    }

    /// The pin's register may be clobbered: its copy is no longer valid.
    pub(in crate::codegen::jitgen) fn invalidate_pin(&mut self) {
        if let Some(p) = &mut self.pin {
            p.valid = false;
        }
    }

    /// The pin's register now holds its slot's value.
    pub(in crate::codegen::jitgen) fn validate_pin(&mut self) {
        if let Some(p) = &mut self.pin {
            p.valid = true;
        }
    }

    /// The pinned slot when its copy is stale (a read should reload it
    /// into the pin register).
    pub(in crate::codegen::jitgen) fn stale_pin_of(&self, slot: SlotId) -> Option<GP> {
        let p = self.pin.filter(|p| p.slot == slot && !p.valid)?;
        // Only the pinned slot itself is ever bound to the pin register,
        // and redefining it dropped that holder with the pin's validity.
        debug_assert!(self.holders[Self::index_of(p.reg)].is_empty());
        Some(p.reg)
    }

    /// The pin register, when an op defining `dst` may compute into it:
    /// `dst` is the pinned local, and the register carries no dirty value
    /// (a deopt snapshot taken before the op may list a dirty holder, and
    /// would store the overwritten register).
    pub(in crate::codegen::jitgen) fn pin_dst(&self, dst: Option<SlotId>) -> Option<GP> {
        let p = self.pin?;
        (Some(p.slot) == dst && self.holders[Self::index_of(p.reg)].iter().all(|h| !h.dirty))
            .then_some(p.reg)
    }

    fn pin_reg(&self) -> Option<GP> {
        self.pin.map(|p| p.reg)
    }

    /// The register a holder caches `slot` in. That is the pin register
    /// only for the pinned slot itself, computed into it (`pin_dst`); a
    /// caller binding *another* slot to the register it got must not take
    /// the pin register ([`Self::is_pin_reg`]).
    pub(in crate::codegen::jitgen) fn holder_reg_of(&self, slot: SlotId) -> Option<GP> {
        self.position_of(slot).map(|(i, _)| GP_ALLOC_SET[i])
    }

    pub(in crate::codegen::jitgen) fn is_pin_reg(&self, reg: GP) -> bool {
        self.pin_reg() == Some(reg)
    }

    /// True when no register is occupied (the common case — flushing then is a
    /// no-op, so the hot path pays nothing).
    pub(in crate::codegen::jitgen) fn is_empty(&self) -> bool {
        self.holders.iter().all(|h| h.is_empty())
    }

    /// The `(reg, slot)` pairs of every resident, clean or dirty. Does not
    /// mutate the file. Used to seed the direct-argument-store hints a call
    /// site captures just before its flush: after the flush spills a dirty
    /// resident, the register still holds the slot's (now home-equal) value,
    /// so `set_arguments` can store it straight into the callee frame
    /// instead of round-tripping through the just-written stack home (a
    /// store-forwarding hop on the call's critical path).
    pub(in crate::codegen::jitgen) fn residents(&self) -> Vec<(GP, SlotId)> {
        self.pairs(|_| true)
    }

    /// The `(reg, slot)` pairs of every **dirty** resident, for inclusion in a
    /// deopt / GC write-back (the values that differ from their stack home and
    /// must be re-homed if the VM resumes). Does not mutate the file.
    pub(in crate::codegen::jitgen) fn dirty_residents(&self) -> Vec<(GP, SlotId)> {
        self.pairs(|h| h.dirty)
    }

    fn pairs(&self, f: impl Fn(&Holder) -> bool) -> Vec<(GP, SlotId)> {
        (0..self.holders.len())
            .flat_map(|i| {
                self.holders[i]
                    .iter()
                    .filter(|h| f(h))
                    .map(move |h| (GP_ALLOC_SET[i], h.slot))
            })
            .collect()
    }

    /// Flush: return the `(reg, slot)` spills for every dirty resident and clear
    /// the file. The caller emits the stores (the block-boundary / pre-non-binop
    /// flush). Clean residents need no store and are simply dropped.
    pub(in crate::codegen::jitgen) fn take_dirty_spills(&mut self) -> Vec<(GP, SlotId)> {
        let spills = self.dirty_residents();
        for h in self.holders.iter_mut() {
            h.clear();
        }
        self.invalidate_pin();
        spills
    }

    /// The block-boundary flush: like [`Self::take_dirty_spills`], but the
    /// pin survives (nothing runs between the flush and the next block
    /// that could clobber it). Also answers a holder of the pinned slot
    /// when the pin is stale, for the caller to copy into the pin register
    /// after the spills.
    pub(in crate::codegen::jitgen) fn take_dirty_spills_keep_pin(
        &mut self,
    ) -> (Vec<(GP, SlotId)>, Option<GP>) {
        let refill = self
            .pin
            .filter(|p| !p.valid)
            .and_then(|p| self.holder_reg_of(p.slot));
        let spills = self.dirty_residents();
        for h in self.holders.iter_mut() {
            h.clear();
        }
        (spills, refill)
    }

    fn tick(&mut self) -> u64 {
        self.clock += 1;
        self.clock
    }

    fn index_of(reg: GP) -> usize {
        GP_ALLOC_SET.iter().position(|&r| r == reg).unwrap()
    }

    /// True when `reg` holds no resident (free for immediate reuse).
    pub(in crate::codegen::jitgen) fn is_free(&self, reg: GP) -> bool {
        self.pin_reg() != Some(reg) && self.holders[Self::index_of(reg)].is_empty()
    }

    /// True when `slot` is the **only** slot `reg` caches. An op may compute in
    /// place in a register only under this condition: with more holders the
    /// register carries other slots' (not yet stored) values, and overwriting
    /// it would corrupt them.
    pub(in crate::codegen::jitgen) fn holds_only(&self, reg: GP, slot: SlotId) -> bool {
        matches!(self.holders[Self::index_of(reg)].as_slice(), [h] if h.slot == slot)
    }

    fn position_of(&self, slot: SlotId) -> Option<(usize, usize)> {
        self.holders.iter().enumerate().find_map(|(i, hs)| {
            hs.iter()
                .position(|h| h.slot == slot)
                .map(|j| (i, j))
        })
    }

    /// The register currently caching `slot`, if any (the reuse lookup).
    ///
    /// A valid loop pin answers too: its register holds the slot's value,
    /// clean. Such a register must only be read — it is not a holder, so a
    /// caller that wants to bind or compute into the register it got takes
    /// [`Self::holder_reg_of`] instead.
    pub(in crate::codegen::jitgen) fn reg_of(&self, slot: SlotId) -> Option<GP> {
        self.holder_reg_of(slot).or_else(|| {
            self.pin
                .filter(|p| p.valid && p.slot == slot)
                .map(|p| p.reg)
        })
    }

    /// The register caching `slot` only if that cache is **dirty** (its value
    /// differs from the stack home). Used before an op that clobbers an operand
    /// register (`Mul`/`Div` destroy `rhs`): the dirty value must be written to
    /// its home first, since the register will not survive the op.
    pub(in crate::codegen::jitgen) fn dirty_reg_of(&self, slot: SlotId) -> Option<GP> {
        self.position_of(slot)
            .filter(|&(i, j)| self.holders[i][j].dirty)
            .map(|(i, _)| GP_ALLOC_SET[i])
    }

    /// A free register that is not `pinned`. Pinned registers hold operands that
    /// are live across this allocation (e.g. when the result slot aliases an
    /// operand slot, `invalidate(dst)` leaves the operand's register unbound but
    /// still in use), so they must never be handed out even when free.
    fn find_free(&self, pinned: &[GP]) -> Option<GP> {
        (0..GP_ALLOC_SET.len())
            .find(|&i| self.holders[i].is_empty() && !self.excluded(pinned, i))
            .map(|i| GP_ALLOC_SET[i])
    }

    /// `GP_ALLOC_SET[i]` may not be handed out: an operand register of the
    /// current op, or the loop pin's.
    fn excluded(&self, pinned: &[GP], i: usize) -> bool {
        let reg = GP_ALLOC_SET[i];
        pinned.contains(&reg) || self.pin_reg() == Some(reg)
    }

    /// Online allocation primitive for the codegen driver: return a register
    /// (a free one, else the **oldest** non-`pinned` resident) plus the spills
    /// the caller must emit for the dirty holders it evicted (one store per
    /// dirty slot the victim carried). The returned register is left
    /// **unbound** — the caller binds it after loading/computing the value (so
    /// the binop result always lands in a register).
    pub(in crate::codegen::jitgen) fn alloc_reg(&mut self, pinned: &[GP]) -> (GP, Vec<(GP, SlotId)>) {
        if let Some(reg) = self.find_free(pinned) {
            return (reg, vec![]);
        }
        let victim_idx = (0..GP_ALLOC_SET.len())
            .filter(|&i| !self.excluded(pinned, i))
            .min_by_key(|&i| self.age[i])
            .expect("more pinned registers than the allocatable set");
        let reg = GP_ALLOC_SET[victim_idx];
        (reg, self.evict_reg(reg))
    }

    /// Forget every holder of `reg`, returning the spills owed for its dirty
    /// ones. Used when an op is about to destroy the register's value
    /// (`Mul`/`Div` clobber `rhs`), and by [`Self::alloc_reg`] for its victim.
    ///
    /// On the pin register this drops the pin's copy (the op is about to
    /// overwrite the register); the pin holds nothing dirty.
    pub(in crate::codegen::jitgen) fn evict_reg(&mut self, reg: GP) -> Vec<(GP, SlotId)> {
        if self.pin_reg() == Some(reg) {
            self.invalidate_pin();
        }
        let idx = Self::index_of(reg);
        let spills = self.holders[idx]
            .iter()
            .filter(|h| h.dirty)
            .map(|h| (reg, h.slot))
            .collect();
        self.holders[idx].clear();
        spills
    }

    /// Record that `reg` now (also) caches `slot`, dropping any prior cache of
    /// `slot`. The register's other holders are kept: they carry the same value,
    /// which is exactly what a slot copy relies on (`%dst = %src` binds `dst`
    /// dirty to `src`'s register). A register handed out by [`Self::alloc_reg`]
    /// is empty, so binding a freshly produced value never shares by accident.
    ///
    /// The pin register takes only its own slot, and then holds it valid.
    pub(in crate::codegen::jitgen) fn bind(&mut self, reg: GP, slot: SlotId, dirty: bool) {
        self.invalidate(slot);
        let age = self.tick();
        let idx = Self::index_of(reg);
        self.holders[idx].push(Holder { slot, dirty });
        self.age[idx] = age;
        if let Some(p) = &mut self.pin
            && p.reg == reg
        {
            debug_assert_eq!(p.slot, slot, "the pin register holds its own slot only");
            p.valid = true;
        }
    }

    /// If `slot` is a GP resident, return its register and mark it **clean** —
    /// the caller is about to write the register to `slot`'s stack home, making
    /// the home current. Used to feed a GP-resident value to a non-GP consumer
    /// that reads the operand from its stack home (e.g. the integer operand of a
    /// mixed `Integer + Float` op), without evicting the resident: it stays
    /// cached (now clean), so a following integer op still reuses it and a later
    /// flush does not re-spill it.
    pub(in crate::codegen::jitgen) fn sync(&mut self, slot: SlotId) -> Option<GP> {
        let (i, j) = self.position_of(slot)?;
        self.holders[i][j].dirty = false;
        Some(GP_ALLOC_SET[i])
    }

    /// Free every register caching a slot at or above `sp` — those temporaries
    /// are dead (popped past the stack pointer), so they are dropped without a
    /// spill, mirroring `clear_above_next_sp`.
    pub(in crate::codegen::jitgen) fn free_above_sp(&mut self, sp: SlotId) {
        for h in self.holders.iter_mut() {
            h.retain(|held| held.slot < sp);
        }
    }

    /// Whether, once the popped temporaries (`>= sp`) and the redefined `dst`
    /// are dropped, `reg` caches nothing but (at most) `slot` — the condition
    /// for an op to compute in place in `reg` without corrupting another
    /// slot's only copy. Answered *before* those drops happen, so an op can
    /// plan its result register ahead of its deopt snapshot.
    pub(in crate::codegen::jitgen) fn will_hold_at_most(
        &self,
        reg: GP,
        slot: SlotId,
        sp: SlotId,
        dst: Option<SlotId>,
    ) -> bool {
        // The pin register is never computed into.
        self.pin_reg() != Some(reg)
            && self.holders[Self::index_of(reg)]
            .iter()
            .all(|h| h.slot == slot || h.slot >= sp || Some(h.slot) == dst)
    }

    /// Make room for one register to be allocated *after* `free_above_sp(sp)`
    /// and `invalidate(dst)` have run, doing neither yet: if some non-`pinned`
    /// register will be free by then, nothing is owed; otherwise the oldest
    /// non-`pinned` register is evicted now and its dirty holders' spills are
    /// returned. An op that side-exits (a deopt) takes its write-back snapshot
    /// before it clears the popped operands, so that the interpreter can
    /// re-read them from their homes; a victim evicted *after* that snapshot
    /// would still be listed in it, and the deopt would then store the
    /// register — by then overwritten with the result — into the victim's
    /// home. Reserving here, before the snapshot, keeps every snapshotted
    /// register intact up to the side exit.
    pub(in crate::codegen::jitgen) fn reserve(
        &mut self,
        pinned: &[GP],
        sp: SlotId,
        dst: Option<SlotId>,
    ) -> Vec<(GP, SlotId)> {
        let free_later = |hs: &Vec<Holder>| hs.iter().all(|h| h.slot >= sp || Some(h.slot) == dst);
        if (0..GP_ALLOC_SET.len())
            .any(|i| !self.excluded(pinned, i) && free_later(&self.holders[i]))
        {
            return vec![];
        }
        let victim_idx = (0..GP_ALLOC_SET.len())
            .filter(|&i| !self.excluded(pinned, i))
            .min_by_key(|&i| self.age[i])
            .expect("more pinned registers than the allocatable set");
        self.evict_reg(GP_ALLOC_SET[victim_idx])
    }
}

impl GpRegFile {
    /// A slot is about to be redefined: drop any stale register cache of it (the
    /// old value is dead, so no spill). The register's other holders stay.
    pub(in crate::codegen::jitgen) fn invalidate(&mut self, slot: SlotId) {
        for h in self.holders.iter_mut() {
            h.retain(|held| held.slot != slot);
        }
        if let Some(p) = &mut self.pin
            && p.slot == slot
        {
            p.valid = false;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sl(i: u16) -> SlotId {
        SlotId::new(i)
    }

    const R8: GP = GP::R8;
    const R9: GP = GP::R9;
    const R10: GP = GP::R10;

    /// A slot copy binds the destination to the source's register as a second
    /// (dirty) holder: both resolve to the register, only the copy owes a store,
    /// and the register is no longer a sole-holder (no in-place compute).
    #[test]
    fn copy_shares_the_register() {
        let mut rf = GpRegFile::new();
        rf.bind(R8, sl(1), /* dirty */ false);
        rf.bind(R8, sl(2), /* dirty */ true);
        assert_eq!(rf.reg_of(sl(1)), Some(R8));
        assert_eq!(rf.reg_of(sl(2)), Some(R8));
        assert_eq!(rf.dirty_reg_of(sl(1)), None);
        assert_eq!(rf.dirty_reg_of(sl(2)), Some(R8));
        assert!(!rf.holds_only(R8, sl(1)));
        assert!(!rf.holds_only(R8, sl(2)));
        assert_eq!(rf.dirty_residents(), vec![(R8, sl(2))]);
        // Dropping one holder leaves the other, now alone in the register.
        rf.invalidate(sl(1));
        assert_eq!(rf.reg_of(sl(1)), None);
        assert!(rf.holds_only(R8, sl(2)));
        // The flush stores the copy only.
        assert_eq!(rf.take_dirty_spills(), vec![(R8, sl(2))]);
        assert!(rf.is_empty());
    }

    /// Evicting a shared register owes one store per dirty holder, and a
    /// popped temporary among the holders is dropped without one.
    #[test]
    fn eviction_spills_every_dirty_holder() {
        let mut rf = GpRegFile::new();
        rf.bind(R8, sl(1), false);
        rf.bind(R8, sl(2), true);
        rf.bind(R8, sl(9), true);
        rf.bind(R9, sl(3), true);
        rf.bind(R10, sl(4), true);
        rf.bind(GP::R11, sl(5), true);
        // %9 is a temporary popped by the stack pointer: no store owed.
        rf.free_above_sp(sl(9));
        assert_eq!(rf.reg_of(sl(9)), None);
        // The file is full; R8 (the oldest) is the victim, spilling %2 only.
        let (reg, spills) = rf.alloc_reg(&[]);
        assert_eq!(reg, R8);
        assert_eq!(spills, vec![(R8, sl(2))]);
        assert!(rf.is_free(R8));
        assert_eq!(rf.reg_of(sl(1)), None);
        // `evict_reg` does the same for a register an op is about to clobber.
        assert_eq!(rf.evict_reg(R9), vec![(R9, sl(3))]);
        assert!(rf.is_free(R9));
    }

    /// Rebinding a slot elsewhere removes it from its old register without
    /// disturbing the register's other holders.
    #[test]
    fn rebind_moves_one_holder() {
        let mut rf = GpRegFile::new();
        rf.bind(R8, sl(1), false);
        rf.bind(R8, sl(2), true);
        rf.bind(R9, sl(2), true);
        assert_eq!(rf.reg_of(sl(2)), Some(R9));
        assert!(rf.holds_only(R8, sl(1)));
        assert_eq!(rf.sync(sl(2)), Some(R9));
        assert!(rf.dirty_residents().is_empty());
    }

    /// `sync` makes a resident's stack home current (returns its register) and
    /// leaves it cached but clean — so a later flush does not re-spill it and a
    /// later op still reuses it. Used to feed a GP-resident integer operand to a
    /// mixed `Integer + Float` op, which reads it from its stack home.
    #[test]
    fn sync_marks_clean_and_keeps_resident() {
        let mut rf = GpRegFile::new();
        rf.bind(R8, sl(7), /* dirty */ true);
        // A dirty resident syncs to its register and is now clean.
        assert_eq!(rf.sync(sl(7)), Some(R8));
        assert!(rf.reg_of(sl(7)) == Some(R8)); // still cached
        assert!(rf.dirty_residents().is_empty()); // but clean now
        // A flush of the (now clean) file emits no spill.
        assert!(rf.take_dirty_spills().is_empty());
        // Syncing a non-resident slot is a no-op.
        assert_eq!(GpRegFile::new().sync(sl(3)), None);
    }
}
