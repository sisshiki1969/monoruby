use super::*;

///
/// §5 stage 3c-i — the register-allocation **policy** seam.
///
/// Every fpr allocation in the JIT funnels through these two primitives (operand
/// loads `load_fpr*` / `fetch_float*`, destination defs `def_F` / `def_Sf*`, and
/// the merge's `apply_join` `TryFresh*` all reach them via `set_new_*` /
/// `try_set_new_*`). Isolating them as a named unit is the Layer-② allocator
/// seam: the body here is today's greedy policy verbatim, and a future loop-aware
/// policy (3c-ii) plugs in here — its furthest-next-use spill-victim choice
/// replaces phase 1's "first all-`Sf` register". The functions take `&mut
/// SlotState` because the policy reads/mutates slot placements (the phase-1
/// demotion). Behaviour-identical to the prior `SlotState` methods.
///
mod alloc_policy {
    use super::*;

    ///
    /// §5 stage 3c-i increment 2 — the explicit allocation-**policy** object the
    /// fpr allocator consults, threaded so the placement decision is a first-class
    /// parameter rather than logic hard-wired into the dataflow.
    ///
    /// Today this is the *seam only*: the default policy ranks phase-1 spill
    /// victims by physical-pool index, reproducing the prior "first all-`Sf`
    /// register" choice exactly, so placements stay byte-identical.
    ///
    /// **Why a swappable policy at all (the honest framing — see doc §15.9).**
    /// The seam is *not* here for a better spill-victim heuristic: §15.9 shows the
    /// victim choice is inherently performance-neutral, because phase 1 only ever
    /// demotes an all-`Sf` register (the stack already holds the canonical value,
    /// so the demote is free and the value reloads lazily) and **never** an `F`
    /// register — a pure unboxed float keeps its fpr through phase 1, and phase 2
    /// spills it *unboxed* to a `VirtFPReg`. So no allocation choice ever boxes an
    /// `F`; reshuffling which `Sf` cache is dropped cannot move the needle (every
    /// victim-policy probe in §15.1 came back a no-op for exactly this reason).
    /// The seam exists for goal 3 (§3 Layer ②): a *fundamentally different*
    /// allocation **strategy** plugs in here — notably the VM-residual allocator's
    /// "no pool, every value in its stack home" policy — not a tweaked JIT victim
    /// rank. `victim_rank` is the minimal pluggable point that proves the seam.
    ///
    #[derive(Default)]
    pub(super) struct AllocCtx;

    impl AllocCtx {
        ///
        /// Phase-0 placement: pick which *vacant* (non-pinned, empty) fpr a fresh
        /// value is assigned to. This is the real performance lever (doc §27): the
        /// physical register a value lands in, and — via the pool/spill boundary —
        /// whether it stays in `xmm` or overflows. The default reproduces the
        /// historical lowest-physical-index first-fit, so placements stay
        /// byte-identical; a global (e.g. loop-aware linear-scan) policy overrides
        /// it to keep loop-carried values resident. Zero extra cost on the default
        /// path (it is the original Phase-0 scan).
        ///
        fn pick_vacant(&self, state: &SlotState, occ: &PoolOccupancy) -> Option<FPReg> {
            // Pool registers only, per this phase's contract ("vacant
            // phys"). A vacant *spill* id must not be re-issued: it may
            // be a persistent raw-f64 home whose binding this path does
            // not carry (issued from the frame's home ledger while the
            // frame was suspended, or dropped at a join) — and homes are
            // written by callee code at runtime, while spill-resident
            // values are not saved across calls. Retired transient spill
            // ids are also skipped; that only costs frame bytes.
            (0..state.fpr_alloc.len().min(PHYS_FPR_POOL))
                .map(FPReg)
                .find(|&fpr| !state.fpr_alloc.is_pinned(fpr) && !occ.occupied(fpr))
        }

        ///
        /// Spill-victim priority for [`try_alloc_fpr`] phase 1: the candidate with
        /// the **smallest** rank is demoted first. The default policy ranks purely
        /// by physical-pool index, so `min_by_key` reproduces the historical
        /// lowest-index-first choice (behaviour-identical). A swapped-in policy
        /// (e.g. the VM-residual allocator) overrides this; per §15.9 the override
        /// is a strategy change, not a perf-victim tweak.
        ///
        fn victim_rank(&self, fpr: FPReg) -> usize {
            fpr.0
        }
    }

    ///
    /// Returns `None` if every fpr holds at least one `F` slot (a real spill;
    /// use [`alloc_fpr`] from a context that has access to `AsmIr`).
    ///
    pub(super) fn try_alloc_fpr(state: &mut SlotState) -> Option<FPReg> {
        try_alloc_fpr_ctx(state, &AllocCtx::default())
    }

    ///
    /// As [`try_alloc_fpr`], but with an explicit [`AllocCtx`] driving the phase-1
    /// spill-victim choice. The two-phase structure (vacant first, then an
    /// all-`Sf` demote) is policy-invariant; only the *victim ranking* is
    /// pluggable.
    ///
    pub(super) fn try_alloc_fpr_ctx(state: &mut SlotState, ctx: &AllocCtx) -> Option<FPReg> {
        // One pass over the slots gives every per-register fact both
        // phases need (the register file is derived from the slot modes,
        // not tracked beside them).
        let occ = state.pool_occupancy();
        // Phase 0: a vacant fpr chosen by the policy (default: lowest index, as
        // before — the real placement lever, doc §27).
        if let Some(fpr) = ctx.pick_vacant(state, &occ) {
            return Some(fpr);
        }
        // Phase 1: among the fprs whose linked slots are *all* `Sf` (stack already
        // holds the canonical value, the fpr is a read-only cache), pick the one
        // the policy ranks lowest and demote its slots to `S`. No asm is emitted —
        // the stack is canonical. The default `victim_rank` is the pool index, so
        // `min_by_key` selects the same register the prior `for 0..len { return
        // first }` scan did.
        // Pool registers only, like phase 0: "freeing" a spill id gains
        // nothing over a fresh phase-2 spill (both are stack slots), and
        // demoting a spill-resident raw-f64 *home* would re-issue an id
        // that callee code still writes at runtime.
        let victim = (0..state.fpr_alloc.len().min(PHYS_FPR_POOL))
            .map(FPReg)
            .filter(|&fpr| !state.fpr_alloc.is_pinned(fpr) && occ.occupied(fpr))
            .filter(|&fpr| occ.all_sf(fpr))
            .min_by_key(|&fpr| ctx.victim_rank(fpr))?;
        // Demoting the slots is what frees the register: the file is
        // derived from the modes, so there is no reverse entry to clear.
        let to_demote: Vec<(SlotId, SfGuarded)> = state
            .fpr_slots(victim)
            .map(|s| match state.mode(s) {
                LinkMode::Sf(_, g) => (s, g),
                _ => unreachable!(),
            })
            .collect();
        for (s, g) in to_demote {
            state.set_mode(s, LinkMode::S(g.into()));
        }
        Some(victim)
    }

    ///
    /// Allocate a new VirtFPReg. Phase 0 (vacant phys) and Phase 1 (Sf-only
    /// demote) first; if both fail, a fresh phase-2 spill slot (`VirtFPReg(N)`,
    /// `N >= PHYS_FPR_POOL`) that lives on the stack and is swapped in at use.
    ///
    pub(super) fn alloc_fpr(state: &mut SlotState) -> FPReg {
        if let Some(x) = try_alloc_fpr(state) {
            return x;
        }
        state.fpr_alloc.push_spill()
    }
}

///
/// The fpr-register file's *bookkeeping*: how many register ids have been
/// issued and which are pinned. Which slots occupy a register is **not**
/// tracked here — it is derived from the slot modes
/// ([`SlotState::fpr_slots`] / [`SlotState::pool_occupancy`]), so the file
/// can never disagree with the placements. Indices `0..PHYS_FPR_POOL` map to
/// physical `xmm2..xmm15`; `>= PHYS_FPR_POOL` are stack spills.
///
#[derive(Clone, Default)]
pub(super) struct FprAllocator {
    /// Ids issued so far: the pool prefix plus every spill id this file
    /// has ever issued or been grown to. A spill id is never re-issued
    /// once vacant (see `pick_vacant`), so this only grows.
    len: usize,
    /// fpr registers that must not be reused by `alloc_fpr` until unpinned.
    pinned: Vec<FPReg>,
}

impl FprAllocator {
    fn new() -> Self {
        Self {
            len: PHYS_FPR_POOL,
            pinned: Vec::new(),
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn is_pinned(&self, fpr: FPReg) -> bool {
        self.pinned.contains(&fpr)
    }

    fn grow_to(&mut self, new_len: usize) {
        self.len = self.len.max(new_len);
    }

    /// Issue a fresh spill id beyond everything issued so far.
    fn push_spill(&mut self) -> FPReg {
        let new_id = self.len;
        self.len += 1;
        FPReg(new_id)
    }

    fn pin(&mut self, fpr: FPReg) {
        if !self.pinned.contains(&fpr) {
            self.pinned.push(fpr);
        }
    }

    fn unpin(&mut self, fpr: FPReg) {
        if let Some(pos) = self.pinned.iter().position(|x| *x == fpr) {
            self.pinned.swap_remove(pos);
        }
    }
}

///
/// Per-pool-register facts collected in one pass over the slots: whether
/// the register holds any slot, and whether every slot it holds is an `Sf`
/// view (the stack is canonical, so the register can be dropped for free).
///
struct PoolOccupancy {
    occupied: [bool; PHYS_FPR_POOL],
    all_sf: [bool; PHYS_FPR_POOL],
}

impl PoolOccupancy {
    fn occupied(&self, fpr: FPReg) -> bool {
        self.occupied[fpr.0]
    }

    fn all_sf(&self, fpr: FPReg) -> bool {
        self.occupied[fpr.0] && self.all_sf[fpr.0]
    }
}

#[derive(Clone, Default)]
pub(crate) struct SlotState {
    /// One record per slot: placement + type (`LinkMode`), liveness, and
    /// the outer-float provenance hints. Every per-slot fact is added,
    /// cleared and merged through this one record.
    slots: Vec<Slot>,
    /// fpr-register allocation state.
    fpr_alloc: FprAllocator,
    /// Per-basic-block local GP register file (the local GP allocator). Empty (and
    /// flushed) at every block boundary, so it never carries state across a
    /// merge despite living in the cloned `SlotState`.
    pub(in crate::codegen::jitgen) gp_regfile: crate::codegen::jitgen::gp_alloc::GpRegFile,
    local_num: usize,
}

///
/// One slot of an abstract frame.
///
#[derive(Clone, Default)]
struct Slot {
    /// Where the live copies of the value are, and what is known about
    /// its type.
    mode: LinkMode,
    /// Liveness (use / kill) information.
    used: IsUsed,
    /// Stage-A use propagation: provenance — the slot's current value
    /// came from a `LoadDynVar` of the frame `outer` levels out, slot
    /// `src`. Recorded by the `LoadDynVar` lowering, cleared on
    /// redefinition ([`SlotState::discard`]); a mode transition that
    /// keeps the value (write-back, unguarded demotion) keeps it.
    /// Consulted when the slot is consumed as a raw f64
    /// ([`AbstractState::use_as_float`]), which marks the owner frame's
    /// slot on the chain. A correctness-neutral hint: it only feeds the
    /// loop-entry float-adoption policy.
    dynvar_src: Option<(u16, SlotId)>,
    /// Stage-A use propagation, the owner-side landing spot: an inlined
    /// callee read this slot through the frame chain and consumed it as a
    /// raw f64. Deliberately *not* folded into `IsUsed` — the type/kill
    /// lattice drives the long-tuned owner-side float policies, and this
    /// signal feeds only the loop-entry `Sf` adoption
    /// (`Liveness::subtree_float_reads`).
    subtree_float_read: bool,
    /// Stage-B home-aliased reads: for a slot holding a bare-`F` home read
    /// of an outer float, the recipe for loading the *boxed* twin straight
    /// from the owner's slot (which still holds the value the home read
    /// took) instead of re-boxing the fpr. Valid only while nothing can
    /// have rewritten the owner's slot: killed at every store through the
    /// frame chain and at every call boundary (`clear_dynvar_aliases`),
    /// and dropped with the binding on any mode transition
    /// ([`SlotState::clear`]).
    dynvar_alias: Option<DynVarAliasLoad>,
}

///
/// Stage-B home-aliased reads: the deferred boxed-twin load — the same
/// chain-resolved static frame offset `LoadDynVarSpecialized` uses,
/// captured at the read so the consult site (deep in the state machinery,
/// with no `JitContext` at hand) can emit it verbatim.
///
#[derive(Debug, Clone, PartialEq)]
pub(in crate::codegen::jitgen) struct DynVarAliasLoad {
    pub(in crate::codegen::jitgen) ids: Vec<crate::codegen::jitgen::context::SpecializedId>,
    pub(in crate::codegen::jitgen) extra: usize,
    pub(in crate::codegen::jitgen) reg: SlotId,
}

impl std::fmt::Debug for SlotState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for i in self.all_regs() {
            write!(f, "[%{}: {:?}] ", i.0, self.mode(i))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl SlotState {
    fn new(cc: &JitContext, default: LinkMode) -> Self {
        let total_reg_num = cc.total_reg_num();
        let local_num = cc.local_num();
        let self_class = Guarded::from_class(cc.self_class());
        let slot = Slot {
            mode: default,
            ..Slot::default()
        };
        let mut ctx = SlotState {
            slots: vec![slot; total_reg_num],
            fpr_alloc: FprAllocator::new(),
            gp_regfile: crate::codegen::jitgen::gp_alloc::GpRegFile::new(),
            local_num,
        };
        ctx.set_S_with_guard(SlotId::self_(), self_class);
        ctx
    }

    pub(super) fn new_loop(cc: &JitContext) -> Self {
        SlotState::new(cc, LinkMode::default())
    }

    pub(super) fn new_method(cc: &JitContext) -> Self {
        let mut ctx = SlotState::new(cc, LinkMode::V);
        for i in cc.locals() {
            ctx.set_mode(i, LinkMode::C(Value::nil()));
        }
        for i in cc.args() {
            ctx.set_mode(i, LinkMode::default());
        }

        if let JitType::Specialized {
            args_info: JitArgumentInfo(Some(args)),
            ..
        } = cc.jit_type()
        {
            for (i, arg) in args.iter().enumerate() {
                match arg {
                    LinkMode::C(_) | LinkMode::MaybeNone | LinkMode::None => {
                        ctx.set_mode(SlotId(i as u16), *arg);
                    }
                    _ => {}
                }
            }
        } else {
            let fid = cc.func_id();
            let info = &cc.store[fid];
            // Set optional arguments to MaybeNone.
            for i in info.req_num()..info.reqopt_num() {
                let slot = SlotId(1 + i as u16);
                ctx.set_MaybeNone(slot);
            }
            // Set keyword arguments to MaybeNone.
            let kw = info.kw_reg_pos();
            for (i, _) in info.kw_names().iter().enumerate() {
                ctx.set_MaybeNone(kw + i);
            }
        }
        ctx
    }

    pub(super) fn slots_len(&self) -> usize {
        self.slots.len()
    }

    /// The slot's abstract type as a lattice element; the sentinels
    /// (`None` / `MaybeNone` / `V`) carry no type and read as ⊤.
    fn ty(&self, slot: SlotId) -> Guarded {
        match self.mode(slot) {
            LinkMode::None | LinkMode::MaybeNone | LinkMode::V => Guarded::Value,
            o => o.guarded(),
        }
    }

    /// The pure type-lattice meet over the two frames' slots — element-wise
    /// `Guarded::join`, no placement involved. For non-sentinel slots this
    /// equals the fused `AbstractFrame::join`'s resulting type, which the
    /// debug replay (`verify_join_replay`) asserts after every merge.
    #[cfg(debug_assertions)]
    pub(super) fn join_ty(&self, other: &SlotState) -> Vec<Guarded> {
        self.all_regs()
            .map(|i| self.ty(i).join(&other.ty(i)))
            .collect()
    }

    pub(super) fn equiv(&self, other: &Self) -> bool {
        assert_eq!(self.slots_len(), other.slots_len());
        self.all_regs().all(|i| self.mode(i).equiv(&other.mode(i)))
    }

    pub(in crate::codegen::jitgen) fn liveness_analysis(&mut self, liveness: &Liveness) {
        let (used_as_float, killed) = (liveness.loop_used_as_float(), liveness.killed());
        self.use_float(used_as_float);
        self.kill_unused(killed);
    }

    fn use_float(&mut self, used_as_float: impl Iterator<Item = (SlotId, bool)>) {
        for (slot, as_f64) in used_as_float {
            match self.mode(slot) {
                LinkMode::S(_) => {
                    if as_f64 {
                        // Liveness-driven hint only — if no fpr is free without
                        // a real spill, leave the slot on the stack and rely on
                        // a lazy `load_fpr` later (which has access to AsmIr
                        // and can spill if needed).
                        let _ = self.try_set_new_Sf(slot, SfGuarded::Float);
                    }
                }
                LinkMode::C(_) => {}
                LinkMode::Sf(_, _) => {}
                LinkMode::F(x) => {
                    if !as_f64 {
                        self.set_Sf(slot, x, SfGuarded::Float);
                    }
                }
                LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                    unreachable!("use_float {:?}", self.mode(slot));
                }
            };
        }
    }

    fn kill_unused(&mut self, unused: impl Iterator<Item = SlotId>) {
        for slot in unused {
            self.discard(slot);
        }
    }

    pub(in crate::codegen::jitgen) fn locals(&self) -> std::ops::Range<SlotId> {
        SlotId(1)..self.temp_start()
    }

    pub(in crate::codegen::jitgen) fn all_regs(&self) -> std::ops::Range<SlotId> {
        SlotId(0)..SlotId(self.slots_len() as u16)
    }

    fn temps(&self) -> std::ops::Range<SlotId> {
        self.temp_start()..SlotId(self.slots_len() as u16)
    }

    pub(super) fn temp_start(&self) -> SlotId {
        SlotId((1 + self.local_num) as u16)
    }

    pub(in crate::codegen::jitgen) fn mode(&self, slot: SlotId) -> LinkMode {
        self.slots[slot.0 as usize].mode
    }

    pub(in crate::codegen::jitgen) fn guarded(&self, slot: SlotId) -> Guarded {
        self.mode(slot).guarded()
    }

    /// True when the value in *slot* is proven to be an immediate (non-heap)
    /// at run time, so the generational-GC write barrier for storing it into
    /// a heap object can be elided.
    ///
    /// `Guarded::Float` is deliberately **not** immediate: the guard also
    /// covers heap `Float` RValues, and an unboxed (`F`/`Sf`) float is boxed
    /// on the way to the store — a non-flonum-representable f64 boxes to a
    /// heap allocation. `Guarded::Class(INTEGER_CLASS)` never occurs (Fixnum
    /// has its own variant and a Bignum stays `Guarded::Value`), and
    /// `BOOL_CLASS` is the IC class both boolean literals collapse to.
    pub(in crate::codegen::jitgen) fn is_guarded_immediate(&self, slot: SlotId) -> bool {
        if let LinkMode::C(v) = self.mode(slot) {
            return v.is_packed_value();
        }
        match self.guarded(slot) {
            Guarded::Fixnum => true,
            Guarded::Float | Guarded::Value => false,
            Guarded::Class(c) => {
                c == NIL_CLASS
                    || c == TRUE_CLASS
                    || c == FALSE_CLASS
                    || c == BOOL_CLASS
                    || c == SYMBOL_CLASS
            }
        }
    }

    /// True when *slot*'s abstract type is exactly `Float` — the allocation-free
    /// (Layer-①) signal the L2-1 loop-entry float adoption policy reads in place
    /// of the analysis-pass placement (`mode == F`). See doc §16.
    pub(in crate::codegen::jitgen) fn is_float_typed(&self, slot: SlotId) -> bool {
        matches!(self.guarded(slot), Guarded::Float)
    }

    pub(in crate::codegen::jitgen) fn class(&self, slot: SlotId) -> Option<ClassId> {
        self.guarded(slot).class()
    }

    /// True only when *slot* is `LinkMode::C` with a packed immediate
    /// value (fixnum, flonum, nil, true, false, symbol).
    ///
    /// Used to gate iseq specialization: passing a heap-resident
    /// `LinkMode::C` (e.g. a class constant) through `from_caller` lets
    /// the callee's body propagate Guarded class info inferred from
    /// polymorphic inline caches back to the caller's dst slot — making
    /// the caller believe the return type is whatever class happened to
    /// win that cache last, which is unsound. Restricting specialization
    /// to immediates preserves the pre-existing behavior.
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn is_C_immediate(&self, slot: SlotId) -> bool {
        matches!(self.mode(slot), LinkMode::C(v) if v.is_immediate().is_some())
    }
}

impl SlotState {
    pub(super) fn set_mode(&mut self, slot: SlotId, mode: LinkMode) {
        self.slots[slot.0 as usize].mode = mode;
    }

    pub(super) fn is_used(&self, slot: SlotId) -> &IsUsed {
        &self.slots[slot.0 as usize].used
    }

    pub(super) fn is_used_mut(&mut self, slot: SlotId) -> &mut IsUsed {
        &mut self.slots[slot.0 as usize].used
    }

    /// The slots bound to *fpr* (as `F` or `Sf`), in slot order — the
    /// register file, read off the slot modes.
    fn fpr_slots(&self, fpr: FPReg) -> impl Iterator<Item = SlotId> + '_ {
        self.all_regs().filter(move |&s| {
            matches!(self.mode(s), LinkMode::F(x) | LinkMode::Sf(x, _) if x == fpr)
        })
    }

    fn pool_occupancy(&self) -> PoolOccupancy {
        let mut occ = PoolOccupancy {
            occupied: [false; PHYS_FPR_POOL],
            all_sf: [true; PHYS_FPR_POOL],
        };
        for s in self.all_regs() {
            match self.mode(s) {
                LinkMode::F(x) if x.0 < PHYS_FPR_POOL => {
                    occ.occupied[x.0] = true;
                    occ.all_sf[x.0] = false;
                }
                LinkMode::Sf(x, _) if x.0 < PHYS_FPR_POOL => {
                    occ.occupied[x.0] = true;
                }
                _ => {}
            }
        }
        occ
    }

    ///
    /// Number of allocated spill slots (including the pool prefix).
    /// Used by `gen_bridge` so the source state can grow its own
    /// `fpr` vec up to the target's width before merge bridging.
    ///
    pub(in crate::codegen::jitgen) fn fpr_len(&self) -> usize {
        self.fpr_alloc.len()
    }

    ///
    /// Pad `self.fpr` with empty slot lists until it reaches at least
    /// *new_len*. The fresh entries correspond to spill slot ids that
    /// were allocated by a sibling branch but not by us; merging at a
    /// confluence point will route any `LinkMode::F(VirtFPReg(N))`
    /// referencing them into a `gen_fpr_swap` / `fpr_move` against a
    /// vacant binding, which `is_fpr_vacant` correctly reports.
    ///
    pub(super) fn grow_fpr_to(&mut self, new_len: usize) {
        self.fpr_alloc.grow_to(new_len);

    }

    /// Mark *fpr* off-limits for subsequent `alloc_fpr` /
    /// `try_alloc_fpr_demote` calls until [`Self::unpin_fpr`] is invoked.
    /// Use this when an fpr has just been produced (loaded or written) and
    /// is needed by an upcoming instruction in the same compile step —
    /// without the pin, a later allocation in the same step can choose the
    /// freshly-loaded fpr as a spill victim and reuse it for an unrelated
    /// value.
    pub(in crate::codegen::jitgen) fn pin_fpr(&mut self, fpr: FPReg) {
        self.fpr_alloc.pin(fpr);
    }

    pub(in crate::codegen::jitgen) fn unpin_fpr(&mut self, fpr: FPReg) {
        self.fpr_alloc.unpin(fpr);
    }

    ///
    /// Try to allocate a new fpr register without emitting any asm.
    ///
    /// Phase 0: returns the first vacant fpr.
    /// Phase 1: if no vacant fpr, finds an fpr whose linked slots are all `Sf`
    ///          (stack already holds the value, fpr is just a read-only cache);
    ///          demotes them all to `S` and returns the freed fpr. No asm is
    ///          emitted because the stack already has the canonical value.
    ///
    /// Returns `None` if every fpr holds at least one `F` slot (would require
    /// a real spill; use [`Self::alloc_fpr`] from a context that has access to
    /// `AsmIr`).
    ///
    fn try_alloc_fpr(&mut self) -> Option<FPReg> {
        alloc_policy::try_alloc_fpr(self)
    }

    fn alloc_fpr(&mut self) -> FPReg {
        alloc_policy::alloc_fpr(self)
    }

    ///
    /// Clear slot *reg* and set LinkMode to V.
    ///
    fn clear(&mut self, slot: SlotId) {
        // Redefining a slot drops any GP resident caching its old value (mirrors
        // the fpr file's derivation). The binop that *creates* a resident
        // rebinds it after this `clear`, so the cache stays correct.
        self.gp_regfile.invalidate(slot);
        // Any transition away from the aliased-`F` binding ends the alias;
        // the consult site takes it *before* transitioning.
        self.slots[slot.0 as usize].dynvar_alias = None;
        if self.mode(slot) == LinkMode::V {
            return;
        }
        self.set_mode(slot, LinkMode::V);
    }

    ///
    /// Discard slot *reg*.
    ///
    /// *reg* is set to V.
    ///
    pub(in crate::codegen::jitgen) fn discard(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            if slot.is_self() {
                panic!("{self:?}")
            }
            self.clear(slot);
            self.is_used_mut(slot).kill();
            // A redefinition ends the dynvar provenance — the new value did
            // not come through the frame chain.
            self.slots[slot.0 as usize].dynvar_src = None;
        }
    }

    pub(in crate::codegen::jitgen) fn discard_temps(&mut self) {
        for slot in self.temps() {
            self.discard(slot);
        }
    }

    ///
    /// _ -> MaybeNone
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_MaybeNone(&mut self, slot: SlotId) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::MaybeNone);
    }

    ///
    /// _ -> MaybeNone
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn set_None(&mut self, slot: SlotId) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::None);
    }

    ///
    /// _ -> S
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn set_S_with_guard(&mut self, slot: SlotId, guarded: Guarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::S(guarded));
    }

    ///
    /// _ -> S
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn set_S(&mut self, slot: SlotId) {
        self.set_S_with_guard(slot, Guarded::Value);
    }

    ///
    /// §15.3/§15.5 (`loop-keep-float`): adopt the back-edge fixpoint's `F` for a
    /// loop-carried float the loop-entry merge collapsed to `S`. In a *loop* JIT
    /// the value enters from the VM as a conservative boxed `S(Value)` even when
    /// the fixpoint proves it is a `Float`; left as `S` the body decodes+reboxes
    /// it every iteration (§15.4). Re-establishing `F` keeps the body unboxed;
    /// the forward entry is unboxed *once* at the pre-header by the `S -> F`
    /// bridge, whose `float_to_fpr` carries the runtime float **guard** (deopt if
    /// the VM value is not a float), so the specialization is sound.
    ///
    /// `promotable(i)` (computed by the caller from the actual predecessor
    /// entries) gates each slot: every predecessor must have a valid `_ -> F`
    /// bridge (`F`/`S`/`Sf`/float-`C`) — otherwise a non-float-`C` path would hit
    /// the `C -> F` `unreachable!` and `F` would be unsound for it.
    ///
    /// **Layer-② policy/mechanism split (doc §16, increment L2-0).** This method
    /// is the *mechanism* — "for each loop-carried slot the boxed loop-entry left
    /// `S`/`Sf`, adopt `F` if a physical fpr is free." The *adoption policy*
    /// `adopt(i)` — which slots should re-adopt `F` — is supplied by the caller.
    /// Today the caller's policy reads the analysis-pass fixpoint's **placement**
    /// (`backedge.mode(i) == F`); L2-1 swaps in a type+liveness policy
    /// (`backedge` type `Float` ∧ used-as-float-in-loop) to decouple this consumer
    /// from the analysis-pass allocation — the first step of making the analysis
    /// pass allocation-free.
    ///
    /// `adopt_sf(i)` selects the slots that re-adopt **`Sf`** instead: the
    /// back-edge placement is `Sf(Float)`, i.e. the slot is *current* on
    /// every path around the loop (the last write boxed it — an owner
    /// unbox, or an inlined block's write-through keep). Adopting `F`
    /// there would declare the slot stale, forcing every block-passing
    /// call site in the body to re-box it each iteration; `Sf` keeps the
    /// claim, so the call sites emit nothing and the back-edge bridge is
    /// an fpr move at worst. The entry is unboxed once at the pre-header
    /// by the `S -> Sf` bridge (guard + unbox, no store — the slot
    /// already holds the boxed value).
    pub(in crate::codegen::jitgen) fn keep_backedge_floats(
        &mut self,
        adopt: impl Fn(SlotId) -> bool,
        adopt_sf: impl Fn(SlotId) -> bool,
        adopt_deferred: impl Fn(SlotId) -> Option<FPReg>,
        promotable: impl Fn(SlotId) -> bool,
    ) {
        for i in self.all_regs() {
            if !promotable(i) {
                continue;
            }
            // Stage 1'': the back edge carries a spill-home view of a
            // slot the loop's subtree *stores* each iteration (the home
            // came from a store-driven promotion). Adopt `F(home)`
            // directly — slot stale, home authoritative: the entry
            // bridge's one guard+unbox establishes the home, the body's
            // deferring stores refresh it in place, and the back edge
            // meets `F(h) ⊔ F(h)` with nothing emitted. Adopting an `Sf`
            // here instead (the pre-deferral behavior) declares the slot
            // current, which the codegen pass's deferring store then
            // breaks — re-boxing at the back edge every iteration.
            if let Some(h) = adopt_deferred(i)
                && matches!(self.mode(i), LinkMode::S(_))
            {
                self.grow_fpr_to(h.0 + 1);
                self.set_F(i, h);
                continue;
            }
            // `try_set_new_F` / `try_set_new_Sf` (no phase-2 spill): only
            // specialize when a physical fpr is actually free. Spilling a
            // *speculative* loop-entry promotion into a `VirtFPReg` is not
            // worth it and is exercised wrongly under register pressure (the
            // `stress-spill-pool` path); leave the slot boxed in that case.
            if adopt_sf(i) && matches!(self.mode(i), LinkMode::S(_)) {
                self.try_set_new_Sf(i, SfGuarded::Float);
            } else if adopt(i) && matches!(self.mode(i), LinkMode::S(_) | LinkMode::Sf(_, _)) {
                self.try_set_new_F(i);
            }
        }
    }

    ///
    /// F/Sf -> F
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_F(&mut self, slot: SlotId, fpr: FPReg) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::F(fpr));
    }

    ///
    /// F/Sf -> Sf
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_Sf(&mut self, slot: SlotId, fpr: FPReg, guarded: SfGuarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::Sf(fpr, guarded));
    }

    ///
    /// C -> F (may emit a victim spill if no fpr is free).
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_new_F(&mut self, slot: SlotId) -> FPReg {
        let x = self.alloc_fpr();
        self.set_F(slot, x);
        x
    }

    ///
    /// C -> F (no asm emit). Returns `None` when only Phase-2 spill could free
    /// an fpr — caller should fall back (e.g. leave the slot as `S`).
    ///
    #[allow(non_snake_case)]
    pub(super) fn try_set_new_F(&mut self, slot: SlotId) -> Option<FPReg> {
        let x = self.try_alloc_fpr()?;
        self.set_F(slot, x);
        Some(x)
    }

    ///
    /// C -> Sf (may emit a victim spill if no fpr is free).
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_new_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> FPReg {
        let x = self.alloc_fpr();
        self.set_Sf(slot, x, guarded);
        x
    }

    ///
    /// C -> Sf (no asm emit). Returns `None` when only Phase-2 spill could free
    /// an fpr.
    ///
    #[allow(non_snake_case)]
    pub(super) fn try_set_new_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> Option<FPReg> {
        let x = self.try_alloc_fpr()?;
        self.set_Sf(slot, x, guarded);
        Some(x)
    }

    ///
    /// F -> Sf
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_Sf_float(&mut self, slot: SlotId, fpr: FPReg) {
        self.set_Sf(slot, fpr, SfGuarded::Float)
    }
}

impl SlotState {
    // APIs for 'def'

    ///
    /// Link *slot* to stack.
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn def_S(&mut self, slot: SlotId) {
        self.def_S_guarded(slot, Guarded::Value);
    }

    ///
    /// Link *slot* to stack with guard.
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn def_S_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self.discard(slot);
        self.set_mode(slot, LinkMode::S(guarded));
    }

    /// Refine `slot`'s abstract type to `Fixnum` **in place**, after a fixnum
    /// class guard has proved it — *without* discarding the GP resident (unlike
    /// `def_S_guarded`), so the value stays register-resident for a following
    /// integer op to reuse guard-free (`gp_ensure` keys off `is_fixnum`). Only an
    /// `S`-mode slot is refined: a constant (`C`) operand is already a known
    /// fixnum, and the other modes never reach a fixnum integer guard.
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn refine_S_fixnum(&mut self, slot: SlotId) {
        if matches!(self.mode(slot), LinkMode::S(_)) {
            self.set_mode(slot, LinkMode::S(Guarded::Fixnum));
        }
    }

    ///
    /// Link *slot* to a new fpr register (may emit a victim spill).
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_F(&mut self, slot: SlotId) -> FPReg {
        let fpr = self.alloc_fpr();
        self.discard(slot);
        self.set_F(slot, fpr);
        fpr
    }

    #[allow(non_snake_case)]
    pub(crate) fn def_F_with_fpr(&mut self, slot: SlotId, fpr: FPReg) -> FPReg {
        self.discard(slot);
        self.set_F(slot, fpr);
        fpr
    }

    ///
    /// Link *slot* to both of the stack and a new fpr register (may emit a
    /// victim spill).
    ///
    #[allow(non_snake_case)]
    fn def_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> FPReg {
        let fpr = self.alloc_fpr();
        self.discard(slot);
        self.set_Sf(slot, fpr, guarded);
        fpr
    }

    #[allow(non_snake_case)]
    pub(crate) fn def_Sf_float(&mut self, slot: SlotId) -> FPReg {
        self.def_Sf(slot, SfGuarded::Float)
    }

    ///
    /// Link *slot* to a concrete flonum value *i*.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_C_float(&mut self, slot: impl Into<Option<SlotId>>, f: f64) -> bool {
        if let Some(imm) = Immediate::flonum(f) {
            self.def_C(slot, imm);
            true
        } else {
            false
        }
    }

    ///
    /// Link *slot* to a concrete value *v*.
    ///
    /// `v` may be any `Value` (immediate or heap-resident). The pointer is kept
    /// alive across GC safepoints via `wb_literal` writing it to its stack slot
    /// before each GC checkpoint, and across constant redefinition via the
    /// `GuardConstVersion` deopt check emitted by `load_constant`.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_C(&mut self, slot: impl Into<Option<SlotId>>, v: impl Into<Value>) {
        if let Some(slot) = slot.into() {
            self.discard(slot);
            self.set_mode(slot, LinkMode::C(v.into()));
        }
    }

    // APIs for 'use'

    /// Liveness: used as f64 with no conversion. The chain-level
    /// [`AbstractState::use_as_float`] is the entry point — it also lands
    /// the stage-A float-read mark on the owner frame of a dynvar-loaded
    /// value, which this frame cannot reach on its own.
    pub(super) fn use_as_float_liveness(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_float();
    }

    /// Stage-A use propagation: where *slot*'s value came from, if it was
    /// loaded through the frame chain (`outer` levels out, slot `src`).
    pub(super) fn dynvar_src(&self, slot: SlotId) -> Option<(u16, SlotId)> {
        self.slots[slot.0 as usize].dynvar_src
    }

    pub(super) fn use_as_value(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_non_float();
    }

    ///
    /// Stage-A use propagation: record that *slot*'s current value came
    /// from a `LoadDynVar` of the frame *outer* levels out, slot *src*.
    ///
    pub(in crate::codegen::jitgen) fn set_dynvar_src(
        &mut self,
        slot: SlotId,
        outer: usize,
        src: SlotId,
    ) {
        if let Ok(outer) = u16::try_from(outer) {
            self.slots[slot.0 as usize].dynvar_src = Some((outer, src));
        }
    }

    ///
    /// Stage-A use propagation: the callee-side mark landing on this
    /// (parked owner) frame — the subtree-read flag the loop-entry `Sf`
    /// adoption reads via [`Liveness::subtree_float_reads`]. `IsUsed` is
    /// deliberately left untouched (see the field comment).
    ///
    pub(in crate::codegen::jitgen) fn mark_subtree_float_read(&mut self, slot: SlotId) {
        if let Some(s) = self.slots.get_mut(slot.0 as usize) {
            s.subtree_float_read = true;
        }
    }

    pub(in crate::codegen::jitgen) fn subtree_float_read(&self, slot: SlotId) -> bool {
        self.slots
            .get(slot.0 as usize)
            .is_some_and(|s| s.subtree_float_read)
    }

    ///
    /// Stage-C loop adoption: every slot of this frame some inlined
    /// callee read as a raw f64.
    ///
    pub(in crate::codegen::jitgen) fn subtree_float_read_slots(&self) -> Vec<SlotId> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(i, s)| s.subtree_float_read.then_some(SlotId(i as u16)))
            .collect()
    }

    ///
    /// Stage-B home-aliased reads: attach the boxed-twin recipe to a fresh
    /// bare-`F` home read of an outer float.
    ///
    pub(in crate::codegen::jitgen) fn set_dynvar_alias(
        &mut self,
        slot: SlotId,
        alias: DynVarAliasLoad,
    ) {
        self.slots[slot.0 as usize].dynvar_alias = Some(alias);
    }

    ///
    /// Stage-B home-aliased reads: consume the alias at its consult site
    /// (the `F` boxed-use arm), before the `F -> Sf` transition clears it.
    ///
    pub(super) fn take_dynvar_alias(&mut self, slot: SlotId) -> Option<DynVarAliasLoad> {
        self.slots[slot.0 as usize].dynvar_alias.take()
    }

    ///
    /// Stage-B home-aliased reads: a store through the frame chain or a
    /// call boundary may rewrite any owner slot, so every alias dies.
    ///
    pub(in crate::codegen::jitgen) fn clear_dynvar_aliases(&mut self) {
        for s in &mut self.slots {
            s.dynvar_alias = None;
        }
    }

    ///
    /// Stage-A use propagation: merge the hint fields at a path join —
    /// provenance is kept only where both paths agree, landed subtree-read
    /// marks from either path remain evidence.
    ///
    pub(super) fn join_subtree_read_meta(&mut self, other: &SlotState) {
        for (l, r) in self.slots.iter_mut().zip(other.slots.iter()) {
            if l.dynvar_src != r.dynvar_src {
                l.dynvar_src = None;
            }
            l.subtree_float_read |= r.subtree_float_read;
            // An alias is a per-path fact about the owner's slot content;
            // it survives a merge only when both paths carry the identical
            // claim.
            if l.dynvar_alias != r.dynvar_alias {
                l.dynvar_alias = None;
            }
        }
    }

    ///
    /// Put *slot*'s value in its frame slot, keeping as much of what the
    /// compiler knows about it as *keep* says. Every write-back the
    /// compiler performs is one of these four policies:
    ///
    /// | mode        | `All`               | `Type`                  | `Nothing`               | `Claims`                       |
    /// |-------------|---------------------|-------------------------|-------------------------|--------------------------------|
    /// | `F(x)`      | box, `Sf(x)`        | box, `S(Float)`         | box, `S(Value)`         | pool: move to a spill home `F(h)`; spill: keep |
    /// | `Sf(x)`     | keep                | `S(guarded)`            | `S(Value)`              | keep                           |
    /// | `C(v)`      | write `v`, keep `C` | write `v`, `S(Value)`   | write `v`, `S(Value)`   | keep                           |
    /// | `S`         | keep                | keep                    | `S(Value)`              | keep                           |
    /// | `V`         | —                   | write `nil`, keep `V`   | write `nil`, `S(Value)` | write `nil`, keep `V`          |
    /// | `MaybeNone` | keep                | keep                    | —                       | keep                           |
    /// | `None`      | —                   | keep                    | —                       | keep                           |
    ///
    /// (`—`: cannot happen there.) A dirty GP resident is re-homed first
    /// under every policy — its value lives only in the register, and the
    /// mode change below would otherwise drop it unspilled — and
    /// `Keep::All` forgets the resident as well, so the slot is genuinely
    /// in its stack home.
    ///
    /// * **`Keep::All`** — the ordinary write-back (an argument being
    ///   passed, a value the interpreter will read): the slot gets the
    ///   value and nothing else is given up. An `F` becomes `Sf` (the box
    ///   in the slot, the float still in the register), a `C` is written
    ///   out but stays a `C`.
    /// * **`Keep::Type`** — a block leaves the unit (`unbox_to_S_at`).
    ///   Views and claims go, since the block's stores never reach the
    ///   compiler's hooks; each slot's `Guarded` stays, a fact about the
    ///   value that is in the slot now. A `C` drops to `S(Value)`, not to
    ///   the constant's own type: the claim is surrendered *because*
    ///   something the compiler cannot see is about to write the slot,
    ///   and it is under no obligation to write the same type —
    ///   `Array#slice_before`'s block dropped its last group when the
    ///   narrow guard survived. (A merge that decides `C` meets `S` is a
    ///   different question and keeps the type, since the value really is
    ///   `v` on that path — see `bridge_at`.)
    /// * **`Keep::Nothing`** — forget everything: every mode drops to
    ///   `S(Value)`, and a temp above sp (`V`) is nil-filled so the
    ///   interpreter never sees an uninitialized word.
    /// * **`Keep::Claims`** — the demotion a *specialized* call needs
    ///   (`locals_unbox_to_S_keeping_claims`): the callee is compiled
    ///   against this frame's abstract state, and every store it makes
    ///   into the frame arrives as a `StoreDynVar` through
    ///   `widen_outer_slot`, so nothing has to be given up. An `Sf` view
    ///   survives because the call saves and restores the frame's
    ///   physical fprs around itself; a `C` is not even written, since the
    ///   slot need not be read (a callee that may capture without a block
    ///   — `Proc.new`, `binding`, `eval`, … — is refused at its call site,
    ///   and a block literal the callee turns into a Proc is caught by
    ///   `immediate_evict`'s capture guard). A pool `F` is the one case
    ///   with work to do: its only copy would sit in the call-site save
    ///   area, which neither the callee's chain reads nor the exits can
    ///   address, so the raw f64 moves to a fresh spill home — one store,
    ///   against the `f64_to_val` per call that boxing it cost — and the
    ///   claim becomes the addressable `F(spill)` (stage 1''), which the
    ///   callee reads and refreshes in place; a spill `F` already is one.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(in crate::codegen::jitgen) fn write_back(&mut self, ir: &mut AsmIr, slot: SlotId, keep: Keep) {
        if let Some(reg) = self.gp_regfile.dirty_reg_of(slot) {
            ir.reg2stack(reg, slot);
        }
        if keep == Keep::All {
            self.gp_regfile.invalidate(slot);
        }
        let spill = match (self.mode(slot), keep) {
            (LinkMode::F(fpr), Keep::All) => {
                self.set_Sf_float(slot, fpr);
                Spill::Fpr(fpr, slot)
            }
            (LinkMode::F(fpr), Keep::Type) => {
                let guarded = self.guarded(slot);
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(guarded));
                Spill::Fpr(fpr, slot)
            }
            (LinkMode::F(fpr), Keep::Nothing) => {
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::Fpr(fpr, slot)
            }
            (LinkMode::F(fpr), Keep::Claims) => {
                if fpr.0 < PHYS_FPR_POOL {
                    let h = self.fpr_alloc.push_spill();
                    self.set_mode(slot, LinkMode::F(h));
                    ir.fpr_move(fpr, h);
                }
                Spill::None
            }
            (LinkMode::Sf(_, _), Keep::All | Keep::Claims) => Spill::None,
            (LinkMode::Sf(_, _), Keep::Type) => {
                let guarded = self.guarded(slot);
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(guarded));
                Spill::None
            }
            (LinkMode::Sf(_, _), Keep::Nothing) => {
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::None
            }
            (LinkMode::C(v), Keep::All) => Spill::Lit(v, slot),
            (LinkMode::C(_), Keep::Claims) => Spill::None,
            (LinkMode::C(v), Keep::Type) => {
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::Lit(v, slot)
            }
            (LinkMode::C(v), Keep::Nothing) => {
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::Lit(v, slot)
            }
            (LinkMode::S(_), Keep::All | Keep::Type | Keep::Claims) => Spill::None,
            (LinkMode::S(_), Keep::Nothing) => {
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::None
            }
            (LinkMode::V, Keep::Type | Keep::Claims) => Spill::Lit(Value::nil(), slot),
            (LinkMode::V, Keep::Nothing) => {
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                Spill::Lit(Value::nil(), slot)
            }
            (LinkMode::MaybeNone, Keep::All | Keep::Type | Keep::Claims)
            | (LinkMode::None, Keep::Type | Keep::Claims) => Spill::None,
            (LinkMode::V | LinkMode::None, Keep::All)
            | (LinkMode::MaybeNone | LinkMode::None, Keep::Nothing) => {
                unreachable!("write_back({keep:?}) {slot:?} {:?} {self:?}", self.mode(slot));
            }
        };
        ir.spill(spill);
    }

    ///
    ///
    /// Forget everything known about *slot*: an outer frame's local that a
    /// callee has just written through `StoreDynVar`. No code is emitted —
    /// the store itself already homed the value — this only stops the mode
    /// the slot used to have from being believed afterwards.
    ///
    pub(in crate::codegen::jitgen) fn invalidate_slot(&mut self, slot: SlotId) {
        self.set_mode(slot, LinkMode::S(Guarded::Value));
    }

    /// Every local this frame is still holding as a constant.
    pub(in crate::codegen::jitgen) fn held_constants(&self) -> Vec<(SlotId, Value)> {
        self.locals()
            .filter_map(|slot| match self.mode(slot) {
                LinkMode::C(v) => Some((slot, v)),
                _ => None,
            })
            .collect()
    }

    ///
    /// Every local whose boxed slot store `write_back(Keep::Claims)`
    /// deferred to a spill home — the raw f64 is current in the home and
    /// the slot is stale. Only that arm creates these, so this is read
    /// at exactly one place: the block-handing call site's
    /// bet-confirmation (`specialized_iseq`).
    ///
    pub(in crate::codegen::jitgen) fn deferred_float_homes(&self) -> Vec<(SlotId, FPReg)> {
        self.locals()
            .filter_map(|slot| match self.mode(slot) {
                LinkMode::F(fpr) if fpr.0 >= PHYS_FPR_POOL => Some((slot, fpr)),
                _ => None,
            })
            .collect()
    }

    ///
    /// Make the slot of each still-deferred home current again, on the
    /// one path into a block-handing call whose bet failed.
    ///
    /// The read twin of `forget_constants`. A `C` claim says the slot
    /// need not be read; a deferred home says the same about a float, and
    /// both are false the moment the block can run outside this unit — it
    /// reads the caller's local through its outer chain, off the stack
    /// slot, which the deferral left holding whatever was there before
    /// (`nil`, for a local whose only write was the deferred store).
    ///
    /// The binding itself survives unless *widen*, because the callee's
    /// already-emitted stores write the *home*: dropping it would make
    /// the continuation read a slot the compiled block never refreshes,
    /// losing every `a += ...` the block performed. A deopt under the
    /// call cannot read a stale slot either — chain escalation means the
    /// continuation never runs compiled, and the deopt write-back boxes
    /// the home. *widen* is for the generic-yield case, where the block
    /// is not compiled here at all: its stores land in the slot, so the
    /// continuation has to read the slot too.
    ///
    pub(in crate::codegen::jitgen) fn home_deferred_floats(
        &mut self,
        ir: &mut AsmIr,
        deferred: &[(SlotId, FPReg)],
        widen: bool,
    ) {
        for &(slot, fpr) in deferred {
            // A path inside the callee's compile may already have widened
            // the slot (a `StoreDynVar` the compiler saw), in which case
            // the store that widened it made the slot current.
            if self.mode(slot) != LinkMode::F(fpr) {
                continue;
            }
            ir.spill(Spill::Fpr(fpr, slot));
            if widen {
                let guarded = self.guarded(slot);
                self.clear(slot);
                self.set_mode(slot, LinkMode::S(guarded));
            }
        }
    }

    /// Resume overlay: adopt the return-path join's claims where this
    /// (parked) frame holds a plain `S`.
    ///
    /// * A kept `C` — sound because every resuming path either still
    ///   holds the claim (the constant is true and its surrender, when it
    ///   comes, will write it) or surrendered it with the deferred write
    ///   emitted on that path's return segment.
    /// * A spill-homed `Sf(Float)` — sound because `Sf` claims the slot
    ///   current (it is, on every path — a widen would have met the join
    ///   to `S`) and the raw-f64 home current likewise. This is what lets
    ///   a state-only placement promotion under the call survive the
    ///   resume; a pool-resident id cannot appear here (promotions into a
    ///   suspended frame are spill-homed by construction).
    pub(in crate::codegen::jitgen) fn overlay_kept_constants(&mut self, other: &SlotState) {
        // Monotone hint bits ride along: the subtree float-read marks the
        // callee landed on its copies of this frame must reach the loop
        // analyses of the resumed compile.
        for (l, r) in self.slots.iter_mut().zip(other.slots.iter()) {
            l.subtree_float_read |= r.subtree_float_read;
        }
        for slot in self.locals() {
            match (self.mode(slot), other.mode(slot)) {
                (LinkMode::S(_), LinkMode::C(v)) => {
                    self.set_mode(slot, LinkMode::C(v));
                }
                (LinkMode::S(_), LinkMode::Sf(fpr, SfGuarded::Float))
                    if fpr.0 >= crate::codegen::PHYS_FPR_POOL =>
                {
                    self.grow_fpr_to(fpr.0 + 1);
                    self.set_Sf(slot, fpr, SfGuarded::Float);
                }
                // Stage 1'': the subtree deferred this slot's boxing —
                // every resuming path either kept the home current (the
                // claim) or boxed it into the slot on its return segment,
                // so adopting the joined `F(spill home)` is exactly as
                // sound as the kept `C` above. The parked mode may be `S`
                // (the claim began as a stage-2 promotion in the subtree)
                // or the pre-call `Sf` (the deferral upgraded it).
                (LinkMode::S(_) | LinkMode::Sf(_, _), LinkMode::F(fpr))
                    if fpr.0 >= crate::codegen::PHYS_FPR_POOL =>
                {
                    self.grow_fpr_to(fpr.0 + 1);
                    self.set_F(slot, fpr);
                }
                _ => {}
            }
        }
    }

    /// Surrender every constant claim this frame holds, writing the
    /// values out (`Keep::Type`'s `C` arm).
    pub(in crate::codegen::jitgen) fn forget_constants(&mut self, ir: &mut AsmIr) {
        for (slot, _) in self.held_constants() {
            self.write_back(ir, slot, Keep::Type);
        }
    }

}

impl SlotState {
    pub fn is_symbol_literal(&self, slot: SlotId) -> Option<IdentId> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.try_symbol()
        } else {
            None
        }
    }

    pub fn is_fixnum_literal(&self, slot: SlotId) -> Option<Fixnum> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.is_immediate()?.try_fixnum()
        } else {
            None
        }
    }

    /// Whether `slot` is a compile-time constant of class `class` — a
    /// static fact that needs no runtime class guard. Matters for heap
    /// constants (a bignum's Guarded state is deliberately `Value`, so a
    /// class-based check would emit the fixnum-tag guard it can never
    /// pass).
    pub(in crate::codegen::jitgen) fn is_const_of_class(&self, slot: SlotId, class: ClassId) -> bool {
        matches!(self.mode(slot), LinkMode::C(v) if v.class() == class)
    }

    /// A compile-time heap-`Integer` (bignum) constant slot, reduced to its
    /// sign: `Some(true)` = above the fixnum window, `Some(false)` = below
    /// it. Every fixnum compares the same way against such a constant, so a
    /// fixnum-guarded operand's comparison folds to that one answer —
    /// dewasm-generated wasm code hits this on every 64-bit op via masks
    /// like `x <= 0xffff_ffff_ffff_ffff`. A denormalized bignum that would
    /// fit the fixnum window is not folded (`None`).
    pub fn is_bigint_literal_sign(&self, slot: SlotId) -> Option<bool> {
        use num::ToPrimitive;
        if let LinkMode::C(v) = self.mode(slot)
            && v.is_immediate().is_none()
            && let RV::BigInt(b) = v.unpack()
        {
            if let Some(i) = b.to_i64()
                && (-(1i64 << 62)..(1i64 << 62)).contains(&i)
            {
                return None;
            }
            Some(b.sign() == num::bigint::Sign::Plus)
        } else {
            None
        }
    }

    /// The tagged `Value` of a fixnum compile-time-constant slot, if any — the
    /// immediate to load straight into a register (the local GP allocator), skipping
    /// the stack-home materialization and the fixnum guard.
    pub fn fixnum_literal_value(&self, slot: SlotId) -> Option<Value> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.is_immediate()?.try_fixnum().map(|_| v)
        } else {
            None
        }
    }

    pub fn is_flonum_literal(&self, slot: SlotId) -> Option<Flonum> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.is_immediate()?.try_flonum()
        } else {
            None
        }
    }

    pub fn is_range_literal(&self, slot: SlotId) -> Option<RangeInner> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.is_range().cloned()
        } else {
            None
        }
    }

    pub fn is_class_or_module_literal(&self, slot: SlotId) -> Option<Module> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.is_class_or_module()
        } else {
            None
        }
    }

    #[allow(non_snake_case)]
    pub fn coerce_C_f64(&self, slot: SlotId) -> Option<f64> {
        if let LinkMode::C(v) = self.mode(slot) {
            match v.unpack() {
                RV::Float(f) => Some(f),
                RV::Fixnum(i) => Some(i as f64),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn is_u16(&self, slot: SlotId) -> Option<u16> {
        let i = self.is_fixnum_literal(slot)?.get();
        u16::try_from(i).ok()
    }

    pub fn is_array_ty(&self, store: &Store, slot: SlotId) -> bool {
        let b = if let Guarded::Class(class) = self.guarded(slot) {
            store[class].is_array_ty_instance()
        } else {
            false
        };
        match self.mode(slot) {
            LinkMode::F(_) => assert!(!b),
            LinkMode::C(v) => assert_eq!(v.is_array_ty(), b),
            _ => {}
        };
        b
    }

    pub fn is_fixnum(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Fixnum;
        match self.mode(slot) {
            LinkMode::F(_) => assert!(!b),
            LinkMode::C(v) => assert_eq!(v.is_fixnum(), b),
            _ => {}
        };
        b
    }

    ///
    /// True when *slot* is not *proven* to hold a non-Integer — i.e. a Fixnum
    /// guard on it may plausibly succeed.
    ///
    /// The stricter [`Self::is_fixnum`] is the right gate for folds that must
    /// be free; this one is for inline paths that emit a guard anyway and only
    /// want to avoid the pathological case of guarding a slot the abstract
    /// state already knows is (say) a Float or a Range, where the guard would
    /// deopt on every execution. A slot the state simply hasn't narrowed
    /// (`Guarded::Value` — e.g. a value just read out of an ivar) passes.
    ///
    pub fn may_be_fixnum(&self, slot: SlotId) -> bool {
        !matches!(self.guarded(slot).class(), Some(class) if class != INTEGER_CLASS)
    }

    pub fn is_float(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Float;
        match self.mode(slot) {
            LinkMode::F(_) => assert!(b),
            LinkMode::C(v) => assert_eq!(v.is_float(), b),
            _ => {}
        };
        b
    }

    pub fn is_class(&self, slot: SlotId, class: ClassId) -> bool {
        match class {
            INTEGER_CLASS => self.is_fixnum(slot),
            FLOAT_CLASS => self.is_float(slot),
            _ => {
                let b = self.guarded(slot) == Guarded::Class(class);
                match self.mode(slot) {
                    LinkMode::F(_) => assert!(!b),
                    LinkMode::C(v) => assert_eq!(v.class() == class, b),
                    _ => {}
                };
                b
            }
        }
    }

    pub fn is_truthy(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => true,
            LinkMode::Sf(_, _) => true,
            LinkMode::C(v) => v.as_bool(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::Value => false,
                // BOOL_CLASS straddles `true` and `false`, so abstract
                // truthiness is unknown — be conservative and return
                // false.
                Guarded::Class(class) => !class.is_falsy() && class != BOOL_CLASS,
            },
        }
    }

    pub fn is_falsy(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => false,
            LinkMode::Sf(_, _) => false,
            LinkMode::C(v) => !v.as_bool(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::Value => false,
                // Same caveat as `is_truthy`: BOOL_CLASS could be either.
                Guarded::Class(class) => class.is_falsy() && class != BOOL_CLASS,
            },
        }
    }

    pub fn is_nil(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => false,
            LinkMode::Sf(_, _) => false,
            LinkMode::C(v) => v.is_nil(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::Value => false,
                Guarded::Class(class) => class.is_nil(),
            },
        }
    }

    pub fn is_not_nil(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => true,
            LinkMode::Sf(_, _) => true,
            LinkMode::C(v) => !v.is_nil(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::Value => false,
                Guarded::Class(class) => !class.is_nil(),
            },
        }
    }

    fn is_fpr_vacant(&self, fpr: FPReg) -> bool {
        self.fpr_slots(fpr).next().is_none()
    }
}

impl SlotState {
    ///
    /// Copy *src* to *dst*.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(in crate::codegen::jitgen) fn copy_slot(
        &mut self,
        ir: &mut AsmIr,
        src: SlotId,
        dst: SlotId,
    ) {
        if src == dst {
            return;
        }
        match self.mode(src) {
            LinkMode::F(x) => {
                self.discard(dst);
                self.set_F(dst, x);
            }
            LinkMode::Sf(x, guarded) => {
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.discard(dst);
                self.set_Sf(dst, x, guarded);
            }
            LinkMode::S(guarded) => {
                // The copy is free: `dst` joins `src`'s register as a dirty
                // holder and no store is emitted now. The store to `dst`'s
                // home is owed at the register's eviction or the next flush —
                // and dropped if `dst` dies first (a `ret`, a popped
                // temporary). A chain `v2 = v1; v3 = v2; …` thus costs nothing
                // per link instead of a load/store pair each, with each
                // link's load waiting on the previous link's store. When
                // `src` is not resident it is loaded (clean) into a fresh
                // register first, so `src` and `dst` share it from here on.
                let reg = match self.gp_regfile.reg_of(src) {
                    Some(reg) => reg,
                    None => {
                        let (reg, spills) = self.gp_regfile.alloc_reg(&[]);
                        for (r, s) in spills {
                            ir.reg2stack(r, s);
                        }
                        ir.stack2reg(src, reg);
                        self.gp_regfile.bind(reg, src, /* dirty */ false);
                        reg
                    }
                };
                // Define first (this drops any stale resident of `dst` via
                // `clear`), then bind the shared register.
                self.def_S_guarded(dst, guarded);
                self.gp_regfile.bind(reg, dst, /* dirty */ true);
            }
            LinkMode::C(v) => {
                self.def_C(dst, v);
            }
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("copy_slot() {:?} {:?}: {:?}", src, self.mode(src), self);
            }
        }
    }
}

impl AbstractFrame {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    pub(crate) fn guard_class(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        r: GP,
        class: ClassId,
        deopt: AsmDeopt,
    ) {
        if self.guard_class_state(slot, class) {
            ir.push(AsmInst::GuardClass(r, class, deopt));
        }
    }

    ///
    /// Analysis half of [`Self::guard_class`] (item ②, step 2): refine the
    /// slot's abstract type to `class` and return whether a runtime guard must
    /// be emitted (`false` when the type already statically matches, so no guard
    /// — and no state change — is needed). Pure state; the codegen wrapper emits
    /// `GuardClass` with the `deopt`, which it (the caller) created *before* this
    /// runs so the deopt's write-back snapshot is the pre-guard placement.
    ///
    pub(in crate::codegen::jitgen) fn guard_class_state(
        &mut self,
        slot: SlotId,
        class: ClassId,
    ) -> bool {
        if self.class(slot) == Some(class) {
            return false;
        }
        let class_guarded = Guarded::from_class(class);
        // Operate on a local copy and write it back (item ② encapsulation;
        // `LinkMode` is `Copy`). The `return false`s below skip both the
        // write-back and the guard emission, exactly as the prior `return`s did.
        let mut mode = self.mode(slot);
        match &mut mode {
            LinkMode::S(guarded) => {
                if class_guarded == *guarded {
                    return false;
                } else if *guarded == Guarded::Value {
                    *guarded = class_guarded;
                } else {
                    // in this case, Guard will always fail
                    *guarded = class_guarded;
                }
            }
            LinkMode::Sf(_, guarded) => {
                match (*guarded, class_guarded) {
                    (SfGuarded::Fixnum, Guarded::Fixnum) | (SfGuarded::Float, Guarded::Float) => {
                        return false;
                    }
                    (SfGuarded::FixnumOrFloat, Guarded::Fixnum) => {
                        *guarded = SfGuarded::Fixnum;
                    }
                    (SfGuarded::FixnumOrFloat, Guarded::Float) => {
                        *guarded = SfGuarded::Float;
                    }
                    (_, _) => {} // in this case, Guard will always fail
                }
            }
            LinkMode::F(_) => {
                if class_guarded == Guarded::Float {
                    return false;
                }
                // in this case, Guard will always fail
            }
            LinkMode::C(v) => {
                if class == INTEGER_CLASS {
                    if v.is_fixnum() {
                        return false;
                    }
                    // If v is Bignum, Guard will fail
                } else {
                    if v.class() == class {
                        return false;
                    }
                    // in this case, Guard will always fail
                }
            }
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!(
                    "guard_class(): current:{:?} given:{:?}",
                    mode, class_guarded
                );
            }
        }
        self.set_mode(slot, mode);
        true
    }

    pub(crate) fn guard_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        let deopt = ir.new_deopt(self);
        self.guard_class(ir, slot, r, INTEGER_CLASS, deopt);
    }

    /// Snapshot the live physical FP pool registers (which the runtime-call
    /// lowerings save/restore around the C-ABI call) **and**, as a side effect,
    /// flush any live GP-pool residents to their stack homes.
    ///
    /// §9 9d-B: GP-pool registers (`r8`–`r11`) are caller-saved, so a value
    /// kept resident there must not survive a C call. Every runtime helper that
    /// can clobber them is preceded by a `get_using_fpr` snapshot (here, or
    /// inside the `ir.<helper>(state, …)` builders), so flushing the GP pool at
    /// this single chokepoint covers them all. Use
    /// [`Self::using_fpr_offset`] where only the stack-offset is needed and no
    /// call (hence no flush) happens.
    pub(crate) fn get_using_fpr(&mut self, ir: &mut AsmIr) -> UsingFpr {
        // Single chokepoint for GP-clobbering calls: every C-ABI call (inline-asm
        // generators, CFunc inlines, the cached method-call path) takes a
        // `get_using_fpr` snapshot first, so flushing the local GP register file
        // here re-homes the residents for all of them — the inline generators no
        // longer each need their own flush, and register-only inlines (which make
        // no call and never call this) keep their residents.
        self.flush_gp(ir);
        // Stage-B home-aliased reads: whatever this snapshot precedes can
        // re-enter Ruby, which can store through the frame chain — no alias
        // survives a call boundary. (Specialized calls kill them in
        // `specialized_compile`.)
        self.clear_dynvar_aliases();
        self.using_fpr_offset()
    }

    /// Pure FP-pool snapshot with no GP-pool flush — for call-free uses that
    /// only need `UsingFpr::offset()` (e.g. reserving inline-frame stack space).
    pub(crate) fn using_fpr_offset(&self) -> UsingFpr {
        let mut b = UsingFpr::new();
        // Only physical pool slots need save/restore at call
        // boundaries; spill slots already live on the stack.
        for s in self.all_regs() {
            if let LinkMode::F(x) | LinkMode::Sf(x, _) = self.mode(s)
                && x.0 < PHYS_FPR_POOL
            {
                b.set(x.0, true);
            }
        }
        b
    }

    fn wb_forward_rest(&self) -> Vec<(SlotId, SlotId, u16)> {
        self.deferred_forward
            .iter()
            .map(|df| (df.rest_local, df.src, df.len))
            .collect()
    }

    /// K1: the deferred `**kwrest` materialization entries —
    /// `(f's kwrest local, [(name, caller slot)])`.
    fn wb_forward_kwrest(&self) -> Vec<(SlotId, Box<[(IdentId, SlotId)]>)> {
        self.deferred_forward
            .iter()
            .filter_map(|df| {
                let (dst, kw_pos, names) = df.kw.as_ref()?;
                let table = names
                    .iter()
                    .enumerate()
                    .map(|(i, name)| (*name, *kw_pos + i))
                    .collect();
                Some((*dst, table))
            })
            .collect()
    }

    pub(super) fn get_gc_write_back(&self) -> WriteBack {
        // A deferred rest slot always physically holds a valid `Value`
        // at any GC safepoint: when the deferral activated the
        // caller-side `set_arguments` stored a real `nil` there; when it
        // did not it holds the normally-built array. So GC scanning is
        // safe without materializing, and the trampoline gate (single
        // forwarding call, no eval/binding) guarantees the frame is not
        // capturable, so no heap snapshot observes it pre-consume.
        let literal = self.wb_literal(|_| true);
        let void = self.wb_void();
        // spill dirty GP residents so the GC marks them (the
        // registers themselves survive the collection — `exec_gc` preserves the
        // caller-saved set). Empty without the feature.
        let gp = self.gp_regfile.dirty_residents();
        WriteBack::new(vec![], literal, void, gp, vec![], vec![])
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        let f = |_| true;
        let fpr = self.wb_fpr(f);
        let literal = self.wb_literal(f);
        // re-home dirty GP residents (a binop result still in a
        // register) so a deopt resuming in the VM reads them from their stack
        // home. Empty without the feature.
        let gp = self.gp_regfile.dirty_residents();
        WriteBack::new(
            fpr,
            literal,
            vec![],
            gp,
            self.wb_forward_rest(),
            self.wb_forward_kwrest(),
        )
    }

    fn fpr_swap(&mut self, l: FPReg, r: FPReg) {
        // A physical fpr swap (`FprSwap`) only changes *which register* holds
        // each live value; every slot keeps its own representation and
        // refinement. The two registers need not share a refinement — e.g. the
        // bridge's `F(l) -> F(r)` arm swaps a pure-Float `F` slot in `l` with
        // whatever occupies `r`, which may be a Fixnum-refined `Sf` slot. So just
        // relabel each slot's register index; do not cross-assign refinements
        // (the `Sf` refinement rides along in `ty`, untouched here).
        // Local-copy RMW per slot (item ② encapsulation; `LinkMode` is `Copy`).
        for slot in self.all_regs() {
            let mut link = self.mode(slot);
            match &mut link {
                LinkMode::F(x) | LinkMode::Sf(x, _) => {
                    if *x == l {
                        *x = r;
                    } else if *x == r {
                        *x = l;
                    }
                }
                LinkMode::S(_)
                | LinkMode::C(_)
                | LinkMode::V
                | LinkMode::MaybeNone
                | LinkMode::None => {}
            }
            self.set_mode(slot, link);
        }
    }

    /// Every register holding an `F` slot, with those slots — the raw
    /// f64s a side exit has to box into their stack homes. Registers in
    /// id order, slots in slot order.
    fn wb_fpr(&self, f: impl Fn(SlotId) -> bool) -> Vec<(FPReg, Vec<SlotId>)> {
        let mut by_reg: Vec<(FPReg, Vec<SlotId>)> = Vec::new();
        for s in self.all_regs() {
            if let LinkMode::F(x) = self.mode(s)
                && f(s)
            {
                match by_reg.iter_mut().find(|(r, _)| *r == x) {
                    Some((_, v)) => v.push(s),
                    None => by_reg.push((x, vec![s])),
                }
            }
        }
        by_reg.sort_by_key(|(r, _)| r.0);
        by_reg
    }

    fn wb_literal(&self, f: impl Fn(SlotId) -> bool) -> Vec<(Value, SlotId)> {
        self.all_regs()
            .filter_map(|idx| match self.mode(idx) {
                LinkMode::C(v) if f(idx) => Some((v, idx)),
                _ => None,
            })
            .collect()
    }

    fn wb_void(&self) -> Vec<SlotId> {
        self.all_regs()
            .filter_map(|idx| match self.mode(idx) {
                LinkMode::V => Some(idx),
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum SfGuarded {
    Fixnum,
    Float,
    FixnumOrFloat,
}

impl Into<Guarded> for SfGuarded {
    fn into(self) -> Guarded {
        match self {
            SfGuarded::Fixnum => Guarded::Fixnum,
            SfGuarded::Float => Guarded::Float,
            SfGuarded::FixnumOrFloat => Guarded::Value,
        }
    }
}

///
/// A pending stack write-back produced by a transfer/eviction primitive
/// (item ②, step 2): the *what* of an eviction, decided by the primitive's
/// analysis (state) half and emitted by the codegen half. This is the first
/// concrete "typed IR record" — a standalone analysis pass collects these
/// instead of pushing `AsmInst`, and the lowering pass replays them via
/// [`Spill::emit`].
///
///
/// What [`SlotState::unbox_to_S_at`] did to an outer-frame claim at the
/// block-handout barrier.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum OuterBarrier {
    /// Nothing given up (no claim, or an unreachable method-caller `F`).
    Kept,
    /// A slot-current claim dropped — report the widen, emit nothing.
    Widened,
    /// A stage-1'' deferred home dropped — the caller emits the
    /// box-from-home surrender write, then reports the widen.
    BoxHome(FPReg),
}

///
/// What [`SlotState::write_back`] keeps of the compiler's knowledge about
/// the slot once its value is in the frame — see the table there.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum Keep {
    /// The slot gets the value; every view and claim stays.
    All,
    /// Views and claims go, the slot's type stays (a block leaves the unit).
    Type,
    /// Everything goes: `S(Value)`.
    Nothing,
    /// Claims and views stay; a pool `F` moves to a spill home (a
    /// specialized call).
    Claims,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum Spill {
    None,
    /// `ir.fpr2stack(fpr, slot)`
    Fpr(FPReg, SlotId),
    /// `ir.lit2stack(value, slot)`
    Lit(Value, SlotId),
}

impl Spill {
    pub(super) fn emit(self, ir: &mut AsmIr) {
        match self {
            Spill::None => {}
            Spill::Fpr(fpr, slot) => ir.fpr2stack(fpr, slot),
            Spill::Lit(v, slot) => ir.lit2stack(v, slot),
        }
    }
}

///
/// An FP-register transfer produced by a transfer primitive (item ②, step 2):
/// either a move into a vacant register or a swap of two live registers. Like
/// [`Spill`], the *what* is decided by a primitive's analysis half and emitted
/// by the codegen half via [`FpXfer::emit`].
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum FpXfer {
    /// `ir.fpr_move(l, r)`
    Move(FPReg, FPReg),
    /// `ir.push(AsmInst::FprSwap(l, r))`
    Swap(FPReg, FPReg),
}

impl FpXfer {
    pub(super) fn emit(self, ir: &mut AsmIr) {
        match self {
            FpXfer::Move(l, r) => ir.fpr_move(l, r),
            FpXfer::Swap(l, r) => ir.push(AsmInst::FprSwap(l, r)),
        }
    }
}

impl AsmIr {
    /// Emit a spill record (a transfer/eviction primitive's analysis-half
    /// decision). A no-op in analysis mode — see [`AsmIr::codegen_mode`].
    pub(in crate::codegen::jitgen) fn spill(&mut self, s: Spill) {
        if self.codegen_mode() {
            s.emit(self);
        }
    }

    /// Emit an FP-register transfer record. A no-op in analysis mode.
    pub(in crate::codegen::jitgen) fn fp_xfer(&mut self, f: FpXfer) {
        if self.codegen_mode() {
            f.emit(self);
        }
    }
}

///
/// Mode of linkage between stack slot and fpr registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LinkMode {
    ///
    /// No Value.
    ///
    /// this is for optional arguments with no passed value.
    ///
    None,
    ///
    /// Maybe No Value.
    ///
    /// this is for optional arguments which may have no passed value.
    ///
    MaybeNone,
    ///
    /// Void.
    ///
    /// this is used for the temp slots above sp.
    ///
    V,
    ///
    /// On the stack slot.
    ///
    S(Guarded),
    ///
    /// On the floating point register (fpr).
    ///
    /// mutation of the corresponding FPR lazily affects the stack slot.
    ///
    F(FPReg),
    ///
    /// On the stack slot and on the floating point register (fpr) which is read-only.
    ///
    Sf(FPReg, SfGuarded),
    ///
    /// Concrete value.
    ///
    /// `Value` may be any packed immediate (fixnum, flonum, nil, true,
    /// false, symbol) or a pointer to a heap-allocated `RValue` (e.g. a
    /// class object loaded from a constant). For heap values the pointer
    /// is kept alive across GC by `wb_literal` writing it to the slot's
    /// stack location before each GC safepoint, and across constant
    /// redefinition by the `GuardConstVersion` deopt check at the load
    /// site.
    ///
    C(Value),
}

impl Default for LinkMode {
    /// A boxed `Value` of unknown class in its stack home.
    fn default() -> Self {
        LinkMode::S(Guarded::Value)
    }
}

impl LinkMode {
    fn none() -> Self {
        LinkMode::None
    }

    fn nil() -> Self {
        LinkMode::C(Value::nil())
    }

    fn guarded(&self) -> Guarded {
        match self {
            LinkMode::S(guarded) => *guarded,
            LinkMode::Sf(_, guarded) => (*guarded).into(),
            LinkMode::F(_) => Guarded::Float,
            LinkMode::C(v) => Guarded::from_concrete_value(*v),
            LinkMode::V => Guarded::Class(NIL_CLASS),
            _ => unreachable!("{:?}", self),
        }
    }

    fn equiv(&self, other: &Self) -> bool {
        match (self, other) {
            (LinkMode::None | LinkMode::MaybeNone | LinkMode::V, _) => self == other,
            (_, LinkMode::None | LinkMode::MaybeNone | LinkMode::V) => false,
            (LinkMode::C(l), LinkMode::C(r)) => l == r,
            (LinkMode::C(_), _) => false,
            (_, LinkMode::C(_)) => false,
            (lhs, rhs) => lhs.guarded() == rhs.guarded(),
        }
    }

    pub(super) fn as_return(&self) -> ReturnValue {
        match self {
            LinkMode::C(v) => ReturnValue::Const(*v),
            LinkMode::MaybeNone | LinkMode::None | LinkMode::V => unreachable!(),
            l => match l.guarded() {
                Guarded::Class(class) => ReturnValue::Class(class),
                Guarded::Fixnum => ReturnValue::Class(INTEGER_CLASS),
                Guarded::Float => ReturnValue::Class(FLOAT_CLASS),
                Guarded::Value => ReturnValue::Value,
            },
        }
    }

    pub(in crate::codegen::jitgen) fn from_caller(
        store: &Store,
        fid: FuncId,
        callid: CallSiteId,
        state: &AbstractState,
    ) -> Vec<Self> {
        let CallSiteInfo { recv, .. } = &store[callid];
        let recv = state.mode(*recv);
        Self::from_caller_inner(store, fid, callid, state, recv)
    }

    pub(in crate::codegen::jitgen) fn from_caller_yield(
        store: &Store,
        fid: FuncId,
        callid: CallSiteId,
        state: &AbstractState,
        self_class: ClassId,
    ) -> Vec<Self> {
        let recv = LinkMode::S(Guarded::Class(self_class));
        Self::from_caller_inner(store, fid, callid, state, recv)
    }

    fn from_caller_inner(
        store: &Store,
        fid: FuncId,
        callid: CallSiteId,
        state: &AbstractState,
        recv: LinkMode,
    ) -> Vec<Self> {
        let CallSiteInfo {
            args,
            pos_num,
            kw_pos,
            kw_args,
            ..
        } = &store[callid];
        let info = &store[fid];
        let mut slots = vec![];
        slots.push(recv);
        let (filled_req, filled_opt, filled_post, rest_len) = info.apply_args(*pos_num);
        let req_len = filled_req.len();
        let opt_len = filled_opt.len();
        let post_len = filled_post.len();
        for i in filled_req {
            slots.push(state.mode(*args + i));
        }
        for _ in req_len..info.req_num() {
            slots.push(Self::nil());
        }
        for i in req_len..req_len + opt_len {
            slots.push(state.mode(*args + i));
        }
        for _ in opt_len..info.opt_num() {
            slots.push(Self::none());
        }
        if info.is_rest() {
            slots.push(Self::S(Guarded::Class(ARRAY_CLASS)));
        }
        let start = req_len + opt_len + rest_len;
        for i in start..start + post_len {
            slots.push(state.mode(*args + i));
        }
        for _ in post_len..info.post_num() {
            slots.push(Self::nil());
        }
        let kw = info.kw_reg_pos();
        assert_eq!(kw.0 as usize, slots.len());
        for k in info.kw_names() {
            if let Some(p) = kw_args.get(k) {
                slots.push(state.mode(*kw_pos + *p));
            } else {
                slots.push(Self::none());
            }
        }
        if info.kw_rest().is_some() {
            slots.push(Self::S(Guarded::Class(HASH_CLASS)));
        }
        slots
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Guarded {
    #[default]
    Value,
    Fixnum,
    Float,
    Class(ClassId),
}

impl Guarded {
    pub fn from_concrete_value(v: Value) -> Self {
        if v.is_fixnum() {
            Guarded::Fixnum
        } else if v.is_float() {
            Guarded::Float
        } else if v.class() == INTEGER_CLASS {
            // Bignum is not Guarded::Fixnum.
            Guarded::Value
        } else {
            // Use the IC class so `true` and `false` literals collapse to
            // a single `BOOL_CLASS` guard, avoiding a deopt when a slot
            // toggles between the two booleans.
            Guarded::Class(v.class_for_ic())
        }
    }

    pub fn from_class(class: ClassId) -> Self {
        match class {
            INTEGER_CLASS => Guarded::Fixnum,
            FLOAT_CLASS => Guarded::Float,
            class => Guarded::Class(class),
        }
    }

    pub fn class(&self) -> Option<ClassId> {
        Some(match self {
            Guarded::Value => return None,
            Guarded::Fixnum => INTEGER_CLASS,
            Guarded::Float => FLOAT_CLASS,
            Guarded::Class(c) => *c,
        })
    }

    /// Type-lattice meet (item ②): two equal types stay; disagreement widens to
    /// `Value` (⊤). This is the *type* component of `AbstractFrame::join` — the
    /// fused join's resulting type equals this meet for every non-sentinel slot
    /// (placement reconciliation is the rest of `join`).
    pub(super) fn join(&self, other: &Self) -> Self {
        if self == other { *self } else { Guarded::Value }
    }
}

impl AbstractFrame {
    ///
    /// Bridge one slot of the frame *outer* levels out from the innermost.
    ///
    /// `outer == 0` is the ordinary [`Self::bridge`]. Beyond that there is
    /// nothing to emit, ever: a frame writes each of its constants to its
    /// slot on the way into the call that hands out its block
    /// (`write_back(Keep::Type)`), so by the time it *is* an outer frame the value is
    /// already there and giving the claim up is pure state. Nor can it hold
    /// an unboxed float — `FprAllocator` is per `SlotState` and its ids are
    /// positional (`FPReg(id)` is `xmm{id+2}` below `PHYS_FPR_POOL`), so
    /// two frames each promoting a slot would both take `FPReg(0)` and both
    /// write `xmm2`; `AbstractState::join` keeps outer frames clear of them
    /// until there is one id space for the whole chain.
    ///
    pub(super) fn bridge_at(
        &mut self,
        ir: &mut AsmIr,
        target: &SlotState,
        slot: SlotId,
        pc: BytecodePtr,
        outer: usize,
        sur: &crate::codegen::jitgen::state::ChainSurrender,
        level: usize,
    ) {
        if outer == 0 {
            return self.bridge(ir, target, slot, pc);
        }
        // An outer frame's fpr is not in a register while we run: the call
        // that suspended that frame saved its physical fprs
        // (`fpr_save_cont`) and restores them on the way back. So nothing
        // here may emit a register operation on one. A suspended frame's
        // placements are path-invariant unless a chain store widened them
        // (`AbstractState::join` keeps identical placements and meets
        // differing ones to `S`), so every arm here is either a no-op or a
        // pure state demotion — including a *method* caller's `F`, whose
        // slot is stale but whose value sits untouched in the call-site
        // save area, unreachable by anything this compile runs (a frame a
        // handed-out block could reach was homed at its own call site).
        match (self.mode(slot), target.mode(slot)) {
            (LinkMode::F(l), LinkMode::F(r)) if l == r => {
                return;
            }
            // Stage 1'' deferred boxing. Weakening `Sf(h)` to the target's
            // deferred `F(h)` is pure state (both claim the home current;
            // the slot-current half is dropped)...
            (LinkMode::Sf(l, _), LinkMode::F(r))
                if l == r && l.0 >= crate::codegen::PHYS_FPR_POOL =>
            {
                self.set_F(slot, l);
            }
            // ...while demoting a deferred `F(h)` to anything slot-current
            // boxes the home into the owner's slot through the chain — the
            // write-side surrender, mirrored per edge like the kept-`C`
            // literal write. Boxing *is* the `F(h) -> Sf(h, Float)`
            // transition (the home stays bound, the slot becomes current),
            // so this arm emits the surrender, records exactly that, and
            // re-enters for the remaining `Sf -> target` step — the
            // ordinary arms below, which every kind of slot-current target
            // already handles: `Sf -> S` drops the view, a same-home
            // `Sf -> Sf` is a no-op, and a *different* home stays the
            // `unreachable!` it was. Re-entry terminates: the second pass
            // starts from `Sf`, and no `Sf` arm re-enters.
            (LinkMode::F(l), LinkMode::Sf(_, _) | LinkMode::S(_))
                if l.0 >= crate::codegen::PHYS_FPR_POOL =>
            {
                let ok = sur.emit_box(ir, level, l, slot);
                debug_assert!(ok, "no chain addressing for a suspended frame at {level}");
                self.set_Sf(slot, l, SfGuarded::Float);
                self.bridge_at(ir, target, slot, pc, outer, sur, level);
            }
            (LinkMode::V, LinkMode::V)
            | (LinkMode::None, LinkMode::None)
            | (LinkMode::MaybeNone, LinkMode::MaybeNone) => {}
            (_, LinkMode::V) => self.discard(slot),
            (_, LinkMode::MaybeNone) => self.set_MaybeNone(slot),
            (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
            (LinkMode::Sf(l, _), LinkMode::Sf(r, _)) if l == r => {}
            (LinkMode::Sf(_, guarded), LinkMode::S(_)) => {
                self.set_S_with_guard(slot, guarded.into());
            }
            // Loop-entry adoption of an outer view (stage C, state-side):
            // the target's `Sf` home is established by the entry init the
            // merge emits on this same edge, so the bridge itself is pure
            // state. The adoption gate guarantees the slot is current
            // (`S`, never a kept `C`) on every entry path.
            (LinkMode::S(_), LinkMode::Sf(fpr, guarded)) => {
                self.grow_fpr_to(fpr.0 + 1);
                self.set_Sf(slot, fpr, guarded);
            }
            (LinkMode::C(v), LinkMode::S(_)) => {
                self.set_mode(slot, LinkMode::S(Guarded::from_concrete_value(v)));
            }
            (LinkMode::S(_), LinkMode::S(_)) => {
                // The target guard is the join of every incoming path's, so
                // it is never narrower than ours — nothing to check.
            }
            (l, r) => unreachable!("outer{outer} {slot:?} {l:?}->{r:?} {target:?}"),
        }
    }

    ///
    /// `write_back(Keep::Type)` for a slot of the frame *outer* levels out:
    /// give the claim up, for the reason above. Most claims go with
    /// nothing emitted; a stage-1'' deferred `F(spill home)` — whose slot
    /// is genuinely stale — reports [`OuterBarrier::BoxHome`] so the
    /// caller ([`AbstractState::unbox_to_S_for_outgoing_block`]) can emit the
    /// box-from-home surrender write through the chain.
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn unbox_to_S_at(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        outer: usize,
    ) -> OuterBarrier {
        if outer == 0 {
            self.write_back(ir, slot, Keep::Type);
            return OuterBarrier::Kept;
        }
        match self.mode(slot) {
            // The slot holds the boxed value; only the read-only view goes.
            LinkMode::Sf(_, _) => {
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                OuterBarrier::Widened
            }
            LinkMode::C(_) => {
                self.set_mode(slot, LinkMode::S(Guarded::Value));
                OuterBarrier::Widened
            }
            // Stage 1'': a deferred-boxing home — the slot is stale, and
            // the block handed out here can reach it, so the barrier must
            // materialize the boxed value before the call.
            LinkMode::F(fpr) if fpr.0 >= PHYS_FPR_POOL => {
                self.set_mode(slot, LinkMode::S(Guarded::Float));
                OuterBarrier::BoxHome(fpr)
            }
            // A suspended *method* caller can hold a pool `F` across its
            // call (the value lives in its call-site save area). The block
            // handed out here cannot reach that frame — every frame a
            // block's lexical chain crosses was homed at its own
            // block-handing call site — so the stale slot is never read
            // and the binding stays.
            LinkMode::F(_) => OuterBarrier::Kept,
            _ => OuterBarrier::Kept,
        }
    }

    ///
    /// Generate bridge AsmIr to merge current state with target state.
    ///
    pub(super) fn bridge(
        &mut self,
        ir: &mut AsmIr,
        target: &SlotState,
        slot: SlotId,
        pc: BytecodePtr,
    ) {
        match (self.mode(slot), target.mode(slot)) {
            (LinkMode::V, LinkMode::V) => {}
            (_, LinkMode::V) => {
                self.discard(slot);
            }
            (LinkMode::F(l), LinkMode::F(r)) => {
                if l != r {
                    if self.is_fpr_vacant(r) {
                        self.set_F(slot, r);
                        ir.fpr_move(l, r);
                    } else {
                        self.gen_fpr_swap(ir, l, r);
                    }
                }
            }
            (
                LinkMode::F(l),
                LinkMode::Sf(r, guarded @ (SfGuarded::Float | SfGuarded::FixnumOrFloat)),
            ) => {
                // F means the fpr holds a float; writing back produces a
                // Value::float, which satisfies both the Float and the
                // FixnumOrFloat guards.
                ir.fpr2stack(l, slot);
                if l == r {
                    // F(l) -> Sf(l)
                    self.set_Sf(slot, l, guarded);
                } else {
                    // F(l) -> Sf(r)
                    self.to_sf(ir, slot, l, r, guarded);
                }
            }
            (LinkMode::F(_), LinkMode::S(_)) => {
                self.write_back(ir, slot, Keep::All);
            }
            (LinkMode::Sf(l, _), LinkMode::Sf(r, guarded)) => {
                if l != r {
                    // Sf(l) -> Sf(r)
                    self.to_sf(ir, slot, l, r, guarded);
                }
            }
            (LinkMode::Sf(_, guarded), LinkMode::S(_)) => {
                self.set_S_with_guard(slot, guarded.into());
            }
            (LinkMode::S(_), LinkMode::Sf(x, SfGuarded::Float)) => {
                // S -> Sf
                ir.stack2reg(slot, GP::Rax);
                let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                if self.is_fpr_vacant(x) {
                    ir.float_to_fpr(GP::Rax, x, deopt);
                    self.set_Sf_float(slot, x);
                } else {
                    let tmp = self.set_new_Sf(slot, SfGuarded::Float);
                    ir.float_to_fpr(GP::Rax, tmp, deopt);
                    self.gen_fpr_swap(ir, x, tmp);
                }
            }
            (LinkMode::S(_), LinkMode::F(x)) => {
                // S -> F: one-time unbox of a boxed float into a pure-fpr
                // binding (no boxed cache) — a loop pre-header entry adopting the
                // back-edge's `F` placement (§15.3). Mirrors the `S -> Sf` arm but
                // sets `F`; reuses `float_to_fpr`, which both backends lower.
                ir.stack2reg(slot, GP::Rax);
                let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                if self.is_fpr_vacant(x) {
                    ir.float_to_fpr(GP::Rax, x, deopt);
                    self.set_F(slot, x);
                } else {
                    let tmp = self.set_new_F(slot);
                    ir.float_to_fpr(GP::Rax, tmp, deopt);
                    self.gen_fpr_swap(ir, x, tmp);
                }
            }
            (LinkMode::Sf(l, _), LinkMode::F(r)) => {
                // Sf -> F: the value is already unboxed in fpr `l`; drop the
                // boxed cache and rebind as pure `F`. Mirrors the `F -> F` arm.
                if l == r {
                    self.set_F(slot, l);
                } else if self.is_fpr_vacant(r) {
                    self.set_F(slot, r);
                    ir.fpr_move(l, r);
                } else {
                    self.gen_fpr_swap(ir, l, r);
                    self.set_F(slot, r);
                }
            }
            (LinkMode::S(_), LinkMode::S(guarded)) => {
                if let Some(class) = guarded.class()
                    && !self.is_class(slot, class)
                {
                    let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                    ir.stack2reg(slot, GP::Rax);
                    ir.push(AsmInst::GuardClass(GP::Rax, class, deopt));
                    self.set_S_with_guard(slot, guarded);
                }
            }
            (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
            (LinkMode::C(l), LinkMode::F(r)) => {
                if let Some(f) = l.try_float() {
                    self.set_F(slot, r);
                    ir.f64_to_fpr(f, r);
                } else {
                    unreachable!()
                }
            }
            (LinkMode::C(l), LinkMode::Sf(r, _)) => {
                self.set_Sf_float(slot, r);
                let (v, f) = if let Some(f) = l.try_float() {
                    (Value::float(f), f)
                } else if let Some(i) = l.try_fixnum() {
                    (Value::integer(i), i as f64)
                } else {
                    unreachable!()
                };
                ir.f64_to_fpr(f, r);
                ir.lit2stack(v, slot);
            }
            (LinkMode::C(v), LinkMode::S(_)) => {
                // C -> S
                let guarded = Guarded::from_concrete_value(v);
                self.set_mode(slot, LinkMode::S(guarded));
                ir.lit2stack(v, slot);
            }
            (LinkMode::None, LinkMode::None) => {}
            (LinkMode::MaybeNone, LinkMode::MaybeNone) => {}
            (l, r) => {
                unreachable!("{slot:?} {l:?}->{r:?} {target:?}");
            }
        }
    }
}

impl AbstractFrame {
    ///
    /// Generate bridge AsmIr from F/Sf(l) to Sf(r).
    ///
    /// Analysis half (item ②, step 2): bind `slot` to `r` as `Sf` and return the
    /// FP-register transfer to emit (a move into a vacant `r`, or a swap when
    /// `r` is occupied). Pure state; codegen wrapper [`Self::to_sf`] emits it.
    fn to_sf_state(&mut self, slot: SlotId, l: FPReg, r: FPReg, guarded: SfGuarded) -> FpXfer {
        if self.is_fpr_vacant(r) {
            self.set_Sf(slot, r, guarded);
            FpXfer::Move(l, r)
        } else {
            self.fpr_swap(l, r);
            FpXfer::Swap(l, r)
        }
    }

    fn to_sf(&mut self, ir: &mut AsmIr, slot: SlotId, l: FPReg, r: FPReg, guarded: SfGuarded) {
        let f = self.to_sf_state(slot, l, r, guarded);
        ir.fp_xfer(f);
    }

    ///
    /// Swap fpr registers `l` and `r`.
    ///
    fn gen_fpr_swap(&mut self, ir: &mut AsmIr, l: FPReg, r: FPReg) {
        self.fpr_swap(l, r);
        ir.push(AsmInst::FprSwap(l, r));
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn test_join() {
        run_test_with_prelude(
            r###"
        $a = false
        p f
        "###,
            r###"
        def f
          if $a
            a = 1.0
            b = 1.0
            c = 100
          else
            b = 2.0
            a = 2.0
          end
          "#{a * b}#{c.inspect}"
        end
        "###,
        );
    }

    /// Merging `F(l)` and `F(r)` with `l != r` used to keep `self`'s
    /// register even though the other entry's value lived elsewhere; the
    /// subsequent per-slot swap then displaced a partner slot sharing the
    /// register from a `copy_slot` alias. Regression test: before the fix
    /// the loop returned `[-1.0, -1.0, -1.0, -1.0, -1.0]` after JIT warm-up.
    #[test]
    fn test_join_float_register_disagreement() {
        run_test(
            r###"
        def test
          res = []
          i = 0
          endv = 1.0
          while i <= 4
            a = -1.0 + i * 0.5
            if a > endv
              a = endv
            end
            res << a
            i += 1
          end
          res
        end
        test
        "###,
        );
    }

    /// Regression test for the `fpr_swap` mixed-refinement panic. A diamond
    /// (`if/else`) inside a loop that updates a `Float` accumulator on both arms
    /// drives the back-edge bridge to `gen_fpr_swap` a pure-`Float` `F` register
    /// against a Fixnum-refined `Sf` register (the loop counter coerced to f64).
    /// `fpr_swap` used to `assert_eq!(guarded_r, Some(Float))` / cross-assign the
    /// partner register's refinement, assuming both swapped registers shared a
    /// refinement — so this aborted the process at `slot.rs` with
    /// `left: Some(Fixnum), right: Some(Float)` in both debug and release. The
    /// swap only relabels register indices now, so each slot keeps its own
    /// refinement. Expected result: `1249925000.0`.
    #[test]
    fn test_fpr_swap_mixed_refinement() {
        // Loop JIT compiles at 15 iterations in test mode, so a small n
        // still exercises the swap path; under gc-stress every safepoint
        // collects and run_test's 25 warm-up runs of a 100k-iteration
        // loop would flirt with the CI timeout.
        let n = if cfg!(feature = "gc-stress") {
            2_000
        } else {
            100_000
        };
        run_test(&format!(
            r###"
        def f(n)
          x = 0.0
          i = 0
          while i < n
            if i.even?
              x += i * 0.5
            else
              x -= 1.0
            end
            i += 1
          end
          x
        end
        f({n})
        "###
        ));
    }

    /// A local still held only in an fpr (`F`) when a block is handed to a
    /// callee outside the unit (`String#each_char` is a Rust builtin, so
    /// the block is not specialized) is boxed into its slot first — the
    /// `F` arm of `write_back(Keep::Type)`: the block reads and
    /// writes `x` through the frame, so the register copy alone would be
    /// stale on both sides.
    #[test]
    fn test_unboxed_local_homed_before_an_outgoing_block() {
        run_test(
            r###"
        def f(a)
          x = a * 2.0
          "abc".each_char { |c| x += c.size }
          y = x * 0.5
          loop { y += 1.0; break }
          [x, y]
        end
        f(1.5)
        "###,
        );
    }

    /// §15.5: a loop-carried float enters a loop JIT from the VM as a boxed
    /// `S(Value)`, but the back-edge fixpoint proves it is a `Float`. The
    /// loop-entry specialization re-adopts `F` (the `S -> F` bridge unboxes the
    /// forward entry once, guarded), so the body stays unboxed. Correctness
    /// regression for that path + the `keep_backedge_floats` promotion gate.
    #[test]
    fn test_loop_carried_float_kept_unboxed() {
        run_test(
            r###"
        def f(n)
          x = 0.0
          y = 1.0
          i = 0
          while i < n
            x = x * 1.5 + i * 0.5
            y = y - x * 0.25
            i += 1
          end
          [x, y]
        end
        f(1000)
        "###,
        );
    }

    /// Regression test for the `alloc_fpr` aliasing bug. When `load_binary_fpr`
    /// loaded `lhs` into fpr `A` and then loaded `rhs`, Phase-1 of
    /// `try_alloc_fpr_demote` could demote `A`'s `Sf` slot back to `S` and hand
    /// `A` back as the rhs fpr. The consuming `ucomisd fpr A, fpr A` then
    /// always reported equal, so `d2 > 0` evaluated false even when
    /// `d2 = 100.0` and the ternary fell through to `Float::INFINITY`. The
    /// trigger needs ~14 simultaneously-live floats so all fprs are occupied
    /// when the second operand is loaded.
    ///
    /// Discovered while running the `khasinski/doom` Ruby port under
    /// monoruby — the renderer's sprite-vs-wall clip uses exactly this
    /// shape and sprites would draw through walls until the fix.
    #[test]
    fn test_alloc_fpr_aliasing_under_pressure() {
        run_test(
            r###"
        def f(d1, d2, x, y, s, c, p)
          px = x - 100.0
          py = y - 100.0
          w  = 160
          c1 = c * p - s * w
          c2 = s * p + c * w
          ta = py * s  + px * c
          tb = py * c1 - px * c2
          s1 = d1 > 0 ? p / d1 : Float::INFINITY
          s2 = d2 > 0 ? p / d2 : Float::INFINITY
          [s1, s2, ta, tb, px, py]
        end
        # Warm the JIT, then probe.
        1500.times { f(50.0, 100.0, 1024.0, -1024.0, 0.5, 0.866, 160.0) }
        f(50.0, 100.0, 1024.0, -1024.0, 0.5, 0.866, 160.0)
        "###,
        );
    }

    /// Same regression, but with the assigned-value differing from both the
    /// `endv` register and the computed register -- exercises the fresh-fpr
    /// rebind path when neither `l` nor `r` is safe to keep.
    #[test]
    fn test_join_float_register_three_way() {
        run_test(
            r###"
        def test
          res = []
          i = 0
          lo  = -2.0
          hi  =  2.0
          while i < 5
            a = -1.0 + i * 0.5
            if a > hi
              a = hi
            elsif a < lo
              a = lo
            end
            res << a
            i += 1
          end
          res
        end
        test
        "###,
        );
    }
}
