use super::*;

///
/// §5 stage 3c-i — the register-allocation **policy** seam.
///
/// Every xmm allocation in the JIT funnels through these two primitives (operand
/// loads `load_xmm*` / `fetch_float*`, destination defs `def_F` / `def_Sf*`, and
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
    /// Returns `None` if every xmm holds at least one `F` slot (a real spill;
    /// use [`alloc_xmm`] from a context that has access to `AsmIr`).
    ///
    pub(super) fn try_alloc_xmm(state: &mut SlotState) -> Option<FPReg> {
        // Phase 0: a vacant xmm.
        for i in 0..state.xmm_alloc.len() {
            let xmm = FPReg(i);
            if state.xmm_alloc.is_pinned(xmm) {
                continue;
            }
            if state.xmm_alloc.is_vacant(xmm) {
                return Some(xmm);
            }
        }
        // Phase 1: an xmm whose linked slots are all `Sf` — demote them.
        for i in 0..state.xmm_alloc.len() {
            let xmm = FPReg(i);
            if state.xmm_alloc.is_pinned(xmm) || state.xmm_alloc.is_vacant(xmm) {
                continue;
            }
            let all_sf = state
                .xmm_alloc
                .slots(xmm)
                .iter()
                .all(|&s| matches!(state.mode(s), LinkMode::Sf(_, _)));
            if !all_sf {
                continue;
            }
            let to_demote: Vec<(SlotId, SfGuarded)> = state
                .xmm_alloc
                .slots(xmm)
                .iter()
                .map(|&s| match state.mode(s) {
                    LinkMode::Sf(_, g) => (s, g),
                    _ => unreachable!(),
                })
                .collect();
            state.xmm_alloc.clear(xmm);
            for (s, g) in to_demote {
                state.set_mode(s, LinkMode::S(g.into()));
            }
            return Some(xmm);
        }
        None
    }

    ///
    /// Allocate a new VirtFPReg. Phase 0 (vacant phys) and Phase 1 (Sf-only
    /// demote) first; if both fail, a fresh phase-2 spill slot (`VirtFPReg(N)`,
    /// `N >= PHYS_XMM_POOL`) that lives on the stack and is swapped in at use.
    ///
    pub(super) fn alloc_xmm(state: &mut SlotState) -> FPReg {
        if let Some(x) = try_alloc_xmm(state) {
            return x;
        }
        state.xmm_alloc.push_spill()
    }
}

///
/// The xmm-register allocation state (item ②, step 1): the reverse map from
/// physical/spill FP registers to the slots bound to them, plus the pin set.
/// `SlotState` owns one of these and drives it through its `xmm_*` methods; the
/// allocation *policy* (`alloc_policy::try_alloc_xmm` / `alloc_xmm`) takes `&mut
/// SlotState` because it also reads/mutates slot placements. Indices
/// `0..PHYS_XMM_POOL` map to physical `xmm2..xmm15`; `>= PHYS_XMM_POOL` are stack
/// spills.
///
#[derive(Clone, Default)]
pub(super) struct XmmAllocator {
    vfpr: Vec<Vec<SlotId>>,
    /// xmm registers that must not be reused by `alloc_xmm` until unpinned.
    pinned: Vec<FPReg>,
}

impl XmmAllocator {
    fn new() -> Self {
        Self {
            vfpr: (0..PHYS_XMM_POOL).map(|_| vec![]).collect(),
            pinned: Vec::new(),
        }
    }

    fn len(&self) -> usize {
        self.vfpr.len()
    }

    fn slots(&self, xmm: FPReg) -> &[SlotId] {
        &self.vfpr[xmm.0 as usize]
    }

    fn is_vacant(&self, xmm: FPReg) -> bool {
        self.vfpr[xmm.0 as usize].is_empty()
    }

    fn is_pinned(&self, xmm: FPReg) -> bool {
        self.pinned.contains(&xmm)
    }

    fn add(&mut self, slot: SlotId, xmm: FPReg) {
        self.vfpr[xmm.0 as usize].push(slot);
    }

    fn remove(&mut self, slot: SlotId, xmm: FPReg) {
        self.vfpr[xmm.0 as usize].retain(|e| *e != slot);
    }

    fn clear(&mut self, xmm: FPReg) {
        self.vfpr[xmm.0 as usize].clear();
    }

    fn grow_to(&mut self, new_len: usize) {
        while self.vfpr.len() < new_len {
            self.vfpr.push(vec![]);
        }
    }

    /// Append a fresh spill slot beyond the physical pool and return its id.
    fn push_spill(&mut self) -> FPReg {
        let new_id = self.vfpr.len();
        self.vfpr.push(vec![]);
        FPReg(new_id)
    }

    fn pin(&mut self, xmm: FPReg) {
        if !self.pinned.contains(&xmm) {
            self.pinned.push(xmm);
        }
    }

    fn unpin(&mut self, xmm: FPReg) {
        if let Some(pos) = self.pinned.iter().position(|x| *x == xmm) {
            self.pinned.swap_remove(pos);
        }
    }

    fn swap(&mut self, l: FPReg, r: FPReg) {
        self.vfpr.swap(l.0 as usize, r.0 as usize);
    }
}

#[derive(Clone, Default)]
pub(crate) struct SlotState {
    /// Per-slot location / representation (the `LinkMode` split into its two
    /// halves; item ②). Paired index-for-index with `ty`.
    place: Vec<Placement>,
    /// Per-slot abstract type (the class lattice). The `Sf` refinement is
    /// recovered from this via `LinkMode::from_parts`. Paired with `place`.
    ty: Vec<Guarded>,
    /// Liveness information.
    liveness: Vec<IsUsed>,
    /// xmm-register allocation state (item ②, step 1).
    xmm_alloc: XmmAllocator,
    r15: Option<SlotId>,
    local_num: usize,
    /// D1 forwarding-rest deferral (transient annotation; see the consumers).
    deferred_rest: Option<(SlotId, SlotId, u16)>,
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
        let default_ty = match default {
            LinkMode::None | LinkMode::MaybeNone | LinkMode::V => Guarded::Value,
            o => o.guarded(),
        };
        let mut ctx = SlotState {
            place: vec![default.placement(); total_reg_num],
            ty: vec![default_ty; total_reg_num],
            liveness: vec![IsUsed::default(); total_reg_num],
            xmm_alloc: XmmAllocator::new(),
            r15: None,
            local_num,
            deferred_rest: None,
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
        // D1: tentatively annotate the forwarding-trampoline rest slot.
        // The slot's `LinkMode` is left at its baseline (`S`): when the
        // deferral activates the caller-side `set_arguments` physically
        // stores a real `nil` there (GC-safe) and the consumer routes
        // from the caller source; deopts rebuild the array via
        // `forward_rest`. When it does *not* activate the caller builds
        // the array normally and the (still-`S`) slot holds it — no
        // spurious `C(nil)` write-back can clobber that array. The
        // annotation only routes the consumer and adds the deopt
        // materialization while live.
        if let Some((dst, src, len)) = cc.forward_rest_deferral() {
            ctx.deferred_rest = Some((dst, src, len));
        }
        ctx
    }

    pub(super) fn slots_len(&self) -> usize {
        self.place.len()
    }

    /// The pure type-lattice meet over the per-slot `ty` vectors — the
    /// analysis-layer join (item ②, the reusable primitive for the standalone
    /// analysis pass in step 2). Element-wise `Guarded::join`; placement /
    /// sentinel reconciliation is a separate concern that the fused
    /// `AbstractFrame::join` still owns today. For non-sentinel slots this equals
    /// the fused join's resulting type (verified arm-by-arm; see
    /// `doc/regalloc_separation.md`).
    #[allow(dead_code)] // wired in by step 2 (standalone analysis pass)
    pub(super) fn join_ty(&self, other: &SlotState) -> Vec<Guarded> {
        self.ty
            .iter()
            .zip(other.ty.iter())
            .map(|(a, b)| a.join(b))
            .collect()
    }

    pub(super) fn no_r15(&self) -> bool {
        self.r15.is_none()
    }

    pub(super) fn equiv(&self, other: &Self) -> bool {
        assert_eq!(self.slots_len(), other.slots_len());
        self.all_regs()
            .all(|i| self.mode(i).equiv(&other.mode(i)))
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
                        // Liveness-driven hint only — if no xmm is free without
                        // a real spill, leave the slot on the stack and rely on
                        // a lazy `load_xmm` later (which has access to AsmIr
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
                LinkMode::G(_) | LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
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

    pub(super) fn locals(&self) -> std::ops::Range<SlotId> {
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
        let i = slot.0 as usize;
        LinkMode::from_parts(self.place[i], self.ty[i])
    }

    pub(in crate::codegen::jitgen) fn guarded(&self, slot: SlotId) -> Guarded {
        self.mode(slot).guarded()
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
        let i = slot.0 as usize;
        self.place[i] = mode.placement();
        // Sentinels carry no type; `from_parts` ignores `ty` for them.
        self.ty[i] = match mode {
            LinkMode::None | LinkMode::MaybeNone | LinkMode::V => Guarded::Value,
            o => o.guarded(),
        };
    }

    /// D1: if `slot` is the deferred forwarding-rest slot, return its
    /// `(src, len)` caller source range.
    pub(in crate::codegen::jitgen) fn deferred_rest_src(
        &self,
        slot: SlotId,
    ) -> Option<(SlotId, u16)> {
        match self.deferred_rest {
            Some((dst, src, len)) if dst == slot => Some((src, len)),
            _ => None,
        }
    }

    /// D1: the frame's `(dst, src, len)` deferral annotation, if any.
    /// Used by array-path forwarding consumers to veto the caller-side
    /// `create_array` skip (`set_needs_rest_array`).
    pub(in crate::codegen::jitgen) fn deferred_rest_tuple(&self) -> Option<(SlotId, SlotId, u16)> {
        self.deferred_rest
    }

    pub(super) fn is_used(&self, slot: SlotId) -> &IsUsed {
        &self.liveness[slot.0 as usize]
    }

    pub(super) fn is_used_mut(&mut self, slot: SlotId) -> &mut IsUsed {
        &mut self.liveness[slot.0 as usize]
    }

    fn xmm(&self, xmm: FPReg) -> &[SlotId] {
        self.xmm_alloc.slots(xmm)
    }

    ///
    /// Number of allocated spill slots (including the pool prefix).
    /// Used by `gen_bridge` so the source state can grow its own
    /// `xmm` vec up to the target's width before merge bridging.
    ///
    pub(super) fn xmm_len(&self) -> usize {
        self.xmm_alloc.len()
    }

    ///
    /// Pad `self.xmm` with empty slot lists until it reaches at least
    /// *new_len*. The fresh entries correspond to spill slot ids that
    /// were allocated by a sibling branch but not by us; merging at a
    /// confluence point will route any `LinkMode::F(VirtFPReg(N))`
    /// referencing them into a `gen_xmm_swap` / `xmm_move` against a
    /// vacant binding, which `is_xmm_vacant` correctly reports.
    ///
    pub(super) fn grow_xmm_to(&mut self, new_len: usize) {
        self.xmm_alloc.grow_to(new_len);
    }

    fn xmm_add(&mut self, slot: SlotId, xmm: FPReg) {
        self.xmm_alloc.add(slot, xmm);
    }

    fn xmm_remove(&mut self, slot: SlotId, xmm: FPReg) {
        self.xmm_alloc.remove(slot, xmm);
    }

    /// Mark *xmm* off-limits for subsequent `alloc_xmm` /
    /// `try_alloc_xmm_demote` calls until [`Self::unpin_xmm`] is invoked.
    /// Use this when an xmm has just been produced (loaded or written) and
    /// is needed by an upcoming instruction in the same compile step —
    /// without the pin, a later allocation in the same step can choose the
    /// freshly-loaded xmm as a spill victim and reuse it for an unrelated
    /// value.
    pub(in crate::codegen::jitgen) fn pin_xmm(&mut self, xmm: FPReg) {
        self.xmm_alloc.pin(xmm);
    }

    pub(in crate::codegen::jitgen) fn unpin_xmm(&mut self, xmm: FPReg) {
        self.xmm_alloc.unpin(xmm);
    }

    ///
    /// Try to allocate a new xmm register without emitting any asm.
    ///
    /// Phase 0: returns the first vacant xmm.
    /// Phase 1: if no vacant xmm, finds an xmm whose linked slots are all `Sf`
    ///          (stack already holds the value, xmm is just a read-only cache);
    ///          demotes them all to `S` and returns the freed xmm. No asm is
    ///          emitted because the stack already has the canonical value.
    ///
    /// Returns `None` if every xmm holds at least one `F` slot (would require
    /// a real spill; use [`Self::alloc_xmm`] from a context that has access to
    /// `AsmIr`).
    ///
    fn try_alloc_xmm(&mut self) -> Option<FPReg> {
        alloc_policy::try_alloc_xmm(self)
    }

    fn alloc_xmm(&mut self) -> FPReg {
        alloc_policy::alloc_xmm(self)
    }

    ///
    /// Clear slot *reg* and set LinkMode to V.
    ///
    fn clear(&mut self, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::Sf(xmm, _) | LinkMode::F(xmm) => {
                assert!(self.xmm(xmm).contains(&slot));
                self.xmm_remove(slot, xmm);
            }
            LinkMode::G(_) => {
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
            }
            LinkMode::C(_) => {}
            LinkMode::S(_) => {}
            LinkMode::MaybeNone | LinkMode::None => {}
            LinkMode::V => return,
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
    pub(in crate::codegen::jitgen) fn keep_backedge_floats(
        &mut self,
        backedge: &SlotState,
        promotable: impl Fn(SlotId) -> bool,
    ) {
        for i in self.all_regs() {
            if matches!(backedge.mode(i), LinkMode::F(_))
                && matches!(self.mode(i), LinkMode::S(_) | LinkMode::Sf(_, _))
                && promotable(i)
            {
                self.set_new_F(i);
            }
        }
    }

    ///
    /// F/Sf -> F
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_F(&mut self, slot: SlotId, xmm: FPReg) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::F(xmm));
        self.xmm_add(slot, xmm);
    }

    ///
    /// F/Sf -> Sf
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_Sf(&mut self, slot: SlotId, xmm: FPReg, guarded: SfGuarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::Sf(xmm, guarded));
        self.xmm_add(slot, xmm);
    }

    ///
    /// C -> F (may emit a victim spill if no xmm is free).
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_new_F(&mut self, slot: SlotId) -> FPReg {
        let x = self.alloc_xmm();
        self.set_F(slot, x);
        x
    }

    ///
    /// C -> F (no asm emit). Returns `None` when only Phase-2 spill could free
    /// an xmm — caller should fall back (e.g. leave the slot as `S`).
    ///
    #[allow(non_snake_case)]
    pub(super) fn try_set_new_F(&mut self, slot: SlotId) -> Option<FPReg> {
        let x = self.try_alloc_xmm()?;
        self.set_F(slot, x);
        Some(x)
    }

    ///
    /// C -> Sf (may emit a victim spill if no xmm is free).
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_new_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> FPReg {
        let x = self.alloc_xmm();
        self.set_Sf(slot, x, guarded);
        x
    }

    ///
    /// C -> Sf (no asm emit). Returns `None` when only Phase-2 spill could free
    /// an xmm.
    ///
    #[allow(non_snake_case)]
    pub(super) fn try_set_new_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> Option<FPReg> {
        let x = self.try_alloc_xmm()?;
        self.set_Sf(slot, x, guarded);
        Some(x)
    }

    ///
    /// F -> Sf
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_Sf_float(&mut self, slot: SlotId, xmm: FPReg) {
        self.set_Sf(slot, xmm, SfGuarded::Float)
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
    pub(super) fn def_S_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self.discard(slot);
        self.set_mode(slot, LinkMode::S(guarded));
    }

    ///
    /// Link *slot* to a new xmm register (may emit a victim spill).
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_F(&mut self, slot: SlotId) -> FPReg {
        let xmm = self.alloc_xmm();
        self.discard(slot);
        self.set_F(slot, xmm);
        xmm
    }

    #[allow(non_snake_case)]
    pub(crate) fn def_F_with_xmm(&mut self, slot: SlotId, xmm: FPReg) -> FPReg {
        self.discard(slot);
        self.set_F(slot, xmm);
        xmm
    }

    ///
    /// Link *slot* to both of the stack and a new xmm register (may emit a
    /// victim spill).
    ///
    #[allow(non_snake_case)]
    fn def_Sf(&mut self, slot: SlotId, guarded: SfGuarded) -> FPReg {
        let xmm = self.alloc_xmm();
        self.discard(slot);
        self.set_Sf(slot, xmm, guarded);
        xmm
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

    ///
    /// Link *slot* to the accumulator.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_G(
        &mut self,
        ir: &mut AsmIr,
        slot: impl Into<Option<SlotId>>,
        guarded: Guarded,
    ) {
        if let Some(slot) = slot.into() {
            self.discard(slot);
            self.writeback_acc(ir);
            self.set_mode(slot, LinkMode::G(guarded));
            self.r15 = Some(slot);
        }
    }

    // APIs for 'use'

    /// used as f64 with no conversion
    pub(super) fn use_as_float(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_float();
    }

    pub(super) fn use_as_value(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_non_float();
    }

    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::S or Sf or C.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    /// Analysis half (item ②, step 2): perform the abstract-state transition and
    /// return the pending stack write-back as a [`Spill`] record, without
    /// touching `AsmIr`. The codegen wrapper [`Self::write_back_slot`] emits it.
    fn write_back_slot_state(&mut self, slot: SlotId) -> Spill {
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                // F -> Sf
                self.set_Sf_float(slot, xmm);
                Spill::Fpr(xmm, slot)
            }
            LinkMode::C(v) => Spill::Lit(v, slot),
            LinkMode::G(guarded) => {
                // G -> S
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
                self.set_mode(slot, LinkMode::S(guarded));
                Spill::Acc(slot)
            }
            LinkMode::Sf(_, _) | LinkMode::S(_) | LinkMode::MaybeNone => Spill::None,
            LinkMode::V | LinkMode::None => {
                eprintln!("{:?}", self);
                unreachable!("write_back_slot() {slot:?} {:?}", self.mode(slot));
            }
        }
    }

    pub(in crate::codegen::jitgen) fn write_back_slot(&mut self, ir: &mut AsmIr, slot: SlotId) {
        let s = self.write_back_slot_state(slot);
        ir.transfer(TransferIR::Spill(s));
    }

    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::S without class guards.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    /// Analysis half (item ②, step 2): see [`Self::write_back_slot_state`].
    #[allow(non_snake_case)]
    fn to_S_unguarded_state(&mut self, slot: SlotId) -> Spill {
        let spill = match self.mode(slot) {
            LinkMode::F(xmm) => Spill::Fpr(xmm, slot),
            LinkMode::C(v) => Spill::Lit(v, slot),
            LinkMode::G(_) => Spill::Acc(slot),
            LinkMode::Sf(_, _) | LinkMode::S(_) => Spill::None,
            LinkMode::V => Spill::Lit(Value::nil(), slot),
            LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("to_S_unguarded() {:?}", self.mode(slot));
            }
        };
        self.clear(slot);
        self.set_mode(slot, LinkMode::S(Guarded::Value));
        spill
    }

    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn to_S_unguarded(&mut self, ir: &mut AsmIr, slot: SlotId) {
        let s = self.to_S_unguarded_state(slot);
        ir.transfer(TransferIR::Spill(s));
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

    pub fn is_i16_literal(&self, slot: SlotId) -> Option<i16> {
        let i = self.is_fixnum_literal(slot)?.get();
        i16::try_from(i).ok()
    }

    //pub fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
    //    let i = self.is_fixnum_literal(slot)?.get();
    //    u8::try_from(i).ok()
    //}

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

    pub(super) fn on_reg(&self, slot: SlotId) -> Option<GP> {
        if self.is_r15(slot) {
            Some(GP::R15)
        } else {
            None
        }
    }

    pub(in crate::codegen::jitgen) fn on_reg_or(&self, slot: SlotId, optb: GP) -> GP {
        if self.is_r15(slot) { GP::R15 } else { optb }
    }

    ///
    ///
    /// Write back acc(`r15``) to the stack slot.
    ///
    /// The slot is set to LinkMode::Stack.
    ///
    ///
    /// Analysis half of [`Self::writeback_acc`] (item ②, step-2 spike): evict
    /// the accumulator (`r15`) owner to its stack home in the *abstract state*
    /// only, returning the evicted slot so the emission half can store it. This
    /// is the pure state transition — a standalone analysis pass calls this and
    /// never touches `AsmIr`; codegen calls `writeback_acc` (this + the store).
    ///
    fn writeback_acc_state(&mut self) -> Option<SlotId> {
        let slot = self.r15?;
        let guarded = self.guarded(slot);
        self.set_mode(slot, LinkMode::S(guarded));
        self.r15 = None;
        Some(slot)
    }

    pub(crate) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        if let Some(slot) = self.writeback_acc_state() {
            ir.acc2stack(slot);
        }
        assert!(!self.all_regs().any(|i| matches!(self.mode(i), LinkMode::G(_))));
        assert!(self.r15.is_none());
    }

    fn is_xmm_vacant(&self, xmm: FPReg) -> bool {
        self.xmm(xmm).is_empty()
    }

    fn is_r15(&self, slot: SlotId) -> bool {
        self.r15 == Some(slot)
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
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.def_S_guarded(dst, guarded);
            }
            LinkMode::C(v) => {
                self.def_C(dst, v);
            }
            LinkMode::G(guarded) => {
                ir.reg2stack(GP::R15, src);
                self.set_S_with_guard(src, guarded);
                self.def_G(ir, dst, guarded)
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
    pub(super) fn guard_class_state(&mut self, slot: SlotId, class: ClassId) -> bool {
        if self.class(slot) == Some(class) {
            return false;
        }
        let class_guarded = Guarded::from_class(class);
        // Operate on a local copy and write it back (item ② encapsulation;
        // `LinkMode` is `Copy`). The `return false`s below skip both the
        // write-back and the guard emission, exactly as the prior `return`s did.
        let mut mode = self.mode(slot);
        match &mut mode {
            LinkMode::S(guarded) | LinkMode::G(guarded) => {
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

    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        let mut b = UsingXmm::new();
        // Only physical pool slots need save/restore at call
        // boundaries; spill slots already live on the stack.
        for i in 0..PHYS_XMM_POOL {
            if !self.xmm_alloc.is_vacant(FPReg(i)) {
                b.set(i, true);
            }
        }
        b
    }

    fn wb_forward_rest(&self) -> Vec<(SlotId, SlotId, u16)> {
        self.deferred_rest.into_iter().collect()
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
        WriteBack::new(vec![], literal, self.r15, void, vec![])
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        let f = |_| true;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, r15, vec![], self.wb_forward_rest())
    }

    fn xmm_swap(&mut self, l: FPReg, r: FPReg) {
        self.xmm_alloc.swap(l, r);
        // A physical xmm swap (`FprSwap`) only changes *which register* holds
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
                | LinkMode::G(_)
                | LinkMode::V
                | LinkMode::MaybeNone
                | LinkMode::None => {}
            }
            self.set_mode(slot, link);
        }
    }

    fn wb_xmm(&self, f: impl Fn(SlotId) -> bool) -> Vec<(FPReg, Vec<SlotId>)> {
        (0..self.xmm_alloc.len())
            .filter_map(|i| {
                let reg = FPReg::new(i);
                let v: Vec<_> = self
                    .xmm_alloc
                    .slots(reg)
                    .iter()
                    .filter(|s| f(**s) && matches!(self.mode(**s), LinkMode::F(_)))
                    .cloned()
                    .collect();
                if v.is_empty() { None } else { Some((reg, v)) }
            })
            .collect()
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
/// The *location / representation* half of a [`LinkMode`], with the type/class
/// (`Guarded`) factored out — the dual of [`LinkMode::guarded`].
///
/// Part of item ② (separating value placement from type analysis; see
/// `doc/regalloc_separation.md`). It records only *where the live copies are*.
/// The `Sf` linkage is the `XmmStack` placement: per review, `Sf` is **not** a
/// type — it is a representation chosen for "Integer def'd / Float use'd" slots
/// by a dedicated def-use + loop analysis. Its `SfGuarded` refinement is exactly
/// the boxed value's class, so it is recovered from the paired `Guarded` (the
/// `SfGuarded → Guarded` map is injective: `FixnumOrFloat ↔ Value`).
/// `LinkMode::from_parts(self.placement(), self.guarded())` reconstructs the
/// original `LinkMode` (verified by a unit test).
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum Placement {
    None,
    MaybeNone,
    /// void (temp slot above sp)
    Void,
    /// boxed `Value` in its stack home
    Stack,
    /// boxed `Value` in the GP accumulator (r15)
    Gp,
    /// unboxed f64 in an xmm (type is implicitly `Float`)
    Xmm(FPReg),
    /// unboxed f64 in an xmm + a read-only boxed cache on the stack (the `Sf`
    /// linkage); the Int/Float refinement is recovered from the paired `Guarded`
    XmmStack(FPReg),
    /// compile-time constant (no register / stack location)
    Const(Value),
}

///
/// A pending stack write-back produced by a transfer/eviction primitive
/// (item ②, step 2): the *what* of an eviction, decided by the primitive's
/// analysis (state) half and emitted by the codegen half. This is the first
/// concrete "typed IR record" — a standalone analysis pass collects these
/// instead of pushing `AsmInst`, and the lowering pass replays them via
/// [`Spill::emit`].
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum Spill {
    None,
    /// `ir.fpr2stack(xmm, slot)`
    Fpr(FPReg, SlotId),
    /// `ir.lit2stack(value, slot)`
    Lit(Value, SlotId),
    /// `ir.acc2stack(slot)`
    Acc(SlotId),
}

impl Spill {
    pub(super) fn emit(self, ir: &mut AsmIr) {
        match self {
            Spill::None => {}
            Spill::Fpr(xmm, slot) => ir.fpr2stack(xmm, slot),
            Spill::Lit(v, slot) => ir.lit2stack(v, slot),
            Spill::Acc(slot) => ir.acc2stack(slot),
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

///
/// Mode of linkage between stack slot and xmm registers.
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
    /// On the general-purpose register (r15).
    ///
    G(Guarded),
    ///
    /// On the floating point register (xmm).
    ///
    /// mutation of the corresponding FPR lazily affects the stack slot.
    ///
    F(FPReg),
    ///
    /// On the stack slot and on the floating point register (xmm) which is read-only.
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

impl LinkMode {
    fn default() -> Self {
        LinkMode::S(Guarded::Value)
    }

    fn none() -> Self {
        LinkMode::None
    }

    fn nil() -> Self {
        LinkMode::C(Value::nil())
    }

    fn guarded(&self) -> Guarded {
        match self {
            LinkMode::S(guarded) | LinkMode::G(guarded) => *guarded,
            LinkMode::Sf(_, guarded) => (*guarded).into(),
            LinkMode::F(_) => Guarded::Float,
            LinkMode::C(v) => Guarded::from_concrete_value(*v),
            LinkMode::V => Guarded::Class(NIL_CLASS),
            _ => unreachable!("{:?}", self),
        }
    }

    ///
    /// The location/representation half of this mode (the dual of
    /// [`Self::guarded`]). See [`Placement`].
    ///
    fn placement(&self) -> Placement {
        match self {
            LinkMode::None => Placement::None,
            LinkMode::MaybeNone => Placement::MaybeNone,
            LinkMode::V => Placement::Void,
            LinkMode::S(_) => Placement::Stack,
            LinkMode::G(_) => Placement::Gp,
            LinkMode::F(x) => Placement::Xmm(*x),
            LinkMode::Sf(x, _) => Placement::XmmStack(*x),
            LinkMode::C(v) => Placement::Const(*v),
        }
    }

    ///
    /// Recombine a placement with a type guard into a `LinkMode` — the inverse
    /// of [`Self::placement`] + [`Self::guarded`]. The `guarded` types the boxed
    /// `Stack` / `Gp` cases and picks the `Sf` refinement for `XmmStack` (the
    /// `Guarded → SfGuarded` inverse: `Value → FixnumOrFloat`); the `Xmm` /
    /// `Const` / sentinel placements carry their own type, so it is ignored.
    ///
    fn from_parts(place: Placement, guarded: Guarded) -> Self {
        match place {
            Placement::None => LinkMode::None,
            Placement::MaybeNone => LinkMode::MaybeNone,
            Placement::Void => LinkMode::V,
            Placement::Stack => LinkMode::S(guarded),
            Placement::Gp => LinkMode::G(guarded),
            Placement::Xmm(x) => LinkMode::F(x),
            Placement::XmmStack(x) => {
                let sf = match guarded {
                    Guarded::Float => SfGuarded::Float,
                    Guarded::Fixnum => SfGuarded::Fixnum,
                    Guarded::Value => SfGuarded::FixnumOrFloat,
                    Guarded::Class(_) => unreachable!("XmmStack with class guard {guarded:?}"),
                };
                LinkMode::Sf(x, sf)
            }
            Placement::Const(v) => LinkMode::C(v),
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
                    if self.is_xmm_vacant(r) {
                        self.set_F(slot, r);
                        ir.fpr_move(l, r);
                    } else {
                        self.gen_xmm_swap(ir, l, r);
                    }
                }
            }
            (
                LinkMode::F(l),
                LinkMode::Sf(r, guarded @ (SfGuarded::Float | SfGuarded::FixnumOrFloat)),
            ) => {
                // F means the xmm holds a float; writing back produces a
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
            (LinkMode::F(_) | LinkMode::G(_), LinkMode::S(_)) => {
                self.write_back_slot(ir, slot);
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
                if self.is_xmm_vacant(x) {
                    ir.float_to_fpr(GP::Rax, x, deopt);
                    self.set_Sf_float(slot, x);
                } else {
                    let tmp = self.set_new_Sf(slot, SfGuarded::Float);
                    ir.float_to_fpr(GP::Rax, tmp, deopt);
                    self.gen_xmm_swap(ir, x, tmp);
                }
            }
            (LinkMode::S(_), LinkMode::F(x)) => {
                // S -> F: one-time unbox of a boxed float into a pure-xmm
                // binding (no boxed cache) — a loop pre-header entry adopting the
                // back-edge's `F` placement (§15.3). Mirrors the `S -> Sf` arm but
                // sets `F`; reuses `float_to_fpr`, which both backends lower.
                ir.stack2reg(slot, GP::Rax);
                let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                if self.is_xmm_vacant(x) {
                    ir.float_to_fpr(GP::Rax, x, deopt);
                    self.set_F(slot, x);
                } else {
                    let tmp = self.set_new_F(slot);
                    ir.float_to_fpr(GP::Rax, tmp, deopt);
                    self.gen_xmm_swap(ir, x, tmp);
                }
            }
            (LinkMode::Sf(l, _), LinkMode::F(r)) => {
                // Sf -> F: the value is already unboxed in xmm `l`; drop the
                // boxed cache and rebind as pure `F`. Mirrors the `F -> F` arm.
                if l == r {
                    self.set_F(slot, l);
                } else if self.is_xmm_vacant(r) {
                    self.set_F(slot, r);
                    ir.fpr_move(l, r);
                } else {
                    self.gen_xmm_swap(ir, l, r);
                    self.set_F(slot, r);
                }
            }
            (LinkMode::G(_), LinkMode::Sf(x, SfGuarded::Float)) => {
                // G -> Sf
                let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                if self.is_xmm_vacant(x) {
                    ir.float_to_fpr(GP::R15, x, deopt);
                    self.set_Sf_float(slot, x);
                } else {
                    let tmp = self.set_new_Sf(slot, SfGuarded::Float);
                    ir.float_to_fpr(GP::R15, tmp, deopt);
                    self.gen_xmm_swap(ir, x, tmp);
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
        if self.is_xmm_vacant(r) {
            self.set_Sf(slot, r, guarded);
            FpXfer::Move(l, r)
        } else {
            self.xmm_swap(l, r);
            FpXfer::Swap(l, r)
        }
    }

    fn to_sf(&mut self, ir: &mut AsmIr, slot: SlotId, l: FPReg, r: FPReg, guarded: SfGuarded) {
        let f = self.to_sf_state(slot, l, r, guarded);
        ir.transfer(TransferIR::FpXfer(f));
    }

    ///
    /// Swap xmm registers `l` and `r`.
    ///
    fn gen_xmm_swap(&mut self, ir: &mut AsmIr, l: FPReg, r: FPReg) {
        self.xmm_swap(l, r);
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

    /// Regression test for the `xmm_swap` mixed-refinement panic. A diamond
    /// (`if/else`) inside a loop that updates a `Float` accumulator on both arms
    /// drives the back-edge bridge to `gen_xmm_swap` a pure-`Float` `F` register
    /// against a Fixnum-refined `Sf` register (the loop counter coerced to f64).
    /// `xmm_swap` used to `assert_eq!(guarded_r, Some(Float))` / cross-assign the
    /// partner register's refinement, assuming both swapped registers shared a
    /// refinement — so this aborted the process at `slot.rs` with
    /// `left: Some(Fixnum), right: Some(Float)` in both debug and release. The
    /// swap only relabels register indices now, so each slot keeps its own
    /// refinement. Expected result: `1249925000.0`.
    #[test]
    fn test_xmm_swap_mixed_refinement() {
        run_test(
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
        f(100000)
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

    /// Regression test for the `alloc_xmm` aliasing bug. When `load_binary_xmm`
    /// loaded `lhs` into xmm `A` and then loaded `rhs`, Phase-1 of
    /// `try_alloc_xmm_demote` could demote `A`'s `Sf` slot back to `S` and hand
    /// `A` back as the rhs xmm. The consuming `ucomisd xmm A, xmm A` then
    /// always reported equal, so `d2 > 0` evaluated false even when
    /// `d2 = 100.0` and the ternary fell through to `Float::INFINITY`. The
    /// trigger needs ~14 simultaneously-live floats so all xmms are occupied
    /// when the second operand is loaded.
    ///
    /// Discovered while running the `khasinski/doom` Ruby port under
    /// monoruby — the renderer's sprite-vs-wall clip uses exactly this
    /// shape and sprites would draw through walls until the fix.
    #[test]
    fn test_alloc_xmm_aliasing_under_pressure() {
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
    /// `endv` register and the computed register -- exercises the fresh-xmm
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

    /// Item ②: `LinkMode` decomposes losslessly into a `Placement` (location /
    /// representation) and a `Guarded` (type), and recombines via `from_parts`.
    /// The `Sf` (`XmmStack`) refinement is recovered from the paired `Guarded`.
    #[test]
    fn linkmode_placement_roundtrip() {
        use super::*;
        let x = FPReg(0);
        // Value-carrying modes: from_parts(placement(), guarded()) == self.
        let value_modes = [
            LinkMode::S(Guarded::Value),
            LinkMode::S(Guarded::Fixnum),
            LinkMode::S(Guarded::Float),
            LinkMode::S(Guarded::Class(NIL_CLASS)),
            LinkMode::G(Guarded::Fixnum),
            LinkMode::F(x),
            LinkMode::Sf(x, SfGuarded::Float),
            LinkMode::Sf(x, SfGuarded::Fixnum),
            LinkMode::Sf(x, SfGuarded::FixnumOrFloat),
            LinkMode::C(Value::nil()),
            LinkMode::C(Value::i32(7)),
            LinkMode::V,
        ];
        for lm in value_modes {
            assert_eq!(LinkMode::from_parts(lm.placement(), lm.guarded()), lm);
        }
        // None / MaybeNone carry no type; placement() round-trips with any guard.
        for lm in [LinkMode::None, LinkMode::MaybeNone] {
            assert_eq!(LinkMode::from_parts(lm.placement(), Guarded::Value), lm);
        }
    }
}
