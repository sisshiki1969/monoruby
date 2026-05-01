use super::*;

#[derive(Clone, Default)]
pub(crate) struct SlotState {
    /// Slot states.
    slots: Vec<LinkMode>,
    /// Liveness information.
    liveness: Vec<IsUsed>,
    /// Information for VirtFPReg slots. Indices 0..14 map to physical
    /// xmm2..xmm15; indices >= 14 are spilled (live on stack — at use
    /// time the codegen swaps them with a scratch xmm).
    vfpr: Vec<Vec<SlotId>>,
    r15: Option<SlotId>,
    local_num: usize,
    /// xmm registers that must not be reused by `alloc_xmm` /
    /// `try_alloc_xmm_demote`. Used by callers that have just produced
    /// an xmm-resident value but have not yet emitted the consuming
    /// instruction — without this, a subsequent allocation can pick
    /// that same xmm as a spill victim (e.g. demote its `Sf` slot to
    /// `S`) and hand the same physical register back, so the consumer
    /// ends up reading both operands from the same register.
    pinned_vfpr: Vec<FPReg>,
}

impl std::fmt::Debug for SlotState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for (i, state) in self.slots.iter().enumerate() {
            write!(f, "[%{i}: {state:?}] ")?;
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
        let mut ctx = SlotState {
            slots: vec![default; total_reg_num],
            liveness: vec![IsUsed::default(); total_reg_num],
            vfpr: (0..PHYS_XMM_POOL).map(|_| vec![]).collect(),
            r15: None,
            local_num,
            pinned_vfpr: Vec::new(),
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
            ctx.set_mode(i, LinkMode::C(Immediate::nil()));
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
                        ctx.slots[i] = arg.clone();
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

    pub(super) fn no_r15(&self) -> bool {
        self.r15.is_none()
    }

    pub(super) fn equiv(&self, other: &Self) -> bool {
        assert_eq!(self.slots.len(), other.slots.len());
        self.slots
            .iter()
            .zip(other.slots.iter())
            .all(|(lhs, rhs)| lhs.equiv(rhs))
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
        SlotId(0)..SlotId(self.slots.len() as u16)
    }

    fn temps(&self) -> std::ops::Range<SlotId> {
        self.temp_start()..SlotId(self.slots.len() as u16)
    }

    pub(super) fn temp_start(&self) -> SlotId {
        SlotId((1 + self.local_num) as u16)
    }

    pub(in crate::codegen::jitgen) fn mode(&self, slot: SlotId) -> LinkMode {
        self.slots[slot.0 as usize]
    }

    pub(in crate::codegen::jitgen) fn guarded(&self, slot: SlotId) -> Guarded {
        self.mode(slot).guarded()
    }

    pub(in crate::codegen::jitgen) fn class(&self, slot: SlotId) -> Option<ClassId> {
        self.guarded(slot).class()
    }

    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn is_C(&self, slot: SlotId) -> bool {
        self.mode(slot).is_C()
    }
}

impl SlotState {
    pub(super) fn set_mode(&mut self, slot: SlotId, mode: LinkMode) {
        self.slots[slot.0 as usize] = mode;
    }

    pub(super) fn is_used(&self, slot: SlotId) -> &IsUsed {
        &self.liveness[slot.0 as usize]
    }

    pub(super) fn is_used_mut(&mut self, slot: SlotId) -> &mut IsUsed {
        &mut self.liveness[slot.0 as usize]
    }

    fn xmm(&self, xmm: FPReg) -> &[SlotId] {
        &self.vfpr[xmm.0 as usize]
    }

    ///
    /// Number of allocated spill slots (including the pool prefix).
    /// Used by `gen_bridge` so the source state can grow its own
    /// `xmm` vec up to the target's width before merge bridging.
    ///
    pub(super) fn xmm_len(&self) -> usize {
        self.vfpr.len()
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
        while self.vfpr.len() < new_len {
            self.vfpr.push(vec![]);
        }
    }

    fn xmm_add(&mut self, slot: SlotId, xmm: FPReg) {
        self.vfpr[xmm.0 as usize].push(slot);
    }

    fn xmm_remove(&mut self, slot: SlotId, xmm: FPReg) {
        self.vfpr[xmm.0 as usize].retain(|e| *e != slot);
    }

    /// Mark *xmm* off-limits for subsequent `alloc_xmm` /
    /// `try_alloc_xmm_demote` calls until [`Self::unpin_xmm`] is invoked.
    /// Use this when an xmm has just been produced (loaded or written) and
    /// is needed by an upcoming instruction in the same compile step —
    /// without the pin, a later allocation in the same step can choose the
    /// freshly-loaded xmm as a spill victim and reuse it for an unrelated
    /// value.
    pub(in crate::codegen::jitgen) fn pin_xmm(&mut self, xmm: FPReg) {
        if !self.pinned_vfpr.contains(&xmm) {
            self.pinned_vfpr.push(xmm);
        }
    }

    pub(in crate::codegen::jitgen) fn unpin_xmm(&mut self, xmm: FPReg) {
        if let Some(pos) = self.pinned_vfpr.iter().position(|x| *x == xmm) {
            self.pinned_vfpr.swap_remove(pos);
        }
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
        let pinned = &self.pinned_vfpr;
        // Phase 0: a vacant xmm.
        for (i, xmm) in self.vfpr.iter().enumerate() {
            if pinned.contains(&FPReg(i as usize)) {
                continue;
            }
            if xmm.is_empty() {
                return Some(FPReg(i as usize));
            }
        }
        // Phase 1: an xmm whose linked slots are all `Sf` — demote them.
        for i in 0..self.vfpr.len() {
            if self.pinned_vfpr.contains(&FPReg(i as usize)) {
                continue;
            }
            if self.vfpr[i].is_empty() {
                continue;
            }
            let all_sf = self.vfpr[i]
                .iter()
                .all(|&s| matches!(self.mode(s), LinkMode::Sf(_, _)));
            if !all_sf {
                continue;
            }
            let to_demote: Vec<(SlotId, SfGuarded)> = self.vfpr[i]
                .iter()
                .map(|&s| match self.mode(s) {
                    LinkMode::Sf(_, g) => (s, g),
                    _ => unreachable!(),
                })
                .collect();
            self.vfpr[i].clear();
            for (s, g) in to_demote {
                self.set_mode(s, LinkMode::S(g.into()));
            }
            return Some(FPReg(i as usize));
        }
        None
    }

    ///
    /// Allocate a new VirtFPReg.
    ///
    /// Tries Phase 0 (vacant phys) and Phase 1 (Sf-only demote) first. If both
    /// fail, allocates a fresh spill slot — a `VirtFPReg(N)` where `N >=
    /// PHYS_XMM_POOL`. The spill slot lives on the stack (the frame's
    /// `stack_offset` grows by 8 bytes for each spill); at code generation,
    /// any operation that uses such a `VirtFPReg` swaps a scratch xmm with
    /// the stack slot to do its work.
    ///
    fn alloc_xmm(&mut self) -> FPReg {
        if let Some(x) = self.try_alloc_xmm() {
            return x;
        }
        // Phase 2: spill — append a new slot beyond the physical pool. The
        // existing F / Sf bindings are left in place; the value lives on the
        // stack and gets swapped in at use time.
        let new_id = self.vfpr.len();
        self.vfpr.push(vec![]);
        FPReg(new_id)
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
    #[allow(non_snake_case)]
    pub(crate) fn def_C(&mut self, slot: impl Into<Option<SlotId>>, v: Immediate) {
        if let Some(slot) = slot.into() {
            self.discard(slot);
            self.set_mode(slot, LinkMode::C(v));
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
    pub(in crate::codegen::jitgen) fn write_back_slot(&mut self, ir: &mut AsmIr, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                // F -> Sf
                self.set_Sf_float(slot, xmm);
                ir.fpr2stack(xmm, slot);
            }
            LinkMode::C(v) => {
                ir.lit2stack(v.into(), slot);
            }
            LinkMode::G(guarded) => {
                // G -> S
                ir.acc2stack(slot);
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
                self.set_mode(slot, LinkMode::S(guarded));
            }
            LinkMode::Sf(_, _) | LinkMode::S(_) | LinkMode::MaybeNone => {}
            LinkMode::V | LinkMode::None => {
                eprintln!("{:?}", self);
                unreachable!("write_back_slot() {slot:?} {:?}", self.mode(slot));
            }
        }
    }

    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::S without class guards.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    #[allow(non_snake_case)]
    pub(in crate::codegen::jitgen) fn to_S_unguarded(&mut self, ir: &mut AsmIr, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                ir.fpr2stack(xmm, slot);
            }
            LinkMode::C(v) => {
                ir.lit2stack(v.into(), slot);
            }
            LinkMode::G(_) => {
                ir.acc2stack(slot);
            }
            LinkMode::Sf(_, _) | LinkMode::S(_) => {}
            LinkMode::V => {
                ir.lit2stack(Value::nil(), slot);
            }
            LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("to_S_unguarded() {:?}", self.mode(slot));
            }
        }
        self.clear(slot);
        self.set_mode(slot, LinkMode::S(Guarded::Value));
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
            v.try_fixnum()
        } else {
            None
        }
    }

    pub fn is_flonum_literal(&self, slot: SlotId) -> Option<Flonum> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.try_flonum()
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
    pub(crate) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        if let Some(slot) = self.r15 {
            let guarded = self.guarded(slot);
            self.set_mode(slot, LinkMode::S(guarded));
            self.r15 = None;
            ir.acc2stack(slot);
        }
        assert!(!self.slots.iter().any(|link| matches!(link, LinkMode::G(_))));
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
        if self.class(slot) == Some(class) {
            return;
        }
        let class_guarded = Guarded::from_class(class);
        match &mut self.slots[slot.0 as usize] {
            LinkMode::S(guarded) | LinkMode::G(guarded) => {
                if class_guarded == *guarded {
                    return;
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
                        return;
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
                    return;
                }
                // in this case, Guard will always fail
            }
            LinkMode::C(v) => {
                if class == INTEGER_CLASS {
                    if v.is_fixnum() {
                        return;
                    }
                    // If v is Bignum, Guard will fail
                } else {
                    if v.class() == class {
                        return;
                    }
                    // in this case, Guard will always fail
                }
            }
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!(
                    "guard_class(): current:{:?} given:{:?}",
                    self.mode(slot),
                    class_guarded
                );
            }
        }
        ir.push(AsmInst::GuardClass(r, class, deopt));
    }

    pub(crate) fn guard_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        let deopt = ir.new_deopt(self);
        self.guard_class(ir, slot, r, INTEGER_CLASS, deopt);
    }

    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        let mut b = UsingXmm::new();
        // Only physical pool slots need save/restore at call
        // boundaries; spill slots already live on the stack.
        self.vfpr
            .iter()
            .enumerate()
            .take(PHYS_XMM_POOL)
            .for_each(|(i, v)| {
                if !v.is_empty() {
                    b.set(i, true);
                }
            });
        b
    }

    pub(super) fn get_gc_write_back(&self) -> WriteBack {
        let literal = self.wb_literal(|_| true);
        let void = self.wb_void();
        WriteBack::new(vec![], literal, self.r15, void)
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        let f = |_| true;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, r15, vec![])
    }

    fn xmm_swap(&mut self, l: FPReg, r: FPReg) {
        let mut guarded_l = None;
        let mut guarded_r = None;
        for link in self.slots.iter() {
            match link {
                LinkMode::F(x) => {
                    if *x == l {
                        if let Some(g) = guarded_l {
                            assert_eq!(g, SfGuarded::Float);
                        }
                        guarded_l = Some(SfGuarded::Float);
                    } else if *x == r {
                        if let Some(g) = guarded_r {
                            assert_eq!(g, SfGuarded::Float);
                        }
                        guarded_r = Some(SfGuarded::Float);
                    }
                }
                LinkMode::Sf(x, guarded) => {
                    if *x == l {
                        if let Some(g) = guarded_l {
                            assert_eq!(g, *guarded);
                        }
                        guarded_l = Some(*guarded);
                    } else if *x == r {
                        if let Some(g) = guarded_r {
                            assert_eq!(g, *guarded);
                        }
                        guarded_r = Some(*guarded);
                    }
                }
                _ => {}
            }
        }
        self.vfpr.swap(l.0 as usize, r.0 as usize);
        self.slots.iter_mut().for_each(|link| match link {
            LinkMode::Sf(x, guarded) => {
                if *x == l {
                    *x = r;
                    *guarded = guarded_r.unwrap();
                } else if *x == r {
                    *x = l;
                    *guarded = guarded_l.unwrap();
                }
            }
            LinkMode::F(x) => {
                if *x == l {
                    *x = r;
                    assert_eq!(guarded_r, Some(SfGuarded::Float));
                } else if *x == r {
                    *x = l;
                    assert_eq!(guarded_l, Some(SfGuarded::Float));
                }
            }
            LinkMode::S(_)
            | LinkMode::C(_)
            | LinkMode::G(_)
            | LinkMode::V
            | LinkMode::MaybeNone
            | LinkMode::None => {}
        });
    }

    fn wb_xmm(&self, f: impl Fn(SlotId) -> bool) -> Vec<(FPReg, Vec<SlotId>)> {
        self.vfpr
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.vfpr[i]
                        .iter()
                        .filter(|reg| f(**reg) && matches!(self.mode(**reg), LinkMode::F(_)))
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((FPReg::new(i as usize), v))
                    }
                }
            })
            .collect()
    }

    fn wb_literal(&self, f: impl Fn(SlotId) -> bool) -> Vec<(Immediate, SlotId)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(idx, link)| match link {
                LinkMode::C(v) if f(SlotId(idx as u16)) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect()
    }

    fn wb_void(&self) -> Vec<SlotId> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(idx, link)| match link {
                LinkMode::V => Some(SlotId(idx as u16)),
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
    /// Concrete value (immediate / packed).
    ///
    C(Immediate),
}

impl LinkMode {
    fn default() -> Self {
        LinkMode::S(Guarded::Value)
    }

    fn none() -> Self {
        LinkMode::None
    }

    fn nil() -> Self {
        LinkMode::C(Immediate::nil())
    }

    fn guarded(&self) -> Guarded {
        match self {
            LinkMode::S(guarded) | LinkMode::G(guarded) => *guarded,
            LinkMode::Sf(_, guarded) => (*guarded).into(),
            LinkMode::F(_) => Guarded::Float,
            LinkMode::C(v) => Guarded::from_concrete_value((*v).into()),
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

    #[allow(non_snake_case)]
    fn is_C(&self) -> bool {
        matches!(self, LinkMode::C(_))
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
                let (v, f) = if let Some(f) = l.try_flonum() {
                    (Value::float(f.get()), f.get())
                } else if let Some(i) = l.try_fixnum() {
                    (Value::integer(i.get()), i.get() as f64)
                } else {
                    unreachable!()
                };
                ir.f64_to_fpr(f, r);
                ir.lit2stack(v, slot);
            }
            (LinkMode::C(v), LinkMode::S(_)) => {
                // C -> S
                let guarded = Guarded::from_concrete_value(v.into());
                self.set_mode(slot, LinkMode::S(guarded));
                ir.lit2stack(v.into(), slot);
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
    fn to_sf(&mut self, ir: &mut AsmIr, slot: SlotId, l: FPReg, r: FPReg, guarded: SfGuarded) {
        if self.is_xmm_vacant(r) {
            self.set_Sf(slot, r, guarded);
            ir.fpr_move(l, r);
        } else {
            self.gen_xmm_swap(ir, l, r);
        }
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
}
