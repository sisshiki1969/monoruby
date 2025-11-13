use super::*;

mod join;
mod read_slot;

#[derive(Clone)]
pub(crate) struct SlotContext {
    /// Slot states.
    slots: Vec<SlotState>,
    /// Liveness information.
    liveness: Vec<IsUsed>,
    /// Information for xmm registers (xmm2 - xmm15).
    xmm: [Vec<SlotId>; 14],
    r15: Option<SlotId>,
    local_num: usize,
}

impl std::fmt::Debug for SlotContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for (i, state) in self.slots.iter().enumerate() {
            write!(f, "[%{i}: {state:?}] ")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl SlotContext {
    fn new(cc: &JitContext) -> Self {
        let total_reg_num = cc.total_reg_num();
        let local_num = cc.local_num();
        let self_class = cc.self_class();
        let mut ctx = SlotContext {
            slots: vec![SlotState::default(); total_reg_num],
            liveness: vec![IsUsed::default(); total_reg_num],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num,
        };
        ctx.set_guard_class(SlotId::self_(), self_class);
        ctx
    }

    pub(super) fn new_loop(cc: &JitContext) -> Self {
        Self::new(cc)
    }

    pub(super) fn new_method(cc: &JitContext, store: &Store) -> Self {
        let mut ctx = Self::new(cc);

        if let JitType::Specialized {
            args_info: JitArgumentInfo(Some(args)),
            ..
        } = cc.jit_type()
        {
            for (i, arg) in args.iter().enumerate() {
                match arg.link {
                    LinkMode::C(_) | LinkMode::MaybeNone | LinkMode::None => {
                        ctx.slots[i] = arg.clone();
                    }
                    _ => {}
                }
            }
        } else {
            let fid = store[cc.iseq_id()].func_id();
            let info = &store[fid];
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
        ctx.clear_temps();
        ctx
    }

    pub(super) fn use_float(&mut self, use_set: &[(SlotId, bool)]) {
        for (slot, coerced) in use_set {
            match self.mode(*slot) {
                LinkMode::S => {}
                LinkMode::C(v) => {
                    if v.is_float() {
                        self.set_new_F(*slot);
                    }
                }
                LinkMode::F(r) if !coerced => {
                    self.set_F(*slot, r);
                }
                LinkMode::Sf(r) | LinkMode::F(r) => {
                    self.set_Sf(*slot, r, Guarded::Value);
                }
                LinkMode::G | LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                    unreachable!("use_float {:?}", self.mode(*slot));
                }
            };
        }
    }

    fn locals(&self) -> std::ops::Range<SlotId> {
        SlotId(1)..self.temp_start()
    }

    fn all_regs(&self) -> std::ops::Range<SlotId> {
        SlotId(0)..SlotId(self.slots.len() as u16)
    }

    fn temps(&self) -> std::ops::Range<SlotId> {
        self.temp_start()..SlotId(self.slots.len() as u16)
    }

    pub(super) fn temp_start(&self) -> SlotId {
        SlotId((1 + self.local_num) as u16)
    }

    ///
    /// Clear temporary slots.
    ///
    /// Temporary slots are set to V.
    ///
    pub(super) fn clear_temps(&mut self) {
        for i in self.temps() {
            self.clear(i);
        }
    }

    pub(super) fn mode(&self, slot: SlotId) -> LinkMode {
        self.slots[slot.0 as usize].link
    }

    fn guarded(&self, slot: SlotId) -> Guarded {
        match self.mode(slot) {
            LinkMode::MaybeNone | LinkMode::None | LinkMode::V => {
                panic!("guarded() {:?}", self);
            }
            _ => {}
        }
        self.slots[slot.0 as usize].guarded
    }

    pub(super) fn state(&self, slot: SlotId) -> &SlotState {
        &self.slots[slot.0 as usize]
    }
}

impl SlotContext {
    fn set_mode(&mut self, slot: SlotId, mode: LinkMode) {
        self.slots[slot.0 as usize].link = mode;
    }

    fn is_used(&self, slot: SlotId) -> &IsUsed {
        &self.liveness[slot.0 as usize]
    }

    fn is_used_mut(&mut self, slot: SlotId) -> &mut IsUsed {
        &mut self.liveness[slot.0 as usize]
    }

    fn set_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self.slots[slot.0 as usize].guarded = guarded;
    }

    fn xmm(&self, xmm: Xmm) -> &[SlotId] {
        &self.xmm[xmm.0 as usize]
    }

    fn xmm_add(&mut self, slot: SlotId, xmm: Xmm) {
        self.xmm[xmm.0 as usize].push(slot);
    }

    fn xmm_remove(&mut self, slot: SlotId, xmm: Xmm) {
        self.xmm[xmm.0 as usize].retain(|e| *e != slot);
    }

    ///
    /// Allocate a new xmm register.
    ///
    /// ### Panics
    /// If there is no vacant xmm register.
    ///
    fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        panic!("no xmm reg is vacant.")
    }

    ///
    /// Clear slot *reg* and set LinkMode to V.
    ///
    fn clear(&mut self, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::Sf(xmm) | LinkMode::F(xmm) => {
                assert!(self.xmm(xmm).contains(&slot));
                self.xmm_remove(slot, xmm);
            }
            LinkMode::G => {
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
            }
            LinkMode::C(_) => {}
            LinkMode::S => {}
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
    pub(super) fn discard(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            self.clear(slot);
            self.set_guarded(slot, Guarded::Value);
            self.is_used_mut(slot).kill();
        }
    }

    ///
    /// _ -> MaybeNone
    ///
    #[allow(non_snake_case)]
    fn set_MaybeNone(&mut self, slot: SlotId) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::MaybeNone);
        self.set_guarded(slot, Guarded::Value);
    }

    ///
    /// _ -> MaybeNone
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_None(&mut self, slot: SlotId) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::None);
        self.set_guarded(slot, Guarded::Value);
    }

    ///
    /// _ -> S
    ///
    #[allow(non_snake_case)]
    fn set_S_with_guard(&mut self, slot: SlotId, guarded: Guarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::S);
        self.set_guarded(slot, guarded);
    }

    ///
    /// _ -> S
    ///
    #[allow(non_snake_case)]
    pub(super) fn set_S(&mut self, slot: SlotId) {
        self.set_S_with_guard(slot, Guarded::Value);
    }

    ///
    /// F/Sf -> F
    ///
    #[allow(non_snake_case)]
    fn set_F(&mut self, slot: SlotId, xmm: Xmm) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::F(xmm));
        self.xmm_add(slot, xmm);
        self.set_guarded(slot, Guarded::Float);
    }

    ///
    /// F/Sf -> Sf
    ///
    #[allow(non_snake_case)]
    fn set_Sf(&mut self, slot: SlotId, xmm: Xmm, guarded: Guarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::Sf(xmm));
        self.xmm_add(slot, xmm);
        self.set_guarded(slot, guarded);
    }

    ///
    /// C -> F
    ///
    #[allow(non_snake_case)]
    fn set_new_F(&mut self, slot: SlotId) -> Xmm {
        let x = self.alloc_xmm();
        self.set_F(slot, x);
        x
    }

    ///
    /// C -> Sf
    ///
    #[allow(non_snake_case)]
    fn set_new_Sf(&mut self, slot: SlotId, guarded: Guarded) -> Xmm {
        let x = self.alloc_xmm();
        self.set_Sf(slot, x, guarded);
        x
    }

    ///
    /// F -> Sf
    ///
    #[allow(non_snake_case)]
    fn set_Sf_float(&mut self, slot: SlotId, xmm: Xmm) {
        self.set_Sf(slot, xmm, Guarded::Float)
    }
}

impl SlotContext {
    // APIs for 'def'

    ///
    /// Link *slot* to stack.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_S(&mut self, slot: SlotId) {
        self.discard(slot);
        self.set_mode(slot, LinkMode::S);
    }

    ///
    /// Link *slot* to stack with guard.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_S_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self.def_S(slot);
        self.set_guarded(slot, guarded);
    }

    ///
    /// Link *slot* to a new xmm register.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_F(&mut self, slot: SlotId) -> Xmm {
        let xmm = self.alloc_xmm();
        self.discard(slot);
        self.set_F(slot, xmm);
        xmm
    }

    ///
    /// Link *slot* to both of the stack and a new xmm register.
    ///
    #[allow(non_snake_case)]
    fn def_Sf(&mut self, slot: SlotId, guarded: Guarded) -> Xmm {
        let xmm = self.alloc_xmm();
        self.discard(slot);
        self.set_Sf(slot, xmm, guarded);
        xmm
    }

    #[allow(non_snake_case)]
    pub(super) fn def_Sf_float(&mut self, slot: SlotId) -> Xmm {
        self.def_Sf(slot, Guarded::Float)
    }

    ///
    /// Link *slot* to a concrete fixnum value *i*.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_C_fixnum(&mut self, slot: impl Into<Option<SlotId>>, i: i64) {
        self.def_C(slot, Value::fixnum(i));
    }

    ///
    /// Link *slot* to a concrete flonum value *i*.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_C_float(&mut self, slot: impl Into<Option<SlotId>>, f: f64) {
        self.def_C(slot, Value::float(f));
    }

    ///
    /// Link *slot* to a concrete value *v*.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_C(&mut self, slot: impl Into<Option<SlotId>>, v: Value) {
        if let Some(slot) = slot.into() {
            let guarded = Guarded::from_concrete_value(v);
            self.discard(slot);
            self.set_mode(slot, LinkMode::C(v));
            self.set_guarded(slot, guarded);
        }
    }

    // APIs for 'use'

    fn use_as_float(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_float();
    }

    fn use_as_value(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_non_float();
    }

    // APIs for 'guard'

    fn set_guard_class(&mut self, slot: SlotId, class: ClassId) {
        if class == INTEGER_CLASS {
            self.set_guarded(slot, Guarded::Fixnum)
        } else if class == FLOAT_CLASS {
            self.set_guarded(slot, Guarded::Float)
        } else {
            self.set_guarded(slot, Guarded::Class(class))
        }
    }
}

impl SlotContext {
    pub fn is_symbol_literal(&self, slot: SlotId) -> Option<IdentId> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.try_symbol()
        } else {
            None
        }
    }

    pub fn is_fixnum_literal(&self, slot: SlotId) -> Option<i64> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.try_fixnum()
        } else {
            None
        }
    }

    pub fn is_float_literal(&self, slot: SlotId) -> Option<f64> {
        if let LinkMode::C(v) = self.mode(slot) {
            v.try_float()
        } else {
            None
        }
    }

    pub fn is_u16_literal(&self, slot: SlotId) -> Option<u16> {
        if let LinkMode::C(v) = self.mode(slot) {
            let i = v.try_fixnum()?;
            u16::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
        if let LinkMode::C(v) = self.mode(slot) {
            let i = v.try_fixnum()?;
            u8::try_from(i).ok()
        } else {
            None
        }
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
            LinkMode::Sf(_) => true,
            LinkMode::C(v) => v.as_bool(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::Value => false,
                Guarded::Class(class) => !class.is_falsy(),
            },
        }
    }

    pub fn is_falsy(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => false,
            LinkMode::Sf(_) => false,
            LinkMode::C(v) => !v.as_bool(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::Value => false,
                Guarded::Class(class) => class.is_falsy(),
            },
        }
    }

    pub fn is_nil(&self, slot: SlotId) -> bool {
        match self.mode(slot) {
            LinkMode::F(_) => false,
            LinkMode::Sf(_) => false,
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
            LinkMode::Sf(_) => true,
            LinkMode::C(v) => !v.is_nil(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::Value => false,
                Guarded::Class(class) => !class.is_nil(),
            },
        }
    }

    fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self.xmm(xmm).is_empty()
    }

    fn is_r15(&self, slot: SlotId) -> bool {
        self.r15 == Some(slot)
    }

    pub(super) fn on_reg(&self, slot: SlotId) -> Option<GP> {
        if self.is_r15(slot) {
            Some(GP::R15)
        } else {
            None
        }
    }

    pub(super) fn on_reg_or(&self, slot: SlotId, optb: GP) -> GP {
        if self.is_r15(slot) { GP::R15 } else { optb }
    }

    ///
    /// Write back acc(`r15``) to the stack slot.
    ///
    /// The slot is set to LinkMode::Stack.
    ///
    pub(crate) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        if let Some(slot) = self.r15 {
            self.set_mode(slot, LinkMode::S);
            self.r15 = None;
            ir.acc2stack(slot);
        }
        assert!(
            !self
                .slots
                .iter()
                .any(|SlotState { link, .. }| matches!(link, LinkMode::G))
        );
        assert!(self.r15.is_none());
    }
}

impl BBContext {
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
        if self.is_class(slot, class) {
            return;
        }
        self.set_guard_class(slot, class);
        ir.push(AsmInst::GuardClass(r, class, deopt));
    }

    pub(crate) fn guard_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP) {
        let deopt = ir.new_deopt(self);
        self.guard_class(ir, slot, r, INTEGER_CLASS, deopt);
    }
}

impl SlotContext {
    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        let mut b = UsingXmm::new();
        self.xmm.iter().enumerate().for_each(|(i, v)| {
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

    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        let mut guarded_l = None;
        let mut guarded_r = None;
        for SlotState { link, guarded, .. } in self.slots.iter() {
            match link {
                LinkMode::F(x) | LinkMode::Sf(x) => {
                    if *x == l {
                        if let Some(g) = guarded_l {
                            assert_eq!(&g, guarded);
                        }
                        guarded_l = Some(*guarded);
                    } else if *x == r {
                        if let Some(g) = guarded_r {
                            assert_eq!(&g, guarded);
                        }
                        guarded_r = Some(*guarded);
                    }
                }
                _ => {}
            }
        }
        self.xmm.swap(l.0 as usize, r.0 as usize);
        self.slots
            .iter_mut()
            .for_each(|SlotState { link, guarded, .. }| match link {
                LinkMode::Sf(x) | LinkMode::F(x) => {
                    if *x == l {
                        *x = r;
                        *guarded = guarded_r.unwrap();
                    } else if *x == r {
                        *x = l;
                        *guarded = guarded_l.unwrap();
                    }
                }
                LinkMode::S
                | LinkMode::C(_)
                | LinkMode::G
                | LinkMode::V
                | LinkMode::MaybeNone
                | LinkMode::None => {}
            });
    }

    fn wb_xmm(&self, f: impl Fn(SlotId) -> bool) -> Vec<(Xmm, Vec<SlotId>)> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| f(**reg) && matches!(self.mode(**reg), LinkMode::F(_)))
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((Xmm::new(i as u16), v))
                    }
                }
            })
            .collect()
    }

    fn wb_literal(&self, f: impl Fn(SlotId) -> bool) -> Vec<(Value, SlotId)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(idx, SlotState { link, .. })| match link {
                LinkMode::C(v) if f(SlotId(idx as u16)) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect()
    }

    fn wb_void(&self) -> Vec<SlotId> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(idx, SlotState { link, .. })| match link {
                LinkMode::V => Some(SlotId(idx as u16)),
                _ => None,
            })
            .collect()
    }

    fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let f = |reg: SlotId| reg.0 as usize <= local_num;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let void = self.wb_void();
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, r15, void)
    }

    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::S or V.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    #[allow(non_snake_case)]
    fn to_S(&mut self, ir: &mut AsmIr, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                ir.xmm2stack(xmm, slot);
            }
            LinkMode::C(v) => {
                ir.push(AsmInst::LitToStack(v, slot));
            }
            LinkMode::G => {
                ir.acc2stack(slot);
            }
            LinkMode::Sf(_) | LinkMode::S => {}
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("to_S() {:?}", self.mode(slot));
            }
        }
        self.clear(slot);
        self.set_mode(slot, LinkMode::S);
    }
}

#[derive(Clone, Default)]
pub(crate) struct Liveness(Vec<IsUsed>);

impl std::fmt::Debug for Liveness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Liveness {{ ")?;
        for (i, is_used) in self.0.iter().enumerate() {
            match is_used {
                IsUsed::Used(UsedAs { ty, .. }) => {
                    write!(f, "[{:?}: UsedAs {:?}] ", SlotId(i as u16), ty)?
                }
                IsUsed::Killed => write!(f, "[{:?}: Killed] ", SlotId(i as u16))?,
                IsUsed::ND => {}
            }
        }
        write!(f, "}}")
    }
}

impl Liveness {
    pub(in crate::codegen::jitgen) fn new(total_reg_num: usize) -> Self {
        Self(vec![IsUsed::default(); total_reg_num])
    }

    pub(in crate::codegen::jitgen) fn join(&mut self, bbctx: &BBContext) {
        for (i, is_used) in &mut self.0.iter_mut().enumerate() {
            is_used.join(bbctx.is_used(SlotId(i as u16)));
        }
    }

    pub fn get_unused(&self) -> Vec<SlotId> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, is_used)| {
                if is_used == &IsUsed::Killed {
                    Some(SlotId(i as u16))
                } else {
                    None
                }
            })
            .collect()
    }

    ///
    /// Extract a set of registers which will be used as Float in this loop,
    /// *and* xmm-linked on the back-edge.
    ///
    pub fn get_loop_used_as_float(&self) -> Vec<(SlotId, bool)> {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(i, b)| match b {
                IsUsed::Used(used) => match used.ty {
                    UseTy::Float => Some((SlotId(i as u16), true)),
                    UseTy::Both => Some((SlotId(i as u16), false)),
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }
}

#[derive(Clone, Default)]
pub(crate) struct SlotState {
    link: LinkMode,
    guarded: Guarded,
}

impl std::fmt::Debug for SlotState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}{}",
            self.link,
            match self.guarded {
                Guarded::Value => "".to_string(),
                Guarded::Fixnum => " <Fixnum>".to_string(),
                Guarded::Float => " <Float>".to_string(),
                Guarded::Class(class) => format!(" <{:?}>", class),
            },
        )
    }
}

impl SlotState {
    fn none() -> Self {
        SlotState {
            link: LinkMode::None,
            guarded: Guarded::Value,
        }
    }

    fn nil() -> Self {
        SlotState {
            link: LinkMode::C(Value::nil()),
            guarded: Guarded::Class(NIL_CLASS),
        }
    }

    #[allow(non_snake_case)]
    pub(super) fn is_C(&self) -> bool {
        matches!(self.link, LinkMode::C(_))
    }

    pub(super) fn from_caller(
        store: &Store,
        fid: FuncId,
        callid: CallSiteId,
        bbctx: &BBContext,
    ) -> Vec<Self> {
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            kw_pos,
            kw_args,
            ..
        } = &store[callid];
        let info = &store[fid];
        let mut slots = vec![];
        slots.push(bbctx.state(*recv).clone());
        let (filled_req, filled_opt, filled_post) = info.apply_args(*pos_num);
        for i in 0..filled_req {
            slots.push(bbctx.state(*args + i).clone());
        }
        for _ in filled_req..info.req_num() {
            slots.push(SlotState::nil());
        }
        for i in filled_req..filled_req + filled_opt {
            slots.push(bbctx.state(*args + i).clone());
        }
        for _ in filled_opt..info.opt_num() {
            slots.push(SlotState::none());
        }
        for i in filled_req + filled_opt..filled_req + filled_opt + filled_post {
            slots.push(bbctx.state(*args + i).clone());
        }
        for _ in filled_post..info.post_num() {
            slots.push(SlotState::nil());
        }
        if info.is_rest() {
            slots.push(SlotState::default());
        }
        let kw = info.kw_reg_pos();
        assert_eq!(kw.0 as usize, slots.len());
        for k in info.kw_names() {
            if let Some(p) = kw_args.get(k) {
                slots.push(bbctx.state(*kw_pos + *p).clone());
            } else {
                slots.push(SlotState::none());
            }
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
        } else {
            Guarded::Class(v.class())
        }
    }

    fn join(&self, other: &Self) -> Self {
        if self == other { *self } else { Guarded::Value }
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

#[derive(Debug, Clone, Copy, PartialEq, Default)]
enum IsUsed {
    ///
    /// Not be used nor be killed.
    ///
    #[default]
    ND,
    ///
    /// Used in any of paths.
    ///
    Used(UsedAs),
    ///
    /// Guaranteed not to be used (= killed) in all paths.
    ///
    Killed,
}

impl IsUsed {
    fn join(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used(l), IsUsed::Used(r)) => IsUsed::Used(l.join(r)),
            (IsUsed::Used(x), _) | (_, IsUsed::Used(x)) => IsUsed::Used(*x),
            (IsUsed::Killed, IsUsed::Killed) => IsUsed::Killed,
            _ => IsUsed::ND,
        };
    }

    fn use_as_float(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.use_as_float(),
            IsUsed::ND => *self = IsUsed::Used(UsedAs::float()),
        }
    }

    fn use_as_non_float(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.use_as_non_float(),
            IsUsed::ND => *self = IsUsed::Used(UsedAs::non_float()),
        }
    }

    fn kill(&mut self) {
        match self {
            IsUsed::Killed => {}
            IsUsed::Used(used) => used.kill(),
            IsUsed::ND => *self = IsUsed::Killed,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct UsedAs {
    ty: UseTy,
    killed: bool,
}

impl UsedAs {
    fn float() -> Self {
        UsedAs {
            ty: UseTy::Float,
            killed: false,
        }
    }

    fn non_float() -> Self {
        UsedAs {
            ty: UseTy::NonFloat,
            killed: false,
        }
    }

    fn use_as_float(&mut self) {
        self.use_as(UseTy::Float);
    }

    fn use_as_non_float(&mut self) {
        self.use_as(UseTy::NonFloat);
    }

    fn use_as(&mut self, other: UseTy) {
        if self.killed {
            return;
        } else {
            self.ty = self.ty.join(&other);
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            ty: self.ty.join(&other.ty),
            killed: self.killed && other.killed,
        }
    }

    fn kill(&mut self) {
        self.killed = true;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseTy {
    Float,
    NonFloat,
    Both,
}

impl UseTy {
    fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (UseTy::Float, UseTy::Float) => UseTy::Float,
            (UseTy::NonFloat, UseTy::NonFloat) => UseTy::NonFloat,
            (_, _) => UseTy::Both,
        }
    }
}

impl BBContext {
    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::S or Sf.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(super) fn write_back_slot(&mut self, ir: &mut AsmIr, slot: SlotId) {
        match self.mode(slot) {
            LinkMode::F(xmm) => {
                // F -> Sf
                self.set_Sf_float(slot, xmm);
                ir.xmm2stack(xmm, slot);
            }
            LinkMode::C(v) => {
                // C -> S
                self.set_mode(slot, LinkMode::S);
                self.set_guarded(slot, Guarded::from_concrete_value(v));
                ir.push(AsmInst::LitToStack(v, slot));
            }
            LinkMode::G => {
                // G -> S
                ir.acc2stack(slot);
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
                self.set_mode(slot, LinkMode::S);
            }
            LinkMode::Sf(_) | LinkMode::S | LinkMode::MaybeNone => {}
            LinkMode::V | LinkMode::None => {
                eprintln!("{:?}", self.slot_state);
                unreachable!("write_back_slot() {slot:?} {:?}", self.mode(slot));
            }
        }
    }

    fn xmm_swap(&mut self, ir: &mut AsmIr, l: Xmm, r: Xmm) {
        self.slot_state.xmm_swap(l, r);
        ir.push(AsmInst::XmmSwap(l, r));
    }

    ///
    /// Discard slots above *next_sp*.
    ///
    pub(in crate::codegen::jitgen) fn clear_above_next_sp(&mut self) {
        let sp = self.next_sp;
        for i in sp..SlotId(self.slots.len() as u16) {
            self.discard(i);
        }
    }

    ///
    /// Copy *src* to *dst*.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn copy_slot(&mut self, ir: &mut AsmIr, src: SlotId, dst: SlotId) {
        if src == dst {
            return;
        }
        let guarded = self.guarded(src);
        match self.mode(src) {
            LinkMode::F(x) => {
                self.discard(dst);
                self.set_F(dst, x);
            }
            LinkMode::Sf(x) => {
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.discard(dst);
                self.set_Sf(dst, x, guarded);
            }
            LinkMode::S => {
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.def_S_guarded(dst, guarded);
            }
            LinkMode::C(v) => {
                self.def_C(dst, v);
            }
            LinkMode::G => {
                ir.reg2stack(GP::R15, src);
                self.set_S_with_guard(src, guarded);
                self.def_G(ir, dst, guarded)
            }
            LinkMode::V | LinkMode::MaybeNone | LinkMode::None => {
                unreachable!("write_back_slot() {:?}", self.mode(src));
            }
        }
    }

    ///
    /// Link *slot* to the accumulator.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_G(
        &mut self,
        ir: &mut AsmIr,
        slot: impl Into<Option<SlotId>>,
        guarded: Guarded,
    ) {
        if let Some(slot) = slot.into() {
            self.discard(slot);
            self.writeback_acc(ir);
            self.set_mode(slot, LinkMode::G);
            self.set_guarded(slot, guarded);
            self.r15 = Some(slot);
        }
    }
}

// write back operations
impl BBContext {
    pub(crate) fn write_back_slots(&mut self, ir: &mut AsmIr, slot: &[SlotId]) {
        slot.iter().for_each(|r| self.write_back_slot(ir, *r));
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(super) fn write_back_range(&mut self, ir: &mut AsmIr, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.write_back_slot(ir, SlotId::new(reg))
        }
    }

    pub(crate) fn write_back_callargs_and_dst(&mut self, ir: &mut AsmIr, callsite: &CallSiteInfo) {
        let CallSiteInfo { recv, dst, .. } = callsite;
        self.write_back_slot(ir, *recv);
        self.write_back_args(ir, callsite);
        self.discard(*dst);
    }

    pub(super) fn write_back_args(&mut self, ir: &mut AsmIr, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            args,
            pos_num,
            kw_pos,
            block_arg,
            ..
        } = callsite;
        self.write_back_range(ir, *args, *pos_num as u16);
        self.write_back_range(ir, *kw_pos, callsite.kw_len() as u16);
        if let Some(block_arg) = block_arg {
            self.write_back_slot(ir, *block_arg);
        }
    }

    #[allow(non_snake_case)]
    pub(super) fn locals_to_S(&mut self, ir: &mut AsmIr) {
        for i in self.locals() {
            self.to_S(ir, i);
        }
    }

    pub(super) fn write_back_locals_if_captured(&mut self, ir: &mut AsmIr) {
        if !self.frame_capture_guarded {
            let wb = self.get_locals_write_back();
            ir.push(AsmInst::WriteBackIfCaptured(wb));
        }
    }
}
