use super::*;

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
    pub(super) fn from(cc: &JitContext) -> Self {
        let mut ctx = Self::new(cc.total_reg_num(), cc.local_num());
        ctx.set_guard_class(SlotId::self_(), cc.self_class());
        ctx
    }

    pub(super) fn from_args(cc: &JitContext) -> Self {
        let mut ctx = Self::from(cc);
        if let JitType::Specialized {
            args_info: JitArgumentInfo(args),
            ..
        } = cc.jit_type()
        {
            for (i, arg) in args.iter().enumerate() {
                match arg.link {
                    LinkMode::C(_) => {
                        ctx.slots[i] = arg.clone();
                    }
                    _ => {}
                }
            }
        }
        ctx
    }

    pub(super) fn from_target(target: &SlotContext, use_set: &[(SlotId, bool)]) -> Self {
        let mut ctx = Self::new(target.slots.len(), target.local_num);
        for (i, state) in ctx.slots.iter_mut().enumerate() {
            state.guarded = target.slots[i].guarded;
        }
        for (slot, coerced) in use_set {
            match target.mode(*slot) {
                LinkMode::S => {}
                LinkMode::C(v) => {
                    if v.is_float() {
                        ctx.set_new_F(*slot);
                    }
                }
                LinkMode::F(r) if !coerced => {
                    ctx.set_F(*slot, r);
                }
                LinkMode::Sf(r) | LinkMode::F(r) => {
                    ctx.set_Sf(*slot, r, Guarded::Value);
                }
                LinkMode::G | LinkMode::V => unreachable!(),
            };
        }
        ctx
    }

    fn new(total_reg_num: usize, local_num: usize) -> Self {
        SlotContext {
            slots: vec![SlotState::default(); total_reg_num],
            liveness: vec![IsUsed::default(); total_reg_num],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num,
        }
    }

    fn locals(&self) -> std::ops::Range<SlotId> {
        SlotId(1)..self.temp_start()
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

    pub(super) fn guarded(&self, slot: SlotId) -> Guarded {
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
            LinkMode::C(_) => {}
            LinkMode::G => {
                assert_eq!(self.r15, Some(slot));
                self.r15 = None;
            }
            LinkMode::S => {}
            LinkMode::V => return,
        }
        self.set_mode(slot, LinkMode::V);
    }

    ///
    /// Discard slot *reg*.
    ///
    /// *reg* is set to S / Guarded::Value.
    ///
    pub(crate) fn discard(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            self.clear(slot);
            self.set_guarded(slot, Guarded::Value);
            self.is_used_mut(slot).kill();
            self.set_mode(slot, LinkMode::S);
        }
    }

    ///
    /// _ -> S
    ///
    #[allow(non_snake_case)]
    fn set_S(&mut self, slot: SlotId, guarded: Guarded) {
        self.clear(slot);
        self.set_mode(slot, LinkMode::S);
        self.set_guarded(slot, guarded);
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
    /// Link *slot* to the given xmm register *xmm*.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_F(&mut self, slot: SlotId, xmm: Xmm) {
        self.discard(slot);
        self.set_F(slot, xmm);
    }

    ///
    /// Link *slot* to a new xmm register.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_new_F(&mut self, slot: SlotId) -> Xmm {
        let xmm = self.alloc_xmm();
        self.def_F(slot, xmm);
        xmm
    }

    ///
    /// Link *slot* to both of the stack and the given xmm register *xmm*.
    ///
    #[allow(non_snake_case)]
    pub(super) fn def_Sf(&mut self, slot: SlotId, xmm: Xmm, guarded: Guarded) {
        self.discard(slot);
        self.set_Sf(slot, xmm, guarded);
    }

    ///
    /// Link *slot* to both of the stack and a new xmm register.
    ///
    #[allow(non_snake_case)]
    fn def_new_Sf(&mut self, slot: SlotId, guarded: Guarded) -> Xmm {
        let x = self.alloc_xmm();
        self.def_Sf(slot, x, guarded);
        x
    }

    #[allow(non_snake_case)]
    pub(super) fn def_new_Sf_float(&mut self, slot: SlotId) -> Xmm {
        self.def_new_Sf(slot, Guarded::Float)
    }

    ///
    /// Link *slot* to a concrete fixnum value *i*.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_C_fixnum(&mut self, slot: impl Into<Option<SlotId>>, i: i64) {
        if let Some(slot) = slot.into() {
            self.def_C(slot, Value::fixnum(i));
        }
    }

    ///
    /// Link *slot* to a concrete flonum value *i*.
    ///
    #[allow(non_snake_case)]
    pub(crate) fn def_C_float(&mut self, slot: impl Into<Option<SlotId>>, f: f64) {
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

    ///
    /// Clear slots that are not to be used.
    ///
    pub(super) fn remove_unused(&mut self, unused: &[SlotId]) {
        for r in unused {
            self.discard(*r);
        }
    }

    // APIs for 'use'

    fn use_as_float(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_float();
    }

    pub(super) fn use_as_value(&mut self, slot: SlotId) {
        self.is_used_mut(slot).use_as_non_float();
    }

    // APIs for 'guard'

    pub(super) fn set_guard_class(&mut self, slot: SlotId, class: ClassId) {
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

    pub(super) fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self.xmm(xmm).is_empty()
    }

    pub(super) fn is_r15(&self, slot: SlotId) -> bool {
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
    pub(super) fn write_back_acc(&mut self, ir: &mut AsmIr, sp: SlotId) {
        if let Some(slot) = self.r15 {
            self.set_mode(slot, LinkMode::S);
            self.r15 = None;
            if slot < sp {
                ir.acc2stack(slot);
            }
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

impl SlotContext {
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

    pub(crate) fn guard_fixnum(&mut self, ir: &mut AsmIr, slot: SlotId, r: GP, deopt: AsmDeopt) {
        self.guard_class(ir, slot, r, INTEGER_CLASS, deopt);
    }

    ///
    /// Type guard for stack slot.
    ///
    /// Generate slot type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// The content of the *slot* must be on stack.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(crate) fn guard_class_stack_slot(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        class: ClassId,
        deopt: AsmDeopt,
    ) {
        if self.is_class(slot, class) {
            return;
        }
        ir.stack2reg(slot, GP::Rax);
        self.set_guard_class(slot, class);
        ir.push(AsmInst::GuardClass(GP::Rax, class, deopt));
    }

    pub(crate) fn guard_lhs_class_for_mode(
        &mut self,
        ir: &mut AsmIr,
        mode: OpMode,
        lhs_class: ClassId,
        deopt: AsmDeopt,
    ) {
        match mode {
            OpMode::RR(lhs, _) | OpMode::RI(lhs, _) => {
                self.guard_class(ir, lhs, GP::Rdi, lhs_class, deopt);
            }
            OpMode::IR(_, _) => {
                if lhs_class != INTEGER_CLASS {
                    ir.push(AsmInst::Deopt(deopt));
                }
            }
        }
    }
}

impl SlotContext {
    pub(super) fn get_using_xmm(&self, sp: SlotId) -> UsingXmm {
        let mut b = UsingXmm::new();
        self.xmm.iter().enumerate().for_each(|(i, v)| {
            if v.iter().any(|slot| slot < &sp) {
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

    pub(super) fn get_write_back(&self, sp: SlotId) -> WriteBack {
        let f = |reg: SlotId| reg < sp;
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
                LinkMode::S | LinkMode::C(_) | LinkMode::G | LinkMode::V => {}
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

    pub(in crate::codegen::jitgen) fn merge(&mut self, bbctx: &BBContext) {
        for (i, is_used) in &mut self.0.iter_mut().enumerate() {
            is_used.merge(bbctx.is_used(SlotId(i as u16)));
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
    pub(super) fn is_concrete_value(&self) -> bool {
        matches!(self.link, LinkMode::C(_))
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
    fn merge(&mut self, other: &Self) {
        *self = match (&self, other) {
            (IsUsed::Used(l), IsUsed::Used(r)) => IsUsed::Used(l.merge(r)),
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

    fn merge(&self, other: &Self) -> Self {
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
    /// LinkMode of the *slot* is set to LinkMode::S or Sf or V.
    ///
    /// ### destroy
    /// - rax, rcx
    ///
    pub(super) fn write_back_slot(&mut self, ir: &mut AsmIr, slot: SlotId) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in write_back_slot()", slot, self.sp);
        };
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
            LinkMode::V => {
                // We must write nil to the stack for GC.
                ir.nil2stack(slot);
            }
            LinkMode::Sf(_) | LinkMode::S => {}
        }
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
    pub(super) fn to_S(&mut self, ir: &mut AsmIr, slot: SlotId) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in to_S()", slot, self.sp);
        };
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
            LinkMode::V => {
                ir.nil2stack(slot);
                return;
            }
            LinkMode::Sf(_) | LinkMode::S => {}
        }
        self.clear(slot);
        self.set_mode(slot, LinkMode::S);
    }

    pub(super) fn xmm_swap(&mut self, ir: &mut AsmIr, l: Xmm, r: Xmm) {
        self.slot_state.xmm_swap(l, r);
        ir.push(AsmInst::XmmSwap(l, r));
    }

    ///
    /// Discard slots above *next_sp*.
    ///
    pub(in crate::codegen::jitgen) fn clear_above_next_sp(&mut self) {
        let sp = self.next_sp;
        for i in sp..SlotId(self.slots.len() as u16) {
            self.discard(i)
        }
    }

    ///
    /// Discard slots above *sp*.
    ///
    pub(in crate::codegen::jitgen) fn clear_above_sp(&mut self) {
        let sp = self.sp;
        for i in sp..SlotId(self.slots.len() as u16) {
            self.discard(i)
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
                self.def_F(dst, x);
            }
            LinkMode::Sf(x) => {
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.def_Sf(dst, x, guarded);
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
                self.set_S(src, guarded);
                self.def_acc(ir, dst, guarded)
            }
            LinkMode::V => {
                unreachable!("write_back_slot() on V");
            }
        }
    }

    ///
    /// Link *slot* to the accumulator.
    ///
    pub(super) fn def_acc(
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

    ///
    /// Allocate new xmm register corresponding to the slot *reg* for read/write f64.
    ///
    pub(super) fn xmm_write(&mut self, slot: SlotId) -> Xmm {
        match self.mode(slot) {
            LinkMode::F(x) if self.xmm(x).len() == 1 => {
                assert_eq!(slot, self.xmm(x)[0]);
                x
            }
            LinkMode::F(_)
            | LinkMode::Sf(_)
            | LinkMode::S
            | LinkMode::C(_)
            | LinkMode::G
            | LinkMode::V => self.def_new_F(slot),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, slot: SlotId) -> u64 {
        self.xmm_write(slot).enc()
    }

    pub(super) fn gen_bridge(
        &mut self,
        ir: &mut AsmIr,
        target: &SlotContext,
        slot: SlotId,
        pc: BytecodePtr,
    ) {
        let guarded = target.guarded(slot);
        match (self.mode(slot), target.mode(slot)) {
            (LinkMode::F(l), LinkMode::F(r)) => {
                if l != r {
                    self.to_xmm(ir, slot, l, r);
                }
            }
            (LinkMode::F(l), LinkMode::Sf(r)) => {
                ir.xmm2stack(l, slot);
                if l == r {
                    // F(l) -> Sf(l)
                    self.set_Sf_float(slot, l);
                } else {
                    // F(l) -> Sf(r)
                    self.to_both(ir, slot, l, r, guarded);
                }
            }
            (LinkMode::Sf(l), LinkMode::Sf(r)) => {
                if l != r {
                    // Sf(l) -> Sf(r)
                    self.to_both(ir, slot, l, r, guarded);
                }
            }
            (LinkMode::Sf(l), LinkMode::S) => {
                self.xmm_remove(slot, l);
                self.set_mode(slot, LinkMode::S);
            }
            (LinkMode::S, LinkMode::S) => {
                if let Some(class) = guarded.class() {
                    let deopt = ir.new_deopt_with_pc(self, pc + 1);
                    self.guard_class_stack_slot(ir, slot, class, deopt);
                }
            }
            (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
            (LinkMode::C(l), LinkMode::F(r)) => {
                if let Some(f) = l.try_float() {
                    self.set_F(slot, r);
                    ir.f64_to_xmm(f, r);
                } else {
                    unreachable!()
                }
            }
            (LinkMode::C(l), LinkMode::Sf(r)) => {
                if let Some(f) = l.try_float() {
                    self.set_Sf_float(slot, r);
                    ir.f64_to_xmm(f, r);
                    ir.lit2reg(Value::float(f), GP::Rax);
                    ir.reg2stack(GP::Rax, slot);
                } else {
                    unreachable!()
                }
            }
            (_, LinkMode::S) => {
                self.write_back_slot(ir, slot);
            }
            (l, r) => unreachable!("src:{:?} target:{:?}", l, r),
        }
    }

    ///
    /// Generate bridge AsmIr from F/Sf(l) to F(r).
    ///
    fn to_xmm(&mut self, ir: &mut AsmIr, slot: SlotId, l: Xmm, r: Xmm) {
        if self.is_xmm_vacant(r) {
            self.set_F(slot, r);
            ir.xmm_move(l, r);
        } else {
            self.xmm_swap(ir, l, r);
        }
    }

    ///
    /// Generate bridge AsmIr from F/Sf(l) to Sf(r).
    ///
    fn to_both(&mut self, ir: &mut AsmIr, slot: SlotId, l: Xmm, r: Xmm, guarded: Guarded) {
        if self.is_xmm_vacant(r) {
            self.set_Sf(slot, r, guarded);
            ir.xmm_move(l, r);
        } else {
            self.xmm_swap(ir, l, r);
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

    pub(super) fn write_back_locals(&mut self, ir: &mut AsmIr) {
        for i in self.locals() {
            self.to_S(ir, i);
        }
    }

    pub(super) fn write_back_locals_if_captured(&mut self, ir: &mut AsmIr) {
        let wb = self.get_locals_write_back();
        ir.push(AsmInst::WriteBackIfCaptured(wb));
    }

    ///
    /// ~~~text
    ///                              other
    ///       
    ///                  F      Sf      f64     i63      C
    ///              +-------+-------+-------+-------+-------+
    ///         F    |   F   |  Sf   |   F   |   S   |   S   |
    ///              +-------+-------+-------+-------+-------+
    ///         Sf   |   Sf  |  Sf   |   Sf  |  Sf   |   S   |
    ///  self        +-------+-------+-------+-------+-------|
    ///         f64  |   F   |  Sf   |  F*1  |   S   |   S   |
    ///              +-------+-------+-------+-------+-------|
    ///         i63  |   S   |  Sf   |   S   |   S   |   S   |
    ///              +-------+-------+-------+-------+-------|
    ///         C    |   S   |   S   |   S   |   S   |  S*2  |
    ///              +-------+-------+-------+-------+-------+
    ///
    ///  *1: if self == other, f64.
    ///  *2: if self == other, Const.
    ///
    /// ~~~
    pub(in crate::codegen::jitgen) fn join(&mut self, other: &BBContext) {
        if !other.class_version_guarded {
            self.unset_class_version_guard();
        }
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            self.is_used_mut(i).merge(other.is_used(i));
            match (self.mode(i), other.mode(i)) {
                (LinkMode::V, LinkMode::V) => {}
                (_, LinkMode::V) | (LinkMode::V, _) => {
                    unreachable!("join() on (#{:?}, #{:?})", self.mode(i), other.mode(i));
                }
                (LinkMode::F(_), LinkMode::F(_)) => {}
                (LinkMode::F(_), LinkMode::C(r)) if r.is_float() => {}
                (LinkMode::F(l), LinkMode::Sf(_))
                | (LinkMode::Sf(l), LinkMode::Sf(_) | LinkMode::F(_)) => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_Sf(i, l, guarded);
                }
                (LinkMode::Sf(l), LinkMode::C(r)) if r.is_float() || r.is_fixnum() => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_Sf(i, l, guarded)
                }
                (LinkMode::C(l), LinkMode::F(_)) if l.is_float() => {
                    self.set_new_F(i);
                }
                (LinkMode::C(l), LinkMode::Sf(_)) if l.is_float() || l.is_fixnum() => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_new_Sf(i, guarded);
                }
                (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
                (LinkMode::C(l), LinkMode::C(r)) if l.is_float() && r.is_float() => {
                    self.set_new_F(i);
                }
                _ => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_S(i, guarded);
                }
            };
        }
    }
}
