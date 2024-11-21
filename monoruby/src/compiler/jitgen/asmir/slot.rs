use super::*;

#[derive(Clone)]
pub(crate) struct SlotContext {
    slots: Vec<SlotState>,
    /// Information for xmm registers (xmm2 - xmm15).
    xmm: [Vec<SlotId>; 14],
    r15: Option<SlotId>,
    local_num: usize,
}

impl std::ops::Index<SlotId> for SlotContext {
    type Output = SlotState;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.slots[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for SlotContext {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.slots[i.0 as usize]
    }
}

impl std::fmt::Debug for SlotContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .slots
            .iter()
            .enumerate()
            .map(|(i, state)| format!("[%{i}: {:?}] ", state))
            .collect();
        write!(f, "[{s}]")
    }
}

impl SlotContext {
    pub(in crate::compiler::jitgen) fn new(total_reg_num: usize, local_num: usize) -> Self {
        SlotContext {
            slots: vec![SlotState::default(); total_reg_num],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num,
        }
    }

    pub(in crate::compiler::jitgen) fn from(cc: &JitContext) -> Self {
        Self::new(cc.total_reg_num, cc.local_num)
    }

    fn xmm(&self, xmm: Xmm) -> &[SlotId] {
        &self.xmm[xmm.0 as usize]
    }

    fn xmm_mut(&mut self, xmm: Xmm) -> &mut Vec<SlotId> {
        &mut self.xmm[xmm.0 as usize]
    }

    fn alias_mut(&mut self, slot: SlotId) -> &mut Vec<SlotId> {
        &mut self[slot].alias
    }

    pub(super) fn set_slot(&mut self, slot: SlotId, mode: LinkMode, guarded: Guarded) {
        self[slot].link = mode;
        self[slot].guarded = guarded;
        self[slot].is_used.kill();
    }

    fn set_mode(&mut self, slot: SlotId, mode: LinkMode) {
        self[slot].link = mode;
    }

    pub(super) fn slot(&self, slot: SlotId) -> LinkMode {
        self[slot].link
    }

    pub(super) fn guarded(&self, slot: SlotId) -> Guarded {
        self[slot].guarded
    }

    fn set_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self[slot].guarded = guarded;
        self[slot].alias.clone().into_iter().for_each(|slot| {
            self[slot].guarded = guarded;
        });
        if let LinkMode::Alias(origin) = self[slot].link {
            self[origin].guarded = guarded;
        };
    }

    pub(super) fn set_guard_fixnum(&mut self, slot: SlotId) {
        self.set_guarded(slot, Guarded::Fixnum)
    }

    pub(super) fn set_guard_float(&mut self, slot: SlotId) {
        self.set_guarded(slot, Guarded::Float)
    }

    pub(super) fn set_guard_array_ty(&mut self, slot: SlotId) {
        self.set_guarded(slot, Guarded::ArrayTy)
    }

    pub(super) fn set_guard_class(&mut self, slot: SlotId, class: ClassId) {
        self.set_guarded(slot, Guarded::Class(class))
    }

    pub(super) fn set_xmm(&mut self, slot: SlotId, xmm: Xmm) {
        self.set_slot(slot, LinkMode::Xmm(xmm), Guarded::Float)
    }

    pub(super) fn set_both_float(&mut self, slot: SlotId, xmm: Xmm) {
        self.set_both(slot, xmm, Guarded::Float)
    }

    fn set_both(&mut self, slot: SlotId, xmm: Xmm, guarded: Guarded) {
        self.set_slot(slot, LinkMode::Both(xmm), guarded)
    }

    pub fn is_u16_literal(&self, slot: SlotId) -> Option<u16> {
        if let LinkMode::ConcreteValue(v) = self[slot].link {
            let i = v.try_fixnum()?;
            u16::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
        if let LinkMode::ConcreteValue(v) = self[slot].link {
            let i = v.try_fixnum()?;
            u8::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_array_ty(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::ArrayTy;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_array_ty(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_array_ty(origin), b),
            _ => {}
        };
        b
    }

    pub fn is_fixnum(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Fixnum;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_fixnum(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_fixnum(origin), b),
            _ => {}
        };
        b
    }

    pub fn is_float(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Float;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_float(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_float(origin), b),
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
                match self[slot].link {
                    LinkMode::Xmm(_) => assert!(!b),
                    LinkMode::ConcreteValue(v) => assert_eq!(v.class() == class, b),
                    LinkMode::Alias(origin) => assert_eq!(self.is_class(origin, class), b),
                    _ => {}
                };
                b
            }
        }
    }

    pub fn is_truthy(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => true,
            LinkMode::Both(_) => true,
            LinkMode::ConcreteValue(v) => v.as_bool(),
            LinkMode::Alias(origin) => self.is_truthy(origin),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::ArrayTy => true,
                Guarded::Value => false,
                Guarded::Class(_) => false,
            },
        }
    }

    pub fn is_falsy(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => false,
            LinkMode::Both(_) => false,
            LinkMode::ConcreteValue(v) => !v.as_bool(),
            LinkMode::Alias(origin) => self.is_falsy(origin),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::ArrayTy => false,
                Guarded::Value => false,
                Guarded::Class(_) => false,
            },
        }
    }

    pub fn is_nil(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => false,
            LinkMode::Both(_) => false,
            LinkMode::ConcreteValue(v) => v.is_nil(),
            LinkMode::Alias(origin) => self.is_nil(origin),
            _ => false,
        }
    }

    pub fn is_not_nil(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => true,
            LinkMode::Both(_) => true,
            LinkMode::ConcreteValue(v) => !v.is_nil(),
            LinkMode::Alias(origin) => self.is_not_nil(origin),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::ArrayTy => true,
                Guarded::Value => false,
                Guarded::Class(_) => true,
            },
        }
    }

    pub(super) fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self.xmm(xmm).is_empty()
    }

    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        let mut guarded_l = None;
        let mut guarded_r = None;
        for SlotState { link, guarded, .. } in self.slots.iter() {
            match link {
                LinkMode::Xmm(x) | LinkMode::Both(x) => {
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
                LinkMode::Both(x) | LinkMode::Xmm(x) => {
                    if *x == l {
                        *x = r;
                        *guarded = guarded_r.unwrap();
                    } else if *x == r {
                        *x = l;
                        *guarded = guarded_l.unwrap();
                    }
                }
                LinkMode::Stack
                | LinkMode::ConcreteValue(_)
                | LinkMode::Accumulator
                | LinkMode::Alias(_) => {}
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
                        .filter(|reg| f(**reg) && matches!(self[**reg].link, LinkMode::Xmm(_)))
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
                LinkMode::ConcreteValue(v) if f(SlotId(idx as u16)) => {
                    Some((*v, SlotId(idx as u16)))
                }
                _ => None,
            })
            .collect()
    }

    fn wb_alias(&self, f: impl Fn(SlotId) -> bool) -> Vec<(SlotId, Vec<SlotId>)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(idx, SlotState { alias, .. })| {
                let v: Vec<_> = alias.iter().filter(|slot| f(**slot)).cloned().collect();
                if v.is_empty() {
                    None
                } else {
                    Some((SlotId(idx as u16), v))
                }
            })
            .collect()
    }

    pub(in crate::compiler::jitgen) fn get_register(&self) -> WriteBack {
        WriteBack::new(vec![], vec![], vec![], self.r15)
    }

    pub(in crate::compiler::jitgen) fn get_write_back(&self, sp: SlotId) -> WriteBack {
        let f = |reg: SlotId| reg < sp;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let alias = self.wb_alias(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, alias, r15)
    }

    pub(super) fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let f = |reg: SlotId| reg.0 as usize <= local_num;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let alias = self.wb_alias(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, alias, r15)
    }

    pub(in crate::compiler::jitgen) fn get_using_xmm(&self, sp: SlotId) -> UsingXmm {
        let mut b = UsingXmm::new();
        self.xmm.iter().enumerate().for_each(|(i, v)| {
            if v.iter().any(|slot| slot < &sp) {
                b.set(i, true);
            }
        });
        b
    }

    ///
    /// Allocate a new xmm register.
    ///
    /// ### Panics
    /// If there is no vacant xmm register.
    ///
    pub(super) fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        panic!("no xmm reg is vacant.")
    }
}

#[derive(Clone)]
pub(crate) struct Liveness(Vec<IsUsed>);

impl Liveness {
    pub(in crate::compiler::jitgen) fn new(total_reg_num: usize) -> Self {
        Self(vec![IsUsed::default(); total_reg_num])
    }

    pub(in crate::compiler::jitgen) fn merge(&mut self, bbctx: BBContext) {
        for (i, is_used) in &mut self.0.iter_mut().enumerate() {
            is_used.merge(&bbctx[SlotId(i as u16)].is_used);
        }
    }

    fn get_unused(&self) -> Vec<SlotId> {
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
}

#[derive(Clone, Default, Debug)]
pub(crate) struct SlotState {
    link: LinkMode,
    guarded: Guarded,
    alias: Vec<SlotId>,
    is_used: IsUsed,
}

impl SlotState {
    pub(super) fn use_as_float(&mut self) {
        self.is_used.use_as_float();
    }

    pub(super) fn use_as_value(&mut self) {
        self.is_used.use_as_non_float();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Guarded {
    #[default]
    Value,
    Fixnum,
    Float,
    ArrayTy,
    Class(ClassId),
}

impl Guarded {
    pub fn from_concrete_value(v: Value) -> Self {
        if v.is_fixnum() {
            Guarded::Fixnum
        } else if v.is_float() {
            Guarded::Float
        } else if v.is_array_ty() {
            Guarded::ArrayTy
        } else {
            Guarded::Class(v.class())
        }
    }

    pub(super) fn union(&self, other: &Self) -> Self {
        if self == other {
            *self
        } else {
            Guarded::Value
        }
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
            self.ty = self.ty.union(&other);
        }
    }

    fn merge(&self, other: &Self) -> Self {
        Self {
            ty: self.ty.union(&other.ty),
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
    fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (UseTy::Float, UseTy::Float) => UseTy::Float,
            (UseTy::NonFloat, UseTy::NonFloat) => UseTy::NonFloat,
            (_, _) => UseTy::Both,
        }
    }
}

impl AsmIr {
    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::Stack or Both.
    ///
    /// ### destroy
    /// - rax, rcx
    pub(super) fn write_back_slot(&mut self, bbctx: &mut BBContext, slot: SlotId) {
        if slot >= bbctx.sp {
            unreachable!("{:?} >= {:?} in write_back_slot()", slot, bbctx.sp);
        };
        let guarded = bbctx.guarded(slot);
        match bbctx.slot(slot) {
            LinkMode::Xmm(xmm) => {
                // Xmm -> Both
                bbctx.set_both_float(slot, xmm);
                self.xmm2stack(xmm, slot);
            }
            LinkMode::ConcreteValue(v) => {
                // Literal -> Stack
                self.lit2stack(bbctx, v, slot);
            }
            LinkMode::Accumulator => {
                // R15 -> Stack
                self.acc2stack(slot);
                self.unlink(bbctx, slot);
                bbctx.set_guarded(slot, guarded);
            }
            LinkMode::Alias(origin) => {
                self.stack2reg(origin, GP::Rax);
                self.reg2stack(GP::Rax, slot);
                self.unlink(bbctx, slot);
                bbctx.set_guarded(slot, guarded);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    ///
    /// Discard slot *reg*.
    ///
    /// *reg* is set to LinkMode::Stack / Guarded::Value.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(crate) fn unlink(&mut self, bbctx: &mut BBContext, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            match bbctx[slot].link {
                LinkMode::Both(xmm) | LinkMode::Xmm(xmm) => {
                    assert!(bbctx.xmm(xmm).contains(&slot));
                    bbctx.xmm_mut(xmm).retain(|e| *e != slot);
                }
                LinkMode::Alias(origin) => {
                    assert_eq!(bbctx[origin].link, LinkMode::Stack);
                    assert!(bbctx[origin].alias.contains(&slot));
                    bbctx[origin].alias.retain(|e| *e != slot);
                }
                LinkMode::ConcreteValue(_) => {}
                LinkMode::Accumulator => {
                    bbctx.r15 = None;
                }
                LinkMode::Stack => {
                    // We must write back all aliases of *reg*.
                    let dst = std::mem::take(&mut bbctx[slot].alias);
                    if !dst.is_empty() {
                        self.stack2reg(slot, GP::R8);
                        for dst in dst {
                            self.reg2stack(GP::R8, dst);
                            assert_eq!(bbctx[dst].link, LinkMode::Alias(slot));
                            bbctx.set_mode(dst, LinkMode::Stack);
                        }
                    }
                }
            }
            bbctx.set_slot(slot, LinkMode::Stack, Guarded::Value)
        }
    }

    ///
    /// Link the slot *reg* to the alias of *origin*.
    ///
    /// ### destroy
    /// - r8
    ///
    fn store_alias(&mut self, bb: &mut BBContext, origin: SlotId, slot: SlotId) {
        self.unlink(bb, slot);
        if bb[origin].link != LinkMode::Stack {
            unreachable!("origin:{:?} reg:{:?} {:?}", origin, slot, bb);
        };
        let guarded = bb.guarded(origin);
        bb.set_slot(slot, LinkMode::Alias(origin), guarded);
        bb.alias_mut(origin).push(slot);
    }

    ///
    /// Link the slot *reg* to the given xmm register *xmm*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn store_xmm(&mut self, bb: &mut BBContext, slot: SlotId, xmm: Xmm) {
        self.unlink(bb, slot);
        bb.set_xmm(slot, xmm);
        bb.xmm_mut(xmm).push(slot);
    }

    ///
    /// Link the slot *reg* to a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn store_new_xmm(&mut self, bbctx: &mut BBContext, slot: SlotId) -> Xmm {
        let xmm = bbctx.alloc_xmm();
        self.store_xmm(bbctx, slot, xmm);
        xmm
    }

    ///
    /// Link the slot *reg* to both of the stack and the given xmm register *xmm*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn store_both(
        &mut self,
        bbctx: &mut BBContext,
        slot: SlotId,
        xmm: Xmm,
        guarded: Guarded,
    ) {
        self.unlink(bbctx, slot);
        bbctx.set_both(slot, xmm, guarded);
        bbctx.xmm_mut(xmm).push(slot);
    }

    pub(in crate::compiler::jitgen) fn store_new_both_integer(
        &mut self,
        bbctx: &mut BBContext,
        slot: SlotId,
    ) -> Xmm {
        self.store_new_both(bbctx, slot, Guarded::Fixnum)
    }

    pub(in crate::compiler::jitgen) fn store_new_both_float(
        &mut self,
        bbctx: &mut BBContext,
        slot: SlotId,
    ) -> Xmm {
        self.store_new_both(bbctx, slot, Guarded::Float)
    }

    ///
    /// Link the slot *reg* to both of the stack and a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    fn store_new_both(&mut self, bbctx: &mut BBContext, slot: SlotId, guarded: Guarded) -> Xmm {
        let x = bbctx.alloc_xmm();
        self.store_both(bbctx, slot, x, guarded);
        x
    }

    ///
    /// Link the slot *reg* to a concrete value *v*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn store_concrete_value(
        &mut self,
        bbctx: &mut BBContext,
        slot: SlotId,
        v: Value,
    ) {
        let guarded = Guarded::from_concrete_value(v);
        bbctx.set_slot(slot, LinkMode::ConcreteValue(v), guarded);
    }

    ///
    /// Link the slot *reg* to the accumulator.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn store_r15(
        &mut self,
        bbctx: &mut BBContext,
        slot: impl Into<Option<SlotId>>,
        guarded: Guarded,
    ) {
        if let Some(slot) = slot.into() {
            assert!(bbctx.r15.is_none());
            self.unlink(bbctx, slot);
            bbctx.set_slot(slot, LinkMode::Accumulator, guarded);
            bbctx.r15 = Some(slot);
        }
    }

    pub(super) fn xmm_swap(&mut self, bbctx: &mut BBContext, l: Xmm, r: Xmm) {
        bbctx.xmm_swap(l, r);
        self.inst.push(AsmInst::XmmSwap(l, r));
    }

    ///
    /// ### destroy
    /// - rax
    ///
    fn lit2stack(&mut self, bbctx: &mut BBContext, v: Value, slot: SlotId) {
        bbctx.set_slot(slot, LinkMode::Stack, Guarded::from_concrete_value(v));
        self.inst.push(AsmInst::LitToStack(v, slot));
    }

    ///
    /// Clear slots above *next_sp*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn clear(&mut self, bbctx: &mut BBContext) {
        let sp = bbctx.next_sp;
        for i in sp..SlotId(bbctx.slots.len() as u16) {
            self.unlink(bbctx, i)
        }
    }

    ///
    /// Clear the LinkMode::R15 slot.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn clear_r15(&mut self, bbctx: &mut BBContext) -> Option<SlotId> {
        let res = bbctx.r15;
        if let Some(r) = res {
            self.unlink(bbctx, r);
        }
        assert!(!bbctx
            .slots
            .iter()
            .any(|SlotState { link, .. }| matches!(link, LinkMode::Accumulator)));
        res
    }

    ///
    /// Copy *src* to *dst*.
    ///
    /// ### destroy
    /// - rax, r8
    ///
    pub(in crate::compiler::jitgen) fn copy_slot(
        &mut self,
        bbctx: &mut BBContext,
        src: SlotId,
        dst: SlotId,
    ) {
        let guarded = bbctx.guarded(src);
        match bbctx[src].link {
            LinkMode::Xmm(x) => {
                self.store_xmm(bbctx, dst, x);
            }
            LinkMode::Both(x) => {
                self.stack2reg(src, GP::Rax);
                self.reg2stack(GP::Rax, dst);
                self.store_both(bbctx, dst, x, guarded);
            }
            LinkMode::Stack => {
                self.store_alias(bbctx, src, dst);
            }
            LinkMode::Alias(origin) => {
                self.store_alias(bbctx, origin, dst);
            }
            LinkMode::ConcreteValue(v) => {
                self.unlink(bbctx, dst);
                self.store_concrete_value(bbctx, dst, v);
            }
            LinkMode::Accumulator => {
                self.reg2stack(GP::R15, src);
                self.unlink(bbctx, src);
                self.store_r15(bbctx, dst, guarded)
            }
        }
    }

    ///
    /// Allocate new xmm register corresponding to the slot *reg* for read/write f64.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn xmm_write(
        &mut self,
        bbctx: &mut BBContext,
        slot: SlotId,
    ) -> Xmm {
        match bbctx[slot].link {
            LinkMode::Xmm(x) if bbctx.xmm(x).len() == 1 => {
                assert_eq!(slot, bbctx.xmm(x)[0]);
                x
            }
            LinkMode::Xmm(_)
            | LinkMode::Both(_)
            | LinkMode::Stack
            | LinkMode::Alias(_)
            | LinkMode::ConcreteValue(_)
            | LinkMode::Accumulator => self.store_new_xmm(bbctx, slot),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, bbctx: &mut BBContext, slot: SlotId) -> u64 {
        self.xmm_write(bbctx, slot).enc()
    }

    pub(super) fn release_locals(&mut self, bbctx: &mut BBContext) {
        for i in 1..1 + bbctx.local_num as u16 {
            self.unlink(bbctx, SlotId(i));
        }
    }
}

impl MergeContext {
    pub(in crate::compiler::jitgen) fn merge(&mut self, other: &SlotContext) {
        let mut ir = AsmIr::new();
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            self[i].is_used.merge(&other[i].is_used);
            match (self[i].link, other[i].link) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => {
                    let guarded = self.guarded(i).union(&other.guarded(i));
                    ir.store_both(&mut self.0, i, l, guarded);
                }
                (LinkMode::Both(l), LinkMode::ConcreteValue(r)) if r.is_float() => {
                    ir.store_both(&mut self.0, i, l, Guarded::Float)
                }
                (LinkMode::ConcreteValue(l), LinkMode::Both(_)) if l.is_float() => {
                    ir.store_new_both_float(&mut self.0, i);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => ir.store_xmm(&mut self.0, i, l),
                (LinkMode::Xmm(l), LinkMode::ConcreteValue(r)) if r.is_float() => {
                    ir.store_xmm(&mut self.0, i, l)
                }
                (LinkMode::ConcreteValue(l), LinkMode::Xmm(_)) if l.is_float() => {
                    ir.store_new_xmm(&mut self.0, i);
                }
                (LinkMode::ConcreteValue(l), LinkMode::ConcreteValue(r)) if l == r => {
                    ir.unlink(&mut self.0, i);
                    ir.store_concrete_value(&mut self.0, i, l)
                }
                _ => ir.unlink(&mut self.0, i),
            };
        }
    }
}
