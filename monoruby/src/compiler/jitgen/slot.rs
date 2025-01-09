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
        for (i, state) in self.slots.iter().enumerate() {
            write!(f, "\n[%{i}: {:?}]", state)?;
        }
        write!(f, "\nr15={:?}", self.r15)
    }
}

impl SlotContext {
    pub(super) fn from(cc: &JitContext) -> Self {
        Self::new(cc.total_reg_num(), cc.local_num())
    }

    pub(super) fn set_slot(&mut self, slot: SlotId, mode: LinkMode, guarded: Guarded) {
        if self[slot].link == LinkMode::Accumulator {
            assert_eq!(self.r15, Some(slot));
            self.r15 = None;
        }
        self[slot].guarded = guarded;
        self[slot].is_used.kill();
        if mode == LinkMode::Accumulator {
            assert!(self.r15.is_none());
            self.r15 = Some(slot);
        }
        self[slot].link = mode;
    }

    pub(super) fn slot(&self, slot: SlotId) -> LinkMode {
        self[slot].link
    }

    pub(super) fn guarded(&self, slot: SlotId) -> Guarded {
        self[slot].guarded
    }

    pub(super) fn set_guard_fixnum(&mut self, slot: SlotId) {
        self.set_guarded(slot, Guarded::Fixnum)
    }

    pub(super) fn set_guard_float(&mut self, slot: SlotId) {
        self.set_guarded(slot, Guarded::Float)
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

    pub fn is_fixnum_literal(&self, slot: SlotId) -> Option<i64> {
        if let LinkMode::ConcreteValue(v) = self[slot].link {
            v.try_fixnum()
        } else {
            None
        }
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

    pub fn is_array_ty(&self, store: &Store, slot: SlotId) -> bool {
        let b = if let Guarded::Class(class) = self.guarded(slot) {
            store[class].is_array_ty_instance()
        } else {
            false
        };
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_array_ty(), b),
            _ => {}
        };
        b
    }

    pub fn is_fixnum(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Fixnum;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_fixnum(), b),
            _ => {}
        };
        b
    }

    pub fn is_float(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Float;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(b),
            LinkMode::ConcreteValue(v) => assert_eq!(v.is_float(), b),
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
            _ => match self.guarded(slot) {
                Guarded::Fixnum => true,
                Guarded::Float => true,
                Guarded::Value => false,
                Guarded::Class(class) => !class.is_falsy(),
            },
        }
    }

    pub fn is_falsy(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => false,
            LinkMode::Both(_) => false,
            LinkMode::ConcreteValue(v) => !v.as_bool(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::Value => false,
                Guarded::Class(class) => class.is_falsy(),
            },
        }
    }

    pub fn is_nil(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => false,
            LinkMode::Both(_) => false,
            LinkMode::ConcreteValue(v) => v.is_nil(),
            _ => match self.guarded(slot) {
                Guarded::Fixnum => false,
                Guarded::Float => false,
                Guarded::Value => false,
                Guarded::Class(class) => class.is_nil(),
            },
        }
    }

    pub fn is_not_nil(&self, slot: SlotId) -> bool {
        match self[slot].link {
            LinkMode::Xmm(_) => true,
            LinkMode::Both(_) => true,
            LinkMode::ConcreteValue(v) => !v.is_nil(),
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

    ///
    /// Clear the LinkMode::R15 slot.
    ///
    pub(super) fn clear_r15(&mut self) -> Option<SlotId> {
        let res = self.r15;
        if let Some(r) = res {
            self.unlink(r);
        }
        assert!(!self
            .slots
            .iter()
            .any(|SlotState { link, .. }| matches!(link, LinkMode::Accumulator)));
        assert!(self.r15.is_none());
        res
    }

    ///
    /// Link the slot *reg* to the given xmm register *xmm*.
    ///
    pub(super) fn store_xmm(&mut self, slot: SlotId, xmm: Xmm) {
        self.unlink(slot);
        self.set_xmm(slot, xmm);
        self.xmm_mut(xmm).push(slot);
    }

    ///
    /// Link the slot *reg* to a new xmm register.
    ///
    pub(super) fn store_new_xmm(&mut self, slot: SlotId) -> Xmm {
        let xmm = self.alloc_xmm();
        self.store_xmm(slot, xmm);
        xmm
    }

    ///
    /// Link the slot *reg* to both of the stack and the given xmm register *xmm*.
    ///
    pub(super) fn store_both(&mut self, slot: SlotId, xmm: Xmm, guarded: Guarded) {
        self.unlink(slot);
        self.set_both(slot, xmm, guarded);
        self.xmm_mut(xmm).push(slot);
    }

    pub(super) fn store_new_both_integer(&mut self, slot: SlotId) -> Xmm {
        self.store_new_both(slot, Guarded::Fixnum)
    }

    pub(super) fn store_new_both_float(&mut self, slot: SlotId) -> Xmm {
        self.store_new_both(slot, Guarded::Float)
    }

    ///
    /// Link the slot *reg* to both of the stack and a new xmm register.
    ///
    fn store_new_both(&mut self, slot: SlotId, guarded: Guarded) -> Xmm {
        let x = self.alloc_xmm();
        self.store_both(slot, x, guarded);
        x
    }

    ///
    /// Link the slot *reg* to a concrete value *v*.
    ///
    pub(crate) fn store_concrete_value(&mut self, slot: SlotId, v: Value) {
        let guarded = Guarded::from_concrete_value(v);
        self.set_slot(slot, LinkMode::ConcreteValue(v), guarded);
    }
}

impl SlotContext {
    fn new(total_reg_num: usize, local_num: usize) -> Self {
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

    fn xmm(&self, xmm: Xmm) -> &[SlotId] {
        &self.xmm[xmm.0 as usize]
    }

    fn xmm_mut(&mut self, xmm: Xmm) -> &mut Vec<SlotId> {
        &mut self.xmm[xmm.0 as usize]
    }

    fn set_guarded(&mut self, slot: SlotId, guarded: Guarded) {
        self[slot].guarded = guarded;
    }

    fn set_both(&mut self, slot: SlotId, xmm: Xmm, guarded: Guarded) {
        self.set_slot(slot, LinkMode::Both(xmm), guarded)
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
                LinkMode::Stack | LinkMode::ConcreteValue(_) | LinkMode::Accumulator => {}
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

    pub(super) fn get_register(&self) -> WriteBack {
        WriteBack::new(vec![], vec![], self.r15)
    }

    pub(super) fn get_write_back(&self, sp: SlotId) -> WriteBack {
        let f = |reg: SlotId| reg < sp;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, r15)
    }

    fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let f = |reg: SlotId| reg.0 as usize <= local_num;
        let xmm = self.wb_xmm(f);
        let literal = self.wb_literal(f);
        let r15 = match self.r15 {
            Some(slot) if f(slot) => Some(slot),
            _ => None,
        };
        WriteBack::new(xmm, literal, r15)
    }

    pub(super) fn get_using_xmm(&self, sp: SlotId) -> UsingXmm {
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

    ///
    /// Discard slot *reg*.
    ///
    /// *reg* is set to LinkMode::Stack / Guarded::Value.
    ///
    pub(crate) fn unlink(&mut self, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            match self[slot].link {
                LinkMode::Both(xmm) | LinkMode::Xmm(xmm) => {
                    assert!(self.xmm(xmm).contains(&slot));
                    self.xmm_mut(xmm).retain(|e| *e != slot);
                }
                LinkMode::ConcreteValue(_) => {}
                LinkMode::Accumulator => {}
                LinkMode::Stack => {}
            }
            self.set_slot(slot, LinkMode::Stack, Guarded::Value)
        }
    }

    ///
    /// Link the slot *reg* to the accumulator.
    ///
    pub(super) fn store_r15(&mut self, slot: impl Into<Option<SlotId>>, guarded: Guarded) {
        if let Some(slot) = slot.into() {
            self.unlink(slot);
            self.set_slot(slot, LinkMode::Accumulator, guarded);
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct Liveness(Vec<IsUsed>);

impl std::fmt::Debug for Liveness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, is_used)| match is_used {
                IsUsed::Used(UsedAs { ty, .. }) => {
                    Some(format!("[{:?}: {:?}] ", SlotId(i as u16), ty))
                }
                IsUsed::Killed => Some(format!("[{:?}: Killed] ", SlotId(i as u16))),
                _ => None,
            })
            .collect();
        write!(f, "Liveness {{{s}}}")
    }
}

impl Liveness {
    pub(in crate::compiler::jitgen) fn new(total_reg_num: usize) -> Self {
        Self(vec![IsUsed::default(); total_reg_num])
    }

    pub(in crate::compiler::jitgen) fn merge(&mut self, bbctx: BBContext) {
        for (i, is_used) in &mut self.0.iter_mut().enumerate() {
            is_used.merge(&bbctx[SlotId(i as u16)].is_used);
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
    is_used: IsUsed,
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
    pub(in crate::compiler::jitgen) fn use_as_float(&mut self) {
        self.is_used.use_as_float();
    }

    pub(in crate::compiler::jitgen) fn use_as_value(&mut self) {
        self.is_used.use_as_non_float();
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

impl BBContext {
    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::Stack or Both.
    ///
    /// ### destroy
    /// - rax, rcx
    pub(super) fn write_back_slot(&mut self, ir: &mut AsmIr, slot: SlotId) {
        if slot >= self.sp {
            unreachable!("{:?} >= {:?} in write_back_slot()", slot, self.sp);
        };
        let guarded = self.guarded(slot);
        match self.slot(slot) {
            LinkMode::Xmm(xmm) => {
                // Xmm -> Both
                self.set_both_float(slot, xmm);
                ir.xmm2stack(xmm, slot);
            }
            LinkMode::ConcreteValue(v) => {
                // Literal -> Stack
                self.lit2stack(ir, v, slot);
            }
            LinkMode::Accumulator => {
                // R15 -> Stack
                ir.acc2stack(slot);
                self.unlink(slot);
                self.set_guarded(slot, guarded);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    pub(super) fn xmm_swap(&mut self, ir: &mut AsmIr, l: Xmm, r: Xmm) {
        self.slot_state.xmm_swap(l, r);
        ir.push(AsmInst::XmmSwap(l, r));
    }

    ///
    /// ### destroy
    /// - rax
    ///
    fn lit2stack(&mut self, ir: &mut AsmIr, v: Value, slot: SlotId) {
        self.set_slot(slot, LinkMode::Stack, Guarded::from_concrete_value(v));
        ir.push(AsmInst::LitToStack(v, slot));
    }

    ///
    /// Clear slots above *next_sp*.
    ///
    pub(in crate::compiler::jitgen) fn clear(&mut self) {
        let sp = self.next_sp;
        for i in sp..SlotId(self.slots.len() as u16) {
            self.unlink(i)
        }
    }

    ///
    /// Copy *src* to *dst*.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(in crate::compiler::jitgen) fn copy_slot(
        &mut self,
        ir: &mut AsmIr,
        src: SlotId,
        dst: SlotId,
    ) {
        if src == dst {
            return;
        }
        let guarded = self.guarded(src);
        match self[src].link {
            LinkMode::Xmm(x) => {
                self.store_xmm(dst, x);
            }
            LinkMode::Both(x) => {
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
                self.store_both(dst, x, guarded);
            }
            LinkMode::Stack => {
                ir.stack2reg(src, GP::Rax);
                self.unlink(dst);
                ir.reg2stack(GP::Rax, dst);
            }
            LinkMode::ConcreteValue(v) => {
                self.unlink(dst);
                self.store_concrete_value(dst, v);
            }
            LinkMode::Accumulator => {
                ir.reg2stack(GP::R15, src);
                self.unlink(src);
                self.store_r15(dst, guarded)
            }
        }
    }

    ///
    /// Allocate new xmm register corresponding to the slot *reg* for read/write f64.
    ///
    pub(in crate::compiler::jitgen) fn xmm_write(&mut self, slot: SlotId) -> Xmm {
        match self[slot].link {
            LinkMode::Xmm(x) if self.xmm(x).len() == 1 => {
                assert_eq!(slot, self.xmm(x)[0]);
                x
            }
            LinkMode::Xmm(_)
            | LinkMode::Both(_)
            | LinkMode::Stack
            | LinkMode::ConcreteValue(_)
            | LinkMode::Accumulator => self.store_new_xmm(slot),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, slot: SlotId) -> u64 {
        self.xmm_write(slot).enc()
    }

    fn release_locals(&mut self) {
        for i in 1..1 + self.local_num as u16 {
            self.unlink(SlotId(i));
        }
    }
}

// write back operations
impl BBContext {
    pub(super) fn write_back_slots(&mut self, ir: &mut AsmIr, slot: &[SlotId]) {
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
        self.unlink(*dst);
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
        let wb = self.get_locals_write_back();
        ir.push(AsmInst::WriteBack(wb));
        self.release_locals();
    }
}

impl MergeContext {
    pub(in crate::compiler::jitgen) fn merge(&mut self, other: &SlotContext) {
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            self[i].is_used.merge(&other[i].is_used);
            match (self[i].link, other[i].link) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => {
                    let guarded = self.guarded(i).union(&other.guarded(i));
                    self.store_both(i, l, guarded);
                }
                (LinkMode::Both(l), LinkMode::ConcreteValue(r)) if r.is_float() => {
                    self.store_both(i, l, Guarded::Float)
                }
                (LinkMode::ConcreteValue(l), LinkMode::Both(_)) if l.is_float() => {
                    self.store_new_both_float(i);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => self.store_xmm(i, l),
                (LinkMode::Xmm(l), LinkMode::ConcreteValue(r)) if r.is_float() => {
                    self.store_xmm(i, l)
                }
                (LinkMode::ConcreteValue(l), LinkMode::Xmm(_)) if l.is_float() => {
                    self.store_new_xmm(i);
                }
                (LinkMode::ConcreteValue(l), LinkMode::ConcreteValue(r)) if l == r => {
                    self.unlink(i);
                    self.store_concrete_value(i, l)
                }
                _ => self.unlink(i),
            };
        }
    }
}
