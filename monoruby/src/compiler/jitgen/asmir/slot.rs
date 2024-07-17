use super::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Guarded {
    Value,
    Fixnum,
    Float,
    ArrayTy,
    Class(ClassId),
}

impl std::default::Default for Guarded {
    fn default() -> Self {
        Guarded::Value
    }
}

impl Guarded {
    pub fn from_literal(v: Value) -> Self {
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
            self.clone()
        } else {
            Guarded::Value
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct SlotState {
    link: LinkMode,
    guarded: Guarded,
    alias: Vec<SlotId>,
}

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
            .map(
                |(
                    i,
                    SlotState {
                        link,
                        guarded,
                        alias,
                    },
                )| { format!("[%{i}: {:?} {:?} {:?}] ", link, guarded, alias) },
            )
            .collect();
        write!(f, "[{s}]")
    }
}

impl SlotContext {
    pub(in crate::compiler::jitgen) fn new(cc: &JitContext) -> Self {
        let len = cc.total_reg_num;
        SlotContext {
            slots: vec![SlotState::default(); len],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num: cc.local_num,
        }
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

    fn set_slot(&mut self, slot: SlotId, mode: LinkMode, guarded: Guarded) {
        self[slot].link = mode;
        self[slot].guarded = guarded;
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

    fn clear_link(&mut self, slot: SlotId) {
        self.set_slot(slot, LinkMode::Stack, Guarded::Value)
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
        if let LinkMode::Literal(v) = self[slot].link {
            let i = v.try_fixnum()?;
            u16::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
        if let LinkMode::Literal(v) = self[slot].link {
            let i = v.try_fixnum()?;
            u8::try_from(i).ok()
        } else {
            None
        }
    }

    pub(super) fn is_array_ty(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::ArrayTy;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::Literal(v) => assert_eq!(v.is_array_ty(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_array_ty(origin), b),
            _ => {}
        };
        b
    }

    pub(super) fn is_fixnum(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Fixnum;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(!b),
            LinkMode::Literal(v) => assert_eq!(v.is_fixnum(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_fixnum(origin), b),
            _ => {}
        };
        b
    }

    pub(super) fn is_float(&self, slot: SlotId) -> bool {
        let b = self.guarded(slot) == Guarded::Float;
        match self[slot].link {
            LinkMode::Xmm(_) => assert!(b),
            LinkMode::Literal(v) => assert_eq!(v.is_float(), b),
            LinkMode::Alias(origin) => assert_eq!(self.is_float(origin), b),
            _ => {}
        };
        b
    }

    pub(super) fn is_class(&self, slot: SlotId, class: ClassId) -> bool {
        match class {
            INTEGER_CLASS => self.is_fixnum(slot),
            FLOAT_CLASS => self.is_float(slot),
            _ => {
                let b = self.guarded(slot) == Guarded::Class(class);
                match self[slot].link {
                    LinkMode::Xmm(_) => assert!(!b),
                    LinkMode::Literal(v) => assert_eq!(v.class() == class, b),
                    LinkMode::Alias(origin) => assert_eq!(self.is_class(origin, class), b),
                    _ => {}
                };
                b
            }
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
                LinkMode::Stack | LinkMode::Literal(_) | LinkMode::R15 | LinkMode::Alias(_) => {}
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
                LinkMode::Literal(v) if f(SlotId(idx as u16)) => Some((*v, SlotId(idx as u16))),
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
        let mut b = UsingXmm::new([0; 1]);
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
    fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        panic!("no xmm reg is vacant.")
    }
}

impl AsmIr {
    pub(super) fn write_back_with_guarded(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        guarded: Guarded,
    ) {
        self.write_back_slot(bb, slot);
        bb.set_slot(slot, LinkMode::Stack, guarded);
    }

    ///
    /// Write back the value of the *slot* to the corresponding stack slot.
    ///
    /// LinkMode of the *slot* is set to LinkMode::Stack or Both.
    ///
    /// ### destroy
    /// - rax, rcx
    pub(super) fn write_back_slot(&mut self, bb: &mut BBContext, slot: SlotId) {
        if slot >= bb.sp {
            unreachable!("{:?} >= {:?} in write_back_slot()", slot, bb.sp);
        };
        let guarded = bb.guarded(slot);
        match bb.slot(slot) {
            LinkMode::Xmm(xmm) => {
                // Xmm -> Both
                bb.set_both_float(slot, xmm);
                self.xmm2stack(xmm, vec![slot]);
            }
            LinkMode::Literal(v) => {
                // Literal -> Stack
                self.lit2stack(bb, v, slot);
            }
            LinkMode::R15 => {
                // R15 -> Stack
                self.acc2stack(slot);
                self.unlink(bb, slot);
                bb.set_guarded(slot, guarded);
            }
            LinkMode::Alias(origin) => {
                self.stack2reg(origin, GP::Rax);
                self.reg2stack(GP::Rax, slot);
                self.unlink(bb, slot);
                bb.set_guarded(slot, guarded);
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
    pub(crate) fn unlink(&mut self, bb: &mut BBContext, slot: impl Into<Option<SlotId>>) {
        if let Some(slot) = slot.into() {
            match bb[slot].link {
                LinkMode::Both(xmm) | LinkMode::Xmm(xmm) => {
                    assert!(bb.xmm(xmm).contains(&slot));
                    bb.xmm_mut(xmm).retain(|e| *e != slot);
                    bb.clear_link(slot);
                }
                LinkMode::Alias(origin) => {
                    // TODO: is it Ok??
                    //assert_eq!(bb[origin].link, LinkMode::Stack);
                    assert!(bb[origin].alias.contains(&slot));
                    bb[origin].alias.retain(|e| *e != slot);
                    bb.clear_link(slot);
                }
                LinkMode::Literal(_) => {
                    bb.clear_link(slot);
                }
                LinkMode::R15 => {
                    bb.r15 = None;
                    bb.clear_link(slot);
                }
                LinkMode::Stack => {
                    // We must write back all aliases of *reg*.
                    let dst = std::mem::take(&mut bb[slot].alias);
                    if !dst.is_empty() {
                        self.stack2reg(slot, GP::R8);
                        for dst in dst {
                            self.reg2stack(GP::R8, dst);
                            assert_eq!(bb[dst].link, LinkMode::Alias(slot));
                            bb.set_mode(dst, LinkMode::Stack);
                        }
                    }
                }
            }
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
    pub(super) fn store_new_xmm(&mut self, bb: &mut BBContext, slot: SlotId) -> Xmm {
        let xmm = bb.alloc_xmm();
        self.store_xmm(bb, slot, xmm);
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
        bb: &mut BBContext,
        slot: SlotId,
        xmm: Xmm,
        guarded: Guarded,
    ) {
        self.unlink(bb, slot);
        bb.set_both(slot, xmm, guarded);
        bb.xmm_mut(xmm).push(slot);
    }

    ///
    /// Link the slot *reg* to both of the stack and a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn store_new_both(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        guarded: Guarded,
    ) -> Xmm {
        let x = bb.alloc_xmm();
        self.store_both(bb, slot, x, guarded);
        x
    }

    ///
    /// Link the slot *reg* to a literal value *v*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn store_literal(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        v: Value,
    ) {
        self.unlink(bb, slot);
        let guarded = Guarded::from_literal(v);
        bb.set_slot(slot, LinkMode::Literal(v), guarded);
    }

    ///
    /// Link the slot *reg* to the accumulator.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn store_r15(
        &mut self,
        bb: &mut BBContext,
        slot: impl Into<Option<SlotId>>,
        guarded: Guarded,
    ) {
        if let Some(slot) = slot.into() {
            assert!(bb.r15.is_none());
            self.unlink(bb, slot);
            bb.set_slot(slot, LinkMode::R15, guarded);
            bb.r15 = Some(slot);
        }
    }

    pub(super) fn xmm_swap(&mut self, bb: &mut BBContext, l: Xmm, r: Xmm) {
        bb.xmm_swap(l, r);
        self.inst.push(AsmInst::XmmSwap(l, r));
    }

    ///
    /// ### destroy
    /// - rax
    ///
    fn lit2stack(&mut self, bb: &mut BBContext, v: Value, slot: SlotId) {
        bb.set_slot(slot, LinkMode::Stack, Guarded::from_literal(v));
        self.inst.push(AsmInst::LitToStack(v, slot));
    }

    ///
    /// Clear slots above *next_sp*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn clear(&mut self, bb: &mut BBContext) {
        let sp = bb.next_sp;
        for i in sp..SlotId(bb.slots.len() as u16) {
            self.unlink(bb, i)
        }
    }

    ///
    /// Clear the LinkMode::R15 slot.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn clear_r15(&mut self, bb: &mut BBContext) -> Option<SlotId> {
        let res = bb.r15;
        if let Some(r) = res {
            self.unlink(bb, r);
        }
        assert!(!bb
            .slots
            .iter()
            .any(|SlotState { link, .. }| matches!(link, LinkMode::R15)));
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
        bb: &mut BBContext,
        src: SlotId,
        dst: SlotId,
    ) {
        let guarded = bb.guarded(src);
        match bb[src].link {
            LinkMode::Xmm(x) => {
                self.store_xmm(bb, dst, x);
            }
            LinkMode::Both(x) => {
                self.stack2reg(src, GP::Rax);
                self.reg2stack(GP::Rax, dst);
                self.store_both(bb, dst, x, guarded);
            }
            LinkMode::Stack => {
                self.store_alias(bb, src, dst);
            }
            LinkMode::Alias(origin) => {
                self.store_alias(bb, origin, dst);
            }
            LinkMode::Literal(v) => {
                self.store_literal(bb, dst, v);
            }
            LinkMode::R15 => {
                self.reg2stack(GP::R15, src);
                self.unlink(bb, src);
                self.store_r15(bb, dst, guarded)
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
        bb: &mut BBContext,
        slot: SlotId,
    ) -> Xmm {
        match bb[slot].link {
            LinkMode::Xmm(x) if bb.xmm(x).len() == 1 => {
                assert_eq!(slot, bb.xmm(x)[0]);
                x
            }
            LinkMode::Xmm(_)
            | LinkMode::Both(_)
            | LinkMode::Stack
            | LinkMode::Alias(_)
            | LinkMode::Literal(_)
            | LinkMode::R15 => self.store_new_xmm(bb, slot),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, bb: &mut BBContext, slot: SlotId) -> u64 {
        self.xmm_write(bb, slot).enc()
    }

    pub(super) fn release_locals(&mut self, bb: &mut BBContext) {
        for i in 1..1 + bb.local_num as u16 {
            self.unlink(bb, SlotId(i));
        }
    }
}

impl MergeContext {
    pub(in crate::compiler::jitgen) fn union(&mut self, other: &SlotContext) {
        let mut ir = AsmIr::new();
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            match (self[i].link, other[i].link) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => {
                    let guarded = self.guarded(i).union(&other.guarded(i));
                    ir.store_both(&mut self.0, i, l, guarded);
                }
                (LinkMode::Both(l), LinkMode::Literal(r)) if r.is_float() => {
                    ir.store_both(&mut self.0, i, l, Guarded::Float)
                }
                (LinkMode::Literal(l), LinkMode::Both(_)) if l.is_float() => {
                    ir.store_new_both(&mut self.0, i, Guarded::Float);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => ir.store_xmm(&mut self.0, i, l),
                (LinkMode::Xmm(l), LinkMode::Literal(r)) if r.is_float() => {
                    ir.store_xmm(&mut self.0, i, l)
                }
                (LinkMode::Literal(l), LinkMode::Xmm(_)) if l.is_float() => {
                    ir.store_new_xmm(&mut self.0, i);
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {
                    ir.store_literal(&mut self.0, i, l)
                }
                _ => ir.unlink(&mut self.0, i),
            };
        }
    }
}
