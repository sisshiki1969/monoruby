use super::*;

#[derive(Debug, Clone)]
struct LinkModes(Vec<LinkMode>);

#[derive(Clone)]
pub(crate) struct SlotState {
    slots: LinkModes,
    alias: Vec<Vec<SlotId>>,
    /// Information for xmm registers (xmm2 - xmm15).
    xmm: [Vec<SlotId>; 14],
    r15: Option<SlotId>,
    local_num: usize,
}

impl std::ops::Index<SlotId> for LinkModes {
    type Output = LinkMode;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.0[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for LinkModes {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.0[i.0 as usize]
    }
}

impl std::ops::Deref for LinkModes {
    type Target = Vec<LinkMode>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl LinkModes {
    fn new(len: usize) -> Self {
        Self(vec![LinkMode::Stack; len])
    }
}

impl std::fmt::Debug for SlotState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .slots
            .iter()
            .enumerate()
            .map(|(i, mode)| match mode {
                LinkMode::Stack => format!("%{i}:Stack ",),
                LinkMode::Alias(o) => format!("%{i}:Alias({:?}) ", o),
                LinkMode::Literal(v) => format!("%{i}:Literal({:?}) ", v),
                LinkMode::Both(x) => format!("%{i}:Both({x:?}) "),
                LinkMode::Xmm(x) => format!("%{i}:Xmm({x:?}) "),
                LinkMode::R15 => format!("%{i}:R15 "),
            })
            .collect();
        write!(f, "[{s}] {:?}", self.alias)
    }
}

impl SlotState {
    pub(in crate::compiler::jitgen) fn new(cc: &JitContext) -> Self {
        let len = cc.total_reg_num;
        SlotState {
            slots: LinkModes::new(len),
            alias: vec![vec![]; len],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num: cc.local_num,
        }
    }

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn slot(&self, slot: SlotId) -> LinkMode {
        self.slots[slot]
    }

    pub fn set_xmm(&mut self, slot: SlotId, xmm: Xmm) {
        self.slots[slot] = LinkMode::Xmm(xmm)
    }

    pub fn set_both(&mut self, slot: SlotId, xmm: Xmm) {
        self.slots[slot] = LinkMode::Both(xmm)
    }

    pub fn set_stack(&mut self, slot: SlotId) {
        self.slots[slot] = LinkMode::Stack
    }

    pub fn is_u16_literal(&self, slot: SlotId) -> Option<u16> {
        if let LinkMode::Literal(v) = self.slots[slot] {
            let i = v.try_fixnum()?;
            u16::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
        if let LinkMode::Literal(v) = self.slots[slot] {
            let i = v.try_fixnum()?;
            u8::try_from(i).ok()
        } else {
            None
        }
    }

    pub fn is_array_ty(&self, slot: SlotId) -> bool {
        match self.slots[slot] {
            LinkMode::Xmm(_) => false,
            LinkMode::Literal(v) => v.is_array_ty(),
            LinkMode::Both(_) | LinkMode::Stack => false,
            LinkMode::R15 => false,
            LinkMode::Alias(origin) => self.is_array_ty(origin),
        }
    }

    pub fn is_fixnum(&self, slot: SlotId) -> bool {
        match self.slots[slot] {
            LinkMode::Xmm(_) => false,
            LinkMode::Literal(v) => v.is_fixnum(),
            LinkMode::Both(_) | LinkMode::Stack => false,
            LinkMode::R15 => false,
            LinkMode::Alias(origin) => self.is_fixnum(origin),
        }
    }

    pub fn is_float(&self, slot: SlotId) -> bool {
        match self.slots[slot] {
            LinkMode::Xmm(_) => true,
            LinkMode::Literal(v) => v.is_float(),
            LinkMode::Both(_) | LinkMode::Stack => false,
            LinkMode::R15 => false,
            LinkMode::Alias(origin) => self.is_float(origin),
        }
    }

    pub fn is_class(&self, slot: SlotId, class: ClassId) -> bool {
        match class {
            INTEGER_CLASS => self.is_fixnum(slot),
            FLOAT_CLASS => self.is_float(slot),
            _ => match self.slots[slot] {
                LinkMode::Xmm(_) => false,
                LinkMode::Literal(v) => v.class() == class,
                LinkMode::Both(_) | LinkMode::Stack => false,
                LinkMode::R15 => false,
                LinkMode::Alias(origin) => self.is_class(origin, class),
            },
        }
    }

    fn xmm(&self, xmm: Xmm) -> &[SlotId] {
        &self.xmm[xmm.0 as usize]
    }

    fn xmm_mut(&mut self, xmm: Xmm) -> &mut Vec<SlotId> {
        &mut self.xmm[xmm.0 as usize]
    }

    fn alias_mut(&mut self, slot: SlotId) -> &mut Vec<SlotId> {
        &mut self.alias[slot.0 as usize]
    }

    pub(super) fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self.xmm(xmm).is_empty()
    }

    pub(super) fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        self.xmm.swap(l.0 as usize, r.0 as usize);
        self.slots.0.iter_mut().for_each(|mode| match mode {
            LinkMode::Both(x) | LinkMode::Xmm(x) => {
                if *x == l {
                    *x = r;
                } else if *x == r {
                    *x = l;
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
                        .filter(|reg| f(**reg) && matches!(self.slots[**reg], LinkMode::Xmm(_)))
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
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Literal(v) if f(SlotId(idx as u16)) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect()
    }

    fn wb_alias(&self, f: impl Fn(SlotId) -> bool) -> Vec<(SlotId, Vec<SlotId>)> {
        self.alias
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| {
                let v: Vec<_> = v.iter().filter(|slot| f(**slot)).cloned().collect();
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
    pub(super) fn xmm_to_both(&mut self, bb: &mut BBContext, xmm: Xmm) {
        for i in bb.xmm(xmm).to_vec() {
            bb.slots[i] = LinkMode::Both(xmm);
        }
        self.xmm2stack(xmm, bb.xmm(xmm).to_vec());
    }

    ///
    /// Discard slot *reg*.
    ///
    /// *reg* is set to LinkMode::Stack.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(crate) fn clear_link(&mut self, bb: &mut BBContext, slot: impl Into<Option<SlotId>>) {
        match slot.into() {
            Some(reg) => match bb.slots[reg] {
                LinkMode::Both(xmm) | LinkMode::Xmm(xmm) => {
                    assert!(bb.xmm(xmm).contains(&reg));
                    bb.xmm_mut(xmm).retain(|e| *e != reg);
                    bb.slots[reg] = LinkMode::Stack;
                }
                LinkMode::Alias(origin) => {
                    assert_eq!(bb.slots[origin], LinkMode::Stack);
                    assert!(bb.alias[origin.0 as usize].contains(&reg));
                    bb.alias[origin.0 as usize].retain(|e| *e != reg);
                    bb.slots[reg] = LinkMode::Stack;
                }
                LinkMode::Literal(_) => {
                    bb.slots[reg] = LinkMode::Stack;
                }
                LinkMode::R15 => {
                    bb.r15 = None;
                    bb.slots[reg] = LinkMode::Stack;
                }
                LinkMode::Stack => {
                    // We must write back all aliases of *reg*.
                    let dst = std::mem::take(&mut bb.alias[reg.0 as usize]);
                    if !dst.is_empty() {
                        self.stack2reg(reg, GP::R8);
                        for dst in dst {
                            self.reg2stack(GP::R8, dst);
                            assert_eq!(bb.slots[dst], LinkMode::Alias(reg));
                            bb.slots[dst] = LinkMode::Stack;
                        }
                    }
                }
            },
            None => {}
        }
    }

    ///
    /// Link the slot *reg* to the alias of *origin*.
    ///
    /// ### destroy
    /// - r8
    ///
    fn link_alias(&mut self, bb: &mut BBContext, origin: SlotId, slot: SlotId) {
        self.clear_link(bb, slot);
        if bb.slots[origin] != LinkMode::Stack {
            unreachable!("origin:{:?} reg:{:?} {:?}", origin, slot, bb);
        };
        bb.slots[slot] = LinkMode::Alias(origin);
        bb.alias_mut(origin).push(slot);
    }

    ///
    /// Link the slot *reg* to the given xmm register *xmm*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_xmm(&mut self, bb: &mut BBContext, slot: SlotId, xmm: Xmm) {
        self.clear_link(bb, slot);
        bb.slots[slot] = LinkMode::Xmm(xmm);
        bb.xmm_mut(xmm).push(slot);
    }

    ///
    /// Link the slot *reg* to a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_new_xmm(&mut self, bb: &mut BBContext, slot: SlotId) -> Xmm {
        let xmm = bb.alloc_xmm();
        self.link_xmm(bb, slot, xmm);
        xmm
    }

    ///
    /// Link the slot *reg* to both of the stack and the given xmm register *xmm*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_both(&mut self, bb: &mut BBContext, slot: SlotId, xmm: Xmm) {
        self.clear_link(bb, slot);
        bb.slots[slot] = LinkMode::Both(xmm);
        bb.xmm_mut(xmm).push(slot);
    }

    ///
    /// Link the slot *reg* to both of the stack and a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn link_new_both(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
    ) -> Xmm {
        let x = bb.alloc_xmm();
        self.link_both(bb, slot, x);
        x
    }

    ///
    /// Link the slot *reg* to a literal value *v*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn link_literal(
        &mut self,
        bb: &mut BBContext,
        slot: SlotId,
        v: Value,
    ) {
        self.clear_link(bb, slot);
        bb.slots[slot] = LinkMode::Literal(v);
    }

    ///
    /// Link the slot *reg* to the accumulator.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_r15(&mut self, bb: &mut BBContext, slot: impl Into<Option<SlotId>>) {
        if let Some(reg) = slot.into() {
            assert!(bb.r15.is_none());
            self.clear_link(bb, reg);
            bb.slots[reg] = LinkMode::R15;
            bb.r15 = Some(reg);
        }
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
            self.clear_link(bb, i)
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
            self.clear_link(bb, r);
        }
        assert!(!bb.slots.iter().any(|f| matches!(f, LinkMode::R15)));
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
        match bb.slots[src] {
            LinkMode::Xmm(x) => {
                self.link_xmm(bb, dst, x);
            }
            LinkMode::Both(x) => {
                self.stack2reg(src, GP::Rax);
                self.reg2stack(GP::Rax, dst);
                self.link_both(bb, dst, x);
            }
            LinkMode::Stack => {
                self.link_alias(bb, src, dst);
            }
            LinkMode::Alias(origin) => {
                self.link_alias(bb, origin, dst);
            }
            LinkMode::Literal(v) => {
                self.link_literal(bb, dst, v);
            }
            LinkMode::R15 => {
                self.reg2stack(GP::R15, src);
                self.clear_link(bb, src);
                self.link_r15(bb, dst);
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
        match bb.slots[slot] {
            LinkMode::Xmm(x) if bb.xmm(x).len() == 1 => {
                assert_eq!(slot, bb.xmm(x)[0]);
                x
            }
            LinkMode::Xmm(_)
            | LinkMode::Both(_)
            | LinkMode::Stack
            | LinkMode::Alias(_)
            | LinkMode::Literal(_)
            | LinkMode::R15 => self.link_new_xmm(bb, slot),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, bb: &mut BBContext, slot: SlotId) -> u64 {
        self.xmm_write(bb, slot).enc()
    }

    pub(super) fn release_locals(&mut self, bb: &mut BBContext) {
        for reg in 1..1 + bb.local_num as u16 {
            self.clear_link(bb, SlotId(reg));
        }
    }

    ///
    /// Write back LinkMode::Alias slots.
    ///
    /// ### destroy
    /// - rax, r8
    ///
    pub(super) fn writeback_alias(&mut self, bb: &mut BBContext) {
        for i in 0..bb.sp.0 {
            let r = SlotId::new(i);
            if let LinkMode::Alias(origin) = bb.slots[r] {
                self.stack2reg(origin, GP::Rax);
                self.reg2stack(GP::Rax, r);
                self.clear_link(bb, r);
            }
        }
    }
}

impl MergeContext {
    pub(in crate::compiler::jitgen) fn merge(&mut self, other: &SlotState) {
        let mut ir = AsmIr::new();
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            match (self.slots[i], other.slots[i]) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => ir.link_both(&mut self.0, i, l),
                (LinkMode::Both(l), LinkMode::Literal(r)) if r.is_float() => {
                    ir.link_both(&mut self.0, i, l)
                }
                (LinkMode::Literal(l), LinkMode::Both(_)) if l.is_float() => {
                    ir.link_new_both(&mut self.0, i);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => ir.link_xmm(&mut self.0, i, l),
                (LinkMode::Xmm(l), LinkMode::Literal(r)) if r.is_float() => {
                    ir.link_xmm(&mut self.0, i, l)
                }
                (LinkMode::Literal(l), LinkMode::Xmm(_)) if l.is_float() => {
                    ir.link_new_xmm(&mut self.0, i);
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => {
                    ir.link_literal(&mut self.0, i, l)
                }
                _ => ir.clear_link(&mut self.0, i),
            };
        }
    }
}
