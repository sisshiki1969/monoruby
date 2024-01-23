use super::*;

#[derive(Clone, PartialEq)]
pub(crate) struct SlotState {
    slots: Vec<LinkMode>,
    alias: Vec<Vec<SlotId>>,
    /// Information for xmm registers (xmm2 - xmm15).
    xmm: [Vec<SlotId>; 14],
    r15: Option<SlotId>,
    local_num: usize,
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

impl std::ops::Index<SlotId> for SlotState {
    type Output = LinkMode;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.slots[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for SlotState {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.slots[i.0 as usize]
    }
}

impl std::ops::Index<Xmm> for SlotState {
    type Output = Vec<SlotId>;
    fn index(&self, i: Xmm) -> &Self::Output {
        &self.xmm[i.0 as usize]
    }
}

impl std::ops::IndexMut<Xmm> for SlotState {
    fn index_mut(&mut self, i: Xmm) -> &mut Self::Output {
        &mut self.xmm[i.0 as usize]
    }
}

impl SlotState {
    pub(in crate::compiler::jitgen) fn new(cc: &JitContext) -> Self {
        SlotState {
            slots: vec![LinkMode::Stack; cc.total_reg_num],
            alias: vec![vec![]; cc.total_reg_num],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            r15: None,
            local_num: cc.local_num,
        }
    }

    pub(in crate::compiler::jitgen) fn len(&self) -> usize {
        self.slots.len()
    }

    pub(super) fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self[xmm].is_empty()
    }

    pub(super) fn xmm_to_both(&mut self, freg: Xmm) {
        for i in self[freg].clone() {
            self[i] = LinkMode::Both(freg);
        }
    }

    pub(super) fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        self.xmm.swap(l.0 as usize, r.0 as usize);
        self.slots.iter_mut().for_each(|mode| match mode {
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

    pub(in crate::compiler::jitgen) fn get_write_back(&self, sp: SlotId) -> WriteBack {
        let xmm = self
            .xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| *reg < &sp && matches!(self[**reg], LinkMode::Xmm(_)))
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((Xmm::new(i as u16), v))
                    }
                }
            })
            .collect();
        let literal = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Literal(v) if SlotId::new(idx as u16) < sp => {
                    Some((*v, SlotId(idx as u16)))
                }
                _ => None,
            })
            .collect();
        let alias = self
            .alias
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| {
                let v: Vec<_> = v.iter().filter(|slot| *slot < &sp).cloned().collect();
                if v.is_empty() {
                    None
                } else {
                    Some((SlotId(idx as u16), v))
                }
            })
            .collect();
        WriteBack::new(xmm, literal, alias, self.r15)
    }

    pub(in crate::compiler::jitgen) fn get_register(&self) -> WriteBack {
        WriteBack::new(vec![], vec![], vec![], self.r15)
    }

    pub(super) fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let xmm = self
            .xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| {
                            reg.0 as usize <= local_num && matches!(self[**reg], LinkMode::Xmm(_))
                        })
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((Xmm::new(i as u16), v))
                    }
                }
            })
            .collect();
        let literal = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Literal(v) if idx <= local_num => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        let alias: Vec<_> = self
            .alias
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| {
                let v: Vec<_> = v
                    .into_iter()
                    .filter(|slot| slot.0 as usize <= local_num)
                    .cloned()
                    .collect();
                if v.is_empty() {
                    None
                } else {
                    Some((SlotId(idx as u16), v))
                }
            })
            .collect();
        let r15 = match self.r15 {
            Some(slot) if slot.0 as usize <= local_num => Some(slot),
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
    ///
    /// Link slot *reg* to the stack.
    ///
    /// xmm registers corresponding to *reg* are deallocated.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(crate) fn link_stack(&mut self, bb: &mut BBContext, reg: impl Into<Option<SlotId>>) {
        match reg.into() {
            Some(reg) => match bb[reg] {
                LinkMode::Both(freg) | LinkMode::Xmm(freg) => {
                    assert!(bb[freg].contains(&reg));
                    bb[freg].retain(|e| *e != reg);
                    bb[reg] = LinkMode::Stack;
                }
                LinkMode::Alias(origin) => {
                    assert_eq!(bb[origin], LinkMode::Stack);
                    assert!(bb.alias[origin.0 as usize].contains(&reg));
                    bb.alias[origin.0 as usize].retain(|e| *e != reg);
                    bb[reg] = LinkMode::Stack;
                }
                LinkMode::Literal(_) => {
                    bb[reg] = LinkMode::Stack;
                }
                LinkMode::R15 => {
                    bb.r15 = None;
                    bb[reg] = LinkMode::Stack;
                }
                LinkMode::Stack => {
                    // We must write back all aliases of *reg*.
                    let dst = std::mem::take(&mut bb.alias[reg.0 as usize]);
                    if !dst.is_empty() {
                        self.stack2reg(reg, GP::R8);
                        for dst in dst {
                            self.reg2stack(GP::R8, dst);
                            assert_eq!(bb[dst], LinkMode::Alias(reg));
                            bb[dst] = LinkMode::Stack;
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
    pub(in crate::compiler::jitgen) fn link_alias(
        &mut self,
        bb: &mut BBContext,
        origin: SlotId,
        reg: SlotId,
    ) {
        self.link_stack(bb, reg);
        if bb[origin] != LinkMode::Stack {
            unreachable!("origin:{:?} reg:{:?} {:?}", origin, reg, bb);
        };
        bb[reg] = LinkMode::Alias(origin);
        bb.alias[origin.0 as usize].push(reg);
    }

    ///
    /// Link the slot *reg* to the given xmm register *freg*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn link_xmm(
        &mut self,
        bb: &mut BBContext,
        reg: SlotId,
        freg: Xmm,
    ) {
        self.link_stack(bb, reg);
        bb[reg] = LinkMode::Xmm(freg);
        bb[freg].push(reg);
    }

    ///
    /// Link the slot *reg* to a new xmm register.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_new_xmm(&mut self, bb: &mut BBContext, reg: SlotId) -> Xmm {
        let freg = bb.alloc_xmm();
        self.link_xmm(bb, reg, freg);
        freg
    }

    ///
    /// Link the slot *reg* to both of the stack and the given xmm register *freg*.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(super) fn link_both(&mut self, bb: &mut BBContext, reg: SlotId, freg: Xmm) {
        self.link_stack(bb, reg);
        bb[reg] = LinkMode::Both(freg);
        bb[freg].push(reg);
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
        reg: SlotId,
    ) -> Xmm {
        let x = bb.alloc_xmm();
        self.link_both(bb, reg, x);
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
        reg: SlotId,
        v: Value,
    ) {
        self.link_stack(bb, reg);
        bb[reg] = LinkMode::Literal(v);
    }

    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn link_r15(
        &mut self,
        bb: &mut BBContext,
        reg: impl Into<Option<SlotId>>,
    ) {
        if let Some(reg) = reg.into() {
            assert!(bb.r15.is_none());
            self.link_stack(bb, reg);
            bb[reg] = LinkMode::R15;
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
            self.link_stack(bb, i)
        }
    }

    ///
    /// Clear the LinkMode::R15 slot.
    ///
    /// ### destroy
    /// - r8
    ///
    pub(in crate::compiler::jitgen) fn clear_r15(&mut self, bb: &mut BBContext) -> Option<SlotId> {
        let res = bb.r15;
        if let Some(r) = res {
            self.link_stack(bb, r);
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
        match bb[src] {
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
                self.link_stack(bb, src);
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
        reg: SlotId,
    ) -> Xmm {
        match bb[reg] {
            LinkMode::Xmm(x) if bb[x].len() == 1 => {
                assert_eq!(reg, bb[x][0]);
                x
            }
            LinkMode::Xmm(_)
            | LinkMode::Both(_)
            | LinkMode::Stack
            | LinkMode::Alias(_)
            | LinkMode::Literal(_)
            | LinkMode::R15 => self.link_new_xmm(bb, reg),
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, bb: &mut BBContext, reg: SlotId) -> u64 {
        self.xmm_write(bb, reg).enc()
    }

    pub(super) fn release_locals(&mut self, bb: &mut BBContext) {
        for reg in 1..1 + bb.local_num as u16 {
            self.link_stack(bb, SlotId(reg));
        }
    }

    ///
    /// Write back LinkMode::Alias slots.
    ///
    /// ### destroy
    /// - rax, r8
    ///
    pub(in crate::compiler::jitgen) fn writeback_alias(&mut self, bb: &mut BBContext) {
        for i in 0..bb.sp.0 {
            let r = SlotId::new(i);
            if let LinkMode::Alias(origin) = bb[r] {
                self.stack2reg(origin, GP::Rax);
                self.reg2stack(GP::Rax, r);
                self.link_stack(bb, r);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(in crate::compiler::jitgen) struct MergeContext(BBContext);

impl std::ops::Deref for MergeContext {
    type Target = BBContext;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for MergeContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl MergeContext {
    pub(in crate::compiler::jitgen) fn new(bb: &BBContext) -> Self {
        MergeContext(bb.clone())
    }

    pub(in crate::compiler::jitgen) fn get(self) -> BBContext {
        self.0
    }

    pub(in crate::compiler::jitgen) fn merge(&mut self, other: &SlotState) {
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            match (&self[i], &other[i]) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => self.link_both(i, *l),
                (LinkMode::Both(l), LinkMode::Literal(r)) if r.is_float() => self.link_both(i, *l),
                (LinkMode::Literal(l), LinkMode::Both(_)) if l.is_float() => {
                    self.link_new_both(i);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => self.link_xmm(i, *l),
                (LinkMode::Xmm(l), LinkMode::Literal(r)) if r.is_float() => self.link_xmm(i, *l),
                (LinkMode::Literal(l), LinkMode::Xmm(_)) if l.is_float() => {
                    self.link_new_xmm(i);
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => self.link_literal(i, *l),
                _ => self.link_clear(i),
            };
        }
    }

    ///
    /// Link slot *reg* to the stack.
    ///
    /// xmm registers corresponding to *reg* are deallocated.
    ///
    pub(super) fn link_clear(&mut self, reg: impl Into<Option<SlotId>>) {
        match reg.into() {
            Some(reg) => match self[reg] {
                LinkMode::Both(freg) | LinkMode::Xmm(freg) => {
                    assert!(self[freg].contains(&reg));
                    self[freg].retain(|e| *e != reg);
                    self[reg] = LinkMode::Stack;
                }
                LinkMode::Alias(origin) => {
                    assert_eq!(self[origin], LinkMode::Stack);
                    assert!(self.alias[origin.0 as usize].contains(&reg));
                    self[reg] = LinkMode::Stack;
                    self.alias[origin.0 as usize].retain(|e| *e != reg);
                }
                LinkMode::Literal(_) => {
                    self[reg] = LinkMode::Stack;
                }
                LinkMode::R15 => {
                    self.r15 = None;
                    self[reg] = LinkMode::Stack;
                }
                LinkMode::Stack => {
                    // We must write back all aliases of *reg*.
                    let dst = std::mem::take(&mut self.alias[reg.0 as usize]);
                    if !dst.is_empty() {
                        for dst in dst {
                            assert_eq!(self[dst], LinkMode::Alias(reg));
                            self[dst] = LinkMode::Stack;
                        }
                    }
                }
            },
            None => {}
        }
    }

    ///
    /// Link the slot *reg* to the given xmm register *freg*.
    ///
    fn link_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.link_clear(reg);
        self[reg] = LinkMode::Xmm(freg);
        self[freg].push(reg);
    }

    ///
    /// Link the slot *reg* to a new xmm register.
    ///
    fn link_new_xmm(&mut self, reg: SlotId) -> Xmm {
        let freg = self.alloc_xmm();
        self.link_xmm(reg, freg);
        freg
    }

    ///
    /// Link the slot *reg* to both of the stack and the given xmm register *freg*.
    ///
    fn link_both(&mut self, reg: SlotId, freg: Xmm) {
        self.link_clear(reg);
        self[reg] = LinkMode::Both(freg);
        self[freg].push(reg);
    }

    ///
    /// Link the slot *reg* to both of the stack and a new xmm register.
    ///
    fn link_new_both(&mut self, reg: SlotId) -> Xmm {
        let x = self.alloc_xmm();
        self.link_both(reg, x);
        x
    }

    ///
    /// Link the slot *reg* to a literal value *v*.
    ///
    fn link_literal(&mut self, reg: SlotId, v: Value) {
        self.link_clear(reg);
        self[reg] = LinkMode::Literal(v);
    }

    pub(in crate::compiler::jitgen) fn remove_unused(&mut self, unused: &[SlotId]) {
        unused.iter().for_each(|reg| self.link_clear(*reg));
    }
}
