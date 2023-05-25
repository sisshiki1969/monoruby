use super::*;

#[derive(Clone, PartialEq)]
pub(super) struct StackSlotInfo {
    slots: Vec<LinkMode>,
    /// Information for xmm registers.
    xmm: XmmInfo,
    local_num: usize,
}

impl std::fmt::Debug for StackSlotInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .slots
            .iter()
            .enumerate()
            .flat_map(|(i, mode)| match mode {
                LinkMode::Stack => None,
                LinkMode::Const(v) => Some(format!("%{i}:Const({:?}) ", v)),
                LinkMode::Both(x) => Some(format!("%{i}:Both({x:?}) ")),
                LinkMode::Xmm(x) => Some(format!("%{i}:Xmm({x:?}) ")),
            })
            .collect();
        write!(f, "[{s}]")
    }
}

impl std::ops::Index<SlotId> for StackSlotInfo {
    type Output = LinkMode;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.slots[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for StackSlotInfo {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.slots[i.0 as usize]
    }
}

impl std::ops::Index<Xmm> for StackSlotInfo {
    type Output = [SlotId];
    fn index(&self, i: Xmm) -> &Self::Output {
        &self.xmm[i]
    }
}

impl StackSlotInfo {
    pub(super) fn new(cc: &JitContext) -> Self {
        StackSlotInfo {
            slots: vec![LinkMode::Stack; cc.total_reg_num],
            xmm: XmmInfo::new(),
            local_num: cc.local_num,
        }
    }

    pub(super) fn len(&self) -> usize {
        self.slots.len()
    }

    ///
    /// Allocate a new xmm register.
    ///
    fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.0.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        unreachable!("no xmm reg is vacant.")
    }

    pub(super) fn link_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.dealloc_xmm(reg);
        self[reg] = LinkMode::Xmm(freg);
        self.xmm[freg].push(reg);
    }

    pub(super) fn link_new_xmm(&mut self, reg: SlotId) -> Xmm {
        let freg = self.alloc_xmm();
        self.link_xmm(reg, freg);
        freg
    }

    pub(super) fn link_both(&mut self, reg: SlotId, freg: Xmm) {
        self.dealloc_xmm(reg);
        self[reg] = LinkMode::Both(freg);
        self.xmm[freg].push(reg);
    }

    pub(super) fn link_new_both(&mut self, reg: SlotId) -> Xmm {
        let freg = self.alloc_xmm();
        self.link_both(reg, freg);
        freg
    }

    pub(super) fn link_const(&mut self, reg: SlotId, v: Value) {
        self.dealloc_xmm(reg);
        self[reg] = LinkMode::Const(v);
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    pub(super) fn dealloc_xmm(&mut self, reg: SlotId) {
        match self[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => {
                assert!(self.xmm[freg].contains(&reg));
                self.xmm[freg].retain(|e| *e != reg);
                self[reg] = LinkMode::Stack;
            }
            LinkMode::Const(_) => {
                self[reg] = LinkMode::Stack;
            }
            LinkMode::Stack => {}
        }
    }

    pub(super) fn dealloc_locals(&mut self) {
        for reg in 1..1 + self.local_num as u16 {
            self.dealloc_xmm(SlotId(reg));
        }
    }

    pub(super) fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        self.xmm.0.swap(l.0 as usize, r.0 as usize);
        self.slots.iter_mut().for_each(|mode| match mode {
            LinkMode::Both(x) | LinkMode::Xmm(x) => {
                if *x == l {
                    *x = r;
                } else if *x == r {
                    *x = l;
                }
            }
            LinkMode::Stack | LinkMode::Const(_) => {}
        });
    }

    ///
    /// Allocate new xmm register to the given stack slot for read/write f64.
    ///
    pub(super) fn xmm_write(&mut self, reg: SlotId) -> Xmm {
        match self[reg] {
            LinkMode::Xmm(freg) if self.xmm[freg].len() == 1 => {
                assert_eq!(reg, self.xmm[freg][0]);
                freg
            }
            LinkMode::Xmm(_) | LinkMode::Both(_) | LinkMode::Stack | LinkMode::Const(_) => {
                self.dealloc_xmm(reg);
                let freg = self.alloc_xmm();
                self.link_xmm(reg, freg);
                freg
            }
        }
    }

    pub(super) fn merge(&mut self, other: &BBContext) {
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            match (&self[i], &other[i]) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => self.link_both(i, *l),
                (LinkMode::Both(l), LinkMode::Const(r)) if r.class() == FLOAT_CLASS => {
                    self.link_both(i, *l)
                }
                (LinkMode::Const(l), LinkMode::Both(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_both(i, x);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => self.link_xmm(i, *l),
                (LinkMode::Xmm(l), LinkMode::Const(r)) if r.class() == FLOAT_CLASS => {
                    self.link_xmm(i, *l)
                }
                (LinkMode::Const(l), LinkMode::Xmm(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_xmm(i, x);
                }
                (LinkMode::Const(l), LinkMode::Const(r)) if l == r => self.link_const(i, *l),
                _ => self.dealloc_xmm(i),
            };
        }
    }

    pub(super) fn get_write_back(&self) -> WriteBack {
        let xmm = self
            .xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm.0[i]
                        .iter()
                        .filter(|reg| matches!(self[**reg], LinkMode::Xmm(_)))
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
        let constants = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Const(v) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, constants)
    }

    pub(super) fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let xmm = self
            .xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm.0[i]
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
        let fixnum = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Const(v) if idx <= local_num => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, fixnum)
    }

    pub(super) fn get_xmm_using(&self) -> Vec<Xmm> {
        self.xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    Some(Xmm::new(i as u16))
                }
            })
            .collect()
    }
}
