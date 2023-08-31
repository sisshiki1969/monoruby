use super::*;

#[derive(Clone, PartialEq)]
pub(crate) struct SlotState {
    slots: Vec<LinkMode>,
    /// Information for xmm registers.
    xmm: [Vec<SlotId>; 14],
    local_num: usize,
}

impl std::fmt::Debug for SlotState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .slots
            .iter()
            .enumerate()
            .flat_map(|(i, mode)| match mode {
                LinkMode::Stack => None,
                LinkMode::Literal(v) => Some(format!("%{i}:Literal({:?}) ", v)),
                LinkMode::Both(x) => Some(format!("%{i}:Both({x:?}) ")),
                LinkMode::Xmm(x) => Some(format!("%{i}:Xmm({x:?}) ")),
            })
            .collect();
        write!(f, "[{s}]")
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

impl SlotState {
    pub(super) fn new(cc: &JitContext) -> Self {
        SlotState {
            slots: vec![LinkMode::Stack; cc.total_reg_num],
            xmm: {
                let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
                v.try_into().unwrap()
            },
            local_num: cc.local_num,
        }
    }

    pub(super) fn len(&self) -> usize {
        self.slots.len()
    }

    pub(super) fn is_xmm_vacant(&self, xmm: Xmm) -> bool {
        self.xmm[xmm.0 as usize].is_empty()
    }

    ///
    /// Allocate a new xmm register.
    ///
    fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        unreachable!("no xmm reg is vacant.")
    }

    pub(super) fn link_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.unlink_xmm(reg);
        self[reg] = LinkMode::Xmm(freg);
        self.xmm[freg.0 as usize].push(reg);
    }

    pub(super) fn link_new_xmm(&mut self, reg: SlotId) -> Xmm {
        let freg = self.alloc_xmm();
        self.link_xmm(reg, freg);
        freg
    }

    pub(super) fn link_both(&mut self, reg: SlotId, freg: Xmm) {
        self.unlink_xmm(reg);
        self[reg] = LinkMode::Both(freg);
        self.xmm[freg.0 as usize].push(reg);
    }

    pub(super) fn link_new_both(&mut self, reg: SlotId) -> Xmm {
        let freg = self.alloc_xmm();
        self.link_both(reg, freg);
        freg
    }

    pub(super) fn link_const(&mut self, reg: SlotId, v: Value) {
        self.unlink_xmm(reg);
        self[reg] = LinkMode::Literal(v);
    }

    pub(super) fn xmm_to_both(&mut self, freg: Xmm) {
        for i in &self.xmm[freg.0 as usize] {
            self.slots[i.0 as usize] = LinkMode::Both(freg);
        }
    }

    pub(super) fn xmm_slots(&self, i: Xmm) -> &[SlotId] {
        &self.xmm[i.0 as usize]
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    pub(crate) fn unlink_xmm(&mut self, reg: impl Into<Option<SlotId>>) {
        match reg.into() {
            Some(reg) => match self[reg] {
                LinkMode::Both(freg) | LinkMode::Xmm(freg) => {
                    assert!(self.xmm[freg.0 as usize].contains(&reg));
                    self.xmm[freg.0 as usize].retain(|e| *e != reg);
                    self[reg] = LinkMode::Stack;
                }
                LinkMode::Literal(_) => {
                    self[reg] = LinkMode::Stack;
                }
                LinkMode::Stack => {}
            },
            None => {}
        }
    }

    pub(super) fn unlink_locals(&mut self) {
        for reg in 1..1 + self.local_num as u16 {
            self.unlink_xmm(SlotId(reg));
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
            LinkMode::Stack | LinkMode::Literal(_) => {}
        });
    }

    ///
    /// Allocate new xmm register to the given stack slot for read/write f64.
    ///
    pub(super) fn xmm_write(&mut self, reg: SlotId) -> Xmm {
        match self[reg] {
            LinkMode::Xmm(freg) if self.xmm[freg.0 as usize].len() == 1 => {
                assert_eq!(reg, self.xmm[freg.0 as usize][0]);
                freg
            }
            LinkMode::Xmm(_) | LinkMode::Both(_) | LinkMode::Stack | LinkMode::Literal(_) => {
                self.unlink_xmm(reg);
                let freg = self.alloc_xmm();
                self.link_xmm(reg, freg);
                freg
            }
        }
    }

    pub(crate) fn xmm_write_enc(&mut self, reg: SlotId) -> u64 {
        self.xmm_write(reg).enc()
    }

    pub(super) fn merge(&mut self, other: &SlotState) {
        for i in 0..self.slots.len() {
            let i = SlotId(i as u16);
            match (&self[i], &other[i]) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => self.link_both(i, *l),
                (LinkMode::Both(l), LinkMode::Literal(r)) if r.class() == FLOAT_CLASS => {
                    self.link_both(i, *l)
                }
                (LinkMode::Literal(l), LinkMode::Both(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_both(i, x);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => self.link_xmm(i, *l),
                (LinkMode::Xmm(l), LinkMode::Literal(r)) if r.class() == FLOAT_CLASS => {
                    self.link_xmm(i, *l)
                }
                (LinkMode::Literal(l), LinkMode::Xmm(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_xmm(i, x);
                }
                (LinkMode::Literal(l), LinkMode::Literal(r)) if l == r => self.link_const(i, *l),
                _ => self.unlink_xmm(i),
            };
        }
    }

    pub(super) fn get_write_back(&self) -> WriteBack {
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
                LinkMode::Literal(v) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, constants)
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
        let constants = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Literal(v) if idx <= local_num => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, constants)
    }

    pub(crate) fn get_xmm_using(&self) -> Vec<Xmm> {
        self.xmm
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
