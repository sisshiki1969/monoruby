use super::*;

pub(crate) struct AsmIr {
    pub(super) inst: Vec<AsmInst>,
    pub(super) side_exit: Vec<SideExit>,
    pub(super) label: usize,
}

impl std::ops::Deref for AsmIr {
    type Target = Vec<AsmInst>;
    fn deref(&self) -> &Self::Target {
        &self.inst
    }
}

impl std::ops::DerefMut for AsmIr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inst
    }
}

impl AsmIr {
    pub fn new() -> Self {
        Self {
            inst: vec![],
            side_exit: vec![],
            label: 0,
        }
    }

    pub(super) fn new_deopt(&mut self, pc: BcPc, wb: WriteBack) -> usize {
        let label = self.new_label();
        self.side_exit.push(SideExit::Deoptimize(pc, wb, label));
        label
    }

    pub(super) fn new_error(&mut self, pc: BcPc, wb: WriteBack) -> usize {
        let label = self.new_label();
        self.side_exit.push(SideExit::Error(pc, wb, label));
        label
    }

    fn new_label(&mut self) -> usize {
        let label = self.label;
        self.label += 1;
        label
    }

    pub(super) fn save_rax_to_acc(&mut self, ctx: &mut BBContext, dst: SlotId) {
        if let Some(acc) = ctx.clear_r15() {
            self.push(AsmInst::AccToStack(acc));
        }
        self.push(AsmInst::RaxToAcc);
        ctx.clear();
        ctx.link_r15(dst);
    }
}

pub(crate) enum AsmInst {
    AccToStack(SlotId),
    RaxToAcc,

    XmmMove(Xmm, Xmm),
    XmmSwap(Xmm, Xmm),

    F64ToXmm(f64, Xmm),
    I64ToBoth(i64, SlotId, Xmm),
    XmmToBoth(Xmm, Vec<SlotId>),
    LitToStack(Value, SlotId),
    NumToXmm(SlotId, Xmm, usize),
    IntToXmm(Option<SlotId>, Xmm, usize),
    FloatToXmm(Option<SlotId>, Xmm, usize),

    GuardFloat(SlotId, usize),

    NewArrayToRax(CallSiteId, Vec<Xmm>),
    NewHashToRax(SlotId, usize, Vec<Xmm>),
    NewRangeToRax(SlotId, SlotId, bool, Vec<Xmm>, usize),
}

pub(super) enum SideExit {
    Deoptimize(BcPc, WriteBack, usize),
    Error(BcPc, WriteBack, usize),
}
