use super::*;

impl BBContext {
    ///
    /// Join contexts.
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
        if !other.frame_capture_guarded {
            self.unset_frame_capture_guard();
        }
        for i in self.all_regs() {
            self.is_used_mut(i).join(other.is_used(i));
            match (self.mode(i), other.mode(i)) {
                (LinkMode::None, LinkMode::None) => {}
                (LinkMode::MaybeNone, _) => {}
                (_, LinkMode::MaybeNone) => {
                    self.set_MaybeNone(i);
                }
                (LinkMode::V, _) => {}
                (_, LinkMode::V) => {
                    self.discard(i);
                }
                (LinkMode::F(_), LinkMode::F(_)) => {}
                (LinkMode::F(_), LinkMode::C(r)) if r.is_float() => {}
                (LinkMode::F(x), LinkMode::Sf(_, _))
                | (LinkMode::Sf(x, _), LinkMode::Sf(_, _) | LinkMode::F(_)) => {
                    let mut guarded = match self.mode(i) {
                        LinkMode::F(_) => SfGuarded::Float,
                        LinkMode::Sf(_, guarded) => guarded,
                        _ => unreachable!(),
                    };
                    let other = match other.mode(i) {
                        LinkMode::F(_) => SfGuarded::Float,
                        LinkMode::Sf(_, guarded) => guarded,
                        _ => unreachable!(),
                    };
                    guarded.join(other);
                    self.set_Sf(i, x, guarded);
                }
                (LinkMode::Sf(x, mut guarded), LinkMode::C(r)) if r.is_float() || r.is_fixnum() => {
                    guarded.join(SfGuarded::from_concrete_value(r));
                    self.set_Sf(i, x, guarded)
                }
                (LinkMode::C(v), LinkMode::F(_)) if v.is_float() => {
                    self.set_new_F(i);
                }
                (LinkMode::C(v), LinkMode::Sf(_, r)) if v.is_float() || v.is_fixnum() => {
                    let mut guarded = SfGuarded::from_concrete_value(v);
                    guarded.join(r);
                    self.set_new_Sf(i, guarded);
                }
                (LinkMode::C(l), LinkMode::C(r)) if l == r => {}
                (LinkMode::C(l), LinkMode::C(r)) if l.is_float() && r.is_float() => {
                    self.set_new_F(i);
                }
                _ => {
                    let guarded = self.guarded(i).join(&other.guarded(i));
                    self.set_S_with_guard(i, guarded);
                }
            };
        }
    }

    ///
    /// Generate bridge AsmIr from F/Sf(l) to Sf(r).
    ///
    fn to_sf(&mut self, ir: &mut AsmIr, slot: SlotId, l: Xmm, r: Xmm, guarded: SfGuarded) {
        if self.is_xmm_vacant(r) {
            self.set_Sf(slot, r, guarded);
            ir.xmm_move(l, r);
        } else {
            self.xmm_swap(ir, l, r);
        }
    }

    ///
    /// Generate bridge AsmIr to merge current state(*bbctx*) with target state(*target*)
    ///
    pub(in crate::codegen::jitgen) fn gen_bridge(
        mut self,
        ir: &mut AsmIr,
        src_bb: Option<BasicBlockId>,
        target: &SlotContext,
        pc: BytecodePtr,
        killed: &[SlotId],
    ) {
        #[cfg(feature = "jit-debug")]
        eprintln!("    from: {src_bb:?} {:?}", self.slot_state);

        for slot in self.all_regs() {
            if killed.contains(&slot) {
                self.discard(slot);
                continue;
            }
            match (self.mode(slot), target.mode(slot)) {
                (LinkMode::V, LinkMode::V) => {}
                (_, LinkMode::V) => {
                    self.discard(slot);
                }
                (LinkMode::F(l), LinkMode::F(r)) => {
                    if l != r {
                        if self.is_xmm_vacant(r) {
                            self.set_F(slot, r);
                            ir.xmm_move(l, r);
                        } else {
                            self.xmm_swap(ir, l, r);
                        }
                    }
                }
                (LinkMode::F(l), LinkMode::Sf(r, SfGuarded::Float)) => {
                    ir.xmm2stack(l, slot);
                    if l == r {
                        // F(l) -> Sf(l)
                        self.set_Sf_float(slot, l);
                    } else {
                        // F(l) -> Sf(r)
                        self.to_sf(ir, slot, l, r, SfGuarded::Float);
                    }
                }
                (LinkMode::Sf(l, _), LinkMode::Sf(r, guarded)) => {
                    if l != r {
                        // Sf(l) -> Sf(r)
                        self.to_sf(ir, slot, l, r, guarded);
                    }
                }
                (LinkMode::Sf(_, guarded), LinkMode::S(_)) => {
                    self.set_S_with_guard(slot, guarded.into());
                }
                (LinkMode::S(_), LinkMode::Sf(x, SfGuarded::Float)) => {
                    // S -> Sf
                    ir.stack2reg(slot, GP::Rax);
                    let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                    if self.is_xmm_vacant(x) {
                        ir.float_to_xmm(GP::Rax, x, deopt);
                        self.set_Sf_float(slot, x);
                    } else {
                        let tmp = self.set_new_Sf(slot, SfGuarded::Float);
                        ir.float_to_xmm(GP::Rax, tmp, deopt);
                        self.xmm_swap(ir, x, tmp);
                    }
                }
                (LinkMode::G(_), LinkMode::Sf(x, SfGuarded::Float)) => {
                    // G -> Sf
                    //ir.stack2reg(slot, GP::Rax);
                    let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                    if self.is_xmm_vacant(x) {
                        ir.float_to_xmm(GP::R15, x, deopt);
                        self.set_Sf_float(slot, x);
                    } else {
                        let tmp = self.set_new_Sf(slot, SfGuarded::Float);
                        ir.float_to_xmm(GP::R15, tmp, deopt);
                        self.xmm_swap(ir, x, tmp);
                    }
                }
                (LinkMode::S(_), LinkMode::S(guarded)) => {
                    if let Some(class) = guarded.class()
                        && !self.is_class(slot, class)
                    {
                        let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                        ir.stack2reg(slot, GP::Rax);
                        ir.push(AsmInst::GuardClass(GP::Rax, class, deopt));
                        self.set_S_with_guard(slot, guarded);
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
                (LinkMode::C(l), LinkMode::Sf(r, _)) => {
                    if let Some(f) = l.try_float() {
                        self.set_Sf_float(slot, r);
                        ir.f64_to_xmm(f, r);
                        ir.lit2reg(Value::float(f), GP::Rax);
                        ir.reg2stack(GP::Rax, slot);
                    } else if let Some(i) = l.try_fixnum() {
                        self.set_Sf_float(slot, r);
                        ir.f64_to_xmm(i as f64, r);
                        ir.lit2reg(Value::fixnum(i), GP::Rax);
                        ir.reg2stack(GP::Rax, slot);
                    } else {
                        unreachable!()
                    }
                }
                (LinkMode::C(_) | LinkMode::F(_) | LinkMode::G(_), LinkMode::S(_)) => {
                    self.write_back_slot(ir, slot);
                }
                (LinkMode::None, LinkMode::None) => {}
                (LinkMode::MaybeNone, LinkMode::MaybeNone) => {}
                (l, r) => {
                    eprintln!("{src_bb:?}: {:?}", self.slot_state);
                    eprintln!("target: {:?}", target);
                    unreachable!("{slot:?} src:{l:?} target:{r:?}");
                }
            }
        }
    }
}
