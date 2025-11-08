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
                    self.set_S_with_guard(i, guarded);
                }
            };
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

    ///
    /// Generate bridge AsmIr to merge current state(*bbctx*) with target state(*target*)
    ///
    pub(in crate::codegen::jitgen) fn gen_bridge(
        mut self,
        ir: &mut AsmIr,
        target: &SlotContext,
        pc: BytecodePtr,
        unused: &[SlotId],
    ) {
        #[cfg(feature = "jit-debug")]
        {
            eprintln!("    src:    {:?}", self.slot_state);
            eprintln!("    target: {:?}", target);
        }
        for slot in self.all_regs() {
            if unused.contains(&slot) {
                self.discard(slot);
                continue;
            }
            let guarded = target.guarded(slot);
            match (self.mode(slot), target.mode(slot)) {
                (LinkMode::V, LinkMode::V) => {}
                (_, LinkMode::V) => {
                    self.discard(slot);
                }
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
                    if let Some(class) = guarded.class()
                        && !self.is_class(slot, class)
                    {
                        let deopt = ir.new_deopt_with_pc(&self, pc + 1);
                        ir.stack2reg(slot, GP::Rax);
                        ir.push(AsmInst::GuardClass(GP::Rax, class, deopt));
                        self.set_guard_class(slot, class);
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
                (LinkMode::C(_) | LinkMode::F(_) | LinkMode::G, LinkMode::S) => {
                    self.write_back_slot(ir, slot);
                }
                (LinkMode::None, LinkMode::None) => {}
                (LinkMode::MaybeNone, LinkMode::MaybeNone) => {}
                (l, r) => {
                    eprintln!("self: {:?}", self.slot_state);
                    eprintln!("target: {:?}", target);
                    unreachable!("{slot:?} src:{l:?} target:{r:?}");
                }
            }
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  bridge end");
    }
}
