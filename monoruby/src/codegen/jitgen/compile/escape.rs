//!
//! Escape analysis: allocation elision for Array literals whose only use is
//! the method call they are the receiver of (`[a, b].max`,
//! `[:x, :y].include?(v)`). See `doc/escape_analysis.md`.
//!
//! An Array literal into a temporary is not allocated when it is compiled.
//! The abstract state records it as an elided object instead
//! (`SlotState::def_virtual`), and the object is either *consumed* by a
//! use that needs no real Array, or *materialized* — allocated after all,
//! exactly as the literal would have been — the moment anything else could
//! observe it. A side exit in between performs the allocation in its write
//! back (`WriteBack::virtual_obj`), so the interpreter never sees the
//! difference.
//!
//! What a use may do with an elided receiver is not decided here: it is the
//! callee's receiver escape summary (`RecvEscape`), declared next to the
//! builtin's definition.
//!

use crate::executor::inline::{ElementsOp, RecvEscape};

use super::*;

impl<'a> JitContext<'a> {
    ///
    /// Elide the allocation of the Array literal `dst = [args..]`, if it
    /// qualifies: no splat, into a temporary, and with its elements in
    /// temporaries that are dead once the literal is built (so nothing in
    /// the bytecode reads them again and only a later push can overwrite
    /// them, which [`Self::settle_virtuals`] watches for).
    ///
    pub(super) fn try_elide_array(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        callid: CallSiteId,
    ) -> bool {
        let cs = &self.store[callid];
        if !cs.splat_pos().is_empty() {
            return false;
        }
        let (src, len) = (cs.args, cs.pos_num as u16);
        if !self.is_temp(dst) || (len != 0 && (!self.is_temp(src) || src < state.next_sp())) {
            return false;
        }
        state.write_back_range(ir, src, len);
        state.def_virtual(dst, VirtualSrc::Slots { src, len, callid });
        true
    }

    ///
    /// Elide the deep copy of a literal Array `dst = literal[..]` whose
    /// elements are all immediates: the copy is indistinguishable from the
    /// template to any use that does not let its receiver escape, so such a
    /// use is handed the template itself.
    ///
    pub(super) fn try_elide_literal(
        &mut self,
        state: &mut AbstractState,
        dst: SlotId,
        val: Value,
    ) -> bool {
        if !self.is_temp(dst) || val.class() != ARRAY_CLASS {
            return false;
        }
        let Some(ary) = val.try_array_ty() else {
            return false;
        };
        if !ary.iter().all(|v| v.is_packed_value()) {
            return false;
        }
        state.def_virtual(dst, VirtualSrc::Literal(val));
        true
    }

    fn is_temp(&self, slot: SlotId) -> bool {
        slot.0 as usize > self.iseq().local_num()
    }

    ///
    /// Run before every instruction: materialize (or drop) each elided
    /// object the instruction could observe.
    ///
    /// An elided object survives an instruction only when
    /// - the instruction may consume it (a method call with it as the
    ///   receiver, which decides for itself — [`Self::consume_virtual`]); or
    /// - it is dead after the instruction, which does not read it (dropped);
    ///   or
    /// - the instruction neither reads it, nor can run Ruby code or reach
    ///   a call ([`virtual_survives`]), nor can overwrite one of its
    ///   element slots (a push at or above them), nor ends the basic block.
    ///
    /// So an elided object never lives across a call (no suspended frame
    /// owes one, and dispatch arms never see one) or across a block
    /// boundary (no merge has to reconcile one).
    ///
    pub(super) fn settle_virtuals(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        trace_ir: &TraceIr,
        bc_pos: BcIndex,
    ) {
        if state.virtuals().is_empty() {
            return;
        }
        let reads = trace_ir.read_slots(self.store);
        let next_sp = state.next_sp();
        let bb_end = self.is_bb_end(bc_pos);
        let survives = virtual_survives(trace_ir);
        for (dst, src) in state.virtuals().to_vec() {
            if self.may_consume_virtual(trace_ir, dst) {
                continue;
            }
            let read = reads.contains(&dst);
            if !read && dst >= next_sp {
                // Dead, and never observed: the allocation is simply gone.
                state.take_virtual(dst);
                state.discard_dead_elements(src);
                continue;
            }
            let overwrites = match src {
                VirtualSrc::Slots { src, len, .. } => len != 0 && next_sp >= src,
                VirtualSrc::Literal(_) => false,
            };
            if read || bb_end || overwrites || !survives {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    ///
    /// Run after an instruction that falls through: if it ends its basic
    /// block, perform the elided allocations still live (dropping the dead
    /// ones), so that no merge ever sees one.
    ///
    pub(super) fn settle_virtuals_at_bb_end(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        bc_pos: BcIndex,
    ) {
        if !self.is_bb_end(bc_pos) {
            return;
        }
        let next_sp = state.next_sp();
        for (dst, src) in state.virtuals().to_vec() {
            if dst >= next_sp {
                state.take_virtual(dst);
                state.discard_dead_elements(src);
            } else {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    fn is_bb_end(&self, bc_pos: BcIndex) -> bool {
        let next = bc_pos + 1usize;
        next.to_usize() >= self.iseq().bytecode().len()
            || self.iseq().bb_info.is_bb_head(next).is_some()
    }

    ///
    /// Whether *trace_ir* is a use that may consume the elided object in
    /// *slot* without allocating it: a method call with it as the receiver
    /// and nowhere else.
    ///
    fn may_consume_virtual(&self, trace_ir: &TraceIr, slot: SlotId) -> bool {
        let TraceIr::MethodCall { callid, .. } = trace_ir else {
            return false;
        };
        let cs = &self.store[*callid];
        cs.recv == slot
            && cs.splat_pos().is_empty()
            && !trace_ir.read_slots(self.store)[1..].contains(&slot)
    }

    ///
    /// Perform the allocation of the elided object in *dst* after all,
    /// exactly as its literal would have.
    ///
    pub(super) fn materialize_virtual(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
    ) {
        let src = state.take_virtual(dst).unwrap();
        match src {
            VirtualSrc::Slots { src, len, callid } => {
                state.write_back_range(ir, src, len);
                let using_fpr = state.get_using_fpr(ir);
                let inline = if len as usize <= ARRAY_INLINE_CAPA {
                    Some((src, len))
                } else {
                    None
                };
                ir.new_array(using_fpr, callid, inline);
                state.def_reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            VirtualSrc::Literal(val) => {
                let using_fpr = state.get_using_fpr(ir);
                ir.deep_copy_lit(using_fpr, val);
                state.def_reg2acc_concrete_value(ir, GP::Rax, dst, val);
            }
        }
        state.discard_dead_elements(src);
    }

    ///
    /// The method call *callid* has an elided object as its receiver
    /// ([`Self::may_consume_virtual`]). Consume it if the callee's receiver
    /// escape summary allows, else materialize it.
    ///
    /// Returns `Some` when the whole call was lowered here; `None` when the
    /// ordinary method-call lowering has to follow — on the materialized
    /// receiver, or, for a literal of immediates and a callee that lets no
    /// receiver escape, on the literal's template.
    ///
    pub(super) fn consume_virtual(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        bc_pos: BcIndex,
    ) -> Option<CompileResult> {
        let cs = &self.store[callid];
        let recv = cs.recv;
        let src = state.virtual_at(recv).unwrap();
        let array = CachedClass::from_class(ARRAY_CLASS);
        // Resolved as the call itself would be (refinements in scope
        // included), and a private method stays the interpreter's to reject.
        let resolved = cs.name.and_then(|name| {
            let (fid, visibility) = self.jit_check_method(array, name)?;
            (!self.jit_visibility_blocks(callid, visibility)).then_some((name, fid))
        });
        let summary = resolved.and_then(|(_, fid)| self.store.recv_escape(fid));
        let plain = cs.kw_args().is_empty()
            && cs.hash_splat_pos().is_empty()
            && cs.block_arg.is_none();
        let block_less = plain && cs.block_fid.is_none();
        let (pos_num, call_dst) = (cs.pos_num, cs.dst);
        // The receiver slot must be dead after the call: whatever the call
        // leaves there is never read again.
        let recv_dead = Some(recv) == call_dst || recv >= self.iseq().get_sp(bc_pos);
        match (src, summary) {
            (
                VirtualSrc::Slots { src, len, .. },
                Some(RecvEscape {
                    elements: Some(op), ..
                }),
            ) if block_less && pos_num == 0 => {
                // The resolution is this unit's assumption: recorded, so the
                // class-version salvage re-asks it, and guarded while the
                // object is still registered, so a redefinition deopts with
                // it materialized.
                let (name, fid) = resolved.unwrap();
                self.inline_method_cache.push(InlineCacheEntry {
                    recv_class: array,
                    name: Some(name),
                    refinements: self.refinements(),
                    func_id: Some(fid),
                });
                self.guard_class_version(state, ir, true);
                state.take_virtual(recv);
                self.emit_elements_op(state, ir, op, src, len, call_dst);
                Some(CompileResult::Continue)
            }
            (
                VirtualSrc::Literal(val),
                Some(RecvEscape {
                    transparent: true, ..
                }),
            ) if plain && recv_dead => {
                // The ordinary lowering that follows records and guards the
                // same resolution; the guard here comes first, while the
                // object is still registered.
                self.guard_class_version(state, ir, true);
                state.take_virtual(recv);
                state.def_C(recv, val);
                None
            }
            _ => {
                self.materialize_virtual(state, ir, recv);
                None
            }
        }
    }

    fn emit_elements_op(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: ElementsOp,
        src: SlotId,
        len: u16,
        dst: Option<SlotId>,
    ) {
        match op {
            ElementsOp::Min | ElementsOp::Max => {
                // The compare can dispatch a user `<=>` (a side effect) and
                // raise. The elements stay in their slots throughout.
                state.write_back_range(ir, src, len);
                state.discard(dst);
                let using_fpr = state.get_using_fpr(ir);
                let error = ir.new_error(state);
                ir.array_min_max(using_fpr, src, len, op == ElementsOp::Min);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                state.unset_side_effect_guard();
            }
            ElementsOp::Size => state.def_C(dst, Value::integer(len as i64)),
            ElementsOp::Empty => state.def_C(dst, Value::bool(len == 0)),
            ElementsOp::First | ElementsOp::Last => match dst {
                None => {}
                Some(dst) if len == 0 => state.def_C(dst, Value::nil()),
                Some(dst) => {
                    let elem = if op == ElementsOp::First {
                        src
                    } else {
                        src + (len - 1)
                    };
                    if !matches!(state.mode(elem), LinkMode::S(_) | LinkMode::C(_)) {
                        state.flush_gp(ir);
                    }
                    state.copy_slot(ir, elem, dst);
                }
            },
        }
    }
}

///
/// Whether an elided object not involved in *trace_ir* may stay elided
/// across it: the instruction runs no Ruby code and reaches no call (so no
/// frame is suspended with the object owed, and no dispatch arm splits the
/// state).
///
fn virtual_survives(trace_ir: &TraceIr) -> bool {
    matches!(
        trace_ir,
        TraceIr::Mov(..)
            | TraceIr::Literal(..)
            | TraceIr::FrozenLiteral(..)
            | TraceIr::InlineCache
            | TraceIr::Array { .. }
    )
}
