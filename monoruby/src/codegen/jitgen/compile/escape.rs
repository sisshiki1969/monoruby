//!
//! Escape analysis: allocation elision for Array, Hash and Range literals
//! whose only use is the method call they are the receiver of (`[a, b].max`,
//! `[:x, :y].include?(v)`, `{a: x}[:a]`, `{get: 1, head: 2}.key?(m)`,
//! `(lo..hi).include?(v)`, `when lo...hi`). See `doc/escape_analysis.md`.
//!
//! A literal into a temporary is not allocated when it is compiled.
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
//! An elided object also crosses into a specialized callee: a `*rest`
//! parameter's `Array` is never built, and an elided literal passed as an
//! argument stays elided in the callee (`VirtualSrc::CallerSlots`, a window
//! of the caller's slots the callee reads through its saved frame pointer
//! or its inline-window offset). Such an object lives in a parameter slot,
//! so its lifetime is a local's: it may survive the callee's own calls (a
//! chain deopt materializes it), is dropped when the parameter is dead, and
//! is materialized when the parameter is read by anything that cannot
//! consume it, or at a basic-block boundary.
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
    /// Elide the deep copy of a literal Array or Hash `dst = literal[..]`
    /// whose elements (keys and values) are all immediates: the copy is
    /// indistinguishable from the template to any use that does not let
    /// its receiver escape, so such a use is handed the template itself.
    ///
    pub(super) fn try_elide_literal(
        &mut self,
        state: &mut AbstractState,
        dst: SlotId,
        val: Value,
    ) -> bool {
        if !self.is_temp(dst) {
            return false;
        }
        let all_immediates = if val.class() == ARRAY_CLASS
            && let Some(ary) = val.try_array_ty()
        {
            ary.iter().all(|v| v.is_packed_value())
        } else if val.class() == HASH_CLASS
            && let Some(h) = val.try_hash_ty()
        {
            h.iter()
                .all(|(k, v)| k.is_packed_value() && v.is_packed_value())
        } else if val.class() == RANGE_CLASS
            && let Some(r) = val.is_range()
        {
            r.start().is_packed_value() && r.end().is_packed_value()
        } else {
            false
        };
        if !all_immediates {
            return false;
        }
        state.def_virtual(dst, VirtualSrc::Literal(val));
        true
    }

    ///
    /// Elide the allocation of the Range `dst = start..end` (`TraceIr::Range`)
    /// if it qualifies: into a temporary, with each endpoint a compile-time
    /// Fixnum or `nil`, or a slot proven to hold a Fixnum — so that building
    /// it later can neither dispatch `<=>` nor fail — and no endpoint
    /// temporary other than `dst`'s own slot below the stack top (such a
    /// slot is pinned until the object is materialized or consumed, like an
    /// Array literal's elements).
    ///
    pub(super) fn try_elide_range(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    ) -> bool {
        if !self.is_temp(dst) {
            return false;
        }
        let next_sp = state.next_sp();
        let endpoint = |state: &AbstractState, slot: SlotId| -> Option<RangeEnd> {
            if let LinkMode::C(v) = state.mode(slot)
                && (v.is_fixnum() || v.is_nil())
            {
                return Some(RangeEnd::Const(v));
            }
            if !state.is_fixnum(slot) || (self.is_temp(slot) && slot != dst && slot < next_sp) {
                return None;
            }
            Some(RangeEnd::Slot(slot))
        };
        let (Some(start), Some(end)) = (endpoint(state, start), endpoint(state, end)) else {
            return false;
        };
        let slots: Vec<SlotId> = [start.slot(), end.slot()].into_iter().flatten().collect();
        state.write_back_slots(ir, &slots);
        state.def_virtual(
            dst,
            VirtualSrc::Range {
                start,
                end,
                exclude_end,
            },
        );
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
    ///   receiver or as an argument handed over elided, which decides for
    ///   itself — [`Self::consume_virtual`], `LinkMode::from_caller`); or
    /// - it is dead after the instruction, which does not read it (dropped);
    ///   or
    /// - the instruction neither reads it, nor can overwrite one of its
    ///   element slots (a push at or above them, or a store into a local
    ///   that is an elided Range's endpoint), nor ends the basic block,
    ///   nor — for an object in a temporary — can run Ruby code or reach a
    ///   call ([`virtual_survives`]).
    ///
    /// So an elided object in a temporary never lives across a call (no
    /// suspended frame owes one, and dispatch arms never see one) nor
    /// across a block boundary. An object in a parameter slot
    /// (`CallerSlots`, or a literal handed over by the caller) does live
    /// across the frame's own calls — its elements are in the caller's
    /// frame, out of this frame's way, and a chain deopt materializes it —
    /// in a body that can neither capture its locals nor rescue
    /// (`caller_slots_ok`); and it lives across a forward block boundary
    /// into a merge ([`Self::merge_carries_virtuals`]), where the merge
    /// keeps it if every path brings the same object and the other paths
    /// materialize theirs on their bridges (`JitContext::settle_entries`).
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
        let carries = bb_end && self.merge_carries_virtuals(bc_pos);
        let survives = virtual_survives(trace_ir);
        let written = survives.then(|| written_slot(trace_ir)).flatten();
        // A call to `binding` / `eval` reads every local of this frame
        // through the frame it captures (`unbox_to_S_for_outgoing_block`),
        // a parameter holding an elided object included.
        let captures = matches!(trace_ir, TraceIr::MethodCall { cache: MethodCache::Cached(e), .. }
            if self.store[e.func_id].possibly_capture_without_block());
        for (dst, src) in state.virtuals().to_vec() {
            if self.may_consume_virtual(trace_ir, dst, src) {
                continue;
            }
            let read = reads.contains(&dst) || captures;
            let dead = if self.is_temp(dst) {
                dst >= next_sp
            } else {
                !self.local_live_after(bc_pos, dst)
            };
            if !read && dead {
                // Dead, and never observed: the allocation is simply gone.
                state.take_virtual(dst);
                state.discard_dead_elements(src);
                continue;
            }
            // A push at or above an element slot, or a store into one
            // (a local endpoint of an elided Range). The object's own slot
            // doubling as an endpoint (`a..b` built where `a` was) is not
            // written while the object is elided.
            let overwrites = src
                .own_slots()
                .any(|s| s != dst && ((self.is_temp(s) && s <= next_sp) || written == Some(s)));
            let carried = carries && !self.is_temp(dst);
            if read || (bb_end && !carried) || overwrites || (self.is_temp(dst) && !survives) {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    ///
    /// Run after an instruction that falls through: if it ends its basic
    /// block, perform the elided allocations still live (dropping the dead
    /// ones), except for a parameter's object a merge may carry.
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
        let carries = self.merge_carries_virtuals(bc_pos);
        self.settle_virtuals_before_branch(state, ir, bc_pos, carries);
    }

    ///
    /// Perform the elided allocations still live after the instruction at
    /// *branch_pos* (the block's last one: a branch, or the instruction a
    /// dispatch's per-arm `CondBr` was sunk into, see
    /// [`JitContext::declare_merge`]), dropping the dead ones. With
    /// *carries*, a parameter's object is left for the merge
    /// ([`Self::merge_carries_virtuals`]).
    ///
    pub(super) fn settle_virtuals_before_branch(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        branch_pos: BcIndex,
        carries: bool,
    ) {
        let next_sp = state.next_sp();
        for (dst, src) in state.virtuals().to_vec() {
            let dead = if self.is_temp(dst) {
                dst >= next_sp
            } else {
                !self.local_live_after(branch_pos, dst)
            };
            if dead {
                state.take_virtual(dst);
                state.discard_dead_elements(src);
            } else if !(carries && !self.is_temp(dst)) {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    ///
    /// Whether the merges the block ending at *bc_pos* branches into may
    /// carry an elided object in a parameter slot (`doc/escape_analysis.md`
    /// §3): a forward merge, compiled from the states of its branches
    /// ([`JitContext::settle_entries`] reconciles them). A loop head is
    /// not one: its back edge is a fixpoint over the body, and the body
    /// is analysed from a head without objects, so every entry of a loop
    /// materializes before branching. Nor is a dispatch arm, whose
    /// per-arm branch merges with the fall-through (`declare_merge`).
    ///
    fn merge_carries_virtuals(&self, bc_pos: BcIndex) -> bool {
        if self.in_dispatch_arm() {
            return false;
        }
        let bb_info = &self.iseq().bb_info;
        let bb = bb_info.get_bb_id(bc_pos);
        bb_info
            .succ(bb)
            .iter()
            .all(|s| bb_info.is_loop_begin(*s).is_none())
    }

    ///
    /// Reconcile the elided objects the branch *entries* of a merge bring
    /// in, before their states are joined: an object every entry carries
    /// identically (same slot, same `VirtualSrc`) stays elided through the
    /// merge; any other is materialized on its entry's bridge, in the
    /// returned per-entry `AsmIr` the bridge is prepended with. At a loop
    /// head every object is materialized (none is expected: the entries
    /// settle before branching into one).
    ///
    pub(in crate::codegen::jitgen) fn settle_entries(
        &mut self,
        entries: &mut [BranchEntry],
        loop_head: bool,
    ) -> Vec<AsmIr> {
        let common: Vec<(SlotId, VirtualSrc)> = if loop_head || entries.is_empty() {
            vec![]
        } else {
            entries[0]
                .state
                .virtuals()
                .iter()
                .filter(|v| entries.iter().all(|e| e.state.virtuals().contains(v)))
                .copied()
                .collect()
        };
        entries
            .iter_mut()
            .map(|entry| {
                let mut ir = AsmIr::new(self);
                for (dst, _) in entry.state.virtuals().to_vec() {
                    if common.iter().any(|(d, _)| *d == dst) {
                        continue;
                    }
                    debug_assert!(!loop_head, "an elided object reached a loop head");
                    self.materialize_virtual(&mut entry.state, &mut ir, dst);
                }
                ir
            })
            .collect()
    }

    ///
    /// Whether the local *slot* may be read after the instruction at
    /// *bc_pos*: by the rest of its basic block, or by any block control
    /// can reach from there. Conservative — a write before the read does
    /// not make it dead — and always true in a body with an exception
    /// handler, where the handler may read anything.
    ///
    fn local_live_after(&self, bc_pos: BcIndex, slot: SlotId) -> bool {
        let iseq = self.iseq();
        if iseq.has_exception_handler() {
            return true;
        }
        let reads_in = |from: BcIndex, to: BcIndex| {
            (from.to_usize()..=to.to_usize()).any(|i| {
                TraceIr::from_pc(iseq.get_pc(BcIndex::from(i)), self.store)
                    .read_slots(self.store)
                    .contains(&slot)
            })
        };
        let bb_info = &iseq.bb_info;
        let bb = bb_info.get_bb_id(bc_pos);
        let end = bb_info[bb].end;
        if bc_pos < end && reads_in(bc_pos + 1usize, end) {
            return true;
        }
        let mut visited = vec![false; bb_info.len()];
        let mut stack: Vec<BasicBlockId> = bb_info.succ(bb).to_vec();
        while let Some(b) = stack.pop() {
            if std::mem::replace(&mut visited[b.index()], true) {
                continue;
            }
            let entry = &bb_info[b];
            if reads_in(entry.begin, entry.end) {
                return true;
            }
            stack.extend_from_slice(bb_info.succ(b));
        }
        false
    }

    ///
    /// Whether the slot is dead once the instruction at *bc_pos* is done:
    /// a temporary above the instruction's stack top, or a local nothing
    /// reads afterwards.
    ///
    fn dead_after(&self, bc_pos: BcIndex, slot: SlotId) -> bool {
        if self.is_temp(slot) {
            slot >= self.iseq().get_sp(bc_pos)
        } else {
            !self.local_live_after(bc_pos, slot)
        }
    }

    ///
    /// Whether a specialized callee *fid* may receive elided objects in
    /// its parameters (`VirtualSrc::CallerSlots`, `doc/escape_analysis.md`):
    /// an iseq whose locals nothing can capture (no `binding` / `eval`,
    /// no block literal, no `super`, no `yield`:
    /// `ISeqInfo::forwarding_no_escape`) and that rescues nothing — so a
    /// parameter whose reads are all behind it is dead for good, and an
    /// object the body did not materialize is never seen by anyone.
    ///
    pub(super) fn caller_slots_ok(&self, fid: FuncId) -> bool {
        let info = &self.store[fid];
        let Some(iseq_id) = info.is_iseq() else {
            return false;
        };
        let iseq = &self.store[iseq_id];
        !info.possibly_capture_without_block()
            && !iseq.has_exception_handler()
            && iseq.forwarding_no_escape
    }

    ///
    /// Materialize every elided object among the positional arguments of
    /// the call site *callid*: the call is lowered by a path that does not
    /// hand them over elided (a generic `send`, a native callee, a trivial
    /// fold that reads them).
    ///
    pub(super) fn settle_virtual_args(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
    ) {
        let cs = &self.store[callid];
        for slot in cs.args..cs.args + cs.pos_num {
            match state.virtual_at(slot) {
                None => {}
                // The trampoline's own rest, forwarded on: the fill
                // (`set_arguments`' forwarding arm) routes the caller's
                // window straight into the callee frame, or materializes
                // it itself when it cannot (`materialize_virtual_args`).
                Some(VirtualSrc::CallerSlots {
                    base: SlotBase::SavedFp,
                    ..
                }) if Self::forwarded_rest_slot(cs) == Some(slot) => {}
                Some(_) => self.materialize_virtual(state, ir, slot),
            }
        }
    }

    ///
    /// The slot of the trailing `...` splat of a forwarding call
    /// `g(x.., ...)`, the shape whose fill can take the trampoline's elided
    /// rest without an `Array` (`set_arguments`' forwarding arm).
    ///
    fn forwarded_rest_slot(cs: &CallSiteInfo) -> Option<SlotId> {
        (cs.forwarding && cs.pos_num >= 1 && cs.splat_pos() == [cs.pos_num - 1])
            .then(|| cs.args + (cs.pos_num - 1))
    }

    fn is_bb_end(&self, bc_pos: BcIndex) -> bool {
        let next = bc_pos + 1usize;
        next.to_usize() >= self.iseq().bytecode().len()
            || self.iseq().bb_info.is_bb_head(next).is_some()
    }

    ///
    /// Whether *trace_ir* is a use that may consume the elided object
    /// *src* in *slot* without allocating it: a method call with it as the
    /// receiver and nowhere else ([`Self::consume_virtual`]); a method
    /// call with it as one positional argument whose callee is a known
    /// iseq that the call will be specialized for with the object handed
    /// over (`LinkMode::from_caller`; the paths that end up not
    /// specializing materialize it, [`Self::settle_virtual_args`]); or,
    /// for the rest `Array` of a forwarding trampoline, the `g(...)`
    /// forward that routes the caller's window straight into `g`'s frame
    /// (`SetArgumentsForwarded::deferred_src`, which materializes it when
    /// it cannot).
    ///
    fn may_consume_virtual(&self, trace_ir: &TraceIr, slot: SlotId, src: VirtualSrc) -> bool {
        // `h[k]` on an elided Hash ([`Self::consume_virtual_index`]).
        if let TraceIr::Index { base, idx, .. } = trace_ir {
            return *base == slot && *idx != slot;
        }
        // The prologue's "`nil` means an empty Hash" check of a `**kwrest`
        // handed over elided: nothing to do, the object stands for the
        // Hash (its slot does hold `nil`).
        if let TraceIr::CheckKwRest(local) = trace_ir {
            return *local == slot && matches!(src, VirtualSrc::CallerKw { .. });
        }
        // `range === v` — `case v when lo..hi` ([`Self::consume_virtual_teq`]).
        if let TraceIr::BinCmp {
            kind: CmpKind::TEq,
            lhs,
            rhs,
            ..
        } = trace_ir
        {
            return *lhs == slot && *rhs != slot && Self::range_ends(src).is_some();
        }
        let TraceIr::MethodCall { callid, cache, .. } = trace_ir else {
            return false;
        };
        let cs = &self.store[*callid];
        let reads = trace_ir.read_slots(self.store);
        if cs.recv == slot {
            return cs.splat_pos().is_empty() && !reads[1..].contains(&slot);
        }
        if !(cs.args..cs.args + cs.pos_num).contains(&slot)
            || reads.iter().filter(|s| **s == slot).count() != 1
        {
            return false;
        }
        match src {
            // The rest `Array` of `def f(...)`: `g(x.., ...)` with the
            // trailing `...` splat being this object, in a framed callee
            // (the only kind with a `*rest`).
            VirtualSrc::CallerSlots {
                base: SlotBase::SavedFp,
                ..
            } => Self::forwarded_rest_slot(cs) == Some(slot),
            VirtualSrc::CallerSlots { .. } | VirtualSrc::CallerKw { .. } => false,
            // An elided Range's endpoints are this frame's slots, which
            // the callee cannot be handed.
            VirtualSrc::Range { .. } => false,
            VirtualSrc::Slots { .. } | VirtualSrc::Literal(_) => {
                cs.splat_pos().is_empty()
                    && cs.block_fid.is_none()
                    && cs.block_arg.is_none()
                    && !self.in_dispatch_arm()
                    && matches!(cache, MethodCache::Cached(e)
                        if self.store.is_simple_call(e.func_id, *callid)
                            && self.caller_slots_ok(e.func_id))
            }
        }
    }

    ///
    /// Perform the allocation of the elided object in *dst* after all,
    /// exactly as its literal would have.
    ///
    pub(in crate::codegen::jitgen) fn materialize_virtual(
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
            VirtualSrc::CallerSlots { src, len, base } => {
                let using_fpr = state.get_using_fpr(ir);
                ir.create_array_from(using_fpr, base, src, len);
                state.def_reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            VirtualSrc::CallerKw { base, .. } => {
                state.materialize_caller_kw(ir, dst, base);
            }
            VirtualSrc::Range {
                start,
                end,
                exclude_end,
            } => {
                let using_fpr = state.get_using_fpr(ir);
                ir.create_range(using_fpr, start, end, exclude_end);
                state.def_reg2acc_class(ir, GP::Rax, dst, RANGE_CLASS);
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
        let class = CachedClass::from_class(src.class_id());
        let elements = match src {
            VirtualSrc::Slots { src, len, .. } => Some((SlotBase::Own, src, len)),
            VirtualSrc::CallerSlots { src, len, base } => Some((base, src, len)),
            VirtualSrc::Literal(_) | VirtualSrc::CallerKw { .. } | VirtualSrc::Range { .. } => None,
        };
        // Resolved as the call itself would be (refinements in scope
        // included), and a private method stays the interpreter's to reject.
        let resolved = cs.name.and_then(|name| {
            let (fid, visibility) = self.jit_check_method(class, name)?;
            (!self.jit_visibility_blocks(callid, visibility)).then_some((name, fid))
        });
        let summary = resolved.and_then(|(_, fid)| self.store.recv_escape(fid));
        let plain = cs.kw_args().is_empty()
            && cs.hash_splat_pos().is_empty()
            && cs.block_arg.is_none();
        let block_less = plain && cs.block_fid.is_none();
        let (pos_num, call_dst, args) = (cs.pos_num, cs.dst, cs.args);
        // The receiver slot must be dead after the call: whatever the call
        // leaves there is never read again.
        let recv_dead = Some(recv) == call_dst || self.dead_after(bc_pos, recv);
        // What the elements lowering would do, decided before anything is
        // emitted: an Array's elements are in frame slots, a Hash's pairs
        // are in its template or in the caller's kw window, a Range's
        // endpoints are constants or frame slots.
        let plan = match summary {
            Some(RecvEscape {
                elements: Some(op), ..
            }) if block_less => match (elements, Self::hash_pairs(state, src), Self::range_ends(src))
            {
                (Some((base, src, len)), _, _) if pos_num == 0 => {
                    Some(ElementsPlan::Array(base, src, len))
                }
                (None, Some(pairs), _) => self
                    .plan_hash_op(state, op, &pairs, args, pos_num)
                    .map(ElementsPlan::Hash),
                (None, None, Some(ends)) => self
                    .plan_range_op(op, ends, args, pos_num)
                    .map(ElementsPlan::Range),
                _ => None,
            },
            _ => None,
        };
        match (plan, summary) {
            (Some(plan), _) => {
                // The resolution is this unit's assumption: recorded, so the
                // class-version salvage re-asks it, and guarded while the
                // object is still registered, so a redefinition deopts with
                // it materialized.
                let (name, fid) = resolved.unwrap();
                self.inline_method_cache.push(InlineCacheEntry {
                    recv_class: class,
                    name: Some(name),
                    refinements: self.refinements(),
                    func_id: Some(fid),
                });
                self.guard_class_version(state, ir, true);
                // The operation observes the elements, not the object: a
                // receiver still live afterwards (a parameter read again
                // later) stays elided for its next use.
                if recv_dead {
                    state.take_virtual(recv);
                }
                match plan {
                    ElementsPlan::Array(base, src, len) => {
                        let RecvEscape {
                            elements: Some(op), ..
                        } = summary.unwrap()
                        else {
                            unreachable!()
                        };
                        self.emit_elements_op(state, ir, op, base, src, len, call_dst);
                    }
                    ElementsPlan::Hash(fold) => self.emit_hash_fold(state, ir, fold, call_dst),
                    ElementsPlan::Range(plan) => {
                        self.emit_range_plan(state, ir, plan, fid, call_dst)
                    }
                }
                Some(CompileResult::Continue)
            }
            (
                None,
                Some(RecvEscape {
                    transparent: true, ..
                }),
            ) if plain && recv_dead && matches!(src, VirtualSrc::Literal(_)) => {
                let VirtualSrc::Literal(val) = src else {
                    unreachable!()
                };
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

    ///
    /// The pairs of an elided Hash, in order: a template's keys and
    /// values, or a handed-over `**kwrest`'s names and the caller's slots
    /// of their values (the frame's table, [`AbstractFrame::caller_kw_table`]).
    ///
    fn hash_pairs(state: &AbstractState, src: VirtualSrc) -> Option<Vec<(Value, HashElem)>> {
        match src {
            VirtualSrc::Literal(val) => {
                let h = val.try_hash_ty()?;
                Some(h.iter().map(|(k, v)| (k, HashElem::Const(v))).collect())
            }
            VirtualSrc::CallerKw { base, .. } => {
                let (_, table) = state.caller_kw_table()?;
                Some(
                    table
                        .iter()
                        .map(|(name, slot)| (Value::symbol(*name), HashElem::Caller(base, *slot)))
                        .collect(),
                )
            }
            _ => None,
        }
    }

    ///
    /// The endpoints of an elided Range, each a compile-time constant or a
    /// slot of this frame holding a Fixnum: of a `VirtualSrc::Range`, or
    /// of a literal template whose endpoints are Fixnum or `nil`. `None`
    /// for any other object (a `'a'..'z'` template included: its element
    /// ops are not modelled, though a transparent callee still takes it).
    ///
    fn range_ends(src: VirtualSrc) -> Option<(RangeEnd, RangeEnd, bool)> {
        match src {
            VirtualSrc::Range {
                start,
                end,
                exclude_end,
            } => Some((start, end, exclude_end)),
            VirtualSrc::Literal(val) => {
                let r = val.is_range()?;
                let fix_or_nil = |v: Value| v.is_fixnum() || v.is_nil();
                (fix_or_nil(r.start()) && fix_or_nil(r.end())).then(|| {
                    (
                        RangeEnd::Const(r.start()),
                        RangeEnd::Const(r.end()),
                        r.exclude_end(),
                    )
                })
            }
            _ => None,
        }
    }

    ///
    /// `dst = lhs === rhs` (`TraceIr::BinCmp`, `case rhs when lo..hi` or an
    /// explicit `(lo..hi) === v`) with an elided Range in *lhs*
    /// ([`Self::may_consume_virtual`]): `Range#===` consumed as a method
    /// call with the object as its receiver would be ([`Self::consume_virtual`]),
    /// with the endpoint compare in place. Otherwise the object is
    /// materialized. Returns whether the instruction was lowered here.
    ///
    pub(super) fn consume_virtual_teq(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        lhs: SlotId,
        rhs: SlotId,
        bc_pos: BcIndex,
        // case/when (and rescue) matching dispatches `===` with funcall
        // semantics; a plain `a === b` is a public-only call.
        fused: bool,
    ) -> bool {
        let src = state.virtual_at(lhs).unwrap();
        let class = CachedClass::from_class(src.class_id());
        let resolved = self
            .jit_check_method(class, IdentId::_TEQ)
            .and_then(|(fid, visibility)| {
                let blocked = visibility == Visibility::Private
                    && !fused
                    && !self
                        .store
                        .get_callsite_id(self.iseq_id(), bc_pos)
                        .is_some_and(|c| self.store[c].is_func_call());
                (!blocked).then_some(fid)
            });
        let summary = resolved.and_then(|fid| self.store.recv_escape(fid));
        let recv_dead = Some(lhs) == dst || self.dead_after(bc_pos, lhs);
        let plan = match summary {
            Some(RecvEscape {
                elements: Some(op), ..
            }) => Self::range_ends(src).and_then(|ends| self.plan_range_op(op, ends, rhs, 1)),
            _ => None,
        };
        match (plan, summary) {
            (Some(plan), _) => {
                self.inline_method_cache.push(InlineCacheEntry {
                    recv_class: class,
                    name: Some(IdentId::_TEQ),
                    refinements: self.refinements(),
                    func_id: resolved,
                });
                self.guard_class_version(state, ir, true);
                if recv_dead {
                    state.take_virtual(lhs);
                }
                self.emit_range_plan(state, ir, plan, resolved.unwrap(), dst);
                true
            }
            (
                None,
                Some(RecvEscape {
                    transparent: true, ..
                }),
            ) if recv_dead && matches!(src, VirtualSrc::Literal(_)) => {
                let VirtualSrc::Literal(val) = src else {
                    unreachable!()
                };
                self.guard_class_version(state, ir, true);
                state.take_virtual(lhs);
                state.def_C(lhs, val);
                false
            }
            _ => {
                self.materialize_virtual(state, ir, lhs);
                false
            }
        }
    }

    ///
    /// Decide the Range elements lowering *op* over the endpoints *ends*
    /// for a call with *pos_num* arguments at *args*: the compare of
    /// `include?` and friends in place; `size` / `count` / `sum` / `min` /
    /// `max` / `first` / `last` folded at compile time when both endpoints
    /// are constants and the closed form applies (`runtime::range_fold`,
    /// under the `Integer#<=>` basic-op license), else through the runtime
    /// helper; `begin` / `end` / `exclude_end?` as the endpoint itself.
    ///
    fn plan_range_op(
        &self,
        op: ElementsOp,
        ends: (RangeEnd, RangeEnd, bool),
        args: SlotId,
        pos_num: usize,
    ) -> Option<RangePlan> {
        let (start, end, exclude_end) = ends;
        let endpoint = |e: RangeEnd| match e {
            RangeEnd::Const(v) => RangePlan::Const(v),
            RangeEnd::Slot(s) => RangePlan::Own(s),
        };
        match (op, pos_num) {
            (ElementsOp::RangeCover, 1) => Some(RangePlan::Cover(start, end, exclude_end, args)),
            (
                ElementsOp::RangeSize
                | ElementsOp::RangeCount
                | ElementsOp::RangeSum
                | ElementsOp::Min
                | ElementsOp::Max
                | ElementsOp::First
                | ElementsOp::Last,
                0,
            ) => {
                if let (RangeEnd::Const(s), RangeEnd::Const(e)) = (start, end)
                    && self.basic_op_assumable(CachedClass::INTEGER, IdentId::_CMP)
                    && let Some(v) = runtime::range_fold(op, s, e, exclude_end)
                {
                    return Some(RangePlan::Fold(v));
                }
                Some(RangePlan::Op(op, start, end, exclude_end))
            }
            (ElementsOp::RangeBegin, 0) => Some(endpoint(start)),
            (ElementsOp::RangeEndpoint, 0) => Some(endpoint(end)),
            (ElementsOp::RangeExcludeEnd, 0) => Some(RangePlan::Const(Value::bool(exclude_end))),
            _ => None,
        }
    }

    /// Emit a Range elements lowering ([`Self::plan_range_op`]); *fid* is
    /// the resolved callee, which the helpers dispatch on a Range built
    /// after all when the in-place answer does not apply.
    fn emit_range_plan(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        plan: RangePlan,
        fid: FuncId,
        dst: Option<SlotId>,
    ) {
        match plan {
            RangePlan::Const(v) => state.def_C(dst, v),
            RangePlan::Fold(v) => {
                self.record_bop_dep(CachedClass::INTEGER, IdentId::_CMP);
                state.def_C(dst, v);
            }
            RangePlan::Own(slot) => {
                if let Some(dst) = dst {
                    if !matches!(state.mode(slot), LinkMode::S(_) | LinkMode::C(_)) {
                        state.flush_gp(ir);
                    }
                    state.copy_slot(ir, slot, dst);
                }
            }
            RangePlan::Cover(start, end, exclude_end, arg) => {
                // The inline Fixnum compare stands in for `Integer#<=>` /
                // `Float#<=>` (what `__cover_num_q` requires intact); the
                // helper re-checks at run time.
                let inline = self.basic_op_assumable(CachedClass::INTEGER, IdentId::_CMP)
                    && self.basic_op_assumable(CachedClass::FLOAT, IdentId::_CMP);
                if inline {
                    self.record_bop_dep(CachedClass::INTEGER, IdentId::_CMP);
                    self.record_bop_dep(CachedClass::FLOAT, IdentId::_CMP);
                }
                state.write_back_slots(ir, &[arg]);
                state.discard(dst);
                let using_fpr = state.get_using_fpr(ir);
                let error = ir.new_error(state);
                ir.range_cover(using_fpr, start, end, exclude_end, arg, fid, inline);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                self.after_range_helper(state);
            }
            RangePlan::Op(op, start, end, exclude_end) => {
                state.discard(dst);
                let using_fpr = state.get_using_fpr(ir);
                let error = ir.new_error(state);
                ir.range_op(using_fpr, op, start, end, exclude_end, fid);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                self.after_range_helper(state);
            }
        }
    }

    /// The Range helpers may dispatch the resolved method on a Range built
    /// after all (a non-numeric argument, a `nil` endpoint the method
    /// rejects): arbitrary Ruby, so the cached guards are re-established
    /// afterwards.
    fn after_range_helper(&mut self, state: &mut AbstractState) {
        state.unset_class_version_guard();
        state.unset_const_version_guard();
        state.unset_side_effect_guard();
    }

    ///
    /// `dst = base[idx]` (`TraceIr::Index`) with an elided Hash in *base*
    /// ([`Self::may_consume_virtual`]): a template's `[]` by a constant
    /// key it holds is the value, and by any other key runs on the
    /// template, as [`Self::consume_virtual`] has it for a method call.
    /// Otherwise the object is materialized. Returns whether the
    /// instruction was lowered here.
    ///
    pub(super) fn consume_virtual_index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        dst: SlotId,
        bc_pos: BcIndex,
    ) -> bool {
        let src = state.virtual_at(base).unwrap();
        let class = CachedClass::from_class(src.class_id());
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        let resolved = self.jit_check_method(class, IdentId::_INDEX).and_then(|(fid, visibility)| {
            (!self.jit_visibility_blocks(callid, visibility)).then_some(fid)
        });
        let summary = resolved.and_then(|fid| self.store.recv_escape(fid));
        let recv_dead = base == dst || self.dead_after(bc_pos, base);
        let plan = match summary {
            Some(RecvEscape {
                elements: Some(op), ..
            }) => Self::hash_pairs(state, src)
                .and_then(|pairs| self.plan_hash_op(state, op, &pairs, idx, 1)),
            _ => None,
        };
        match (plan, summary) {
            (Some(fold), _) => {
                self.inline_method_cache.push(InlineCacheEntry {
                    recv_class: class,
                    name: Some(IdentId::_INDEX),
                    refinements: self.refinements(),
                    func_id: resolved,
                });
                self.guard_class_version(state, ir, true);
                if recv_dead {
                    state.take_virtual(base);
                }
                self.emit_hash_fold(state, ir, fold, Some(dst));
                true
            }
            (
                None,
                Some(RecvEscape {
                    transparent: true, ..
                }),
            ) if recv_dead && matches!(src, VirtualSrc::Literal(_)) => {
                let VirtualSrc::Literal(val) = src else {
                    unreachable!()
                };
                self.guard_class_version(state, ir, true);
                state.take_virtual(base);
                state.def_C(base, val);
                false
            }
            _ => {
                self.materialize_virtual(state, ir, base);
                false
            }
        }
    }

    ///
    /// Decide the Hash elements lowering *op* over *pairs* for a call with
    /// *pos_num* arguments at *args*: a lookup is folded only for a key
    /// that is a compile-time packed immediate — for which `eql?` is bit
    /// equality and no `hash` is dispatched — and, for `[]` / `fetch`,
    /// only when it is present (a miss would dispatch `default` or raise).
    ///
    fn plan_hash_op(
        &self,
        state: &AbstractState,
        op: ElementsOp,
        pairs: &[(Value, HashElem)],
        args: SlotId,
        pos_num: usize,
    ) -> Option<HashFold> {
        let const_key = |slot: SlotId| match state.mode(slot) {
            LinkMode::C(k) if k.is_packed_value() => Some(k),
            _ => None,
        };
        let lookup = |k: Value| pairs.iter().find(|(k0, _)| k0.id() == k.id()).map(|(_, v)| *v);
        let found = |v: HashElem| match v {
            HashElem::Const(v) => HashFold::Const(v),
            HashElem::Caller(base, slot) => HashFold::Caller(base, slot),
        };
        match op {
            ElementsOp::Size if pos_num == 0 => {
                Some(HashFold::Const(Value::integer(pairs.len() as i64)))
            }
            ElementsOp::Empty if pos_num == 0 => Some(HashFold::Const(Value::bool(pairs.is_empty()))),
            ElementsOp::HashIndex if pos_num == 1 => lookup(const_key(args)?).map(found),
            ElementsOp::HashFetch if pos_num == 1 => lookup(const_key(args)?).map(found),
            // `fetch(key, default)`: the default is the answer on a miss.
            ElementsOp::HashFetch if pos_num == 2 => Some(match lookup(const_key(args)?) {
                Some(v) => found(v),
                None => HashFold::Own(args + 1usize),
            }),
            ElementsOp::HashHasKey if pos_num == 1 => {
                let k = const_key(args)?;
                Some(HashFold::Const(Value::bool(lookup(k).is_some())))
            }
            _ => None,
        }
    }

    /// Emit a folded Hash elements lowering ([`Self::plan_hash_op`]).
    fn emit_hash_fold(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        fold: HashFold,
        dst: Option<SlotId>,
    ) {
        match (fold, dst) {
            (_, None) => {}
            (HashFold::Const(v), Some(dst)) => state.def_C(dst, v),
            (HashFold::Own(slot), Some(dst)) => {
                if !matches!(state.mode(slot), LinkMode::S(_) | LinkMode::C(_)) {
                    state.flush_gp(ir);
                }
                state.copy_slot(ir, slot, dst);
            }
            (HashFold::Caller(base, slot), Some(dst)) => {
                state.flush_gp(ir);
                ir.load_caller_slot(base, slot);
                state.def_rax2acc(ir, dst);
            }
        }
    }

    /// The elements are the `len` slots at *src* in the frame *base*
    /// names: this one (an elided literal), or the caller's (an object
    /// handed over in a parameter).
    fn emit_elements_op(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: ElementsOp,
        base: SlotBase,
        src: SlotId,
        len: u16,
        dst: Option<SlotId>,
    ) {
        match op {
            ElementsOp::Min | ElementsOp::Max => {
                // The compare can dispatch a user `<=>` (a side effect) and
                // raise. The elements stay in their slots throughout.
                if base == SlotBase::Own {
                    state.write_back_range(ir, src, len);
                }
                state.discard(dst);
                let using_fpr = state.get_using_fpr(ir);
                let error = ir.new_error(state);
                ir.array_min_max(using_fpr, base, src, len, op == ElementsOp::Min);
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
                    if base == SlotBase::Own {
                        if !matches!(state.mode(elem), LinkMode::S(_) | LinkMode::C(_)) {
                            state.flush_gp(ir);
                        }
                        state.copy_slot(ir, elem, dst);
                    } else {
                        state.flush_gp(ir);
                        ir.load_caller_slot(base, elem);
                        state.def_rax2acc(ir, dst);
                    }
                }
            },
            ElementsOp::HashIndex | ElementsOp::HashFetch | ElementsOp::HashHasKey => {
                unreachable!("a Hash lowering on an Array's elements")
            }
            ElementsOp::RangeCover
            | ElementsOp::RangeSize
            | ElementsOp::RangeCount
            | ElementsOp::RangeSum
            | ElementsOp::RangeBegin
            | ElementsOp::RangeEndpoint
            | ElementsOp::RangeExcludeEnd => {
                unreachable!("a Range lowering on an Array's elements")
            }
        }
    }
}

/// How [`JitContext::consume_virtual`] lowers an elements op.
enum ElementsPlan {
    /// Over the `len` element slots at the slot, in the frame the base names.
    Array(SlotBase, SlotId, u16),
    /// A folded Hash lookup.
    Hash(HashFold),
    /// Over an elided Range's endpoints.
    Range(RangePlan),
}

/// The answer of a Range elements lowering ([`JitContext::plan_range_op`]).
#[derive(Clone, Copy)]
enum RangePlan {
    Const(Value),
    /// A constant folded under the `Integer#<=>` basic-op license.
    Fold(Value),
    /// A copy of this frame's slot (an endpoint).
    Own(SlotId),
    /// `include?` and friends: the endpoints, and the argument slot.
    Cover(RangeEnd, RangeEnd, bool, SlotId),
    /// Through the runtime helper of the op.
    Op(ElementsOp, RangeEnd, RangeEnd, bool),
}

/// Where an elided Hash's value is.
#[derive(Clone, Copy)]
enum HashElem {
    /// In the template (a `Literal` object).
    Const(Value),
    /// In the caller's slot, in the frame the base names (a `CallerKw`
    /// object).
    Caller(SlotBase, SlotId),
}

/// The answer of a folded Hash elements lowering.
#[derive(Clone, Copy)]
enum HashFold {
    Const(Value),
    /// A copy of this frame's slot (the default of `fetch(k, default)`).
    Own(SlotId),
    /// The caller's slot, in the frame the base names.
    Caller(SlotBase, SlotId),
}

impl AbstractState {
    ///
    /// Materialize, inside an argument fill, every elided object among the
    /// positional arguments of *callsite* that the fill cannot hand over
    /// (a non-specialized callee, a forward it cannot route). The FP pool
    /// is already saved at this point (`fpr_save_cont`), so the
    /// allocation saves nothing.
    ///
    pub(in crate::codegen::jitgen) fn materialize_virtual_args(
        &mut self,
        ir: &mut AsmIr,
        callsite: &CallSiteInfo,
    ) {
        for slot in callsite.args..callsite.args + callsite.pos_num {
            let Some(src) = self.take_virtual(slot) else {
                continue;
            };
            let using_fpr = UsingFpr::new();
            match src {
                VirtualSrc::Slots { src, len, callid } => {
                    self.write_back_range(ir, src, len);
                    let inline = (len as usize <= ARRAY_INLINE_CAPA).then_some((src, len));
                    ir.new_array(using_fpr, callid, inline);
                    self.def_reg2acc_class(ir, GP::Rax, slot, ARRAY_CLASS);
                }
                VirtualSrc::Literal(val) => {
                    ir.deep_copy_lit(using_fpr, val);
                    self.def_reg2acc_concrete_value(ir, GP::Rax, slot, val);
                }
                VirtualSrc::CallerSlots { src, len, base } => {
                    ir.create_array_from(using_fpr, base, src, len);
                    self.def_reg2acc_class(ir, GP::Rax, slot, ARRAY_CLASS);
                }
                VirtualSrc::CallerKw { base, .. } => self.materialize_caller_kw(ir, slot, base),
                VirtualSrc::Range {
                    start,
                    end,
                    exclude_end,
                } => {
                    ir.create_range(using_fpr, start, end, exclude_end);
                    self.def_reg2acc_class(ir, GP::Rax, slot, RANGE_CLASS);
                }
            }
            self.discard_dead_elements(src);
        }
    }

    ///
    /// Build the `**kwrest` Hash a handed-over object stands for
    /// (`VirtualSrc::CallerKw`) into *dst*, from the frame's table
    /// ([`AbstractFrame::caller_kw_table`]) over the caller's kw window.
    /// Symbol keys only: no Ruby code runs, so the FP pool needs no save.
    ///
    pub(in crate::codegen::jitgen) fn materialize_caller_kw(
        &mut self,
        ir: &mut AsmIr,
        dst: SlotId,
        base: SlotBase,
    ) {
        let (kwrest, table) = self
            .caller_kw_table()
            .expect("a handed-over kwrest has its table");
        debug_assert_eq!(*kwrest, dst);
        let rest_kw = table.iter().map(|(name, slot)| (*slot, *name)).collect();
        self.flush_gp(ir);
        ir.kw_rest_from(base, rest_kw);
        self.def_reg2acc_class(ir, GP::Rax, dst, HASH_CLASS);
    }
}

///
/// Whether an elided object not involved in *trace_ir* may stay elided
/// across it: the instruction runs no Ruby code and reaches no call (so no
/// frame is suspended with the object owed, and no dispatch arm splits the
/// state).
///
/// The slot an instruction [`virtual_survives`] accepts writes.
fn written_slot(trace_ir: &TraceIr) -> Option<SlotId> {
    match trace_ir {
        TraceIr::Mov(dst, _)
        | TraceIr::Literal(dst, _)
        | TraceIr::FrozenLiteral(dst, _)
        | TraceIr::Array { dst, .. } => Some(*dst),
        _ => None,
    }
}

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
