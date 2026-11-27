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
    /// Elide the deep copy of a literal Array or Range `dst = literal`
    /// into a temporary whose elements are all immediates: the copy is
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
            if !state.is_fixnum(slot)
                || (self.is_temp(slot) && slot != dst && slot < next_sp)
            {
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
    /// So an elided object never lives across a block boundary, and one in
    /// a temporary never lives across a call either (no suspended frame
    /// owes one, and dispatch arms never see one). An object in a
    /// parameter slot (`CallerSlots`, or a literal handed over by the
    /// caller) does live across the frame's own calls — its elements are
    /// in the caller's frame, out of this frame's way, and a chain deopt
    /// materializes it — in a body that can neither capture its locals nor
    /// rescue (`caller_slots_ok`).
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
        // An object in a temporary outlives the instructions that write
        // nothing but a plain destination (`defined_slot`): the receiver
        // of `r.include?(a + 1)`, moved into a temporary while the
        // argument is computed. A call is not among them (its arguments
        // are pushed over the temporaries), nor is anything whose writes
        // `defined_slot` cannot name.
        let survives = virtual_survives(trace_ir)
            || (!matches!(trace_ir, TraceIr::MethodCall { .. })
                && defined_slot(trace_ir, self.store).is_some());
        let written = survives
            .then(|| defined_slot(trace_ir, self.store))
            .flatten();
        // A call that may read every local of this frame behind the
        // compiler's back (`doc/escape_analysis.md` §10): a block literal
        // (its body reads the frame's locals through the outer chain, a
        // `binding` in it captures the frame, and so does the callee by
        // asking the block it was handed for its binding, whatever the
        // body holds: #2058), a callee that captures
        // its caller's frame (`binding` / `eval`,
        // `unbox_to_S_for_outgoing_block`), or a call resolved by neither
        // (a generic `send` could be any of these).
        let capture = self.capture_point(trace_ir);
        for (dst, src) in state.virtuals().to_vec() {
            if self.may_consume_virtual(trace_ir, dst, src, bc_pos) {
                continue;
            }
            let captured = match capture {
                Some(Capture::Frame) => true,
                Some(Capture::Block) => !self.is_temp(dst),
                None => false,
            };
            let read = reads.contains(&dst) || captured;
            let dead = if self.is_temp(dst) {
                dst >= next_sp
            } else {
                !self.local_live_after(bc_pos, dst)
            };
            // An instruction that redefines the slot without reading it
            // (`x = 1..n` over an elided `x`) ends the object as a dead
            // one does: nothing can observe it any more.
            let redefined = defined_slot(trace_ir, self.store) == Some(dst);
            // A store into an element slot: a push onto one (an argument
            // of the use pushed over an element, `[a, b].include?(x)`), or
            // a store into a local endpoint of an elided Range. The
            // object's own slot doubling as an element (`a..b` built where
            // `a` was) is not written while the object is elided, and an
            // object the instruction redefines is dropped by the
            // definition, whatever else it writes.
            let overwrites = src
                .own_slots()
                .any(|s| s != dst && written == Some(s));
            if !read && (dead || redefined) {
                // An instruction that may send a message keeps a local's
                // object through itself (`doc/escape_analysis.md` §10):
                // if it leaves by a side exit, the interpreter runs it,
                // and the callee it finds there may be `binding`. The
                // object stays elided (the exit's write-back builds it)
                // and is dropped once the instruction is done: by its
                // own definition of the slot, by the block's end
                // ([`Self::settle_virtuals_at_bb_end`]) or by the next
                // instruction. One whose elements the instruction
                // overwrites cannot be kept that way, and is built.
                let kept = !self.is_temp(dst) && trace_ir.may_dispatch();
                if !kept {
                    // Dead, and never observed: the allocation is simply
                    // gone.
                    state.take_virtual(dst);
                    state.discard_dead_elements(src, dst);
                    continue;
                }
                if !overwrites {
                    continue;
                }
            }
            if read || bb_end || overwrites || (self.is_temp(dst) && !survives) {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    ///
    /// Run after an instruction that falls through: if it ends its basic
    /// block, perform the elided allocations still live (dropping the dead
    /// ones).
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
        self.settle_virtuals_before_branch(state, ir, bc_pos, &[], DeadLocal::Drop);
    }

    ///
    /// Perform the elided allocations still live after the instruction at
    /// *branch_pos* (the block's last one: a branch, or the instruction a
    /// dispatch's per-arm `CondBr` was sunk into, see
    /// [`JitContext::declare_merge`]), dropping the dead ones. The objects
    /// in *keep* are left alone. *dead_local* says what becomes of a local's object
    /// nothing can observe after the branch, which depends on what is
    /// still to run before it ([`DeadLocal`]).
    ///
    pub(super) fn settle_virtuals_before_branch(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        branch_pos: BcIndex,
        keep: &[SlotId],
        dead_local: DeadLocal,
    ) {
        let next_sp = state.next_sp();
        let settled: Vec<_> = state
            .virtuals()
            .iter()
            .filter(|(dst, _)| !keep.contains(dst))
            .cloned()
            .collect();
        for (dst, src) in settled {
            let dead = if self.is_temp(dst) {
                dst >= next_sp
            } else {
                !self.local_live_after(branch_pos, dst)
            };
            let dead_local = if self.is_temp(dst) {
                DeadLocal::Drop
            } else {
                dead_local
            };
            match (dead, dead_local) {
                (true, DeadLocal::Drop) => {
                    state.take_virtual(dst);
                    state.discard_dead_elements(src, dst);
                }
                // Left for the branch, whose own settling drops the
                // object without emitting anything (`settle_virtuals`).
                (true, DeadLocal::Keep) => {}
                (true, DeadLocal::Build) => self.materialize_virtual(state, ir, dst),
                (false, _) => self.materialize_virtual(state, ir, dst),
            }
        }
    }

    ///
    /// Settle the elided objects for the `CondBr` at *sink_pos* before
    /// the instruction that produces its condition (reading the slots
    /// *reads*) is lowered with that branch as its sink (`CondBrSink`).
    ///
    /// Such a producer may leave its answer in the condition flags, and
    /// nothing may be emitted between it and the branch: the allocation
    /// performed for an object still live at the block's end
    /// ([`Self::settle_virtuals_before_branch`], run again at the
    /// `CondBr`) is a call, which clobbers them. So the block end is
    /// settled here, ahead of the producer (issue #1837), and the `CondBr`
    /// finds only the objects a merge carries, for which it emits
    /// nothing. The objects the producer reads are left to it:
    /// [`Self::settle_virtuals`] already materialized those it cannot
    /// consume, and a consuming lowering (a predicate on an elided
    /// receiver) runs with the sink installed, answering in the flags or
    /// declining before it emits anything.
    ///
    /// A local's object nothing can observe after the branch is not
    /// dropped here: the producer is still to run, and may send a message
    /// (`doc/escape_analysis.md` §10). It is dropped where the branch
    /// itself is settled, which emits nothing.
    ///
    pub(super) fn settle_virtuals_for_sink(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        reads: &[SlotId],
        sink_pos: BcIndex,
    ) {
        self.settle_virtuals_before_branch(state, ir, sink_pos, reads, DeadLocal::Keep);
    }

    ///
    /// Materialize the elided objects the branch *entries* of the merge
    /// *bbid* bring in, before their states are joined. Only one entry
    /// can bring any: a method's start, into its first block, with the
    /// objects its caller handed over in parameters (§6) — kept, as the
    /// method's own. Every other block boundary is crossed by none
    /// ([`Self::settle_virtuals`] builds or drops each object at its
    /// block's end), so this finds nothing there on a sound path and is
    /// kept as the merge's safety net. The allocations go in the returned
    /// per-entry `AsmIr` the bridge is prepended with.
    ///
    pub(in crate::codegen::jitgen) fn settle_entries(
        &mut self,
        bbid: BasicBlockId,
        entries: &mut [BranchEntry],
    ) -> PreBridges {
        // A method whose first block is a loop head is handed nothing
        // (`caller_slots_ok`): the bridge of its start runs before its
        // prologue, where nothing can be built.
        let method_start = self.position().is_none()
            && bbid == BasicBlockId::new(0)
            && self.iseq().bb_info.is_loop_begin(bbid).is_none();
        entries
            .iter_mut()
            .map(|entry| {
                let mut ir = AsmIr::new(self);
                if !method_start {
                    self.materialize_all(&mut entry.state, &mut ir);
                }
                ir
            })
            .collect()
    }

    ///
    /// Materialize every elided object of *state*.
    ///
    pub(in crate::codegen::jitgen) fn materialize_all(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
    ) {
        for (slot, _) in state.virtuals().to_vec() {
            self.materialize_virtual(state, ir, slot);
        }
    }

    ///
    /// Whether *trace_ir* is a call that may read every local of this
    /// frame behind the compiler's back (`doc/escape_analysis.md` §10):
    /// `Some(Capture::Frame)` for a call resolved to a callee that
    /// captures its caller's frame (`binding` / `eval`,
    /// `unbox_to_S_for_outgoing_block`) or resolved to nothing the
    /// compiler can name (a generic `send` could be any of these);
    /// `Some(Capture::Block)` for a call passing a block literal, whose
    /// body reads the frame's locals through the outer chain and may
    /// capture the frame from inside, and whose binding (the frame) the
    /// callee can ask for; `None` otherwise.
    ///
    pub(in crate::codegen::jitgen) fn capture_point(&self, trace_ir: &TraceIr) -> Option<Capture> {
        let TraceIr::MethodCall { callid, cache, .. } = trace_ir else {
            return None;
        };
        let known = matches!(cache, MethodCache::Cached(e)
            if !self.store[e.func_id].possibly_capture_without_block());
        if !known {
            Some(Capture::Frame)
        } else if self.store[*callid].block_fid.is_some() {
            Some(Capture::Block)
        } else {
            None
        }
    }

    ///
    /// Whether the object the local *slot* holds may be observed after
    /// the instruction at *bc_pos*, as the interpreter sees the frame
    /// (`doc/escape_analysis.md` §10): by the rest of its basic block, or
    /// by any block control can reach from there, counting every
    /// instruction that may send a message as a read of every local. A
    /// side exit hands the frame to the interpreter, where a call the
    /// compilation resolved to a callee that captures nothing may reach
    /// `binding` after all, so the compilation's own capture points
    /// ([`Self::capture_point`]) are not the measure. A write before any
    /// read ends the slot's life on that path (`defined_slot`); the
    /// instructions whose destination is not plain are neither a read
    /// nor a kill. Always true in a body with an exception handler, where
    /// the handler may read anything, or a lambda literal. Answered from
    /// the frame liveness (`slot_analysis::SlotAnalysis`).
    ///
    fn local_live_after(&self, bc_pos: BcIndex, slot: SlotId) -> bool {
        self.table_frame_live_after(bc_pos, slot)
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
            // The objects enter the body's first block, which must not
            // be a merge (`settle_entries`).
            && iseq.bb_info.is_loop_begin(BasicBlockId::new(0)).is_none()
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
    fn may_consume_virtual(
        &self,
        trace_ir: &TraceIr,
        slot: SlotId,
        src: VirtualSrc,
        bc_pos: BcIndex,
    ) -> bool {
        // `a[i]` on an elided literal ([`Self::consume_virtual_index`]).
        if let TraceIr::Index { base, idx, .. } = trace_ir {
            return *base == slot && *idx != slot;
        }
        // `y = x` into a temporary with `x` dead afterwards: the object
        // moves to `y` ([`Self::consume_virtual_mov`]).
        // Or `x` still live: a temporary's second view of the object,
        // for a call that only reads its elements
        // ([`Self::alias_consumed_by_call`]).
        if let TraceIr::Mov(dst, from) = trace_ir {
            return *from == slot
                && *dst != slot
                && self.is_temp(*dst)
                && (self.dead_after(bc_pos, slot)
                    || self.alias_consumed_by_call(*dst, src, bc_pos));
        }
        // A plain `return` of the object out of a specialized method into
        // a call site that discards the result: the object is dropped.
        if let TraceIr::Ret(ret) = trace_ir {
            return *ret == slot && self.return_discarded();
        }
        // `a, b = ary` ([`Self::consume_virtual_expand`]): the elements go
        // straight to their targets. Only when nothing reads the Array
        // afterwards: the value of a multiple assignment is its right-hand
        // side, so `@a, @b = a, b` as a method's last expression returns
        // the very slot the expansion reads.
        if let TraceIr::ExpandArray {
            src: ary,
            dst: (_, _, rest_pos),
        } = trace_ir
        {
            return *ary == slot
                && rest_pos.is_none()
                && src.class_id() == ARRAY_CLASS
                && self.dead_after(bc_pos, slot);
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
        // `yield [k, v]` into a block that auto-splats it
        // ([`Self::yield_expanded_for`]).
        if let TraceIr::Yield { callid } = trace_ir {
            return self.yield_expanded_for(*callid, slot, src, bc_pos).is_some();
        }
        let TraceIr::MethodCall { callid, cache, .. } = trace_ir else {
            return false;
        };
        let cs = &self.store[*callid];
        let reads = trace_ir.read_slots(self.store);
        if cs.recv == slot {
            return cs.splat_pos().is_empty() && !reads[1..].contains(&slot);
        }
        // The argument slot is the callee's once handed over (`set_arguments`
        // and `fill_window` take the object): a local passed in its own
        // slot (`get(x)`) hands it over only when nothing reads the local
        // afterwards.
        if !(cs.args..cs.args + cs.pos_num).contains(&slot)
            || reads.iter().filter(|s| **s == slot).count() != 1
            || !self.dead_after(bc_pos, slot)
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
            VirtualSrc::CallerSlots { .. } => false,
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
    /// The expanded twin of the `yield [e1, .., en]` site *callid*
    /// (`CallSiteInfo::yield_expanded`) when the yield may pass the
    /// elements in place of the elided Array *src* in *slot*: the Array is
    /// the one built from the twin's argument temporaries and nothing
    /// reads it afterwards, and the block — the literal one specialized
    /// in place — auto-splats a lone Array argument
    /// (`FuncInfo::single_arg_expand`), so it binds `e1, .., en` exactly
    /// as it would bind the Array, takes no keywords, and is a simple
    /// call for the twin.
    ///
    pub(super) fn yield_expanded_for(
        &self,
        callid: CallSiteId,
        slot: SlotId,
        src: VirtualSrc,
        bc_pos: BcIndex,
    ) -> Option<CallSiteId> {
        let cs = &self.store[callid];
        let exp = cs.yield_expanded?;
        let ecs = &self.store[exp];
        let fid = self.current_method_given_block()?.block_fid;
        let info = &self.store[fid];
        (cs.args == slot
            && matches!(src, VirtualSrc::Slots { src, len, .. }
                if src == ecs.args && len as usize == ecs.pos_num)
            && !self.in_dispatch_arm()
            && self.dead_after(bc_pos, slot)
            && info.is_iseq().is_some()
            && info.single_arg_expand()
            && info.no_keyword()
            && self.store.is_simple_call(fid, exp))
            .then_some(exp)
    }

    ///
    /// Whether `tmp = x` (*dst* a temporary, *src* the object a still-live
    /// parameter holds) copies the object into a temporary for a call that
    /// reads its elements and nothing else: `x.include?(a + 1)` evaluates
    /// its receiver into a temporary before its argument. The temporary
    /// then takes a second
    /// view of the same object, which that call consumes, while the local
    /// keeps its own. The two views are never told apart: the call's
    /// method reads the elements and neither mutates nor lets out the
    /// receiver (`RecvEscape::elements`), so even a lowering that builds
    /// the temporary's copy after all gives the same answer. Between the
    /// copy and the call only instructions writing a plain destination
    /// that is not the temporary may come, in the same basic block.
    ///
    fn alias_consumed_by_call(&self, dst: SlotId, src: VirtualSrc, bc_pos: BcIndex) -> bool {
        if !self.is_temp(dst) {
            return false;
        }
        let a = self.slot_analysis(self.iseq_id());
        let Some(pos) = a.alias_stop(&self.iseq().bb_info, bc_pos, dst) else {
            return false;
        };
        let class = CachedClass::from_class(src.class_id());
        let elements = |name: IdentId| {
            self.jit_check_method(class, name)
                .and_then(|(fid, _)| self.store.recv_escape(fid))
                .is_some_and(|e| e.elements.is_some())
        };
        if a.reads(pos).iter().filter(|s| **s == dst).count() != 1 {
            return false;
        }
        let Some(callid) = a.call(pos) else {
            return false;
        };
        let cs = &self.store[callid];
        cs.recv == dst
            && cs.block_fid.is_none()
            && cs.block_arg.is_none()
            && cs.kw_args().is_empty()
            && cs.hash_splat_pos().is_empty()
            && cs.splat_pos().is_empty()
            && cs.name.is_some_and(elements)
    }

    ///
    /// `tmp = src` with an elided object in *src*: dead afterwards, the
    /// object is now the temporary's; still live, the temporary takes a
    /// second view of it ([`Self::alias_consumed_by_call`]).
    ///
    pub(super) fn consume_virtual_mov(
        &mut self,
        state: &mut AbstractState,
        dst: SlotId,
        src: SlotId,
        bc_pos: BcIndex,
    ) {
        if !self.dead_after(bc_pos, src) {
            let obj = state.virtual_at(src).unwrap();
            state.def_virtual(dst, obj);
            return;
        }
        let obj = state.take_virtual(src).unwrap();
        state.def_virtual(dst, obj);
        state.discard_dead_elements(obj, src);
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
        // The object is observed here (whatever reads the slot next
        // reads it): a read of the slot's value for the loop liveness,
        // which the redefinition below would otherwise record as a kill
        // — and a loop-entry seed (`predict_loop_entries`) only types a
        // slot the loop reads.
        // The allocation is a call: it may not come between a producer's
        // condition flags (or a dispatch's per-arm branches) and the
        // `CondBr` they are for ([`Self::settle_virtuals_for_sink`]).
        debug_assert!(
            !self.cond_flags_pending() && !self.condbr_done_pending(),
            "an elided object materialized while a CondBr's condition is pending"
        );
        state.note_read(dst);
        let src = state.take_virtual(dst).unwrap();
        match src {
            VirtualSrc::Slots { src, len, callid } => {
                state.write_back_range(ir, src, len);
                let using_fpr = state.get_using_fpr(ir);
                let inline = (len as usize <= ARRAY_INLINE_CAPA).then_some((src, len));
                ir.new_array(using_fpr, callid, inline);
                state.def_reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            VirtualSrc::Literal(val) if val.class().is_always_frozen() => {
                // A `lo..hi` literal (`FrozenLiteral`) is the object itself
                // at every evaluation (`value_deep_copy` returns it as is):
                // load it as the plain literal is, with no helper call.
                state.def_lit2gp(ir, dst, val);
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
        state.discard_dead_elements(src, dst);
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
            VirtualSrc::Literal(_) | VirtualSrc::Range { .. } => None,
        };
        // Resolved as the call itself would be (refinements in scope
        // included), and a private method stays the interpreter's to reject.
        let resolved = cs.name.and_then(|name| {
            let (fid, visibility) = self.jit_check_method(class, name)?;
            (!self.jit_visibility_blocks(callid, visibility, fid)).then_some((name, fid))
        });
        let summary = resolved.and_then(|(_, fid)| self.store.recv_escape(fid));
        let plain =
            cs.kw_args().is_empty() && cs.hash_splat_pos().is_empty() && cs.block_arg.is_none();
        let block_less = plain && cs.block_fid.is_none();
        let (pos_num, call_dst, args) = (cs.pos_num, cs.dst, cs.args);
        // The receiver slot must be dead after the call: whatever the call
        // leaves there is never read again.
        let recv_dead = Some(recv) == call_dst || self.dead_after(bc_pos, recv);
        // An argument that is itself an elided object was left for the
        // call to hand over (`may_consume_virtual` decides each object on
        // its own: `(1..10).cover?(1..5)`, `Range#cover?` being Ruby): its
        // slot holds no value, which the lowerings here would read. The
        // two consumers exclude each other, and the call is the one that
        // takes both: the receiver is built (a literal is its template,
        // at no cost) and the argument stays elided into the callee
        // (issue #1980).
        let args_elided = (0..pos_num).any(|i| state.virtual_at(args + i).is_some());
        // What the elements lowering would do, decided before anything is
        // emitted: an Array's elements are in frame slots, a Range's
        // endpoints are constants or frame slots.
        let plan = match summary {
            Some(RecvEscape {
                elements: Some(op), ..
            }) if block_less && !args_elided => match (elements, Self::range_ends(src)) {
                (Some((base, src, len)), _) if pos_num == 0 => {
                    Some(ElementsPlan::Array(base, src, len))
                }
                (None, Some(ends)) => self
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
                    super_of: None,
                    visibility: None,
                });
                self.guard_class_version(state, ir, true);
                if let ElementsPlan::Range(plan) = &plan {
                    self.guard_range_cover_arg(state, ir, plan);
                }
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
                        let newarray = self.store[callid].newarray_recv;
                        self.emit_elements_op(state, ir, op, base, src, len, call_dst, newarray);
                    }
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
    /// `dst.., = src` (`TraceIr::ExpandArray` without a rest target) with an
    /// elided Array in *src* ([`Self::may_consume_virtual`]): each target
    /// takes its element — a template's constant, a copy of the slot
    /// holding it (this frame's, or the caller's through the base) — or
    /// `nil` past the end, exactly as the expansion of the built Array
    /// would, and the object is gone.
    ///
    pub(super) fn consume_virtual_expand(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        src: SlotId,
        dst: SlotId,
        len: u16,
    ) {
        let virt = state.take_virtual(src).unwrap();
        // The targets may overlap the element slots (`a, b = [x, y]`
        // expands into the temporaries right above the object): copy from
        // the far end first, as a `memmove` does, so no element is
        // overwritten before it is read.
        let own = match virt {
            VirtualSrc::Slots { src, .. } => Some(src),
            _ => None,
        };
        let order: Vec<u16> = if own.is_some_and(|elems| dst > elems) {
            (0..len).rev().collect()
        } else {
            (0..len).collect()
        };
        for i in order {
            let target = dst + i;
            match virt {
                VirtualSrc::Literal(val) => {
                    let elem = val.as_array().get(i as usize).copied().unwrap_or_default();
                    state.def_C(target, elem);
                }
                VirtualSrc::Slots { src, len: n, .. } => {
                    if i < n {
                        self.copy_element(state, ir, src + i, target);
                    } else {
                        state.def_C(target, Value::nil());
                    }
                }
                VirtualSrc::CallerSlots { src, len: n, base } => {
                    if i < n {
                        state.flush_gp(ir);
                        ir.load_caller_slot(base, src + i);
                        state.def_rax2acc(ir, target);
                    } else {
                        state.def_C(target, Value::nil());
                    }
                }
                VirtualSrc::Range { .. } => unreachable!("an Array expansion of a Range"),
            }
        }
        // The object and the elements no target took are gone.
        let is_target = |s: SlotId| (dst.0..dst.0 + len).contains(&s.0);
        let next_sp = state.next_sp();
        for s in std::iter::once(src).chain(virt.own_slots()) {
            if !is_target(s) && (s == src || s >= next_sp) && !state.is_virtual_src(s) {
                state.discard(s);
            }
        }
    }

    ///
    /// `dst = elem`, an element of an elided Array in this frame's slot
    /// *elem*: a slot copy, unless the element slot is `V` (the object's
    /// own slot doubling as an element), which is read from the stack.
    ///
    fn copy_element(&self, state: &mut AbstractState, ir: &mut AsmIr, elem: SlotId, dst: SlotId) {
        match state.mode(elem) {
            LinkMode::V => {
                state.flush_gp(ir);
                ir.load_caller_slot(SlotBase::Own, elem);
                state.def_rax2acc(ir, dst);
            }
            LinkMode::S(_) | LinkMode::C(_) => state.copy_slot(ir, elem, dst),
            _ => {
                state.flush_gp(ir);
                state.copy_slot(ir, elem, dst);
            }
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
                    super_of: None,
                    visibility: None,
                });
                self.guard_class_version(state, ir, true);
                self.guard_range_cover_arg(state, ir, &plan);
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
                    // A Bignum or `Infinity` answer is a heap object the
                    // compiled code would hold with no GC root; it is
                    // made per call by the helper instead, as CRuby
                    // makes a new one (#2059).
                    && v.is_packed_value()
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

    /// A frameless body cannot take the `include?` helper (it may run Ruby
    /// code, a non-numeric argument's `<=>`, while the window is
    /// invisible), so there a non-Fixnum argument leaves the body instead:
    /// the guard's exit hands the call back (or materializes the frame)
    /// and the interpreter answers it. Emitted while the Range is still
    /// registered, so a materializing exit rebuilds it.
    fn guard_range_cover_arg(&self, state: &mut AbstractState, ir: &mut AsmIr, plan: &RangePlan) {
        if let RangePlan::Cover(_, _, _, arg) = *plan
            && self.current_frame().asm_info.frameless
            && !state.is_fixnum(arg)
            && self.basic_op_assumable(CachedClass::INTEGER, IdentId::_CMP)
            && self.basic_op_assumable(CachedClass::FLOAT, IdentId::_CMP)
        {
            state.load(ir, arg, GP::Rdi);
            state.guard_fixnum(ir, arg, GP::Rdi);
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
                // With Fixnum endpoints (`range_ends`) and a Fixnum `arg`
                // the two compares are the whole answer: no helper, no
                // error exit, nothing that runs Ruby code.
                let pure = inline && state.is_fixnum(arg);
                state.write_back_slots(ir, &[arg]);
                state.discard(dst);
                // Only the conditional branch that follows reads the answer
                // (`raise unless (0...n).include?(i)`): leave it in the
                // condition flags. Nothing may come between them, so the GP
                // residents are settled first and the dead temp holds a nil,
                // as `nil?` does.
                let flags_kind = range_cover_flags_kind(start, end, exclude_end);
                if pure
                    && let Some(kind) = flags_kind
                    && let Some(dst) = self.wants_cond_flags(dst)
                {
                    state.flush_gp(ir);
                    let using_fpr = state.get_using_fpr(ir);
                    ir.range_cover(using_fpr, start, end, exclude_end, arg, fid, true, true, true);
                    state.def_C(dst, Value::nil());
                    self.set_cond_flags(dst, CondFlags::Int(kind));
                    return;
                }
                let using_fpr = state.get_using_fpr(ir);
                if pure {
                    ir.range_cover(using_fpr, start, end, exclude_end, arg, fid, inline, true, false);
                    state.def_rax2acc(ir, dst);
                } else {
                    let error = ir.new_error(state);
                    ir.range_cover(
                        using_fpr,
                        start,
                        end,
                        exclude_end,
                        arg,
                        fid,
                        inline,
                        false,
                        false,
                    );
                    ir.handle_error(error);
                    state.def_rax2acc(ir, dst);
                    self.after_range_helper(state);
                }
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
    /// `dst = base[idx]` (`TraceIr::Index`) with an elided object in *base*
    /// ([`Self::may_consume_virtual`]): a literal of immediates whose `[]`
    /// lets no receiver escape runs on the template, as
    /// [`Self::consume_virtual`] has it for a method call; any other object
    /// is materialized. The ordinary `[]` lowering follows either way.
    ///
    pub(super) fn consume_virtual_index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        dst: SlotId,
        bc_pos: BcIndex,
    ) {
        let src = state.virtual_at(base).unwrap();
        let class = CachedClass::from_class(src.class_id());
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        let transparent = self
            .jit_check_method(class, IdentId::_INDEX)
            .filter(|(fid, visibility)| !self.jit_visibility_blocks(callid, *visibility, *fid))
            .and_then(|(fid, _)| self.store.recv_escape(fid))
            .is_some_and(|e| e.transparent);
        let recv_dead = base == dst || self.dead_after(bc_pos, base);
        if let VirtualSrc::Literal(val) = src
            && transparent
            && recv_dead
        {
            self.guard_class_version(state, ir, true);
            state.take_virtual(base);
            state.def_C(base, val);
        } else {
            self.materialize_virtual(state, ir, base);
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
        // The call is written on the Array literal itself
        // (`CallSiteInfo::newarray_recv`).
        newarray: bool,
    ) {
        match op {
            ElementsOp::Min | ElementsOp::Max => {
                // The compare can dispatch a user `<=>` (a side effect) and
                // raise. The elements stay in their slots throughout.
                if base == SlotBase::Own {
                    state.write_back_elements(ir, src, len);
                }
                state.discard(dst);
                let using_fpr = state.get_using_fpr(ir);
                let error = ir.new_error(state);
                let op = MinMaxOp::new(op == ElementsOp::Min, newarray);
                ir.array_min_max(using_fpr, base, src, len, op);
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
                        self.copy_element(state, ir, elem, dst);
                    } else {
                        state.flush_gp(ir);
                        ir.load_caller_slot(base, elem);
                        state.def_rax2acc(ir, dst);
                    }
                }
            },
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

impl AbstractFrame {
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
            if self.virtual_at(slot).is_some() {
                self.materialize_virtual_at(ir, slot, UsingFpr::new());
            }
        }
    }

    ///
    /// Perform the allocation of the elided object in *slot* of this frame
    /// alone, with *using_fpr* saved around it: the frame-level half of
    /// [`JitContext::materialize_virtual`], for the places that hold no
    /// `AbstractState` — an argument fill, a return segment.
    ///
    pub(in crate::codegen::jitgen) fn materialize_virtual_at(
        &mut self,
        ir: &mut AsmIr,
        slot: SlotId,
        using_fpr: UsingFpr,
    ) {
        let src = self.take_virtual(slot).unwrap();
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
            VirtualSrc::Range {
                start,
                end,
                exclude_end,
            } => {
                ir.create_range(using_fpr, start, end, exclude_end);
                self.def_reg2acc_class(ir, GP::Rax, slot, RANGE_CLASS);
            }
        }
        self.discard_dead_elements(src, slot);
    }
}

/// The slot *trace_ir* defines without reading ([`TraceIr::defined_slot`]).
fn defined_slot(trace_ir: &TraceIr, store: &Store) -> Option<SlotId> {
    trace_ir.defined_slot(store)
}

///
/// What [`JitContext::settle_virtuals_before_branch`] does with a local's
/// elided object nothing can observe after the branch
/// (`doc/escape_analysis.md` §10).
///
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::codegen::jitgen) enum DeadLocal {
    /// Nothing runs between here and the branch: the object is gone.
    Drop,
    /// The instruction producing the branch's condition is still to run
    /// and may send a message, so the object has to outlive it. It stays
    /// elided, for the branch's own settling to drop.
    Keep,
    /// As `Keep`, where the producer is a dispatch that branches in every
    /// arm: no arm takes an elided object over, so it is built.
    Build,
}

/// What a call may capture of this frame ([`JitContext::capture_point`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::codegen::jitgen) enum Capture {
    /// The whole frame, through a `binding` / `eval` (or a call the
    /// compiler cannot name).
    Frame,
    /// The whole frame as well, through a block literal passed to the
    /// call: its body reads the frame's locals through the outer chain,
    /// and the callee may keep it and ask for its binding. Told apart
    /// from `Frame` because the block is run by a callee the compiler
    /// knows: only what the block's body touches is a read for the
    /// compiled code's own liveness (`slot_analysis.rs`, Layer B).
    Block,
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
