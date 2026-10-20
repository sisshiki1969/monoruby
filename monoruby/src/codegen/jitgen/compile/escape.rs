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
        if self.is_temp(dst) {
            if len != 0 && (!self.is_temp(src) || src < state.next_sp()) {
                return false;
            }
            state.write_back_range(ir, src, len);
            state.def_virtual(dst, VirtualSrc::Slots { src, len, callid });
            return true;
        }
        // Into a local (`doc/escape_analysis.md` §10): the elements move
        // into the local's scratch window, where no push can reach them.
        let Some((base, cap)) = self.local_scratch(state, dst) else {
            return false;
        };
        if len > cap {
            return false;
        }
        self.copy_to_scratch(state, ir, src, base, len);
        state.def_virtual(
            dst,
            VirtualSrc::Slots {
                src: base,
                len,
                callid,
            },
        );
        true
    }

    ///
    /// Whether an elided object may be kept in the local *dst*
    /// (`doc/escape_analysis.md` §10): the frame is proven on the stack
    /// and uncaptured (`no_capture_guard`: nothing can read the local
    /// behind the compiler's back, and every way to capture it from here
    /// on materializes the object first, [`Self::settle_virtuals`]), and
    /// rescues nothing (a handler could read any local). A frameless
    /// callee qualifies: its window is its whole frame, scratch windows
    /// included, addressed through the shifted LFP like any slot of its
    /// own, and its materializing exit builds the object from the window
    /// as a framed side exit does.
    ///
    fn local_elision_ok(&self, state: &AbstractState, dst: SlotId) -> bool {
        !self.is_temp(dst)
            && state.no_capture_guard()
            && !self.iseq().has_exception_handler()
    }

    ///
    /// The scratch window `(base, len)` of the local *dst*, when an object
    /// may be kept in it ([`Self::local_elision_ok`]) and the bytecode
    /// reserved one (`ISeqInfo::local_scratch`: an Array or Range literal
    /// is assigned to the local).
    ///
    fn local_scratch(&self, state: &AbstractState, dst: SlotId) -> Option<(SlotId, u16)> {
        if !self.local_elision_ok(state, dst) {
            return None;
        }
        self.iseq().local_scratch(dst)
    }

    ///
    /// Copy the slots `src .. src + len` into the scratch slots
    /// `base .. base + len` and home them there, so that the elements of
    /// an object a local holds are in memory for a side exit, a chain
    /// deopt and the collector.
    ///
    fn copy_to_scratch(
        &self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        src: SlotId,
        base: SlotId,
        len: u16,
    ) {
        // A plain stack-to-stack copy: the window slot is homed and holds
        // no register (a `copy_slot` would leave it sharing a resident,
        // whose later re-use could write back over the element), and keeps
        // only the Fixnum proof an elided Range's endpoint relies on.
        state.flush_gp(ir);
        for i in 0..len {
            let (from, to) = (src + i, base + i);
            let guarded = if state.is_fixnum(from) {
                Guarded::Fixnum
            } else {
                Guarded::Value
            };
            state.write_back_slots(ir, &[from]);
            ir.stack2reg(from, GP::Rax);
            ir.reg2stack(GP::Rax, to);
            state.def_S_guarded(to, guarded);
        }
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
        if !self.is_temp(dst) && !self.local_elision_ok(state, dst) {
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
        // Into a local (`doc/escape_analysis.md` §10): the slot endpoints
        // move into the local's scratch window, out of the way of every
        // later write to a local or a push.
        let scratch = if self.is_temp(dst) {
            None
        } else {
            match self.local_scratch(state, dst) {
                Some((base, cap)) if cap >= 2 => Some(base),
                _ => return false,
            }
        };
        let next_sp = state.next_sp();
        let endpoint = |state: &AbstractState, slot: SlotId| -> Option<RangeEnd> {
            if let LinkMode::C(v) = state.mode(slot)
                && (v.is_fixnum() || v.is_nil())
            {
                return Some(RangeEnd::Const(v));
            }
            if !state.is_fixnum(slot)
                || (scratch.is_none() && self.is_temp(slot) && slot != dst && slot < next_sp)
            {
                return None;
            }
            Some(RangeEnd::Slot(slot))
        };
        let (Some(mut start), Some(mut end)) = (endpoint(state, start), endpoint(state, end))
        else {
            return false;
        };
        if let Some(base) = scratch {
            for (i, e) in [&mut start, &mut end].into_iter().enumerate() {
                if let RangeEnd::Slot(slot) = *e {
                    self.copy_to_scratch(state, ir, slot, base + i as u16, 1);
                    *e = RangeEnd::Slot(base + i as u16);
                }
            }
        }
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
    /// into a merge ([`Self::merge_carries_virtual`]), where the merge
    /// keeps it if every path brings the same object and the other paths
    /// materialize theirs on their bridges (`JitContext::settle_entries`)
    /// — a loop head included, when its loop neither writes the local nor
    /// captures it ([`Self::loop_keeps_virtual`]).
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
        // The scratch window the instruction may fill: an Array or Range
        // literal into a local, or an object moved into one
        // ([`Self::consume_virtual_mov`]), copies its elements there.
        let window = match trace_ir {
            TraceIr::Array { dst, .. } | TraceIr::Range { dst, .. } | TraceIr::Mov(dst, _) => {
                self.iseq().local_scratch(*dst)
            }
            _ => None,
        };
        // A call that may read every local of this frame behind the
        // compiler's back (`doc/escape_analysis.md` §10): a block literal
        // (its body reads the frame's locals through the outer chain, and
        // a `binding` in it captures the frame), a callee that captures
        // its caller's frame (`binding` / `eval`,
        // `unbox_to_S_for_outgoing_block`), or a call resolved by neither
        // (a generic `send` could be any of these).
        let capture = self.capture_point(trace_ir);
        for (dst, src) in state.virtuals().to_vec() {
            if self.may_consume_virtual(state, trace_ir, dst, src, bc_pos) {
                continue;
            }
            let captured = match capture {
                Some(Capture::Frame) => true,
                Some(Capture::Block) => !self.is_temp(dst) && self.touched_by_blocks(dst),
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
            if !read && (dead || redefined) {
                // Dead, and never observed: the allocation is simply gone.
                state.take_virtual(dst);
                state.discard_dead_elements(src, dst);
                continue;
            }
            // A store into an element slot: a push onto one (an argument
            // of the use pushed over an element, `[a, b].include?(x)`), a
            // store into a local endpoint of an elided Range, or a fill of
            // the scratch window holding the elements. The object's own
            // slot doubling as an element (`a..b` built where `a` was; the
            // first element of an Array returned elided) is not written
            // while the object is elided, and an object the instruction
            // redefines is dropped by the definition, whatever else it
            // writes.
            let in_window =
                |s: SlotId| window.is_some_and(|(base, len)| base <= s && s < base + len);
            let overwrites = src
                .own_slots()
                .any(|s| s != dst && (written == Some(s) || in_window(s)));
            let carried = bb_end && self.merge_carries_virtual(bc_pos, dst, src);
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
        self.settle_virtuals_before_branch(state, ir, bc_pos, true);
    }

    ///
    /// Perform the elided allocations still live after the instruction at
    /// *branch_pos* (the block's last one: a branch, or the instruction a
    /// dispatch's per-arm `CondBr` was sunk into, see
    /// [`JitContext::declare_merge`]), dropping the dead ones. With
    /// *carries*, a local's object is left for the merges that may carry
    /// it ([`Self::merge_carries_virtual`]).
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
                state.discard_dead_elements(src, dst);
            } else if !(carries && self.merge_carries_virtual(branch_pos, dst, src)) {
                self.materialize_virtual(state, ir, dst);
            }
        }
    }

    ///
    /// Whether every merge the block ending at *bc_pos* branches into may
    /// carry the elided object *src* in the local *dst*
    /// (`doc/escape_analysis.md` §3): a forward merge, compiled from the
    /// states of its branches ([`JitContext::settle_entries`] reconciles
    /// them), or a loop head whose loop can keep the object
    /// ([`Self::loop_keeps_virtual`]). An object in a temporary is never
    /// carried, nor is anything out of a dispatch arm, whose per-arm
    /// branch merges with the fall-through (`declare_merge`).
    ///
    fn merge_carries_virtual(&self, bc_pos: BcIndex, dst: SlotId, src: VirtualSrc) -> bool {
        if self.is_temp(dst) || self.in_dispatch_arm() {
            return false;
        }
        let bb_info = &self.iseq().bb_info;
        let bb = bb_info.get_bb_id(bc_pos);
        bb_info
            .succ(bb)
            .iter()
            .all(|s| match bb_info.is_loop_begin(*s) {
                None => true,
                Some((start, end)) => self.loop_keeps_virtual(start, end, dst, src),
            })
    }

    ///
    /// Whether the loop `loop_start..=loop_end` can keep the elided object
    /// *src* in the local *dst* across its head (`doc/escape_analysis.md`
    /// §3): by the tables, no block of the loop writes the local (nor a
    /// local among the object's element slots; the others are the local's
    /// scratch window, which only a literal into the local writes), and no
    /// capture point in the loop reaches it. The body may still read the
    /// object in a way that materializes it; the back-edge fixpoint then
    /// says so, and the loop's entries materialize it instead
    /// ([`JitContext::settle_entries`]).
    ///
    fn loop_keeps_virtual(
        &self,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
        dst: SlotId,
        src: VirtualSrc,
    ) -> bool {
        let iseq_id = self.iseq_id();
        let a = self.slot_analysis(iseq_id);
        if a.all_live() {
            return false;
        }
        let iseq = self.iseq();
        let window = iseq.local_scratch(dst);
        let in_window = |s: SlotId| window.is_some_and(|(base, len)| base <= s && s < base + len);
        let locals = std::iter::once(dst).chain(src.own_slots().filter(|s| !self.is_temp(*s)));
        if src.own_slots().any(|s| self.is_temp(s) && !in_window(s)) {
            return false;
        }
        let (begin, end) = (iseq.bb_info[loop_start].begin, iseq.bb_info[loop_end].end);
        let u = self.unit_liveness(iseq_id);
        for slot in locals {
            if a.defined_in_blocks(loop_start, loop_end, slot) || u.captured_in(begin, end, slot) {
                return false;
            }
        }
        true
    }

    ///
    /// Reconcile the elided objects the branch *entries* of a merge bring
    /// in, before their states are joined: an object every entry carries
    /// identically (same slot, same `VirtualSrc`) — and, at a loop head
    /// whose back edge *backedge* is recorded, one the back edge carries
    /// too — stays elided through the merge; any other is materialized on
    /// its entry's bridge, in the returned per-entry `AsmIr` the bridge is
    /// prepended with.
    ///
    pub(in crate::codegen::jitgen) fn settle_entries(
        &mut self,
        entries: &mut [BranchEntry],
        backedge: Option<&[(SlotId, VirtualSrc)]>,
    ) -> Vec<AsmIr> {
        let common: Vec<(SlotId, VirtualSrc)> = if entries.is_empty() {
            vec![]
        } else {
            entries[0]
                .state
                .virtuals()
                .iter()
                .filter(|v| entries.iter().all(|e| e.state.virtuals().contains(v)))
                .filter(|v| backedge.is_none_or(|be| be.contains(v)))
                .copied()
                .collect()
        };
        entries
            .iter_mut()
            .map(|entry| {
                let mut ir = AsmIr::new(self);
                self.materialize_except(&mut entry.state, &mut ir, &common);
                ir
            })
            .collect()
    }

    ///
    /// Materialize every elided object of *state* but those in *keep*
    /// (same slot, same `VirtualSrc`).
    ///
    pub(in crate::codegen::jitgen) fn materialize_except(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        keep: &[(SlotId, VirtualSrc)],
    ) {
        for v in state.virtuals().to_vec() {
            if !keep.contains(&v) {
                self.materialize_virtual(state, ir, v.0);
            }
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
    /// body reads the frame's locals through the outer chain (and may
    /// capture the frame from inside); `None` otherwise.
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
    /// Whether a block literal of this frame (or a block nested in one)
    /// may read or write the local *slot*: a `LoadDynVar` / `StoreDynVar`
    /// of it at the block's depth (a write replaces the elided object
    /// behind the compiler's back, so it has to be real by then), or — since a `binding` or `eval` inside the block captures
    /// the whole frame — a call in the block resolved to a capturing
    /// callee or to nothing the compiler can name, a lambda literal
    /// (whose body is not reachable from here), or a handler in the
    /// block (whose instructions the JIT does not decode).
    ///
    fn touched_by_blocks(&self, slot: SlotId) -> bool {
        self.table_touched_by_blocks(slot)
    }

    ///
    /// Whether the local *slot* may be read after the instruction at
    /// *bc_pos*: by the rest of its basic block, or by any block control
    /// can reach from there, counting a capture point
    /// ([`Self::capture_point`]) as a read of every local a block or a
    /// `binding` could reach ([`Self::touched_by_blocks`]). A write before
    /// any read ends the slot's life on that path (`defined_slot`); the
    /// instructions whose destination is not plain are neither a read
    /// nor a kill. Always true in a body with an exception handler, where
    /// the handler may read anything. Answered from the liveness tables
    /// (`slot_analysis::UnitLiveness`).
    ///
    fn local_live_after(&self, bc_pos: BcIndex, slot: SlotId) -> bool {
        self.table_live_after(bc_pos, slot)
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
    fn may_consume_virtual(
        &self,
        state: &AbstractState,
        trace_ir: &TraceIr,
        slot: SlotId,
        src: VirtualSrc,
        bc_pos: BcIndex,
    ) -> bool {
        // `h[k]` on an elided Hash ([`Self::consume_virtual_index`]).
        if let TraceIr::Index { base, idx, .. } = trace_ir {
            return *base == slot && *idx != slot;
        }
        // `y = x` with `x` dead afterwards: the object moves to `y`
        // ([`Self::consume_virtual_mov`]).
        // Or `x` still live: a temporary's second view of the object,
        // for a call that only reads its elements
        // ([`Self::alias_consumed_by_call`]).
        if let TraceIr::Mov(dst, from) = trace_ir {
            return *from == slot
                && *dst != slot
                && ((self.dead_after(bc_pos, slot) && self.mov_target_ok(state, *dst, src))
                    || self.alias_consumed_by_call(*dst, src, bc_pos));
        }
        // A plain `return` of the object out of a specialized method, into
        // a call site that takes it elided (`doc/escape_analysis.md` §9).
        // A call site that discards the result drops the object instead.
        if let TraceIr::Ret(ret) = trace_ir {
            return *ret == slot
                && (self.return_virtual_in_caller(src).is_some() || self.return_discarded());
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
    /// Whether the elided object *src* may move into *dst* by a `Mov`
    /// (`doc/escape_analysis.md` §10): into a temporary as it is (its
    /// elements stay where they are, pinned or in a local's window);
    /// into a local under the local gate, with its own-frame elements
    /// copied into the local's window — so a `Slots` or slot-endpoint
    /// `Range` needs a window wide enough, while a handed-over object or
    /// a template needs none. A `**kwrest` object is tied to its
    /// parameter slot (the frame's `caller_kw` table) and does not move.
    ///
    fn mov_target_ok(&self, state: &AbstractState, dst: SlotId, src: VirtualSrc) -> bool {
        if matches!(src, VirtualSrc::CallerKw { .. }) {
            return false;
        }
        if self.is_temp(dst) {
            return true;
        }
        if !self.local_elision_ok(state, dst) {
            return false;
        }
        let need = src.own_slots().count() as u16;
        need == 0
            || self
                .iseq()
                .local_scratch(dst)
                .is_some_and(|(_, cap)| need <= cap)
    }

    ///
    /// Whether `tmp = x` (*dst* a temporary, *src* the object a still-live
    /// local holds) copies the object into a temporary for a call that
    /// reads its elements and nothing else: `x.include?(a + 1)` evaluates
    /// its receiver into a temporary before its argument, and so does
    /// `x[i % 3]` (the `%d = %base[%idx]` index instruction, which is
    /// `[]` on the object's class). The temporary then takes a second
    /// view of the same object, which that call consumes, while the local
    /// keeps its own. The two views are never told apart: the call's
    /// method reads the elements and neither mutates nor lets out the
    /// receiver (`RecvEscape::elements`), so even a lowering that builds
    /// the temporary's copy after all gives the same answer. Between the
    /// copy and the call only instructions writing a plain destination
    /// that is not the temporary may come, in the same basic block.
    ///
    fn alias_consumed_by_call(&self, dst: SlotId, src: VirtualSrc, bc_pos: BcIndex) -> bool {
        if !self.is_temp(dst) || matches!(src, VirtualSrc::CallerKw { .. }) {
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
            let TraceIr::Index { base, idx, .. } =
                TraceIr::from_pc(self.iseq().get_pc(pos), self.store)
            else {
                return false;
            };
            return base == dst && idx != dst && elements(IdentId::_INDEX);
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
    /// `dst = src` with an elided object in *src*: dead afterwards
    /// ([`Self::mov_target_ok`]), the object is now *dst*'s — into a
    /// local, the elements in this frame's slots are copied into the
    /// local's scratch window first. Still live, *dst* takes a second
    /// view of the object ([`Self::alias_consumed_by_call`]).
    ///
    pub(super) fn consume_virtual_mov(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
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
        let moved = if self.is_temp(dst) {
            obj
        } else {
            let base = self.iseq().local_scratch(dst).map(|(base, _)| base);
            match obj {
                VirtualSrc::Slots { src, len, callid } => {
                    let base = base.unwrap();
                    self.copy_to_scratch(state, ir, src, base, len);
                    VirtualSrc::Slots {
                        src: base,
                        len,
                        callid,
                    }
                }
                VirtualSrc::Range {
                    mut start,
                    mut end,
                    exclude_end,
                } => {
                    for (i, e) in [&mut start, &mut end].into_iter().enumerate() {
                        if let RangeEnd::Slot(slot) = *e {
                            let to = base.unwrap() + i as u16;
                            self.copy_to_scratch(state, ir, slot, to, 1);
                            *e = RangeEnd::Slot(to);
                        }
                    }
                    VirtualSrc::Range {
                        start,
                        end,
                        exclude_end,
                    }
                }
                VirtualSrc::Literal(_) | VirtualSrc::CallerSlots { .. } => obj,
                VirtualSrc::CallerKw { .. } => unreachable!(),
            }
        };
        state.def_virtual(dst, moved);
        // The elements left behind by a copy are dead with `src`.
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
        state.note_read(dst);
        let src = state.take_virtual(dst).unwrap();
        match src {
            VirtualSrc::Slots { src, len, callid } => {
                state.write_back_range(ir, src, len);
                let using_fpr = state.get_using_fpr(ir);
                if src != self.store[callid].args {
                    // The elements moved into a local's scratch window
                    // (§10): built over the window, which the literal's
                    // own allocation (whose cold path reads the call
                    // site's argument slots) would not read.
                    ir.create_array_from(using_fpr, SlotBase::Own, src, len);
                } else {
                    let inline = if len as usize <= ARRAY_INLINE_CAPA {
                        Some((src, len))
                    } else {
                        None
                    };
                    ir.new_array(using_fpr, callid, inline);
                }
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
                if base == SlotBase::Own {
                    state.write_back_elements(ir, src, len);
                }
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
            VirtualSrc::Literal(_) | VirtualSrc::CallerKw { .. } | VirtualSrc::Range { .. } => None,
        };
        // Resolved as the call itself would be (refinements in scope
        // included), and a private method stays the interpreter's to reject.
        let resolved = cs.name.and_then(|name| {
            let (fid, visibility) = self.jit_check_method(class, name)?;
            (!self.jit_visibility_blocks(callid, visibility)).then_some((name, fid))
        });
        let summary = resolved.and_then(|(_, fid)| self.store.recv_escape(fid));
        let plain =
            cs.kw_args().is_empty() && cs.hash_splat_pos().is_empty() && cs.block_arg.is_none();
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
            }) if block_less => match (
                elements,
                Self::hash_pairs(state, src),
                Self::range_ends(src),
            ) {
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
        // The targets may overlap the element slots (`a, b = f(x, y)`
        // expands into the temporaries right above the result, which hold
        // the elements of an Array returned elided): copy from the far
        // end first, as a `memmove` does, so no element is overwritten
        // before it is read.
        let own = match virt {
            VirtualSrc::Slots { src, .. }
            | VirtualSrc::CallerSlots {
                src,
                base: SlotBase::Own,
                ..
            } => Some(src),
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
                VirtualSrc::Slots { src, len: n, .. }
                | VirtualSrc::CallerSlots {
                    src,
                    len: n,
                    base: SlotBase::Own,
                } => {
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
                VirtualSrc::CallerKw { .. } | VirtualSrc::Range { .. } => {
                    unreachable!("an Array expansion of a Hash or Range")
                }
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
    /// *elem*: a slot copy, unless the element is in the object's own slot
    /// (the first element of an Array returned elided, `V`), which is read
    /// from the stack.
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
    /// `dst = base[idx]` (`TraceIr::Index`) with an elided object in *base*
    /// ([`Self::may_consume_virtual`]). An elided Hash: a template's `[]`
    /// by a constant key it holds is the value, and by any other key runs
    /// on the template, as [`Self::consume_virtual`] has it for a method
    /// call. An elided Array whose elements are in frame slots: `[]` by a
    /// constant Integer is the element's slot (or nil), and by a Fixnum
    /// (proven, or guarded on the site's cached index class *ic*) reads
    /// the slot in place (`AsmInst::ElidedArrayIndex`); either way the
    /// object stays elided for its next use. Otherwise the object is
    /// materialized. Returns whether the instruction was lowered here.
    ///
    pub(super) fn consume_virtual_index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        dst: SlotId,
        ic: Option<(CachedClass, CachedClass)>,
        bc_pos: BcIndex,
    ) -> bool {
        let src = state.virtual_at(base).unwrap();
        let class = CachedClass::from_class(src.class_id());
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        let resolved =
            self.jit_check_method(class, IdentId::_INDEX)
                .and_then(|(fid, visibility)| {
                    (!self.jit_visibility_blocks(callid, visibility)).then_some(fid)
                });
        let summary = resolved.and_then(|fid| self.store.recv_escape(fid));
        let recv_dead = base == dst || self.dead_after(bc_pos, base);
        let elements = match src {
            VirtualSrc::Slots { src, len, .. } => Some((SlotBase::Own, src, len)),
            VirtualSrc::CallerSlots { src, len, base } => Some((base, src, len)),
            VirtualSrc::Literal(_) | VirtualSrc::CallerKw { .. } | VirtualSrc::Range { .. } => None,
        };
        let plan = match (summary, elements) {
            (
                Some(RecvEscape {
                    elements: Some(ElementsOp::ArrayIndex),
                    ..
                }),
                Some((base, src, len)),
            ) => {
                let idx_class = state
                    .class(idx)
                    .map(CachedClass::from_class)
                    .or(ic.map(|(_, class)| class));
                Self::plan_array_index(state, base, src, len, idx, idx_class).map(IndexPlan::Array)
            }
            (
                Some(RecvEscape {
                    elements: Some(op), ..
                }),
                None,
            ) => Self::hash_pairs(state, src)
                .and_then(|pairs| self.plan_hash_op(state, op, &pairs, idx, 1))
                .map(IndexPlan::Hash),
            _ => None,
        };
        match (plan, summary) {
            (Some(plan), _) => {
                self.inline_method_cache.push(InlineCacheEntry {
                    recv_class: class,
                    name: Some(IdentId::_INDEX),
                    refinements: self.refinements(),
                    func_id: resolved,
                });
                self.guard_class_version(state, ir, true);
                match plan {
                    IndexPlan::Hash(fold) => {
                        if recv_dead {
                            state.take_virtual(base);
                        }
                        self.emit_hash_fold(state, ir, fold, Some(dst));
                    }
                    IndexPlan::Array(ArrayIndexPlan::Nil) => {
                        if recv_dead {
                            state.take_virtual(base);
                        }
                        state.def_C(dst, Value::nil());
                    }
                    IndexPlan::Array(ArrayIndexPlan::Elem(SlotBase::Own, elem)) => {
                        if recv_dead {
                            state.take_virtual(base);
                        }
                        self.copy_element(state, ir, elem, dst);
                    }
                    IndexPlan::Array(ArrayIndexPlan::Elem(elem_base, elem)) => {
                        if recv_dead {
                            state.take_virtual(base);
                        }
                        state.flush_gp(ir);
                        ir.load_caller_slot(elem_base, elem);
                        state.def_rax2acc(ir, dst);
                    }
                    IndexPlan::Array(ArrayIndexPlan::Dynamic(elem_base, src, len)) => {
                        // The read is in place: the elements stay in their
                        // slots, so this frame's are written back first. The
                        // index guard deopts with the object still registered
                        // (the side exit builds it).
                        if elem_base == SlotBase::Own {
                            state.write_back_elements(ir, src, len);
                        }
                        state.load_fixnum(ir, idx, GP::Rsi);
                        if recv_dead {
                            state.take_virtual(base);
                        }
                        ir.elided_array_index(elem_base, src, len);
                        state.def_rax2acc(ir, dst);
                    }
                }
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
    /// Decide `[]` by *idx* on an elided Array whose `len` elements are the
    /// slots at *src* in the frame *base* names: a compile-time Integer
    /// index names its element's slot (counting from the end when
    /// negative) or is out of range, and a Fixnum index (*idx_class*) is
    /// read in place. Any other index (a Range, a non-Integer) is not
    /// lowered here.
    ///
    fn plan_array_index(
        state: &AbstractState,
        base: SlotBase,
        src: SlotId,
        len: u16,
        idx: SlotId,
        idx_class: Option<CachedClass>,
    ) -> Option<ArrayIndexPlan> {
        if let LinkMode::C(v) = state.mode(idx) {
            let i = v.try_fixnum()?;
            let i = if i < 0 { i + len as i64 } else { i };
            return Some(if (0..len as i64).contains(&i) {
                ArrayIndexPlan::Elem(base, src + i as u16)
            } else {
                ArrayIndexPlan::Nil
            });
        }
        (idx_class == Some(CachedClass::INTEGER)).then_some(ArrayIndexPlan::Dynamic(base, src, len))
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
        let lookup = |k: Value| {
            pairs
                .iter()
                .find(|(k0, _)| k0.id() == k.id())
                .map(|(_, v)| *v)
        };
        let found = |v: HashElem| match v {
            HashElem::Const(v) => HashFold::Const(v),
            HashElem::Caller(base, slot) => HashFold::Caller(base, slot),
        };
        match op {
            ElementsOp::Size if pos_num == 0 => {
                Some(HashFold::Const(Value::integer(pairs.len() as i64)))
            }
            ElementsOp::Empty if pos_num == 0 => {
                Some(HashFold::Const(Value::bool(pairs.is_empty())))
            }
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
                    state.write_back_elements(ir, src, len);
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
                        self.copy_element(state, ir, elem, dst);
                    } else {
                        state.flush_gp(ir);
                        ir.load_caller_slot(base, elem);
                        state.def_rax2acc(ir, dst);
                    }
                }
            },
            // `[]` with an argument is `TraceIr::Index`, lowered by
            // `consume_virtual_index`; as a method call it has no plan.
            ElementsOp::ArrayIndex => unreachable!("Array#[] as an argument-less call"),
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

/// How [`JitContext::consume_virtual_index`] lowers `base[idx]`.
enum IndexPlan {
    /// A folded Hash lookup.
    Hash(HashFold),
    /// An elided Array's element.
    Array(ArrayIndexPlan),
}

/// The answer of `[]` by an Integer on an elided Array
/// ([`JitContext::plan_array_index`]).
#[derive(Clone, Copy)]
enum ArrayIndexPlan {
    /// A constant index out of range.
    Nil,
    /// The element's slot, in the frame the base names.
    Elem(SlotBase, SlotId),
    /// A Fixnum index, read in place over the `len` slots at the slot, in
    /// the frame the base names (`AsmInst::ElidedArrayIndex`).
    Dynamic(SlotBase, SlotId, u16),
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
        store: &Store,
        ir: &mut AsmIr,
        callsite: &CallSiteInfo,
    ) {
        for slot in callsite.args..callsite.args + callsite.pos_num {
            if self.virtual_at(slot).is_some() {
                self.materialize_virtual_at(store, ir, slot, UsingFpr::new());
            }
        }
    }

    ///
    /// Perform the allocation of the elided object in *slot* of this frame
    /// alone, with *using_fpr* saved around it: the frame-level half of
    /// [`JitContext::materialize_virtual`], for the places that hold no
    /// `AbstractState` — an argument fill, a return segment
    /// (`doc/escape_analysis.md` §9).
    ///
    pub(in crate::codegen::jitgen) fn materialize_virtual_at(
        &mut self,
        store: &Store,
        ir: &mut AsmIr,
        slot: SlotId,
        using_fpr: UsingFpr,
    ) {
        let src = self.take_virtual(slot).unwrap();
        match src {
            VirtualSrc::Slots { src, len, callid } => {
                self.write_back_range(ir, src, len);
                if src != store[callid].args {
                    // Elements in a local's scratch window (§10), as in
                    // `JitContext::materialize_virtual`.
                    ir.create_array_from(using_fpr, SlotBase::Own, src, len);
                } else {
                    let inline = (len as usize <= ARRAY_INLINE_CAPA).then_some((src, len));
                    ir.new_array(using_fpr, callid, inline);
                }
                self.def_reg2acc_class(ir, GP::Rax, slot, ARRAY_CLASS);
            }
            VirtualSrc::Literal(val) => {
                ir.deep_copy_lit(using_fpr, val);
                self.def_reg2acc_concrete_value(ir, GP::Rax, slot, val);
            }
            VirtualSrc::CallerSlots { src, len, base } => {
                if base == SlotBase::Own {
                    self.write_back_elements(ir, src, len);
                }
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
        self.discard_dead_elements(src, slot);
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
/// The slot *trace_ir* defines without reading ([`TraceIr::defined_slot`]).
fn defined_slot(trace_ir: &TraceIr, store: &Store) -> Option<SlotId> {
    trace_ir.defined_slot(store)
}

/// What a call may capture of this frame ([`JitContext::capture_point`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::codegen::jitgen) enum Capture {
    /// The whole frame, through a `binding` / `eval` (or a call the
    /// compiler cannot name).
    Frame,
    /// The locals a block literal passed to the call reads.
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
