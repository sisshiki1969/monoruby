//! Intra-block dispatch: several arms inside one basic block, rejoined.
//!
//! The CFG-driven merge machinery (`branch_map` / `gen_bridges_for_branches`)
//! keys on `BasicBlockId`, so it cannot describe a merge *inside* a block.
//! That is what a class dispatch needs — the arms are not basic blocks, they
//! are alternatives within one instruction's lowering:
//!
//! ```text
//!         br_class_ne recv, C0 -> L1
//!         <arm 0>              ; state refined to C0
//!         br merge
//!   L1:   br_class_ne recv, C1 -> L2
//!         <arm 1>
//!         br merge
//!   L2:   <residual>           ; guard -> deopt, or a generic call
//!   merge:
//! ```
//!
//! **The merge state is declared, not computed.** A true join would have to
//! see every arm's exit state, which means compiling them all before deciding
//! what to bridge to — and the arms have already been emitted by then. So the
//! merge is fixed up front, conservatively: the operands are flushed to their
//! stack homes before the split, `dst` becomes an unknown `Value`, and every
//! cached invariant is dropped because any arm may have run arbitrary Ruby.
//! Each arm then bridges from wherever it happens to have landed
//! ([`AbstractState::gen_bridge_all`] handles the rest).
//!
//! Declaring rather than joining costs precision, and only that: `S(Value)` is
//! the top of the lattice, so every arm can reach it. Two arms that both left
//! a `Fixnum` in a register have it boxed at the merge where a real join would
//! have kept the type. For the dispatches built on this so far — a polymorphic
//! `[]` and a polymorphic send, both of which end in a call whose result is an
//! opaque `Value` anyway — that costs nothing. A *numeric* dispatch (fixnum
//! arm / float arm / generic residual) would care, and the lattice already has
//! the element it wants: `LinkMode::Sf(fpr, SfGuarded::FixnumOrFloat)`, a
//! boxed slot with a live float shadow. Teaching `declare_merge` to take a
//! caller-supplied merge shape is the natural next step.

use super::*;

///
/// The merge point of an intra-block dispatch, declared before the arms are
/// emitted. Threaded from [`JitContext::declare_merge`] through each
/// [`JitContext::end_arm`] to [`JitContext::bind_merge`].
///
pub(super) struct DispatchMerge {
    /// The state every arm bridges to.
    target: AbstractState,
    /// Where the arms converge.
    merge: JitLabel,
    /// The bytecode position the bridges are emitted at.
    pc: BytecodePtr,
}

impl<'a> JitContext<'a> {
    ///
    /// Open an intra-block dispatch.
    ///
    /// Flushes *operands* to their stack homes — the arms diverge, so a value
    /// left in a register by one of them is not where the next one, or the
    /// merge, expects it — and returns the entry state each arm should clone
    /// plus the declared merge.
    ///
    pub(super) fn declare_merge(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        operands: &[SlotId],
        dst: Option<SlotId>,
    ) -> (AbstractState, DispatchMerge) {
        let pc = state.pc();
        state.write_back_slots(ir, operands);
        state.flush_gp(ir);
        let entry = state.clone();

        let mut target = entry.clone();
        if let Some(dst) = dst {
            target.def_S(dst);
        }
        // `next_sp` is the *post*-instruction stack top (the compile loop sets
        // it before lowering and clears above it afterwards), so the operand
        // slots an arm consumed are already dead at the merge. Kill them here
        // too: an arm that dropped one — `send` frees everything above sp —
        // would otherwise have to bridge `V` back to a live mode, which is not
        // a direction the lattice has.
        target.clear_above_next_sp();
        // Any arm may have called into Ruby, so nothing cached survives the
        // merge. Conservative in the same direction as `send`.
        target.unset_class_version_guard();
        target.unset_const_version_guard();
        target.unset_side_effect_guard();

        let merge = self.label();
        (
            entry,
            DispatchMerge {
                target,
                merge,
                pc,
            },
        )
    }

    ///
    /// Close one arm: bridge its state to the declared merge, and jump there.
    ///
    /// The last arm may fall through instead (`jump` false), which saves a
    /// branch when the merge label is emitted immediately after it.
    ///
    pub(super) fn end_arm(
        &mut self,
        mut arm: AbstractState,
        ir: &mut AsmIr,
        m: &DispatchMerge,
        jump: bool,
    ) {
        // The GP-pool residency is a layer *below* the link modes, and
        // the bridge does not see it: a slot both states call `S` bridges to
        // nothing even when this arm's only copy of its value is parked in a
        // pool register (which is exactly where a call result lands). The
        // merge state inherited an empty file from `declare_merge`, so every
        // arm has to pay its own residents back to their stack homes first.
        arm.flush_gp(ir);
        arm.gen_bridge_all(ir, &m.target.slot_states(), m.pc, &self.chain_surrender_table());
        if jump {
            ir.push(AsmInst::Br(m.merge));
        }
    }

    ///
    /// Close the dispatch: bind the merge label and install the merge state.
    ///
    pub(super) fn bind_merge(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        m: DispatchMerge,
    ) {
        ir.push(AsmInst::Label(m.merge));
        *state = m.target;
    }
}
