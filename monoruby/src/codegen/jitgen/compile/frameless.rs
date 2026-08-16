//! Frame-free expansion of the constructor idiom.
//!
//! `def initialize(a, b) = (@a = a; @b = b)` is the single most common
//! method body in object-heavy Ruby, and today it costs a full method
//! frame to run three `mov`s. This module recognises that body and lets
//! [`compile_method_call`](JitContext::compile_method_call) emit the
//! stores straight into the caller, with no frame pushed at all.
//!
//! # How this differs from the other two folds
//!
//! `ISeqHint::ConstReturn` / `SelfReturn` elide the call by *discarding*
//! the body — nothing of the callee is emitted. Specialization
//! (`specialized_iseq`) does the opposite: it emits the whole body, tuned
//! for the call site, and still **calls** it. This sits between them: the
//! body really runs, but as the caller's own instructions.
//!
//! # What makes the frame dispensable
//!
//! Not "the body cannot side-exit" — `attr_writer` already inlines behind
//! a frozen guard, and a deopt is fine because it rebuilds from the
//! *caller's* frame state, which is the one that exists. What the callee
//! must not do is *need a frame of its own*: no call or `yield` (a
//! callee's callee walks `cfp` and would find the caller's frame), nothing
//! that can raise or appear in a backtrace, no `binding`, no `super`, no
//! access to an `outer` frame.
//!
//! Restricting the body to `StoreIvar` from a parameter slot, plus the
//! `Ret`, buys all of that at once: every instruction is a move between
//! things the caller already holds. The frozen guard is the one side exit,
//! and it is hoisted ahead of the stores so the expansion is all-or-nothing
//! (see [`ivar_store_body`] for why that matters).

use super::*;

///
/// Where a callee parameter's value lives, as seen from the frame being
/// compiled.
///
#[derive(Clone, Copy)]
pub(super) enum ArgSlot {
    /// A slot of this frame — the ordinary case.
    Own(SlotId),
    ///
    /// A slot of the **caller's** frame, reachable only through the saved
    /// frame pointer.
    ///
    /// This is where a `...` forward's positionals stay when D1 defers the
    /// rest `Array`: `Class#new`'s `o.__builtin_initialize__(...)` never
    /// materializes them into its own slots, so the expansion reads them
    /// where the caller left them (`AsmInst::LoadCallerSlot`). They are
    /// live there for the whole call, and on the control-frame chain, so
    /// this is as GC-safe as the rest-array materialization that would
    /// otherwise read the same window.
    ///
    Caller(SlotId),
}

///
/// The recognised shape of a frame-free constructor body.
///
/// Parameter indices are 0-based: index `i` is the callee's `SlotId(i + 1)`,
/// and the call site maps it back to whichever caller slot supplies that
/// argument.
///
pub(super) struct IvarStoreBody {
    ///
    /// `(ivar name, parameter index)` in body order. Order is preserved
    /// because it is observable: it fixes the order the ivar slots are
    /// created in, and hence `#instance_variables`.
    ///
    pub stores: Vec<(IdentId, u16)>,
    ///
    /// The parameter index whose value the body returns — the last
    /// assignment's RHS. `initialize`'s result is discarded by `Class#new`,
    /// so this is usually dead, but a direct `obj.send(:initialize, ...)`
    /// can read it.
    ///
    pub ret: u16,
}

///
/// The most ivar stores a body may contain and still be expanded.
///
/// Each store is one `mov` in the caller, so the only cost of a larger
/// body is the caller's code size, multiplied by the number of sites that
/// construct this class. Six is `OBJECT_INLINE_IVAR`, i.e. every store an
/// object can take without spilling to the heap table — past that the
/// expansion would be declined anyway.
///
const MAX_STORES: usize = 6;

///
/// Recognise `def initialize(a, b, ...) = (@x = a; @y = b; ...)`.
///
/// Returns `None` for anything else. The test is purely *static* — the
/// shape of the body. The call site adds the conditions that depend on it
/// (the argument shape binds without `ArgumentError`, the receiver class
/// is known, its ivar slots exist and are inline); see
/// [`JitContext::expand_ivar_stores`].
///
/// # Why a single basic block
///
/// A branch would be legal in principle (`CondBr` reads the Value itself,
/// so it needs no operand class and no call), but a *conditional* store
/// breaks the all-or-nothing property the hoisted frozen guard relies on:
/// the guard would have to prove the object unfrozen even on the path that
/// stores nothing, turning a legal call into a deopt. Straight-line bodies
/// are what the target idiom is, so this stays the whole scope for now.
///
pub(super) fn ivar_store_body(store: &Store, iseq_id: ISeqId) -> Option<IvarStoreBody> {
    let iseq = &store[iseq_id];
    // An `outer` means the body reads locals through `outer_lfp` — exactly
    // the frame it would not have.
    if iseq.outer.is_some() {
        return None;
    }
    let func = &store[iseq.func_id()];
    // `is_simple` is the JIT's usual "plain positional parameters only"
    // test: no optional, rest, post, keyword, block or forwarding
    // parameter. Each of those needs the argument binding that frame setup
    // performs; the caller already holds the plain ones in slots.
    if !func.meta().is_simple() || iseq.block_param().is_some() {
        return None;
    }
    let pos_num = func.params().total_positional_args();
    if pos_num == 0 || pos_num > u16::MAX as usize {
        return None;
    }
    if iseq.bb_info.len() != 1 {
        return None;
    }
    let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(0)];
    // Parameters occupy `SlotId(1) ..= SlotId(pos_num)`; `SlotId(0)` is self.
    let param_index = |slot: SlotId| -> Option<u16> {
        let i = slot.0;
        (i >= 1 && i as usize <= pos_num).then(|| i - 1)
    };
    let mut stores: Vec<(IdentId, u16)> = vec![];
    let mut ret = None;
    for idx in begin..=end {
        match TraceIr::from_pc(iseq.get_pc(idx), store) {
            // The prologue. Binding positional arguments the caller has.
            TraceIr::InitMethod(..) => {}
            TraceIr::StoreIvar(src, name, _) => {
                if stores.len() >= MAX_STORES || ret.is_some() {
                    return None;
                }
                stores.push((name, param_index(src)?));
            }
            TraceIr::Ret(slot) => {
                if ret.is_some() {
                    return None;
                }
                ret = Some(param_index(slot)?);
            }
            _ => return None,
        }
    }
    if stores.is_empty() {
        // A body with no store is `ISeqHint`'s business, not ours.
        return None;
    }
    Some(IvarStoreBody {
        stores,
        ret: ret?,
    })
}
