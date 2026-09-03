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
use crate::bytecodegen::BinOpK;

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
                if stores.len() >= MAX_STORES {
                    return None;
                }
                stores.push((name, param_index(src)?));
            }
            // A basic block ends at its terminator, so this runs at most
            // once and always last.
            TraceIr::Ret(slot) => ret = Some(param_index(slot)?),
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

///
/// One value a frame-free leaf body can reach for.
///
/// All three are things the *caller* already holds: an argument in one of
/// its slots, an inline ivar of the receiver it is about to send to, or a
/// constant. None of them needs a frame to name.
///
#[derive(Clone, Copy, Debug)]
pub(super) enum LeafValue {
    /// The callee's parameter `i` (0-based), i.e. its `SlotId(i + 1)`.
    Param(u16),
    /// `@name` of the receiver.
    SelfIvar(IdentId),
    /// A fixnum literal.
    Fixnum(Value),
}

///
/// A body that computes one value by folding operands into an accumulator,
/// optionally stores it to an ivar, and returns it.
///
/// `@count += 1`, `@a * 2`, `@sum + x`, `@a` — the small readers, writers
/// and counters that make up most of the leaf methods in object-heavy Ruby,
/// and today cost a full frame each.
///
/// The chain is deliberately *left-linear*: every step's right operand is a
/// leaf. That keeps the expansion to two registers, and it is the shape
/// bytecodegen produces for these expressions anyway.
///
pub(super) struct LeafBody {
    /// The value the accumulator starts at.
    pub base: LeafValue,
    /// Folded in left to right: `acc = acc <kind> operand`.
    pub steps: Vec<(BinOpK, LeafValue)>,
    /// When set, the accumulator is stored to this ivar before returning.
    pub store: Option<IdentId>,
}

/// The longest accumulator chain that is still worth expanding. Each step
/// is two guards plus an arithmetic instruction in the caller, repeated at
/// every site that calls this method.
const MAX_STEPS: usize = 3;

/// What a callee slot holds, as an accumulator chain under construction.
#[derive(Clone)]
struct Chain {
    base: LeafValue,
    steps: Vec<(BinOpK, LeafValue)>,
}

impl Chain {
    fn leaf(base: LeafValue) -> Self {
        Self {
            base,
            steps: vec![],
        }
    }
}

///
/// Recognise a leaf body of the [`LeafBody`] shape.
///
/// The frame conditions are [`ivar_store_body`]'s, and for the same
/// reasons: no `outer`, plain positional parameters, a single basic block,
/// and an instruction set with no call, no `yield`, nothing that raises and
/// nothing that appears in a backtrace.
///
/// # Why the store must come last
///
/// Every guard this body needs — the operand class checks, the overflow
/// check, the frozen check — side-exits to the *call* instruction, and the
/// interpreter then performs the whole call itself. That is only sound
/// while nothing has happened yet, so the one effect a body may have has to
/// come after every exit. Requiring a single `StoreIvar` immediately before
/// the `Ret` buys that outright; anything else is declined.
///
/// `Div` and `Rem` are excluded for the same reason in reverse: they raise
/// `ZeroDivisionError`, which needs a frame to raise *from*.
///
pub(super) fn leaf_expr_body(store: &Store, iseq_id: ISeqId) -> Option<LeafBody> {
    let iseq = &store[iseq_id];
    if iseq.outer.is_some() {
        return None;
    }
    let func = &store[iseq.func_id()];
    if !func.meta().is_simple() || iseq.block_param().is_some() {
        return None;
    }
    let pos_num = func.params().total_positional_args();
    if pos_num > u16::MAX as usize {
        return None;
    }
    if iseq.bb_info.len() != 1 {
        return None;
    }
    let BasicBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(0)];

    // Parameters arrive in `SlotId(1) ..= SlotId(pos_num)`; `SlotId(0)` is self.
    let mut slots: HashMap<SlotId, Chain> = HashMap::default();
    for i in 0..pos_num {
        slots.insert(
            SlotId::new(i as u16 + 1),
            Chain::leaf(LeafValue::Param(i as u16)),
        );
    }

    let mut store_to: Option<(IdentId, SlotId)> = None;
    let mut ret: Option<SlotId> = None;
    for idx in begin..=end {
        // A store is the body's one effect, so nothing that can side-exit
        // may follow it. `Ret` is all that is allowed to.
        let after_store = store_to.is_some();
        match TraceIr::from_pc(iseq.get_pc(idx), store) {
            TraceIr::InitMethod(..) if !after_store => {}
            TraceIr::FrozenLiteral(dst, v) | TraceIr::Literal(dst, v) if !after_store => {
                v.try_fixnum()?;
                slots.insert(dst, Chain::leaf(LeafValue::Fixnum(v)));
            }
            TraceIr::LoadIvar(dst, name, _) if !after_store => {
                slots.insert(dst, Chain::leaf(LeafValue::SelfIvar(name)));
            }
            TraceIr::Mov(dst, src) if !after_store => {
                let chain = slots.get(&src)?.clone();
                slots.insert(dst, chain);
            }
            TraceIr::BinOp {
                kind,
                dst: Some(dst),
                lhs,
                rhs,
                ..
            } if !after_store => {
                if !matches!(kind, BinOpK::Add | BinOpK::Sub | BinOpK::Mul) {
                    return None;
                }
                let lhs = slots.get(&lhs)?.clone();
                let rhs = slots.get(&rhs)?;
                // Left-linear only: a nested right operand would need a
                // third register and a spill slot to evaluate.
                if !rhs.steps.is_empty() {
                    return None;
                }
                let operand = rhs.base;
                if lhs.steps.len() >= MAX_STEPS {
                    return None;
                }
                let mut chain = lhs;
                chain.steps.push((kind, operand));
                slots.insert(dst, chain);
            }
            TraceIr::StoreIvar(src, name, _) if !after_store => {
                store_to = Some((name, src));
            }
            // A basic block ends at its terminator, so this runs last.
            TraceIr::Ret(slot) => ret = Some(slot),
            _ => return None,
        }
    }

    let ret = ret?;
    // When the body stores, it must return the very value it stored — the
    // expansion computes the accumulator once.
    if let Some((_, src)) = store_to
        && src != ret
    {
        return None;
    }
    let chain = slots.remove(&ret)?;
    // A body that only hands back a parameter is `ISeqHint`'s business.
    if chain.steps.is_empty() && store_to.is_none() && matches!(chain.base, LeafValue::Param(_)) {
        return None;
    }
    Some(LeafBody {
        base: chain.base,
        steps: chain.steps,
        store: store_to.map(|(name, _)| name),
    })
}
