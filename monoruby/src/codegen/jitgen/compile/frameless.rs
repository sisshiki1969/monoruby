//! Frame-free expansion of the small-body idioms.
//!
//! `def initialize(a, b) = (@a = a; @b = b)` and `def inc = @count += 1`
//! are the most common method bodies in object-heavy Ruby, and today each
//! costs a full method frame to run a handful of `mov`s. This module
//! recognises them — [`ivar_store_body`] the constructor, [`leaf_expr_body`]
//! the accessor/counter — and lets
//! [`compile_method_call`](JitContext::compile_method_call) emit the body
//! straight into the caller, with no frame pushed at all.
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
//! callee's callee walks `cfp` and would find the caller's frame), no
//! `binding`, no `super`, no access to an `outer` frame.
//!
//! Raising is not on that list either, as long as the raise is reached by a
//! *guard*. `x / 0` deopts to the call instruction in both backends, and
//! the interpreter then performs the whole call — building the real frame
//! and raising `ZeroDivisionError` from it, with the backtrace that frame
//! gives it. What the guards buy is not "cannot raise" but **nothing has
//! happened yet**: the expansion is all-or-nothing, so an exit can hand the
//! entire call back. Both recognisers therefore hoist every guard ahead of
//! every store — the frozen guard in [`ivar_store_body`], and in
//! [`leaf_expr_body`] the single store is required to be the last
//! instruction before the `Ret`.

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
/// One step of a frame-free leaf body, applied in order to a single
/// accumulator.
///
/// The accumulator model is what keeps the expansion to two registers and
/// no stack slots, and it is not a restriction bytecodegen fights: these
/// bodies are single expressions, so their bytecode already threads one
/// value through one temporary.
///
#[derive(Clone, Copy, Debug)]
pub(super) enum LeafOp {
    /// `acc = value`. Needs no guard — every [`LeafValue`] is something the
    /// caller can name outright.
    Load(LeafValue),
    /// `acc = acc <kind> operand`, fixnum arithmetic. Guards both operands
    /// and, for `Div`, the zero divisor.
    Bin(BinOpK, LeafValue),
    ///
    /// `acc = acc <kind> operand`, comparison against a *tagged* value.
    /// Guards both operands as the carried class; the accumulator becomes a
    /// bool.
    ///
    /// The class is not always `Integer`. Both backends compare with a plain
    /// register compare (`cmpq` / `cmp x, x`), which for `==` / `!=` is bit
    /// equality — and bit equality *is* equality for every immediate:
    /// `Symbol`, `nil`, `true`, `false` as well as fixnums. Ordering is a
    /// fixnum-only reading of those bits, so the recogniser admits the other
    /// classes for equality alone.
    ///
    Cmp(CmpKind, LeafValue, ClassId),
    /// `@name = acc`.
    Store(IdentId),
}

impl LeafOp {
    /// Whether this op can side-exit. The ordering rule is stated in terms
    /// of these: none may follow a [`LeafOp::Store`].
    fn guards(&self) -> bool {
        matches!(self, LeafOp::Bin(..) | LeafOp::Cmp(..))
    }
}

///
/// A body that computes one value through an accumulator, storing to ivars
/// along the way, and returns it.
///
/// `@count += 1`, `@a * 2`, `@sum + x`, `@a > 0`, `@a`, `@n = 1; @m = 2` —
/// the small readers, writers, counters and predicates that make up most of
/// the leaf methods in object-heavy Ruby, and today cost a full frame each.
///
pub(super) struct LeafBody {
    /// Applied in order; the accumulator's final value is returned.
    pub ops: Vec<LeafOp>,
}

/// The most ops a body may have and still be expanded. Each guarding op is
/// two guards plus an arithmetic instruction in the caller, repeated at
/// every site that calls this method.
const MAX_OPS: usize = 8;

/// What a callee slot holds: the op sequence that computes it.
#[derive(Clone)]
struct Chain(Vec<LeafOp>);

impl Chain {
    fn leaf(base: LeafValue) -> Self {
        Self(vec![LeafOp::Load(base)])
    }

    /// The value this chain is, when it is a bare [`LeafValue`] — the only
    /// form admitted as an operand, which is what keeps the accumulator
    /// chain left-linear and the register need at two.
    fn as_value(&self) -> Option<LeafValue> {
        match self.0[..] {
            [LeafOp::Load(v)] => Some(v),
            _ => None,
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
/// # Why no guard may follow a store
///
/// Every guard this body needs — the operand class checks, the overflow
/// check, the zero-divisor check, the frozen check — side-exits to the
/// *call* instruction, and the interpreter then performs the whole call
/// itself. That is only sound while **nothing has happened yet**, so the
/// body's effects have to come after every exit.
///
/// That is the rule, and it is weaker than "one store, immediately before
/// the `Ret`", which is what the first version required. A body may store
/// as many times as it likes (`def reset = (@n = 0; @m = 0)`) as long as no
/// guarding op follows the first store; the hoisted frozen guard covers
/// every one of them, since nothing between them can call out and freeze
/// the receiver. What is declined is a guard *after* an effect — that exit
/// could no longer hand the call back, because part of it already ran.
///
/// `Div` needs no special treatment: a zero divisor is *already* a deopt in
/// both backends (x86-64 stamps `_divide_by_zero` and jumps to the exit,
/// aarch64 does `cbz x(r), deopt`), so it lands on the call instruction like
/// every other guard and the interpreter raises `ZeroDivisionError` from the
/// real frame it then builds — backtrace and all. `Rem` is the one exclusion,
/// and for a lowering reason rather than a semantic one: `BinOpK::Rem` is
/// `unreachable!()` in the register form of the integer binop.
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

    // Stores are the body's effects, so they are emitted where the body puts
    // them rather than folded into a slot's chain. `ops` is what has been
    // committed to the accumulator so far; a slot's chain is spliced onto it
    // when the slot is finally used.
    let mut ops: Vec<LeafOp> = vec![];
    let mut ret: Option<SlotId> = None;
    for idx in begin..=end {
        match TraceIr::from_pc(iseq.get_pc(idx), store) {
            TraceIr::InitMethod(..) => {}
            TraceIr::FrozenLiteral(dst, v) | TraceIr::Literal(dst, v) => {
                v.try_fixnum()?;
                slots.insert(dst, Chain::leaf(LeafValue::Fixnum(v)));
            }
            TraceIr::LoadIvar(dst, name, _) => {
                slots.insert(dst, Chain::leaf(LeafValue::SelfIvar(name)));
            }
            TraceIr::Mov(dst, src) => {
                let chain = slots.get(&src)?.clone();
                slots.insert(dst, chain);
            }
            TraceIr::BinOp {
                kind,
                dst: Some(dst),
                lhs,
                rhs,
                ic,
                polymorphic,
            } => {
                if !matches!(
                    kind,
                    BinOpK::Add | BinOpK::Sub | BinOpK::Mul | BinOpK::Div
                ) {
                    return None;
                }
                saw_fixnums(ic, polymorphic)?;
                let chain = extend(&slots, lhs, rhs, LeafOp::Bin(kind, LeafValue::Param(0)))?;
                slots.insert(dst, chain);
            }
            TraceIr::BinCmp {
                kind,
                dst: Some(dst),
                lhs,
                rhs,
                ic,
                polymorphic,
            } => {
                let class = cmp_operand_class(ic, polymorphic, kind)?;
                let chain = extend(
                    &slots,
                    lhs,
                    rhs,
                    LeafOp::Cmp(kind, LeafValue::Param(0), class),
                )?;
                slots.insert(dst, chain);
            }
            TraceIr::StoreIvar(src, name, _) => {
                // The stored value's chain is committed here, so its guards
                // land ahead of this store — and of every later one.
                let chain = slots.get(&src)?.clone();
                commit(&mut ops, chain)?;
                ops.push(LeafOp::Store(name));
                // The slot now *is* the accumulator: a later use of it (the
                // `ret` of `@n += 1`, say) must not recompute the chain.
                slots.insert(src, Chain(vec![]));
            }
            // A basic block ends at its terminator, so this runs last.
            TraceIr::Ret(slot) => ret = Some(slot),
            _ => return None,
        }
    }

    let chain = slots.remove(&ret?)?;
    commit(&mut ops, chain)?;
    // A body that only hands back a parameter is `ISeqHint`'s business.
    if let [LeafOp::Load(LeafValue::Param(_))] = ops[..] {
        return None;
    }
    if ops.len() > MAX_OPS {
        return None;
    }
    // The rule: nothing that can side-exit may follow an effect.
    let first_store = ops.iter().position(|op| matches!(op, LeafOp::Store(_)));
    if let Some(i) = first_store
        && ops[i..].iter().any(LeafOp::guards)
    {
        return None;
    }
    Some(LeafBody { ops })
}

///
/// Require that the VM has actually seen this operation on two fixnums.
///
/// The expansion is *all* fixnum: it guards both operands as `Integer` and
/// lowers to the integer register forms. Taking it at a site the VM has only
/// ever run on Floats does not produce a wrong answer — the guard exits and
/// the interpreter performs the call — but it replaces a working specialized
/// call with a guaranteed deopt on every execution, which is worse than not
/// expanding at all. Float-heavy code (`app_aobench.rb`'s vector methods) is
/// full of exactly these small bodies, so the inline cache is the difference
/// between a win and a regression there.
///
/// An empty cache means the VM never reached the instruction, which is no
/// evidence either; a polymorphic site means it saw more than one operand
/// class, so a fixnum-only expansion would exit on the others.
///
fn saw_fixnums(ic: Option<(ClassId, ClassId)>, polymorphic: bool) -> Option<()> {
    cmp_operand_class(ic, polymorphic, CmpKind::Lt).filter(|c| *c == INTEGER_CLASS)?;
    Some(())
}

///
/// The class both operands of a comparison were observed to have, when the
/// expansion can decide that comparison with a register compare.
///
/// For `==` / `!=` / `===` the compare is bit equality, which is equality
/// for every immediate — so `Symbol`, `nil`, `true` and `false` join
/// `Integer` here. `graphql`'s parser is the motivating shape:
///
/// ```ruby
/// def at?(expected_token_name)
///   @token_name == expected_token_name
/// end
/// ```
///
/// — 74 call sites, on the hot path of every parse, and an `Integer`-only
/// gate turned every one of them away.
///
/// Ordering reads those same bits as a signed number, which is only the
/// right answer for a tagged fixnum (`Symbol#<` compares the *names*, via
/// `Comparable`), so the other classes are admitted for equality alone.
///
fn cmp_operand_class(
    ic: Option<(ClassId, ClassId)>,
    polymorphic: bool,
    kind: CmpKind,
) -> Option<ClassId> {
    // A polymorphic site saw more than one operand class, so a
    // single-class guard would exit on the others.
    if polymorphic {
        return None;
    }
    // An empty cache means the VM never reached the instruction, which is
    // no evidence either way.
    let (lhs, rhs) = ic?;
    if lhs != rhs {
        return None;
    }
    let equality = matches!(kind, CmpKind::Eq | CmpKind::Ne | CmpKind::TEq);
    match lhs {
        INTEGER_CLASS => Some(lhs),
        NIL_CLASS | TRUE_CLASS | FALSE_CLASS | SYMBOL_CLASS if equality => Some(lhs),
        _ => None,
    }
}

///
/// Splice `chain` onto the ops committed so far.
///
/// An empty chain means the slot already *is* the accumulator (it was just
/// stored), so there is nothing to recompute. Otherwise the chain must start
/// by loading the accumulator afresh — a chain that continued from some
/// other slot's value would need that value still to be in the accumulator,
/// which only the empty case guarantees.
///
fn commit(ops: &mut Vec<LeafOp>, chain: Chain) -> Option<()> {
    if chain.0.is_empty() {
        return Some(());
    }
    if !matches!(chain.0[0], LeafOp::Load(_)) {
        return None;
    }
    ops.extend(chain.0);
    Some(())
}

///
/// Build the chain for `lhs <op> rhs`, where `op`'s operand field is a
/// placeholder to be filled with `rhs`'s value.
///
/// The right operand must be a bare [`LeafValue`]: a nested one would need a
/// third register and a slot to evaluate into, and bytecodegen does not
/// produce it for these bodies anyway.
///
fn extend(
    slots: &HashMap<SlotId, Chain>,
    lhs: SlotId,
    rhs: SlotId,
    op: LeafOp,
) -> Option<Chain> {
    let operand = slots.get(&rhs)?.as_value()?;
    let mut chain = slots.get(&lhs)?.clone();
    if chain.0.is_empty() {
        return None;
    }
    chain.0.push(match op {
        LeafOp::Bin(kind, _) => LeafOp::Bin(kind, operand),
        LeafOp::Cmp(kind, _, class) => LeafOp::Cmp(kind, operand, class),
        _ => return None,
    });
    Some(chain)
}
