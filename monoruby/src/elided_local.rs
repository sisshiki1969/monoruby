//! Markers for elided objects the JIT dropped from a local variable, and
//! their materialization when a frame's locals become observable
//! (`doc/escape_analysis.md` §12, issue #1910).
//!
//! The escape analysis keeps `x = [a, b]` out of the heap while `x` is
//! only read by uses the compiler folds, and drops the object altogether
//! once nothing the compiler can see reads `x` again. "Nothing the
//! compiler can see" counts every call the inline caches name: a call
//! cached to a callee that cannot capture its caller's frame is not a
//! read. But a side exit hands the frame to the interpreter, and there
//! the same call site may reach `binding` under another name — the
//! Binding then has to show the Array the compiled code never built.
//!
//! So a dropped object leaves a *marker* behind: a hidden object, one per
//! distinct source description, that the compiled code holds as the
//! local's constant value. It costs nothing on the fast path (a constant
//! is only stored by a write-back), it is carried through merges and loop
//! heads by the machinery every constant already uses, and it is
//! overwritten by the next assignment to the local like any value. The
//! elements it refers to are in the local's scratch window, which only
//! the next literal assigned to the same local rewrites.
//!
//! Nothing in the bytecode reads the local while the marker is in it
//! (that is what made the object dead), so the only way to see one is to
//! capture the frame: [`materialize`] runs wherever a Binding is made or
//! used and wherever source is compiled against a live frame, and
//! replaces each marker in the frame chain with the object it stands
//! for.

use crate::codegen::jitgen::{RangeEnd, VirtualSrc};
use crate::*;
use std::cell::RefCell;

///
/// What a marker stands for: a [`VirtualSrc`] whose materialization
/// reads nothing but the frame the marker is found in.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Elided {
    /// An Array of the frame's slots `src .. src + len`.
    Slots { src: SlotId, len: u16 },
    /// A deep copy of the literal with this id (`Value::id`).
    Literal(u64),
    /// A Range over constants or slots of the frame.
    Range {
        start: End,
        end: End,
        exclude_end: bool,
    },
}

/// An endpoint of [`Elided::Range`]: a constant's id, or a slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum End {
    Const(u64),
    Slot(SlotId),
}

impl From<RangeEnd> for End {
    fn from(e: RangeEnd) -> Self {
        match e {
            RangeEnd::Const(v) => End::Const(v.id()),
            RangeEnd::Slot(s) => End::Slot(s),
        }
    }
}

/// Every marker handed out, both ways.
#[derive(Default)]
pub(crate) struct ElidedLocals {
    /// The marker of each description.
    markers: HashMap<Elided, Value>,
    /// The description of each marker, by the marker's id.
    described: HashMap<u64, Elided>,
    /// The literals the descriptions refer to by id, kept for the
    /// collector (the bytecode that owns them may be gone by the time a
    /// marker is read).
    literals: Vec<Value>,
}

pub(crate) static ELIDED_LOCALS: vm::VmField<RefCell<ElidedLocals>> =
    vm::VmField::new(vm::elided_locals);

///
/// The marker for an object *src* describes, dropped from a local of a
/// frame: `None` when building it needs more than that frame (an object
/// handed over by the caller, whose elements are in the caller's frame).
///
/// A literal that is the same object at every evaluation (a frozen
/// `lo..hi`) is its own marker.
///
pub(crate) fn marker(src: VirtualSrc) -> Option<Value> {
    let (elided, literal) = match src {
        VirtualSrc::Slots { src, len, .. } => (Elided::Slots { src, len }, None),
        VirtualSrc::Literal(v) if v.class().is_always_frozen() => return Some(v),
        VirtualSrc::Literal(v) => (Elided::Literal(v.id()), Some(v)),
        VirtualSrc::Range {
            start,
            end,
            exclude_end,
        } => (
            Elided::Range {
                start: start.into(),
                end: end.into(),
                exclude_end,
            },
            None,
        ),
        VirtualSrc::CallerSlots { .. } | VirtualSrc::CallerKw { .. } => return None,
    };
    if let Some(marker) = ELIDED_LOCALS.with_borrow(|t| t.markers.get(&elided).copied()) {
        return Some(marker);
    }
    // Allocated outside the borrow: the collector reads the table.
    let marker = Value::object(OBJECT_CLASS);
    ELIDED_LOCALS.with_borrow_mut(|t| {
        t.markers.insert(elided, marker);
        t.described.insert(marker.id(), elided);
        t.literals.extend(literal);
    });
    Some(marker)
}

///
/// Replace every marker in the frame *lfp* and the frames of its outer
/// chain with the object it stands for.
///
/// Called before the locals of a frame can be read by name: when a
/// Binding is made or used, and when source is compiled against a live
/// frame (`eval`, `instance_eval` with a String).
///
pub(crate) fn materialize(lfp: Lfp) {
    if ELIDED_LOCALS.with_borrow(|t| t.described.is_empty()) {
        return;
    }
    let mut frame = Some(lfp);
    while let Some(mut f) = frame {
        let meta = f.meta();
        if !meta.is_native() {
            // Slot 0 is `self`.
            for slot in SlotId(1)..SlotId(meta.reg_num()) {
                let Some(v) = f.register(slot) else {
                    continue;
                };
                if v.is_packed_value() {
                    continue;
                }
                let Some(elided) = ELIDED_LOCALS.with_borrow(|t| t.described.get(&v.id()).copied())
                else {
                    continue;
                };
                let obj = build(f, elided);
                // SAFETY: *slot* is a register of the frame (below its
                // `reg_num`), which holds a `Value` like any other.
                unsafe { f.set_register(slot, Some(obj)) };
            }
        }
        frame = f.outer();
    }
}

/// The object *elided* describes, over the frame *lfp*.
fn build(lfp: Lfp, elided: Elided) -> Value {
    let slot = |s: SlotId| lfp.register(s).unwrap_or_default();
    match elided {
        Elided::Slots { src, len } => Value::array_from_iter((0..len).map(|i| slot(src + i))),
        Elided::Literal(id) => Value::from_u64(id).deep_copy(),
        Elided::Range {
            start,
            end,
            exclude_end,
        } => {
            let end_of = |e: End| match e {
                End::Const(id) => Value::from_u64(id),
                End::Slot(s) => slot(s),
            };
            Value::range(end_of(start), end_of(end), exclude_end)
        }
    }
}

/// Mark the markers (compiled code holds them as constants) and the
/// literals they describe.
pub(crate) fn mark(alloc: &mut alloc::Allocator<RValue>) {
    use alloc::GC;
    ELIDED_LOCALS.with_borrow(|t| {
        for marker in t.markers.values() {
            marker.mark(alloc);
        }
        for literal in &t.literals {
            literal.mark(alloc);
        }
    });
}
