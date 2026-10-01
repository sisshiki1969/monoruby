//!
//! Phase-0 measurement (`profile` feature only): count the joins whose type
//! component widens to ⊤ (`Guarded::Value` / `ReturnValue::Value`), keyed by
//! the pair of operand kinds, to size the nil-related share before deciding
//! whether a `NilOr` lattice element is worth introducing.
//!

use super::*;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

///
/// Where a widening was observed.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Site {
    /// `Guarded::join` (state/slot.rs) — the raw type-lattice meet. In a
    /// release build its only caller is the generic arm of `decide_join`,
    /// so these pairs are a *subset* of `Frame`'s (without const-ness); in
    /// a debug build the `join_ty` shadow pass goes through here too.
    Guarded,
    /// The type component of `AbstractFrame::join_with` (`decide_join`):
    /// the generic arm, plus the `Sf`/`C` arms whose `SfGuarded` meet
    /// widens to `FixnumOrFloat` (which reads back as ⊤).
    Frame,
    /// `ReturnState::join` (state.rs) falling back to `ReturnValue::Value`.
    Return,
}

///
/// The kind of one join operand, collapsed to what matters for the nil
/// question: nil and the boolean classes get their own buckets, every other
/// class keeps its `ClassId` so the table can name it.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum TyKind {
    Nil,
    Bool,
    Fixnum,
    Float,
    Class(ClassId),
    /// Already ⊤ on this side (`Guarded::Value`, `SfGuarded::FixnumOrFloat`,
    /// `ReturnValue::Value`).
    Top,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct Op {
    /// The operand was a compile-time constant (`LinkMode::C` /
    /// `ReturnValue::Const`).
    is_const: bool,
    /// The operand was a `NilOr` of `kind` (`kind` is the non-nil half).
    nil_or: bool,
    kind: TyKind,
}

impl Op {
    fn sort_key(&self) -> (u8, u32, u8, u8) {
        let (d, c) = match self.kind {
            TyKind::Nil => (0, 0),
            TyKind::Bool => (1, 0),
            TyKind::Fixnum => (2, 0),
            TyKind::Float => (3, 0),
            TyKind::Class(c) => (4, c.u32()),
            TyKind::Top => (5, 0),
        };
        (d, c, self.nil_or as u8, self.is_const as u8)
    }

    fn is_nil(&self) -> bool {
        self.kind == TyKind::Nil || self.nil_or
    }

    fn render(&self, store: &Store) -> String {
        let kind = match self.kind {
            TyKind::Nil => "nil".to_string(),
            TyKind::Bool => "bool".to_string(),
            TyKind::Fixnum => "Fixnum".to_string(),
            TyKind::Float => "Float".to_string(),
            TyKind::Class(c) => format!("Class({})", store.debug_class_name(c)),
            TyKind::Top => "Value".to_string(),
        };
        let kind = if self.nil_or {
            format!("nil|{kind}")
        } else {
            kind
        };
        if self.is_const {
            format!("C[{kind}]")
        } else {
            kind
        }
    }
}

fn op(g: &Guarded, is_const: bool) -> Op {
    let (nil_or, kind) = match g {
        Guarded::Fixnum => (false, TyKind::Fixnum),
        Guarded::Float => (false, TyKind::Float),
        Guarded::Value => (false, TyKind::Top),
        Guarded::Class(c) => match *c {
            NIL_CLASS => (false, TyKind::Nil),
            BOOL_CLASS | TRUE_CLASS | FALSE_CLASS => (false, TyKind::Bool),
            c => (false, TyKind::Class(c)),
        },
        Guarded::NilOr(nn) => match nn {
            NonNil::Fixnum => (true, TyKind::Fixnum),
            NonNil::Float => (true, TyKind::Float),
            NonNil::Class(c) => (true, TyKind::Class(*c)),
        },
    };
    Op {
        is_const,
        nil_or,
        kind,
    }
}

fn op_sf(g: SfGuarded, is_const: bool) -> Op {
    let kind = match g {
        SfGuarded::Fixnum => TyKind::Fixnum,
        SfGuarded::Float => TyKind::Float,
        SfGuarded::FixnumOrFloat => TyKind::Top,
    };
    Op {
        is_const,
        nil_or: false,
        kind,
    }
}

static TABLE: LazyLock<Mutex<HashMap<(Site, Op, Op), u64>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// `AsmInst::GuardClass` instructions pushed into codegen-mode streams —
/// the emitted-guard count the phase-1 before/after comparison reads.
static GUARD_CLASS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Dispatch-entry receiver classification (`method_call`): how often a
/// call site compiles with a lattice-proven class, a `NilOr`, or ⊤.
static RECV_PROVEN: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static RECV_TOP: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static RECV_NILOR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static RECV_OTHER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static NILOR_RECV_NAMES: LazyLock<Mutex<HashMap<(Option<IdentId>, Op), u64>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Classify the receiver's abstract state at the dispatch choke point.
pub(crate) fn record_dispatch_recv(
    mode: LinkMode,
    proven: Option<ClassId>,
    name: Option<IdentId>,
) {
    use std::sync::atomic::Ordering;
    if proven.is_some() {
        RECV_PROVEN.fetch_add(1, Ordering::Relaxed);
        return;
    }
    match mode {
        LinkMode::S(g @ Guarded::NilOr(_)) => {
            RECV_NILOR.fetch_add(1, Ordering::Relaxed);
            *NILOR_RECV_NAMES
                .lock()
                .unwrap()
                .entry((name, op(&g, false)))
                .or_insert(0) += 1;
        }
        LinkMode::S(Guarded::Value) => {
            RECV_TOP.fetch_add(1, Ordering::Relaxed);
        }
        _ => {
            RECV_OTHER.fetch_add(1, Ordering::Relaxed);
        }
    }
}

pub(crate) fn count_guard_class() {
    GUARD_CLASS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

fn record(site: Site, a: Op, b: Op) {
    // The joins are commutative; normalize the pair so `x ⊔ y` and `y ⊔ x`
    // land in one bucket.
    let (a, b) = if a.sort_key() <= b.sort_key() {
        (a, b)
    } else {
        (b, a)
    };
    *TABLE.lock().unwrap().entry((site, a, b)).or_insert(0) += 1;
}

/// `Guarded::join` widened `l ⊔ r` (`l != r`) to `Value`.
pub(super) fn record_guarded(l: &Guarded, r: &Guarded) {
    record(Site::Guarded, op(l, false), op(r, false));
}

/// The generic arm of `decide_join`: records only when the meet widens all
/// the way to `Value` (a meet to `NilOr` keeps its type).
pub(super) fn record_frame_generic(l: &Guarded, l_const: bool, r: &Guarded, r_const: bool) {
    if l != r && l.join_raw(r) == Guarded::Value {
        record(Site::Frame, op(l, l_const), op(r, r_const));
    }
}

/// An `Sf`-involved arm of `decide_join`: the `SfGuarded` meet of two
/// differing operands is `FixnumOrFloat`, which reads back as ⊤.
pub(super) fn record_frame_sf(l: SfGuarded, l_const: bool, r: SfGuarded, r_const: bool) {
    if l != r {
        record(Site::Frame, op_sf(l, l_const), op_sf(r, r_const));
    }
}

/// `ReturnState::join` fell back to `ReturnValue::Value`.
pub(super) fn record_return(l: &ReturnValue, r: &ReturnValue) {
    let to_op = |v: &ReturnValue| match v {
        ReturnValue::Const(v) => op(&Guarded::from_concrete_value(*v), true),
        ReturnValue::Typed(g) => op(g, false),
        // `UD` cannot reach the fallback (joined-away earlier); fold it
        // into ⊤ defensively rather than panicking in a stats hook.
        ReturnValue::Value | ReturnValue::UD => Op {
            is_const: false,
            nil_or: false,
            kind: TyKind::Top,
        },
    };
    let (l, r) = (to_op(l), to_op(r));
    if l.kind == TyKind::Top && r.kind == TyKind::Top {
        // Both sides had already lost their type; nothing widens here.
        return;
    }
    record(Site::Return, l, r);
}

///
/// Print the table, `show_stats` style. The nil share that gates Phase 1 is
/// computed over `Frame` + `Return` (the `Guarded` site is a subset of
/// `Frame` and would double-count).
///
pub(crate) fn dump(store: &Store) {
    let table = TABLE.lock().unwrap();
    eprintln!();
    eprintln!("type-widening join stats (lhs x rhs -> Value)");
    for (site, title) in [
        (Site::Frame, "AbstractFrame::join_with type component"),
        (Site::Return, "ReturnState::join"),
        (
            Site::Guarded,
            "Guarded::join (subset of join_with's generic arm)",
        ),
    ] {
        let mut rows: Vec<(&(Site, Op, Op), &u64)> =
            table.iter().filter(|((s, _, _), _)| *s == site).collect();
        let total: u64 = rows.iter().map(|(_, c)| **c).sum();
        let nil: u64 = rows
            .iter()
            .filter(|((_, a, b), _)| a.is_nil() || b.is_nil())
            .map(|(_, c)| **c)
            .sum();
        eprintln!();
        eprintln!(
            " [{title}]  total: {total}   nil-involved: {nil} ({:.2}%)",
            percent(nil, total)
        );
        eprintln!("    {:>12}   pair", "count");
        eprintln!("  ----------------------------------------------------------------------");
        rows.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
        for ((_, a, b), count) in rows {
            eprintln!(
                "    {:>12}   {} x {}",
                count,
                a.render(store),
                b.render(store)
            );
        }
    }
    let (mut total, mut nil) = (0u64, 0u64);
    for ((site, a, b), count) in table.iter() {
        if matches!(site, Site::Frame | Site::Return) {
            total += count;
            if a.is_nil() || b.is_nil() {
                nil += count;
            }
        }
    }
    eprintln!();
    eprintln!(
        " [frame + return]  total: {total}   nil-involved: {nil} ({:.2}%)",
        percent(nil, total)
    );
    eprintln!(
        " GuardClass emitted: {}",
        GUARD_CLASS.load(std::sync::atomic::Ordering::Relaxed)
    );
    let g = |c: &std::sync::atomic::AtomicU64| c.load(std::sync::atomic::Ordering::Relaxed);
    eprintln!();
    eprintln!(
        " dispatch-entry receivers: proven-class {}  top {}  NilOr {}  other {}",
        g(&RECV_PROVEN),
        g(&RECV_TOP),
        g(&RECV_NILOR),
        g(&RECV_OTHER)
    );
    let names = NILOR_RECV_NAMES.lock().unwrap();
    let mut rows: Vec<_> = names.iter().collect();
    rows.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
    for ((name, guarded), count) in rows.into_iter().take(20) {
        eprintln!(
            "    {:>8}   {} on {}",
            count,
            name.map_or("<super>".to_string(), |n| n.to_string()),
            guarded.render(store)
        );
    }
}

fn percent(part: u64, total: u64) -> f64 {
    if total == 0 {
        0.0
    } else {
        part as f64 * 100.0 / total as f64
    }
}
