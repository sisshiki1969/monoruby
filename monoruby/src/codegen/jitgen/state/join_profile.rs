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
}

fn percent(part: u64, total: u64) -> f64 {
    if total == 0 {
        0.0
    } else {
        part as f64 * 100.0 / total as f64
    }
}

// ---------------------------------------------------------------------
// TypeIc-vs-lattice divergence: at every `TypeIc` the codegen pass
// compiles, classify what the abstract lattice already knows about the
// register against what the instruction's IC recorded — sampled *before*
// `speculate_type_ic` refines the slot, so the lattice side is the
// state's own knowledge, never the IC's reflection. Counted per
// compilation (a recompile or a specialization counts again: the table
// measures what the compiler sees, as often as it sees it), weighted
// both by site and by the IC's own VM execution counter.
// ---------------------------------------------------------------------

///
/// The relation of the lattice's knowledge (`L`) to the IC's proof (`P` =
/// what a membership guard over the recorded set would establish,
/// `Guarded::from_cached`/`from_cached_set`).
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum IcLattice {
    /// L = ⊤, IC empty: neither side knows anything (the site never ran
    /// in the VM tier under this body).
    TopIcEmpty,
    /// L = ⊤, IC usable (mono, or a foldable `{nil, c}`): the IC fills a
    /// vacuum — exactly where `speculate_type_ic` fires.
    TopIcUsable,
    /// L = ⊤, IC an unfoldable pair: observation exists but proves
    /// nothing the lattice can carry.
    TopIcUnfoldable,
    /// L = ⊤, IC megamorphic.
    TopIcMega,
    /// L typed, IC empty: a proven path the VM tier never executed
    /// (specialized bodies compiled from caller context).
    TypedIcEmpty,
    /// L typed and the IC's proof is exactly it.
    TypedAgree,
    /// L typed, IC proof strictly narrower (`L ⊔ P = L`, `P ≠ L`): the
    /// observation refines the proof — e.g. lattice `NilOr(c)` from a
    /// merge, IC mono-`c` because nil never actually flowed. The
    /// narrowing the ⊤-only speculation gate leaves on the table.
    TypedIcNarrower,
    /// L typed, IC proof strictly wider but compatible (`L ⊔ P = P`):
    /// the IC aggregates other contexts — e.g. lattice `Class(c)` on a
    /// specialized path, IC `{nil, c}` across all callers.
    TypedIcWider,
    /// L typed, IC megamorphic or unfoldable (its proof is ⊤): the
    /// aggregate view lost what this path proves.
    TypedIcTop,
    /// L typed, IC proof disjoint (`L ⊔ P = ⊤`, both ≠ ⊤): a genuine
    /// contradiction — a guard from the IC would always fail here. The
    /// shape the lattice-first precedence exists to neutralize.
    TypedDisjoint,
}

const IC_LATTICE_ORDER: [(IcLattice, &str); 10] = [
    (IcLattice::TopIcEmpty, "top    & IC empty"),
    (IcLattice::TopIcUsable, "top    & IC usable (speculated)"),
    (IcLattice::TopIcUnfoldable, "top    & IC unfoldable pair"),
    (IcLattice::TopIcMega, "top    & IC megamorphic"),
    (IcLattice::TypedAgree, "typed  & IC agrees"),
    (IcLattice::TypedIcEmpty, "typed  & IC empty"),
    (IcLattice::TypedIcNarrower, "typed  & IC narrower"),
    (IcLattice::TypedIcWider, "typed  & IC wider (compatible)"),
    (IcLattice::TypedIcTop, "typed  & IC top (mega/unfoldable)"),
    (IcLattice::TypedDisjoint, "typed  & IC disjoint"),
];

/// (sites, exec-weight) per bucket.
static IC_LATTICE: LazyLock<Mutex<HashMap<IcLattice, (u64, u64)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// The divergent buckets broken down by (lattice kind, IC-proof kind),
/// so a disagreement can be named.
static IC_LATTICE_DETAIL: LazyLock<Mutex<HashMap<(IcLattice, Op, Op), (u64, u64)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

pub(in crate::codegen::jitgen) fn record_type_ic(
    mode: LinkMode,
    a: Option<CachedClass>,
    b: Option<CachedClass>,
    execs: u32,
) {
    let lattice = match mode {
        LinkMode::S(Guarded::Value) => None,
        LinkMode::S(g) => Some(g),
        LinkMode::Sf(_, g) => Some(g.into()),
        LinkMode::F(_) => Some(Guarded::Float),
        // The guarded view the compiler itself reads off a constant
        // (`Value` for the classes the lattice cannot carry, e.g. a
        // Bignum literal — those few count as ⊤, matching what every
        // downstream consumer sees).
        LinkMode::C(v) => match Guarded::from_concrete_value(v) {
            Guarded::Value => None,
            g => Some(g),
        },
        // Not a value-bearing state; a TypeIc should never see these.
        LinkMode::V | LinkMode::None | LinkMode::MaybeNone => return,
    };
    // The IC's proof: megamorphic latch first ((0, MEGA) is a Bignum
    // latch on a never-otherwise-recorded site, not an empty IC).
    let ic = if b == Some(CachedClass::MEGA) {
        Some(Guarded::Value)
    } else {
        match (a, b) {
            (None, _) => None,
            (Some(a), None) => Some(Guarded::from_cached(a)),
            (Some(a), Some(b)) => Some(Guarded::from_cached_set(&[a, b])),
        }
    };
    let bucket = match (lattice, ic) {
        (None, None) => IcLattice::TopIcEmpty,
        (None, Some(Guarded::Value)) => {
            // Mega latch vs unfoldable pair: both read back as ⊤, told
            // apart by the raw words.
            if b == Some(CachedClass::MEGA) {
                IcLattice::TopIcMega
            } else {
                IcLattice::TopIcUnfoldable
            }
        }
        (None, Some(_)) => IcLattice::TopIcUsable,
        (Some(_), None) => IcLattice::TypedIcEmpty,
        (Some(l), Some(p)) => {
            if p == l {
                IcLattice::TypedAgree
            } else if p == Guarded::Value {
                IcLattice::TypedIcTop
            } else if l.join_raw(&p) == p {
                IcLattice::TypedIcWider
            } else if l.join_raw(&p) == l {
                IcLattice::TypedIcNarrower
            } else {
                IcLattice::TypedDisjoint
            }
        }
    };
    let e = execs as u64;
    {
        let mut t = IC_LATTICE.lock().unwrap();
        let ent = t.entry(bucket).or_insert((0, 0));
        ent.0 += 1;
        ent.1 += e;
    }
    if matches!(
        bucket,
        IcLattice::TypedIcNarrower
            | IcLattice::TypedIcWider
            | IcLattice::TypedDisjoint
            | IcLattice::TypedIcTop
    ) && let (Some(l), Some(p)) = (lattice, ic)
    {
        let mut t = IC_LATTICE_DETAIL.lock().unwrap();
        let ent = t.entry((bucket, op(&l, false), op(&p, false))).or_insert((0, 0));
        ent.0 += 1;
        ent.1 += e;
    }
}

pub(crate) fn dump_type_ic_lattice(store: &Store) {
    let table = IC_LATTICE.lock().unwrap();
    if table.is_empty() {
        return;
    }
    let (mut sites, mut execs) = (0u64, 0u64);
    for (s, e) in table.values() {
        sites += s;
        execs += e;
    }
    eprintln!();
    eprintln!(" type-ic lattice-vs-IC divergence (per compiled TypeIc, codegen pass):");
    eprintln!(
        "    {:34} {:>8}  {:>6}   {:>12}  {:>6}",
        "", "sites", "", "exec-weight", ""
    );
    for (bucket, label) in IC_LATTICE_ORDER {
        let (s, e) = table.get(&bucket).copied().unwrap_or((0, 0));
        eprintln!(
            "    {:34} {:>8} {:>6.2}%   {:>12} {:>6.2}%",
            label,
            s,
            percent(s, sites),
            e,
            percent(e, execs)
        );
    }
    let detail = IC_LATTICE_DETAIL.lock().unwrap();
    if !detail.is_empty() {
        eprintln!("    divergent pairs (lattice vs IC proof):");
        let mut rows: Vec<_> = detail.iter().collect();
        rows.sort_unstable_by(|(_, a), (_, b)| (b.1, b.0).cmp(&(a.1, a.0)));
        for ((bucket, l, p), (s, e)) in rows.into_iter().take(20) {
            eprintln!(
                "      {:11} {} vs {}   {} sites  {} execs",
                format!("{:?}", bucket),
                l.render(store),
                p.render(store),
                s,
                e
            );
        }
    }
}
