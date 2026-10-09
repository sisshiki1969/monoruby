//! Per-(class, instance variable) type tracking.
//!
//! Every write of an instance variable is checked against a small type
//! state kept per class and per [`IvarId`], which only ever widens:
//!
//! ```text
//!   Empty ──> Mono(kind) ──> Top
//!     (each non-Top state also carries a "nil was written" bit)
//! ```
//!
//! Because *every* write path observes (the Rust chokepoint
//! [`RValue::set_ivar_by_ivarid`], a class change of the object, and the
//! JIT's inline stores through [`IvarTy`]-keyed checks), the state is an
//! over-approximation of every value any instance of the class has ever
//! held in that slot — not a sample. That is what lets the JIT treat a
//! load as typed.
//!
//! The state is keyed by the object's *actual* class (a singleton class
//! included), which is also what the JIT specializes `self` on and what
//! the ivar slot layout (`ClassInfo::ivar_names`) is keyed on.

use std::cell::RefCell;
use std::sync::atomic::AtomicU64;

use monoasm::DestLabel;

use crate::*;

/// The type state of one instance variable slot of one class.
///
/// Encoding (one word, so the JIT can bake it in as an immediate):
/// - bits 0..=2: kind tag ([`IvarTy::EMPTY`], [`IvarTy::FIXNUM`], …)
/// - bit 3: some `nil` was written
/// - bits 32..: the class id of a [`IvarTy::CLASS`] kind
#[derive(Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub(crate) struct IvarTy(u64);

impl std::fmt::Debug for IvarTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nil = if self.nil() { "|nil" } else { "" };
        match self.tag() {
            Self::EMPTY => write!(f, "Empty{nil}"),
            Self::FIXNUM => write!(f, "Fixnum{nil}"),
            Self::FLOAT => write!(f, "Float{nil}"),
            Self::BOOL => write!(f, "Bool{nil}"),
            Self::CLASS => write!(f, "Class({:?}){nil}", self.class_id()),
            _ => write!(f, "Top"),
        }
    }
}

/// What a single written value contributes to an [`IvarTy`].
#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Nil,
    Mono(u64),
    Top,
}

impl IvarTy {
    const EMPTY: u64 = 0;
    const FIXNUM: u64 = 1;
    const FLOAT: u64 = 2;
    const BOOL: u64 = 3;
    const CLASS: u64 = 4;
    const TOP_TAG: u64 = 7;
    pub(crate) const NIL_BIT: u64 = 0b1000;

    pub(crate) const TOP: Self = Self(Self::TOP_TAG);

    fn tag(self) -> u64 {
        self.0 & 0b111
    }

    /// Some `nil` was written.
    pub(crate) fn nil(self) -> bool {
        self.0 & Self::NIL_BIT != 0
    }

    pub(crate) fn is_top(self) -> bool {
        self.tag() == Self::TOP_TAG
    }

    fn class_id(self) -> ClassId {
        ClassId::new((self.0 >> 32) as u32)
    }

    /// The single non-nil type every write so far agreed on, as the class
    /// the JIT guards on: `INTEGER_CLASS` means a Fixnum (never a Bignum),
    /// `FLOAT_CLASS` a flonum or heap Float. `None` for Empty / nil-only /
    /// Top.
    pub(crate) fn mono_class(self) -> Option<ClassId> {
        match self.tag() {
            Self::FIXNUM => Some(INTEGER_CLASS),
            Self::FLOAT => Some(FLOAT_CLASS),
            Self::BOOL => Some(BOOL_CLASS),
            Self::CLASS => Some(self.class_id()),
            _ => None,
        }
    }

    /// The class of a [`IvarTy::CLASS`] state whose instances are heap
    /// objects (which can change class); `None` otherwise.
    pub(crate) fn heap_class(self) -> Option<ClassId> {
        if self.tag() == Self::CLASS && self.class_id() != SYMBOL_CLASS {
            Some(self.class_id())
        } else {
            None
        }
    }

    /// Nothing but `nil` was ever written (or nothing at all, if `!nil()`).
    #[cfg(test)]
    pub(crate) fn is_nil_or_empty(self) -> bool {
        self.tag() == Self::EMPTY
    }

    #[inline]
    fn kind_of(val: Value) -> Kind {
        if let Some(rv) = val.try_rvalue() {
            // Heap objects first: one header read gives the class.
            match rv.class() {
                FLOAT_CLASS => Kind::Mono(Self::FLOAT),
                // A Bignum is an `Integer` the Fixnum guard rejects; keep
                // `INTEGER_CLASS` meaning "Fixnum" by never recording it.
                INTEGER_CLASS => Kind::Top,
                class => Kind::Mono(Self::CLASS | ((class.u32() as u64) << 32)),
            }
        } else if val.is_nil() {
            Kind::Nil
        } else if val.is_fixnum() {
            Kind::Mono(Self::FIXNUM)
        } else if val.id() & 0b11 == 0b10 {
            // flonum
            Kind::Mono(Self::FLOAT)
        } else if val.id() == TRUE_VALUE || val.id() == FALSE_VALUE {
            Kind::Mono(Self::BOOL)
        } else {
            Kind::Mono(Self::CLASS | ((val.class().u32() as u64) << 32))
        }
    }

    /// The state after one more write of *val*.
    #[inline]
    fn join(self, val: Value) -> Self {
        if self.is_top() {
            return self;
        }
        match Self::kind_of(val) {
            Kind::Nil => Self(self.0 | Self::NIL_BIT),
            Kind::Top => Self::TOP,
            Kind::Mono(k) => {
                if self.tag() == Self::EMPTY {
                    Self(k | (self.0 & Self::NIL_BIT))
                } else if self.0 & !Self::NIL_BIT == k {
                    self
                } else {
                    Self::TOP
                }
            }
        }
    }

    /// The state after one more write of *val*, without recording it.
    pub(crate) fn joined(self, val: Value) -> Self {
        self.join(val)
    }

    /// The raw word (for the JIT).
    pub(crate) fn get(self) -> u64 {
        self.0
    }
}

/// One slot's state, and the compilation units that assumed it.
#[derive(Default)]
struct Entry {
    ty: IvarTy,
    /// The class-version snapshot words of the units compiled on the
    /// assumption that `ty` holds (see [`register_unit`]). Poisoned and
    /// dropped when `ty` changes.
    deps: Vec<DestLabel>,
    /// A copy of `ty` at a fixed address, for the JIT store checks to read
    /// ([`state_word`]); allocated on first request and never freed.
    mirror: Option<&'static AtomicU64>,
}

/// One assumption a compilation unit made: ivar slot `ivarid` of `class`
/// had state `ty` when the unit was compiled.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct IvarTyDep {
    pub class: ClassId,
    pub ivarid: IvarId,
    pub ty: IvarTy,
}

/// The type states, indexed by class id, then [`IvarId`].
#[derive(Default)]
pub(crate) struct IvarTyTable {
    table: Vec<Vec<Entry>>,
    /// Each unit's assumptions, keyed by the address of its class-version
    /// snapshot word, for salvage ([`unit_holds`]).
    units: HashMap<u64, Vec<IvarTyDep>>,
    /// Classes some instance of which changed class (got a singleton
    /// class, an `IO#reopen`, …) after it could have been stored: an ivar
    /// whose state records such a class may now hold an object of another
    /// class, so a load is not typed by it.
    escaped: HashSet<ClassId>,
    /// Classes some state has recorded ([`IvarTy::heap_class`]). Only an
    /// instance of one of these can sit in an ivar whose state types its
    /// loads by that class, so a class change of any other class's
    /// instance (the builtin singletons made at startup, a metaclass) is
    /// not an escape.
    recorded: HashSet<ClassId>,
    /// The units whose typed loads relied on a class not escaping.
    class_deps: HashMap<ClassId, Vec<DestLabel>>,
    /// Bumped whenever an assumption some unit made is broken: the
    /// safepoint poll compares it across its call ([`poison_epoch`]).
    epoch: u64,
}

impl IvarTyTable {
    #[inline]
    fn get(&self, class: ClassId, id: IvarId) -> IvarTy {
        self.table
            .get(class.u32() as usize)
            .and_then(|v| v.get(id.into_usize()))
            .map(|e| e.ty)
            .unwrap_or_default()
    }

    fn entry(&mut self, class: ClassId, id: IvarId) -> &mut Entry {
        let c = class.u32() as usize;
        if self.table.len() <= c {
            self.table.resize_with(c + 1, Vec::new);
        }
        let v = &mut self.table[c];
        let i = id.into_usize();
        if v.len() <= i {
            v.resize_with(i + 1, Entry::default);
        }
        &mut v[i]
    }

    /// Record *val*; returns the version words to poison, if the state
    /// changed under units that assumed it.
    fn observe(&mut self, class: ClassId, id: IvarId, val: Value) -> Vec<DestLabel> {
        let e = self.entry(class, id);
        let new = e.ty.join(val);
        if new == e.ty {
            return vec![];
        }
        e.ty = new;
        if let Some(m) = e.mirror {
            m.store(new.0, std::sync::atomic::Ordering::Relaxed);
        }
        let deps = std::mem::take(&mut e.deps);
        if let Some(c) = new.heap_class() {
            self.recorded.insert(c);
        }
        deps
    }
}

pub(crate) static IVAR_TYS: vm::VmField<RefCell<IvarTyTable>> = vm::VmField::new(vm::ivar_tys);

/// The type state of *class*'s ivar slot *id*.
pub(crate) fn ivar_ty(class: ClassId, id: IvarId) -> IvarTy {
    IVAR_TYS.with_borrow(|t| t.get(class, id))
}

/// Record that *val* was written to ivar slot *id* of an instance of
/// *class*. A change of a state some compiled unit assumed poisons that
/// unit's class-version word: its next class-version guard misses, and
/// salvage ([`unit_holds`]) refuses, so the unit is recompiled — and a
/// frame running it deopts at that guard, which every typed ivar load is
/// dominated by since the last point Ruby code (or this store) could run.
#[inline(never)]
pub(crate) fn observe(class: ClassId, id: IvarId, val: Value) {
    // Nearly every write leaves the state as it is: decide that under a
    // shared borrow, without touching the dependency lists.
    let unchanged = IVAR_TYS.with_borrow(|t| {
        let ty = t.get(class, id);
        ty.is_top() || ty.join(val) == ty
    });
    if !unchanged {
        widen(class, id, val);
    }
}

#[cold]
fn widen(class: ClassId, id: IvarId, val: Value) {
    let labels = IVAR_TYS.with_borrow_mut(|t| t.observe(class, id, val));
    if !labels.is_empty() {
        poison(labels);
    }
}

#[cold]
fn poison(labels: Vec<DestLabel>) {
    IVAR_TYS.with_borrow_mut(|t| t.epoch += 1);
    crate::codegen::CODEGEN.with(|codegen| {
        let mut codegen = codegen.borrow_mut();
        for label in &labels {
            codegen.set_class_version(crate::codegen::VERSION_IMM_SENTINEL as u32, label);
        }
    });
}

/// The address of a word that always holds the current state of *class*'s
/// ivar slot *id* (see `AsmInst::IvarTyCheck`).
pub(crate) fn state_word(class: ClassId, id: IvarId) -> u64 {
    IVAR_TYS.with_borrow_mut(|t| {
        let e = t.entry(class, id);
        let ty = e.ty;
        let m = *e
            .mirror
            .get_or_insert_with(|| Box::leak(Box::new(AtomicU64::new(ty.0))));
        m as *const AtomicU64 as u64
    })
}

/// How many times an assumption of a compiled unit has been broken.
///
/// A loop's typed ivar loads are not re-guarded after its safepoint poll,
/// which is the one point inside a call-free loop body where other Ruby
/// code (another thread, a trap handler, a finalizer) can run. The poll
/// instead deopts the frame (`executor::POLL_DEOPT`) when this moved
/// while it was away.
pub(crate) fn poison_epoch() -> u64 {
    IVAR_TYS.with_borrow(|t| t.epoch)
}

/// An instance of *class* is changing class. See `IvarTyTable::escaped`.
pub(crate) fn note_class_escape(class: ClassId) {
    let labels = IVAR_TYS.with_borrow_mut(|t| {
        if t.recorded.contains(&class) && t.escaped.insert(class) {
            t.class_deps.remove(&class).unwrap_or_default()
        } else {
            vec![]
        }
    });
    if !labels.is_empty() {
        poison(labels);
    }
}

/// May a load typed by state *ty* rely on its class? `false` once an
/// instance of the class has changed class.
pub(crate) fn class_reliable(ty: IvarTy) -> bool {
    match ty.heap_class() {
        Some(c) => IVAR_TYS.with_borrow(|t| !t.escaped.contains(&c)),
        None => true,
    }
}

/// The type state of *class*'s ivar slot *id* in words, for listings:
/// `Integer`, `Point|nil`, `nil`, `Empty`, `Top`, with ` (escaped)` when
/// the class is not trusted for typed loads (see [`note_class_escape`]).
#[cfg(feature = "emit-asm")]
pub(crate) fn describe(store: &crate::globals::Store, class: ClassId, id: IvarId) -> String {
    let ty = ivar_ty(class, id);
    let nil = if ty.nil() { "|nil" } else { "" };
    match ty.mono_class() {
        Some(c) => {
            let name = match ty.tag() {
                IvarTy::FIXNUM => "Integer".to_string(),
                IvarTy::BOOL => "true|false".to_string(),
                _ => store.debug_class_name(c),
            };
            let escaped = if class_reliable(ty) { "" } else { " (escaped)" };
            format!("{name}{nil}{escaped}")
        }
        None if ty.is_top() => "Top".to_string(),
        None if ty.nil() => "nil".to_string(),
        None => "Empty".to_string(),
    }
}

/// File a freshly compiled unit's assumptions under its class-version
/// word *label* (at *addr*).
pub(crate) fn register_unit(deps: Vec<IvarTyDep>, label: &DestLabel, addr: u64) {
    if deps.is_empty() {
        return;
    }
    IVAR_TYS.with_borrow_mut(|t| {
        for dep in &deps {
            let e = t.entry(dep.class, dep.ivarid);
            if e.ty != dep.ty {
                // Cannot happen: no Ruby code runs during a compile. If
                // it did, salvage would refuse the unit (`unit_holds`) at
                // its first version miss.
                e.deps.push(label.clone());
                continue;
            }
            if !e.deps.contains(label) {
                e.deps.push(label.clone());
            }
            if let Some(c) = dep.ty.heap_class() {
                let v = t.class_deps.entry(c).or_default();
                if !v.contains(label) {
                    v.push(label.clone());
                }
            }
        }
        t.units.insert(addr, deps);
    });
}

/// Do all assumptions of the unit whose class-version word is at *addr*
/// still hold? (Salvage refuses otherwise.)
pub(crate) fn unit_holds(addr: u64) -> bool {
    IVAR_TYS.with_borrow(|t| match t.units.get(&addr) {
        None => true,
        Some(deps) => deps.iter().all(|d| {
            t.get(d.class, d.ivarid) == d.ty
                && d.ty.heap_class().is_none_or(|c| !t.escaped.contains(&c))
        }),
    })
}

/// The JIT's out-of-line half of an inline store's type check (reached
/// through the `ivar_ty_observe` stub only when the inline test failed).
pub(crate) extern "C" fn jit_ivar_ty_observe(base: Value, id: u32, val: Value) {
    observe(base.class(), IvarId::new(id), val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn join_widens_monotonically() {
        let t = IvarTy::default();
        let t = t.join(Value::nil());
        assert!(t.nil() && t.is_nil_or_empty());
        let t = t.join(Value::integer(1));
        assert_eq!(Some(INTEGER_CLASS), t.mono_class());
        assert!(t.nil());
        let t = t.join(Value::integer(2));
        assert_eq!(Some(INTEGER_CLASS), t.mono_class());
        let t2 = t.join(Value::float(1.0));
        assert!(t2.is_top());
        assert!(t2.join(Value::integer(3)).is_top());
        let b = IvarTy::default()
            .join(Value::bool(true))
            .join(Value::bool(false));
        assert_eq!(Some(BOOL_CLASS), b.mono_class());
        let big = IvarTy::default()
            .join(Value::integer(1))
            .join(Value::bigint(num::BigInt::from(1u64) << 100));
        assert!(big.is_top());
    }
}
