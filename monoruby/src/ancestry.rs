//! Compile-time `is_a?` answers a JIT unit baked in, and whether they
//! still hold.
//!
//! `kernel_is_a` folds `x.is_a?(K)` from the superclass chain of the
//! receiver's class as it stands at compile time. The class version moves
//! when a module is included or prepended, so the unit's version guard
//! catches the change — but the class-version *salvage*
//! (`Store::salvage_method_unit`) re-stamps a unit whose recorded method
//! resolutions all still hold, and `Kernel#is_a?` resolves exactly as
//! before. The answer, not the resolution, is what changed. So every fold
//! records the `(class, target) -> answer` it assumed under the unit's
//! class-version word, and salvage re-walks each chain
//! ([`unit_holds`]) and refuses when an answer moved.

use crate::*;
use std::cell::RefCell;

/// One baked-in answer: an instance of `class` `is_a?(target)` ⇔ `answer`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct AncestryDep {
    pub class: ClassId,
    pub target: ClassId,
    pub answer: bool,
}

/// The assumptions of every live unit, keyed by the address of the unit's
/// class-version word.
#[derive(Default)]
pub(crate) struct AncestryTable {
    units: HashMap<u64, Vec<AncestryDep>>,
}

pub(crate) static ANCESTRY: vm::VmField<RefCell<AncestryTable>> = vm::VmField::new(vm::ancestry);

/// The compile-time `is_a?` answer for one class: walk its raw superclass
/// chain (ICLASS entries included, so included / prepended modules are
/// seen) looking for `target`. `None` for a class with no backing Module
/// object (the synthetic BOOL_CLASS), whose chain is not meaningful.
pub(crate) fn answer(store: &Store, class: ClassId, target: ClassId) -> Option<bool> {
    let mut cur = Some(store[class].try_get_module()?);
    while let Some(m) = cur {
        if m.id() == target {
            return Some(true);
        }
        cur = m.superclass();
    }
    Some(false)
}

/// File a freshly compiled unit's assumptions under its class-version
/// word at *addr*.
pub(crate) fn register_unit(deps: Vec<AncestryDep>, addr: u64) {
    if deps.is_empty() {
        return;
    }
    ANCESTRY.with_borrow_mut(|t| {
        t.units.insert(addr, deps);
    });
}

/// Do all assumptions of the unit whose class-version word is at *addr*
/// still hold? Salvage refuses otherwise, and the unit is recompiled —
/// so a refused record is dropped here.
pub(crate) fn unit_holds(store: &Store, addr: u64) -> bool {
    let holds = ANCESTRY.with_borrow(|t| match t.units.get(&addr) {
        None => true,
        Some(deps) => deps
            .iter()
            .all(|d| answer(store, d.class, d.target) == Some(d.answer)),
    });
    if !holds {
        ANCESTRY.with_borrow_mut(|t| {
            t.units.remove(&addr);
        });
    }
    holds
}
