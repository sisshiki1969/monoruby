//! The class → entry table in front of a long class-guard chain.
//!
//! A method is compiled once per class of `self`, and each compilation
//! adds a guard to the chain at the method's entry (`Codegen::compile_patch`).
//! A call that enters through the method's generic entry — from the
//! interpreter, an invoker, a generic send — tries the guards one after
//! another, so a method with many self classes pays for a walk whose
//! length is the number of classes compiled before the one it wants, each
//! step in a different page of code. Once the chain has
//! [`CLASS_DISPATCH_THRESHOLD`] guards, the entry goes to one stub that
//! looks the class up here instead.
//!
//! The table is an accelerator, not a replacement: the chain stays
//! complete and keeps growing at its tail. The stub takes it for what
//! the table does not hold — an immediate `self`, a heap `Integer` or
//! `Float` (whose guards are more than a class comparison) — and jumps
//! straight to its tail, where the next class is counted and compiled,
//! when a class is in neither.

use super::*;

/// The chain length from which the entry dispatches through the table.
pub(crate) const CLASS_DISPATCH_THRESHOLD: usize = 4;

/// What the dispatch stub reads. Its address is baked into the stub, so it
/// never moves; the table it points at is replaced when it grows.
#[repr(C)]
struct Header {
    /// The slots: `mask + 1` of them.
    table: *const Slot,
    /// Capacity − 1; the capacity is a power of two.
    mask: u64,
    /// Where a class the table does not hold goes: the end of the chain,
    /// which counts the call and compiles the class once it is hot.
    miss: u64,
    /// The first guard of the chain.
    chain: u64,
}

pub(crate) const CLASS_DISPATCH_TABLE: usize = std::mem::offset_of!(Header, table);
pub(crate) const CLASS_DISPATCH_MASK: usize = std::mem::offset_of!(Header, mask);
pub(crate) const CLASS_DISPATCH_MISS: usize = std::mem::offset_of!(Header, miss);
pub(crate) const CLASS_DISPATCH_CHAIN: usize = std::mem::offset_of!(Header, chain);

/// One slot: `class == 0` is empty (a `ClassId` is never zero).
#[repr(C)]
#[derive(Clone, Copy)]
struct Slot {
    class: u32,
    _pad: u32,
    target: u64,
}

pub(crate) const CLASS_DISPATCH_SLOT_SHIFT: usize = 4;
pub(crate) const CLASS_DISPATCH_SLOT_TARGET: usize = std::mem::offset_of!(Slot, target);
const _: () = assert!(std::mem::size_of::<Slot>() == 1 << CLASS_DISPATCH_SLOT_SHIFT);

const EMPTY: Slot = Slot {
    class: 0,
    _pad: 0,
    target: 0,
};

///
/// A method's class → entry table (open addressing, linear probing, at
/// most half full so a probe for an absent class ends at an empty slot).
///
/// The header and every table are leaked: compiled code holds the
/// header's address and may be anywhere in a probe's worth of
/// instructions away from a table, and an iseq's dispatch state is only
/// ever discarded by the rare whole-iseq eviction.
///
#[derive(Clone)]
pub(crate) struct ClassDispatch {
    header: *mut Header,
    len: usize,
}

impl std::fmt::Debug for ClassDispatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ClassDispatch({} classes)", self.len)
    }
}

impl ClassDispatch {
    /// Can a `self` of *class* be found by its class word alone? Not the
    /// classes with immediate instances — and of those, `Integer` and
    /// `Float` also have heap instances, which the stub therefore sends
    /// down the chain when it does not find them here.
    pub(crate) fn holds(class: ClassId) -> bool {
        !matches!(
            class,
            NIL_CLASS | TRUE_CLASS | FALSE_CLASS | SYMBOL_CLASS | INTEGER_CLASS | FLOAT_CLASS
        )
    }

    pub(crate) fn new(chain: u64, miss: u64) -> Self {
        const INITIAL: usize = 16;
        let table = Box::leak(vec![EMPTY; INITIAL].into_boxed_slice());
        let header = Box::leak(Box::new(Header {
            table: table.as_ptr(),
            mask: INITIAL as u64 - 1,
            miss,
            chain,
        }));
        Self { header, len: 0 }
    }

    /// The address the stub loads the header fields from.
    pub(crate) fn header_addr(&self) -> u64 {
        self.header as u64
    }

    /// The chain has a new tail.
    pub(crate) fn set_miss(&mut self, miss: u64) {
        // SAFETY: the header is leaked and written only under the GVL.
        unsafe { (*self.header).miss = miss };
    }

    /// The chain has a new first guard.
    #[cfg(target_arch = "aarch64")]
    pub(crate) fn set_chain(&mut self, chain: u64) {
        // SAFETY: as `set_miss`.
        unsafe { (*self.header).chain = chain };
    }

    /// Enter *class* → *target*, or replace its target.
    pub(crate) fn insert(&mut self, class: ClassId, target: u64) {
        debug_assert!(Self::holds(class));
        // SAFETY: the header and the table it points at are leaked; both
        // are written only here, under the GVL, never while compiled code
        // is in the middle of a probe (this runs from a compile request).
        unsafe {
            let cap = (*self.header).mask as usize + 1;
            if (self.len + 1) * 2 > cap {
                let old = std::slice::from_raw_parts((*self.header).table, cap);
                let new = Box::leak(vec![EMPTY; cap * 2].into_boxed_slice());
                let mask = cap * 2 - 1;
                for slot in old.iter().filter(|s| s.class != 0) {
                    Self::put(new, mask, *slot);
                }
                (*self.header).table = new.as_ptr();
                (*self.header).mask = mask as u64;
            }
            let mask = (*self.header).mask as usize;
            let table = std::slice::from_raw_parts_mut((*self.header).table as *mut Slot, mask + 1);
            if Self::put(
                table,
                mask,
                Slot {
                    class: class.u32(),
                    _pad: 0,
                    target,
                },
            ) {
                self.len += 1;
            }
        }
    }

    /// Returns whether *slot*'s class was new to the table.
    fn put(table: &mut [Slot], mask: usize, slot: Slot) -> bool {
        let mut i = slot.class as usize & mask;
        loop {
            if table[i].class == 0 {
                table[i] = slot;
                return true;
            }
            if table[i].class == slot.class {
                table[i].target = slot.target;
                return false;
            }
            i = (i + 1) & mask;
        }
    }
}
