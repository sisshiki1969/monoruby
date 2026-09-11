use super::*;
use std::cell::RefCell;

///
/// The payload of an `ObjectSpace::WeakMap` (`ObjTy::WEAKMAP`).
///
/// Both halves of every pair are *weak*: `mark` deliberately traces
/// neither, so a key or value reachable only from here is collected, and
/// the pair goes with it (`clear_dead`, run between the mark and the
/// sweep). That is CRuby's `ObjectSpace::WeakMap`, and it is what makes
/// `WeakRef` a real weak reference rather than the strong-holding stub
/// monoruby shipped before.
///
/// Pairs are held in a `Vec` keyed by object identity — CRuby's WeakMap
/// compares keys with `equal?`, so the key is the `Value`'s bits and no
/// `hash` / `eql?` (which could re-enter Ruby, impossible during a
/// collection) is ever needed. A weak map is a registry, typically of a
/// few entries; the linear scan is not worth a hash table, and it keeps
/// `clear_dead` a single pass with no rehashing.
///
/// Neither half needs a write barrier. The barrier exists so an old
/// object's young children are re-found by a minor GC, and these
/// children are never traced at all.
#[derive(Debug, Clone, Default)]
pub struct WeakMapInner {
    entries: Vec<(Value, Value)>,
}

impl WeakMapInner {
    pub fn new() -> Self {
        Self::default()
    }

    /// The value stored under `key`, by identity.
    pub fn get(&self, key: Value) -> Option<Value> {
        self.entries
            .iter()
            .find(|(k, _)| k.id() == key.id())
            .map(|(_, v)| *v)
    }

    /// Store `value` under `key`, replacing any pair with that key.
    pub fn insert(&mut self, key: Value, value: Value) {
        match self.entries.iter_mut().find(|(k, _)| k.id() == key.id()) {
            Some(e) => e.1 = value,
            None => self.entries.push((key, value)),
        }
    }

    /// Remove the pair under `key`, answering the value it held.
    pub fn remove(&mut self, key: Value) -> Option<Value> {
        let i = self.entries.iter().position(|(k, _)| k.id() == key.id())?;
        Some(self.entries.remove(i).1)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Value, Value)> + '_ {
        self.entries.iter().copied()
    }

    /// Trace nothing: that is the whole point of a weak map.
    pub fn mark(&self, _alloc: &mut crate::alloc::Allocator<RValue>) {}

    /// Drop every pair with a dead half. Called once per collection,
    /// after marking and before the sweep, so `is_marked` still answers
    /// for this cycle.
    fn clear_dead(&mut self, alloc: &crate::alloc::Allocator<RValue>) {
        self.entries
            .retain(|(k, v)| survives(*k, alloc) && survives(*v, alloc));
    }
}

/// Whether a half keeps its pair alive. An immediate (Integer, Symbol,
/// nil, true, false, flonum) has no cell and can never be collected,
/// so it always does — storing under an immediate key is allowed, and
/// such a pair lives exactly as long as its value.
fn survives(v: Value, alloc: &crate::alloc::Allocator<RValue>) -> bool {
    match v.try_rvalue() {
        None => true,
        Some(rv) => alloc.is_marked_ref(rv),
    }
}

thread_local! {
    /// Every live weak map on this thread, as raw cells.
    ///
    /// This is not a root: the pointers are never marked, and an entry
    /// whose map is itself unmarked is dropped in the same pass that
    /// clears dead pairs, so a collected weak map leaves nothing behind.
    /// Weak maps are per-thread because the heap is.
    static WEAKMAPS: RefCell<Vec<*mut RValue>> = const { RefCell::new(Vec::new()) };
}

/// Record a freshly allocated weak map so the collector can find it.
pub(crate) fn register(v: Value) {
    if let Some(rv) = v.try_rvalue() {
        let p = rv as *const RValue as *mut RValue;
        WEAKMAPS.with(|m| m.borrow_mut().push(p));
    }
}

///
/// Clear the dead halves of every live weak map, and forget the maps
/// that died themselves.
///
/// Runs between the mark and the sweep: every reachable object has its
/// mark bit set by now, and no cell has been reclaimed yet, so reading
/// a pair's mark bit is both meaningful and safe.
///
pub(crate) fn clear_dead(alloc: &mut crate::alloc::Allocator<RValue>) {
    WEAKMAPS.with(|maps| {
        let mut maps = maps.borrow_mut();
        maps.retain(|&p| {
            // SAFETY: cells are reclaimed by the sweep, which has not run
            // yet, and a map is dropped from this list in the same cycle
            // it is found dead — so `p` still points at a live cell.
            let rv = unsafe { &mut *p };
            if !alloc.is_marked_ref(rv) {
                return false;
            }
            rv.as_weakmap_mut().clear_dead(alloc);
            true
        });
    });
}
