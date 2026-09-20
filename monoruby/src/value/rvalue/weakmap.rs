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
    /// Whether the *value* half is weak too.
    ///
    /// `ObjectSpace::WeakMap` is weak on both halves. Its sibling
    /// `ObjectSpace::WeakKeyMap` is weak-key and **strong**-value: a
    /// value reachable only from the map stays alive, and the pair goes
    /// only when its key dies. The two differ in nothing else, so they
    /// share this payload and `mark` / `clear_dead` read the flag.
    weak_values: bool,
}

impl WeakMapInner {
    /// A map whose keys *and* values are weak — `ObjectSpace::WeakMap`.
    pub fn new() -> Self {
        Self {
            entries: vec![],
            weak_values: true,
        }
    }

    /// A map whose keys are weak and whose values are held strongly —
    /// `ObjectSpace::WeakKeyMap`.
    pub fn new_weak_keys() -> Self {
        Self {
            entries: vec![],
            weak_values: false,
        }
    }

    /// The pairs, for a caller that has to compare keys with Ruby's
    /// `#hash` / `#eql?` and so cannot do it from in here.
    pub fn entries(&self) -> &[(Value, Value)] {
        &self.entries
    }

    /// Replace the pair at `index` outright.
    ///
    /// A `WeakKeyMap` given a key *equal* to one it holds takes the new
    /// key as well as the new value — CRuby's newest key wins, so
    /// `#getkey` afterwards answers the one most recently stored.
    pub fn set_at(&mut self, index: usize, key: Value, value: Value) {
        self.entries[index] = (key, value);
    }

    /// Append a pair whose key the caller has already established is
    /// not present.
    pub fn push(&mut self, key: Value, value: Value) {
        self.entries.push((key, value));
    }

    /// Remove the pair at `index`, answering the value it held.
    pub fn remove_at(&mut self, index: usize) -> Value {
        self.entries.remove(index).1
    }

    pub fn clear(&mut self) {
        self.entries.clear();
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

    /// Trace nothing — that is the whole point of a weak map — unless
    /// the values are the strong half, in which case they are traced
    /// and the keys still are not.
    pub fn mark(&self, alloc: &mut crate::alloc::Allocator<RValue>) {
        if self.weak_values {
            return;
        }
        for (_, v) in self.entries.iter() {
            v.mark(alloc);
        }
    }

    /// Drop every pair with a dead half. Called once per collection,
    /// after marking and before the sweep, so `is_marked` still answers
    /// for this cycle.
    fn clear_dead(&mut self, alloc: &crate::alloc::Allocator<RValue>) {
        let weak_values = self.weak_values;
        self.entries
            .retain(|(k, v)| survives(*k, alloc) && (!weak_values || survives(*v, alloc)));
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
