//! The fstring table.
//!
//! CRuby keeps one String per content (`rb_fstring`): `-str`, a frozen
//! string literal, and the copy a Hash makes of an unfrozen String key
//! (`rb_hash_key_str`) all answer it, so `h.keys[0].equal?(-"k")`. This
//! is that table: content → the fstring of that content.
//!
//! The table is *weak*. The Strings a program names in its source or
//! asks for with `-str` are also held by `Store::frozen_str_pool`, which
//! roots them; a Hash key copy must not be rooted — a program that
//! stores a million distinct keys and drops the Hash would keep every
//! one of them. So the collector does not trace this table: an entry is
//! found by content while its String is alive and taken out when the
//! String's cell is freed (`RValue::free`, the way CRuby's `rb_str_free`
//! deletes from its table).
//!
//! An entry may name a String nothing references any more, until the
//! next collection sweeps it. Handing that String out again is sound:
//! its cell has not been freed (the sweep that frees it removes the
//! entry in the same step, with no Ruby code in between), and an
//! fstring is a bare frozen String, with nothing behind it to trace.
//!
//! The key is the 64-bit hash of the content, computed once per
//! operation and never hashed again. Two contents with one hash share a
//! slot the first of them keeps: the other is still an fstring (the
//! header bit), just not one a later lookup finds.

use super::*;
use crate::vm::FSTRINGS;
use std::hash::{BuildHasherDefault, Hash, Hasher};

/// The keys are hashes already.
#[derive(Default)]
pub(crate) struct PassThrough(u64);

impl Hasher for PassThrough {
    fn write(&mut self, _: &[u8]) {
        unreachable!("the fstring table is keyed by u64")
    }

    fn write_u64(&mut self, n: u64) {
        self.0 = n;
    }

    fn finish(&self) -> u64 {
        self.0
    }
}

/// Content hash → the fstring of that content, as a raw cell (not a
/// root).
#[derive(Default)]
pub(crate) struct FstringTable {
    table: std::collections::HashMap<u64, Value, BuildHasherDefault<PassThrough>>,
}

fn content_hash(bytes: &[u8], enc: Encoding) -> u64 {
    let mut hasher = fxhash::FxHasher::default();
    hasher.write(bytes);
    enc.hash(&mut hasher);
    // The table takes its bucket from the low bits and its tag from the
    // high ones; Fx leaves the low ones the weaker.
    let h = hasher.finish();
    h ^ (h >> 32)
}

fn has_content(v: Value, bytes: &[u8], enc: Encoding) -> bool {
    let s = v.as_rstring_inner();
    s.encoding() == enc && s.as_bytes() == bytes
}

fn lookup_hashed(hash: u64, bytes: &[u8], enc: Encoding) -> Option<Value> {
    FSTRINGS.with_borrow(|t| {
        t.table
            .get(&hash)
            .copied()
            .filter(|v| has_content(*v, bytes, enc))
    })
}

fn insert_hashed(hash: u64, v: Value) {
    debug_assert!(v.rvalue().is_fstr());
    FSTRINGS.with_borrow_mut(|t| {
        t.table.entry(hash).or_insert(v);
    });
}

/// The fstring of `(bytes, enc)`, if one is alive.
pub(crate) fn lookup(bytes: &[u8], enc: Encoding) -> Option<Value> {
    lookup_hashed(content_hash(bytes, enc), bytes, enc)
}

/// The fstring of `(bytes, enc)`: the one that is alive, or the String
/// `make` answers — a frozen bare String of that content with the
/// fstring bit set — which becomes it.
///
/// `make` runs under the table's borrow, in the slot the one probe
/// found. It allocates, and an allocation never collects (it requests a
/// collection, which runs at a safepoint), so `forget` cannot re-enter
/// the table from under it.
pub(crate) fn intern(bytes: &[u8], enc: Encoding, make: impl FnOnce() -> Value) -> Value {
    use std::collections::hash_map::Entry;
    let hash = content_hash(bytes, enc);
    FSTRINGS.with_borrow_mut(|t| match t.table.entry(hash) {
        Entry::Occupied(e) => {
            let v = *e.get();
            // Another content with this hash keeps the slot.
            if has_content(v, bytes, enc) {
                v
            } else {
                make()
            }
        }
        Entry::Vacant(e) => {
            let v = make();
            debug_assert!(v.rvalue().is_fstr());
            *e.insert(v)
        }
    })
}

/// Record `v` — a frozen bare String with the fstring bit set, whose
/// content `lookup` has just not found.
pub(crate) fn insert(v: Value) {
    let s = v.as_rstring_inner();
    insert_hashed(content_hash(s.as_bytes(), s.encoding()), v);
}

/// `cell`, an fstring, is being freed: it stops being findable. A
/// String that was made an fstring without this table (`Symbol#name`,
/// `Module#name`) is simply not found. During the interpreter's
/// teardown there is no table left to update.
pub(crate) fn forget(cell: &RValue) {
    // SAFETY: the caller hands in a String cell.
    let s = unsafe { cell.as_rstring() };
    let hash = content_hash(s.as_bytes(), s.encoding());
    let _ = FSTRINGS.try_with(|t| {
        let mut t = t.borrow_mut();
        if t.table
            .get(&hash)
            .is_some_and(|v| std::ptr::eq(v.rvalue(), cell))
        {
            t.table.remove(&hash);
        }
    });
}

/// How many Strings the table holds, for the tests.
#[cfg(test)]
pub(crate) fn len() -> usize {
    FSTRINGS.with_borrow(|t| t.table.len())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn key_copies_leave_the_table_when_they_are_collected() {
        // Each test runs on a thread, and so an interpreter, of its own.
        let before = super::len();
        let res = run_test_no_result_check(
            r##"
            h = {}
            20_000.times { |i| h["fstring-table-key-#{i}"] = i }
            # A second Hash asks for the same contents and adds nothing.
            g = {}
            20_000.times { |i| g["fstring-table-key-#{i}"] = i }
            h.keys[123].equal?(g.keys[123]) && h.keys[7].equal?(-"fstring-table-key-7")
            "##,
        );
        assert!(res.as_bool());
        // The 20,000 keys, once, and the frozen literals of the builtins.
        let held = super::len() - before;
        assert!((20_000..23_000).contains(&held), "held {held}");
        // Nothing references the Hashes of the run above any more; what
        // stays is the literals of the interpreter that runs this.
        run_test_no_result_check("GC.start; GC.start; nil");
        let left = super::len().saturating_sub(before);
        assert!(left < 3_000, "{left} of {held} entries survived");
    }
}
