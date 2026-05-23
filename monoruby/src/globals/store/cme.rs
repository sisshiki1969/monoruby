// Foundation for the CmeId-in-frame migration. The interning table and
// accessors below are exercised by the unit tests now and wired into
// method definition / frame setup / `super` resolution in the following
// step; until then the runtime build has no caller.
#![allow(dead_code)]

use super::*;

///
/// Callable Method Entry (CME).
///
/// Bundles the per-invocation identity of a method call that a bare
/// [`FuncId`] cannot express:
///
/// - `func_id`   : the method body being run.
/// - `owner`     : the class/module in whose method table the method
///   was found (CRuby's *defined_class*). `super` searches the ancestor
///   chain *above* this class.
/// - `called_id` : the name the method was invoked by. This differs
///   from the method's definition name for aliases, which is what
///   distinguishes `__callee__` (the call name) from `__method__` (the
///   definition name).
///
/// The same `func_id` can map to several `(owner, called_id)` pairs — a
/// module method mixed into many classes, an `alias_method`, a shared
/// `define_method` body — so the CME, not the `FuncId`, is the correct
/// granularity for `super` / `__callee__`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Cme {
    func_id: FuncId,
    owner: ClassId,
    called_id: IdentId,
}

impl Cme {
    pub(crate) fn new(func_id: FuncId, owner: ClassId, called_id: IdentId) -> Self {
        Self {
            func_id,
            owner,
            called_id,
        }
    }

    pub(crate) fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub(crate) fn owner(&self) -> ClassId {
        self.owner
    }

    pub(crate) fn called_id(&self) -> IdentId {
        self.called_id
    }
}

///
/// Interned handle into [`CmeTable`].
///
/// Stored as a `NonZeroU32` so it fits the 32-bit `func_id` field of
/// [`Meta`] (the planned `cref_or_me`-style migration) and can later be
/// packed into a frame slot without growing the layout.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct CmeId(std::num::NonZeroU32);

impl CmeId {
    pub(crate) fn new(id: u32) -> Self {
        Self(std::num::NonZeroU32::new(id).unwrap())
    }

    pub fn get(&self) -> u32 {
        self.0.get()
    }
}

///
/// Interning table for [`Cme`]s.
///
/// Entries are append-only and de-duplicated: interning the same
/// `(func_id, owner, called_id)` triple always yields the same
/// [`CmeId`]. Because a `Cme` holds only plain integer ids (no GC
/// pointers), the table never participates in the GC mark phase.
///
/// Indexing is 1-based to match [`CmeId`]'s `NonZeroU32` (`CmeId(n)`
/// addresses `table[n - 1]`).
///
#[derive(Debug, Default)]
pub(crate) struct CmeTable {
    table: Vec<Cme>,
    map: HashMap<Cme, CmeId>,
}

impl std::ops::Index<CmeId> for CmeTable {
    type Output = Cme;
    fn index(&self, index: CmeId) -> &Cme {
        &self.table[index.get() as usize - 1]
    }
}

impl CmeTable {
    ///
    /// Intern *cme*, returning the existing [`CmeId`] if an identical
    /// entry was interned before, or a freshly allocated one otherwise.
    ///
    pub(crate) fn intern(&mut self, cme: Cme) -> CmeId {
        if let Some(&id) = self.map.get(&cme) {
            return id;
        }
        self.table.push(cme);
        let id = CmeId::new(self.table.len() as u32);
        self.map.insert(cme, id);
        id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cme(func: u32, owner: u32, name: u32) -> Cme {
        Cme::new(FuncId::new(func), ClassId::new(owner), IdentId::from(name))
    }

    #[test]
    fn intern_dedups_identical_entries() {
        let mut table = CmeTable::default();
        let a = table.intern(cme(1, 2, 3));
        let b = table.intern(cme(1, 2, 3));
        assert_eq!(a, b);
    }

    #[test]
    fn intern_distinguishes_owner_and_called_id() {
        let mut table = CmeTable::default();
        let base = table.intern(cme(1, 2, 3));
        let other_owner = table.intern(cme(1, 9, 3));
        let other_name = table.intern(cme(1, 2, 9));
        assert_ne!(base, other_owner);
        assert_ne!(base, other_name);
        assert_ne!(other_owner, other_name);
    }

    #[test]
    fn index_round_trips_fields() {
        let mut table = CmeTable::default();
        let id = table.intern(cme(7, 2, 5));
        let entry = &table[id];
        assert_eq!(entry.func_id(), FuncId::new(7));
        assert_eq!(entry.owner(), ClassId::new(2));
        assert_eq!(entry.called_id(), IdentId::from(5));
    }
}
