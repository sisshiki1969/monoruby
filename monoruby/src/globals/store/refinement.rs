use super::*;

///
/// An interned set of activated refinements.
///
/// A scope's activated refinements would naturally be a
/// `Hash[refined_class => refinement_module]`, which every method lookup
/// under that scope would have to walk. Interning turns it into a `u32`
/// instead: sets are hash-consed, so equal sets get equal ids, and the id
/// can be compared, stored in a cache entry and treated as a compile-time
/// constant by the JIT.
///
/// `RefinementSetId::EMPTY` (`0`) is "no refinements activated" — the state
/// of every scope in every program that never calls `Module#refine`. It is
/// the value the whole no-refinements fast path keys off. See
/// `doc/refinements.md` §6.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
#[repr(transparent)]
pub struct RefinementSetId(u32);

impl RefinementSetId {
    pub const EMPTY: Self = Self(0);

    pub fn is_empty(self) -> bool {
        self == Self::EMPTY
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

impl Default for RefinementSetId {
    fn default() -> Self {
        Self::EMPTY
    }
}

///
/// The interning pool for [`RefinementSetId`].
///
/// A set is stored as a `(refined class, refinement module)` list kept
/// sorted by refined class, so two activations that reach the same set by
/// different routes intern to the same id.
///
/// Later activations of the same refined class win, matching CRuby: a
/// second `using` that refines an already-refined class replaces the
/// earlier entry for that class rather than stacking behind it.
///
#[derive(Debug, Default)]
pub(crate) struct RefinementTable {
    /// `sets[0]` is always the empty set, so `RefinementSetId::EMPTY == 0`.
    sets: Vec<Vec<(ClassId, ClassId)>>,
    index: HashMap<Vec<(ClassId, ClassId)>, RefinementSetId>,
    /// Union of every method name any refinement anywhere defines.
    ///
    /// This is what keeps the cost proportional to *how much is refined*
    /// rather than to *whether refinements are used*: the global method
    /// cache and the per-class memoized predicates keep their existing key
    /// and simply decline the names in here, so a program refining
    /// `String#blank?` pays nothing on `Array#each`.
    refined_names: HashSet<IdentId>,
    /// False until the first `Module#refine` call. While false, every
    /// resolution path takes exactly the code it takes today.
    active: bool,
}

impl RefinementTable {
    pub(super) fn new() -> Self {
        Self {
            sets: vec![vec![]],
            index: HashMap::default(),
            refined_names: HashSet::default(),
            active: false,
        }
    }

    /// Whether any refinement exists in this process. `false` for
    /// essentially every program; the no-refinements fast paths test it.
    pub(crate) fn is_active(&self) -> bool {
        self.active
    }

    /// Whether *name* is defined by any refinement. Only meaningful when
    /// [`Self::is_active`]; callers test that first so the `HashSet` probe
    /// never happens in a refinement-free program.
    pub(crate) fn is_refined_name(&self, name: IdentId) -> bool {
        self.refined_names.contains(&name)
    }

    /// Record that a refinement defines *name*, and mark the process as
    /// hosting refinements.
    pub(crate) fn add_refined_name(&mut self, name: IdentId) {
        self.active = true;
        self.refined_names.insert(name);
    }

    /// Mark the process as hosting refinements even before any method has
    /// been defined in one (an empty `refine Foo do end` still has to flip
    /// the gate, so that a later `def` inside the refinement module is not
    /// missed).
    pub(crate) fn activate(&mut self) {
        self.active = true;
    }

    /// The `(refined class, refinement module)` pairs of *set*.
    pub(crate) fn entries(&self, set: RefinementSetId) -> &[(ClassId, ClassId)] {
        &self.sets[set.0 as usize]
    }

    /// The refinement module activated for *class_id* in *set*, if any.
    pub(crate) fn lookup(&self, set: RefinementSetId, class_id: ClassId) -> Option<ClassId> {
        if set.is_empty() {
            return None;
        }
        self.entries(set)
            .iter()
            .find(|(refined, _)| *refined == class_id)
            .map(|(_, module)| *module)
    }

    /// Intern *entries* (any order) and return its id.
    fn intern(&mut self, mut entries: Vec<(ClassId, ClassId)>) -> RefinementSetId {
        entries.sort_unstable_by_key(|(refined, _)| refined.u32());
        if entries.is_empty() {
            return RefinementSetId::EMPTY;
        }
        if let Some(id) = self.index.get(&entries) {
            return *id;
        }
        let id = RefinementSetId(self.sets.len() as u32);
        self.index.insert(entries.clone(), id);
        self.sets.push(entries);
        id
    }

    ///
    /// `using`: the set *base* extended with every refinement *added*
    /// carries. Returns the interned id of the result.
    ///
    pub(crate) fn activated(
        &mut self,
        base: RefinementSetId,
        added: &[(ClassId, ClassId)],
    ) -> RefinementSetId {
        if added.is_empty() {
            return base;
        }
        let mut entries = self.entries(base).to_vec();
        for (refined, module) in added {
            match entries.iter_mut().find(|(c, _)| c == refined) {
                // Re-refining a class the scope already refines replaces
                // the earlier entry.
                Some(slot) => slot.1 = *module,
                None => entries.push((*refined, *module)),
            }
        }
        self.intern(entries)
    }
}

impl Store {
    pub(crate) fn refinements(&self) -> &RefinementTable {
        &self.refinements
    }

    pub(crate) fn refinements_mut(&mut self) -> &mut RefinementTable {
        &mut self.refinements
    }

    ///
    /// The refinement set that code written in *iseq* resolves under:
    /// its own cell if it has one, else the enclosing body's.
    ///
    /// A method reads the snapshot `def` wrote. A scope that ran `using`
    /// reads what it wrote. Anything else — an ordinary block — walks out
    /// through `outer`, so it sees its home scope's set *as of the call*,
    /// including a `using` that ran after the block was created.
    ///
    pub(crate) fn iseq_refinements(&self, iseq: ISeqId) -> RefinementSetId {
        let mut cur = Some(iseq);
        while let Some(id) = cur {
            if let Some(set) = self[id].refinements {
                return set;
            }
            cur = self[id].outer;
        }
        RefinementSetId::EMPTY
    }

    ///
    /// The iseq that owns the refinement cell *iseq* resolves through —
    /// where a `using` executed in that body must write.
    ///
    /// A body that already owns a cell keeps it. Otherwise the `using`
    /// creates one *on this body*, not on the enclosing scope: the block
    /// forms where `using` is legal (`Module.new { using R }`,
    /// `class_eval { using R }`) are scopes in their own right, and the
    /// activation must not leak out of them.
    ///
    pub(crate) fn refinement_cell_owner(&self, iseq: ISeqId) -> ISeqId {
        iseq
    }
}
