//! Global variable table.
//!
//! All global variables (including special variables like `$~`, `$&`, `$1`,
//! `$LOAD_PATH`, `$stdin`, etc.) live in a single table keyed by [`IdentId`].
//!
//! Each entry in the table is one of:
//!
//! - [`GvarEntry::Simple`] — a plain storage cell holding a [`Value`].
//! - [`GvarEntry::Alias`] — a redirection to another entry, used to implement
//!   `alias $new $old` on global variables.
//! - [`GvarEntry::Hooked`] — an entry whose reads and writes are delegated to
//!   a Rust getter / setter pair. This is analogous to CRuby's
//!   `rb_define_virtual_variable` / `rb_define_hooked_variable` /
//!   `rb_define_readonly_variable`.
//!
//! The table uses a two-level structure: a `HashMap<IdentId, GvarId>` index
//! plus a `Vec<GvarEntry>` store so that entries can be referenced by a stable
//! numeric id (useful for alias redirection and for potential inline caching
//! in the JIT).

use super::*;

/// Opaque identifier for an entry in the [`GvarTable`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GvarId(u32);

impl GvarId {
    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Getter function for a hooked global variable.
///
/// `name` is the [`IdentId`] that was looked up, which lets a single getter
/// service several related names (e.g. a single function reading `$1`..`$9`).
pub type GvarGetter =
    fn(vm: &mut Executor, globals: &mut Globals, name: IdentId) -> Value;

/// Setter function for a hooked global variable. Returning `Err` raises.
pub type GvarSetter =
    fn(vm: &mut Executor, globals: &mut Globals, name: IdentId, val: Value) -> Result<()>;

/// A single entry in the global variable table.
pub enum GvarEntry {
    /// Plain global variable — value is stored in place.
    Simple(Value),
    /// Alias to another entry.
    Alias(GvarId),
    /// Virtual / hooked global variable serviced by Rust callbacks.
    Hooked {
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    },
}

/// Table of all global variables.
#[derive(Default)]
pub struct GvarTable {
    /// Name → entry id.
    index: HashMap<IdentId, GvarId>,
    /// Entries. Position is stable: entries are never reordered or removed.
    entries: Vec<GvarEntry>,
}

/// Maximum number of alias hops that [`GvarTable::resolve`] will follow before
/// giving up.
const ALIAS_MAX_DEPTH: usize = 16;

impl GvarTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Look up the (non-alias) entry id for `name`, allocating a fresh
    /// [`GvarEntry::Simple(nil)`] entry on first reference.
    fn entry_id(&mut self, name: IdentId) -> GvarId {
        if let Some(&id) = self.index.get(&name) {
            return id;
        }
        let id = GvarId::from_index(self.entries.len());
        self.entries.push(GvarEntry::Simple(Value::nil()));
        self.index.insert(name, id);
        id
    }

    /// Resolve `id` through any alias chain and return the terminal entry id.
    ///
    /// If the chain is longer than [`ALIAS_MAX_DEPTH`] the original id is
    /// returned — this is a defensive measure against alias cycles introduced
    /// e.g. by `alias $a $b; alias $b $a`.
    fn resolve(&self, mut id: GvarId) -> GvarId {
        for _ in 0..ALIAS_MAX_DEPTH {
            match &self.entries[id.index()] {
                GvarEntry::Alias(next) => id = *next,
                _ => return id,
            }
        }
        id
    }

    /// Return the entry id for `name` if it has one, without creating a new
    /// entry. Alias chains are *not* resolved here.
    pub fn lookup(&self, name: IdentId) -> Option<GvarId> {
        self.index.get(&name).copied()
    }

    /// Read `name`. Returns `nil` when the name is undefined.
    ///
    /// This takes `&mut Executor` and `&mut Globals` because hooked variables
    /// may need to mutate runtime state (e.g. `$~` writes).
    pub fn get(vm: &mut Executor, globals: &mut Globals, name: IdentId) -> Value {
        let id = match globals.gvars.index.get(&name).copied() {
            Some(id) => globals.gvars.resolve(id),
            None => return Value::nil(),
        };
        match &globals.gvars.entries[id.index()] {
            GvarEntry::Simple(v) => *v,
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { getter, .. } => {
                let getter = *getter;
                getter(vm, globals, name)
            }
        }
    }

    /// Write `val` into `name`.
    ///
    /// Returns `Err` if the target is a read-only hooked variable or the
    /// setter itself raises.
    pub fn set(
        vm: &mut Executor,
        globals: &mut Globals,
        name: IdentId,
        val: Value,
    ) -> Result<()> {
        // Intern the entry first so we can assign to plain globals without
        // going through the hook path.
        let id = globals.gvars.entry_id(name);
        let id = globals.gvars.resolve(id);
        match &mut globals.gvars.entries[id.index()] {
            GvarEntry::Simple(slot) => {
                *slot = val;
                Ok(())
            }
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { setter, .. } => match *setter {
                Some(setter) => setter(vm, globals, name, val),
                None => Err(MonorubyErr::nameerr(format!(
                    "can't set variable {}",
                    name
                ))),
            },
        }
    }

    /// Plain getter that only consults [`GvarEntry::Simple`] entries and
    /// ignores hooks. Returns `None` if `name` is unknown or is currently a
    /// hook/alias rather than a simple value.
    ///
    /// This is the fast-path used at pre-known plain global variable names
    /// (e.g. `$0`, `$!`, `$*`) where no hook is ever involved.
    pub fn get_simple(&self, name: IdentId) -> Option<Value> {
        let id = self.index.get(&name).copied()?;
        let id = self.resolve(id);
        if let GvarEntry::Simple(v) = self.entries[id.index()] {
            Some(v)
        } else {
            None
        }
    }

    /// Plain setter that stores into the resolved [`GvarEntry::Simple`]
    /// entry, creating one if necessary. Hooks are bypassed — callers should
    /// use [`GvarTable::set`] instead when hook semantics are required.
    pub fn set_simple(&mut self, name: IdentId, val: Value) {
        let id = self.entry_id(name);
        let id = self.resolve(id);
        match &mut self.entries[id.index()] {
            GvarEntry::Simple(slot) => *slot = val,
            // If someone already installed a hook at this name, we still want
            // assignments to `$stdin = ...` (etc.) to land somewhere sensible.
            // Overwrite the hook with a `Simple` slot. Phase B will replace
            // these early-boot assignments with hook-aware calls.
            entry => *entry = GvarEntry::Simple(val),
        }
    }

    /// Install a hooked entry for `name`. Replaces any previously registered
    /// hook/alias, but if `name` already held a `Simple` value its contents
    /// are discarded.
    pub fn define_hook(
        &mut self,
        name: IdentId,
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    ) {
        let id = self.entry_id(name);
        self.entries[id.index()] = GvarEntry::Hooked { getter, setter };
    }

    /// Install an alias: reading or writing `new_name` is redirected to
    /// `old_name`.
    pub fn define_alias(&mut self, new_name: IdentId, old_name: IdentId) {
        let target = self.entry_id(old_name);
        let target = self.resolve(target);
        let id = self.entry_id(new_name);
        if id == target {
            // alias $x $x — nothing to do.
            return;
        }
        self.entries[id.index()] = GvarEntry::Alias(target);
    }

    /// Iterate over the [`Value`]s stored directly in [`GvarEntry::Simple`]
    /// entries so the GC can mark them. Hooked entries are not iterated here;
    /// their backing storage (e.g. [`Globals::load_path`]) is marked through
    /// its owner.
    pub fn mark_values<F: FnMut(Value)>(&self, mut f: F) {
        for entry in &self.entries {
            if let GvarEntry::Simple(v) = entry {
                f(*v);
            }
        }
    }
}
