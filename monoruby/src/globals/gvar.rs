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

/// How `defined?` should treat a [`GvarEntry::Hooked`] entry.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GvarDefinedKind {
    /// `defined?` always returns `"global-variable"`. Used for variables that
    /// CRuby considers always-present, e.g. `$~`, `$LOAD_PATH`,
    /// `$LOADED_FEATURES`.
    Always,
    /// `defined?` calls the getter and returns `"global-variable"` only when
    /// the result is non-nil. Used for derived match-data variables like
    /// `$&`, `$'`, `` $` ``, `$1`..`$N`, which CRuby parses as BACK_REF /
    /// NTH_REF nodes and evaluates against the current `$~`.
    OnNonNil,
}

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
        defined_kind: GvarDefinedKind,
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

    /// Runtime `defined?($name)` check.
    ///
    /// Returns `true` when the variable should report as `"global-variable"`:
    /// - Plain `Simple` entries are always defined once registered.
    /// - `Hooked` entries with [`GvarDefinedKind::Always`] are always defined.
    /// - `Hooked` entries with [`GvarDefinedKind::OnNonNil`] call the getter
    ///   and report defined only when the result is non-nil — this mirrors
    ///   CRuby's BACK_REF / NTH_REF semantics for `$&`, `$'`, `` $` ``,
    ///   `$1`..`$N`, where the variable is "defined" only after a successful
    ///   match has populated `$~`.
    pub fn defined_runtime(
        vm: &mut Executor,
        globals: &mut Globals,
        name: IdentId,
    ) -> bool {
        let id = match globals.gvars.index.get(&name).copied() {
            Some(id) => globals.gvars.resolve(id),
            None => return false,
        };
        match &globals.gvars.entries[id.index()] {
            GvarEntry::Simple(_) => true,
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked {
                defined_kind: GvarDefinedKind::Always,
                ..
            } => true,
            GvarEntry::Hooked {
                getter,
                defined_kind: GvarDefinedKind::OnNonNil,
                ..
            } => {
                let getter = *getter;
                !getter(vm, globals, name).is_nil()
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

    /// Returns `true` if `name` has any entry — Simple, Alias, or Hooked.
    /// Used by the gvar unit tests as a cheap "is the name registered"
    /// probe; runtime `defined?` evaluation goes through
    /// [`GvarTable::defined_runtime`] instead so it can also call hook
    /// getters when appropriate.
    #[cfg(test)]
    pub fn is_defined(&self, name: IdentId) -> bool {
        self.index.contains_key(&name)
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
    ///
    /// Panics if `name` resolves to a [`GvarEntry::Hooked`] entry: that
    /// combination means Rust-side code is trying to assign to a virtual
    /// variable without going through its setter, which is almost certainly
    /// a bug at the call site.
    pub fn set_simple(&mut self, name: IdentId, val: Value) {
        let id = self.entry_id(name);
        let id = self.resolve(id);
        match &mut self.entries[id.index()] {
            GvarEntry::Simple(slot) => *slot = val,
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { .. } => panic!(
                "set_simple called on hooked global variable {name}; \
                 use set_gvar through the hook API instead"
            ),
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
        defined_kind: GvarDefinedKind,
    ) {
        let id = self.entry_id(name);
        self.entries[id.index()] = GvarEntry::Hooked {
            getter,
            setter,
            defined_kind,
        };
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

///
/// Register hook-based special variables at `Globals::new` time.
///
/// All `$~`, `$&`, `$'`, `$1`..`$9`, `$LOAD_PATH` / `$:`, `$LOADED_FEATURES`
/// / `$"` are installed here as [`GvarEntry::Hooked`] entries so they can be
/// freely `alias`ed and so no bytecode or runtime code needs to know their
/// names by hand.
///
pub fn init_builtin_gvars(globals: &mut Globals) {
    // --- Regexp-related special variables -----------------------------------

    fn get_match_data(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        vm.get_last_matchdata()
    }

    fn set_match_data(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
        val: Value,
    ) -> Result<()> {
        // CRuby only allows `$~ = nil | MatchData`. We mirror the previous
        // behaviour: nil clears the capture state, anything else is a no-op
        // for now (see TODO in original runtime::set_special_var).
        if val.is_nil() {
            vm.clear_capture_special_variables();
        }
        Ok(())
    }

    fn get_last_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        vm.sp_last_match()
    }

    fn get_post_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        vm.sp_post_match()
    }

    fn get_pre_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        vm.sp_pre_match()
    }

    /// `$1`, `$2`, ... — `name` is of the form `$n`. Strip the `$` and parse
    /// the decimal digits; any non-numeric suffix yields `nil`.
    fn get_match_nth(
        vm: &mut Executor,
        _globals: &mut Globals,
        name: IdentId,
    ) -> Value {
        let s = name.get_name();
        let rest = match s.strip_prefix('$') {
            Some(rest) => rest,
            None => return Value::nil(),
        };
        match rest.parse::<i64>() {
            Ok(n) => vm.get_special_matches(n),
            Err(_) => Value::nil(),
        }
    }

    // $~ is always considered defined (CRuby parses it as a regular global
    // variable reference).
    globals.define_hooked_variable(
        IdentId::get_id("$~"),
        get_match_data,
        Some(set_match_data),
        GvarDefinedKind::Always,
    );
    // $&, $', $`, $1..$N are derived from $~ and CRuby parses them as
    // BACK_REF / NTH_REF nodes. defined? returns nil unless $~ is set.
    globals.define_hooked_variable(
        IdentId::get_id("$&"),
        get_last_match,
        None,
        GvarDefinedKind::OnNonNil,
    );
    globals.define_hooked_variable(
        IdentId::get_id("$'"),
        get_post_match,
        None,
        GvarDefinedKind::OnNonNil,
    );
    globals.define_hooked_variable(
        IdentId::get_id("$`"),
        get_pre_match,
        None,
        GvarDefinedKind::OnNonNil,
    );

    // $1 through $9 are the common case; higher-numbered matches are also
    // allowed by name — they all share the same getter which parses the
    // numeric suffix out of `name`.
    for n in 1..=9 {
        globals.define_hooked_variable(
            IdentId::get_id(&format!("${}", n)),
            get_match_nth,
            None,
            GvarDefinedKind::OnNonNil,
        );
    }

    // --- Library load path --------------------------------------------------

    fn get_load_path_hook(
        _vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        globals.get_load_path()
    }

    globals.define_hooked_variable(
        IdentId::get_id("$LOAD_PATH"),
        get_load_path_hook,
        None,
        GvarDefinedKind::Always,
    );
    // `$:` is an alias of `$LOAD_PATH`.
    globals.alias_global_variable(IdentId::get_id("$:"), IdentId::get_id("$LOAD_PATH"));

    // --- Loaded features ----------------------------------------------------

    fn get_loaded_features_hook(
        _vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Value {
        globals.get_loaded_features()
    }

    globals.define_hooked_variable(
        IdentId::get_id("$LOADED_FEATURES"),
        get_loaded_features_hook,
        None,
        GvarDefinedKind::Always,
    );
    globals.alias_global_variable(
        IdentId::get_id("$\""),
        IdentId::get_id("$LOADED_FEATURES"),
    );
}

// ============================================================================
// Unit tests
//
// These tests cover the pure-Rust internals of `GvarTable` (allocation, alias
// resolution, hook lookup, cycle detection) without needing a full
// `Globals`/`Executor` to be constructed. End-to-end Ruby-level behaviour for
// `$~`, `$&`, `$1..$9`, alias chains, and read-only hooks is exercised by
// the integration tests in `monoruby/src/builtins/class.rs`.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;

    fn n(s: &str) -> IdentId {
        IdentId::get_id(s)
    }

    /// Dummy getter that always returns nil. We can't actually fire hooks in
    /// these tests because the [`GvarGetter`] signature requires
    /// `&mut Executor` and `&mut Globals`, which would be expensive to
    /// construct. The hook *registration* and *resolution* are what we test
    /// here; the live invocation is covered by the Ruby-level integration
    /// tests.
    fn dummy_get(_vm: &mut Executor, _g: &mut Globals, _n: IdentId) -> Value {
        Value::nil()
    }

    fn dummy_set(
        _vm: &mut Executor,
        _g: &mut Globals,
        _n: IdentId,
        _v: Value,
    ) -> Result<()> {
        Ok(())
    }

    #[test]
    fn simple_set_get_round_trip() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_a"), Value::integer(42));
        assert_eq!(t.get_simple(n("$test_a")), Some(Value::integer(42)));
    }

    #[test]
    fn unknown_name_returns_none_and_is_not_defined() {
        let t = GvarTable::new();
        assert!(t.get_simple(n("$test_undefined_xxxx")).is_none());
        assert!(!t.is_defined(n("$test_undefined_xxxx")));
    }

    #[test]
    fn is_defined_recognizes_simple_alias_and_hook() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_def_simple"), Value::integer(1));
        t.define_hook(n("$test_def_hook"), dummy_get, None, GvarDefinedKind::Always);
        t.define_alias(n("$test_def_alias"), n("$test_def_simple"));
        assert!(t.is_defined(n("$test_def_simple")));
        assert!(t.is_defined(n("$test_def_hook")));
        assert!(t.is_defined(n("$test_def_alias")));
        assert!(!t.is_defined(n("$test_def_unknown")));
    }

    #[test]
    fn alias_redirects_simple_writes_and_reads() {
        // alias $test_b -> $test_a; writing $test_b should land in $test_a's
        // slot and vice versa.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_a"), Value::integer(1));
        t.define_alias(n("$test_b"), n("$test_a"));
        // read through alias
        assert_eq!(t.get_simple(n("$test_b")), Some(Value::integer(1)));
        // write through alias
        t.set_simple(n("$test_b"), Value::integer(99));
        assert_eq!(t.get_simple(n("$test_a")), Some(Value::integer(99)));
        assert_eq!(t.get_simple(n("$test_b")), Some(Value::integer(99)));
    }

    #[test]
    fn alias_chain_resolves_to_terminal_entry() {
        // $c -> $b -> $a, all reads/writes land in $a's slot.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_chain_a"), Value::integer(0));
        t.define_alias(n("$test_chain_b"), n("$test_chain_a"));
        t.define_alias(n("$test_chain_c"), n("$test_chain_b"));
        t.set_simple(n("$test_chain_c"), Value::integer(7));
        assert_eq!(t.get_simple(n("$test_chain_a")), Some(Value::integer(7)));
        assert_eq!(t.get_simple(n("$test_chain_b")), Some(Value::integer(7)));
        assert_eq!(t.get_simple(n("$test_chain_c")), Some(Value::integer(7)));
    }

    #[test]
    fn alias_cycle_does_not_infinite_loop() {
        // Construct $x -> $y -> $x and verify resolve() bails out instead of
        // looping forever.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_cycle_x"), Value::integer(1));
        t.set_simple(n("$test_cycle_y"), Value::integer(2));
        t.define_alias(n("$test_cycle_x"), n("$test_cycle_y"));
        t.define_alias(n("$test_cycle_y"), n("$test_cycle_x"));
        // get_simple just needs to terminate; the value it returns is
        // unspecified once a cycle is involved.
        let _ = t.get_simple(n("$test_cycle_x"));
        let _ = t.get_simple(n("$test_cycle_y"));
        // is_defined should still return true.
        assert!(t.is_defined(n("$test_cycle_x")));
        assert!(t.is_defined(n("$test_cycle_y")));
    }

    #[test]
    fn define_hook_replaces_simple_entry() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_replace"), Value::integer(123));
        // After installing a hook, get_simple should report None because the
        // entry is now Hooked, not Simple.
        t.define_hook(
            n("$test_replace"),
            dummy_get,
            Some(dummy_set),
            GvarDefinedKind::Always,
        );
        assert!(t.get_simple(n("$test_replace")).is_none());
        assert!(t.is_defined(n("$test_replace")));
    }

    #[test]
    fn define_hook_with_no_setter_is_read_only() {
        // We can't actually fire the hook here without a live Globals, but we
        // can verify the entry was installed and that get_simple correctly
        // reports None for it (so that the unified `defined?` path uses
        // `is_defined` instead of `get_simple`).
        let mut t = GvarTable::new();
        t.define_hook(n("$test_ro"), dummy_get, None, GvarDefinedKind::Always);
        assert!(t.get_simple(n("$test_ro")).is_none());
        assert!(t.is_defined(n("$test_ro")));
    }

    #[test]
    fn alias_to_hook_resolves_through_to_hook_entry() {
        // alias $test_h_alias -> $test_h_hook (a hooked entry).
        // get_simple should still report None because the resolved entry is
        // Hooked, but is_defined should report true.
        let mut t = GvarTable::new();
        t.define_hook(n("$test_h_hook"), dummy_get, None, GvarDefinedKind::Always);
        t.define_alias(n("$test_h_alias"), n("$test_h_hook"));
        assert!(t.get_simple(n("$test_h_alias")).is_none());
        assert!(t.is_defined(n("$test_h_alias")));
    }

    #[test]
    fn alias_to_self_is_a_noop() {
        // `alias $x $x` must not destroy the entry.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_self_alias"), Value::integer(11));
        t.define_alias(n("$test_self_alias"), n("$test_self_alias"));
        assert_eq!(
            t.get_simple(n("$test_self_alias")),
            Some(Value::integer(11))
        );
    }

    #[test]
    #[should_panic(expected = "set_simple called on hooked global variable")]
    fn set_simple_panics_on_hook() {
        // The hardening introduced in Phase A: Rust-side mistakes that try
        // to plain-write to a hooked variable surface immediately as a
        // panic instead of silently bypassing the hook.
        let mut t = GvarTable::new();
        t.define_hook(
            n("$test_panic_hook"),
            dummy_get,
            Some(dummy_set),
            GvarDefinedKind::Always,
        );
        t.set_simple(n("$test_panic_hook"), Value::integer(0));
    }

    #[test]
    fn mark_values_visits_only_simple_entries() {
        // Hooked entries' backing storage is owned elsewhere (e.g.
        // `Globals::load_path`); only Simple slots should be visited by the
        // GC marker.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_mark_simple"), Value::integer(5));
        t.define_hook(n("$test_mark_hook"), dummy_get, None, GvarDefinedKind::Always);
        let mut visited = 0;
        t.mark_values(|_| visited += 1);
        assert_eq!(visited, 1);
    }
}
