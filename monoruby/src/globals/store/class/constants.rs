use super::*;

/// Visibility of a constant. Constants are public by default;
/// `Module#private_constant` makes them private. Visibility is stored
/// alongside the constant's value/autoload-path inside `ConstState`, so it
/// persists across an autoload-to-loaded transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum ConstVisibility {
    #[default]
    Public,
    Private,
}

impl ConstVisibility {
    pub(crate) fn is_private(self) -> bool {
        matches!(self, ConstVisibility::Private)
    }
}

/// State of a constant slot in a `ClassInfo`'s constant table.
///
/// `kind` distinguishes a fully loaded constant from a registered autoload,
/// and `visibility` records whether the constant is public or private. The
/// two are kept independent so that toggling visibility on an autoload entry
/// is preserved when the autoload is later triggered.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ConstState {
    pub kind: ConstStateKind,
    pub visibility: ConstVisibility,
    /// Set by `Module#deprecate_constant`. When true, every read of
    /// the constant emits a `warning: constant <Name> is deprecated`
    /// message via `Kernel#warn` (so it honours `Warning[:deprecated]`
    /// and any custom `Warning.warn` override).
    pub deprecated: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ConstStateKind {
    Loaded(Value),
    Autoload(AutoloadEntry),
}

/// Per-constant autoload registration plus its load-state. Mirrors
/// CRuby's `autoload_const` + `autoload_data`: the `feature` is the
/// path to require, and `state` tracks whether a load is in progress
/// or has been declared "done without defining the constant" so that
/// subsequent references don't keep retrying.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AutoloadEntry {
    pub feature: std::path::PathBuf,
    pub state: AutoloadState,
}

/// Load state for an autoload-registered constant.
///
/// `Idle` is the initial state right after `Module#autoload`. The next
/// triggering reference flips to `Loading` for the duration of the
/// `require`. monoruby is single-threaded so the flag is just a
/// recursion guard — a same-(only)-thread re-reference must not
/// re-enter `require`. After the require returns successfully but the
/// constant is *still* registered as autoload (i.e. the file did not
/// define it), the entry is removed entirely so that subsequent
/// references raise `NameError` rather than retrying. If `require`
/// itself raises (e.g. `LoadError`), the state is reverted to `Idle`
/// so a future reference may try again.
///
/// `Consumed` is the post-`require` state used when a *direct* `require`
/// of the autoload's file completed without defining the constant.
/// CRuby keeps the entry's name visible in `Module#constants(false)`
/// but reports `const_defined?` as false and `autoload?` as nil.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AutoloadState {
    Idle,
    Loading,
    Consumed,
}

impl AutoloadEntry {
    pub(crate) fn new(feature: std::path::PathBuf) -> Self {
        Self {
            feature,
            state: AutoloadState::Idle,
        }
    }
}

impl ConstState {
    pub(crate) fn loaded(value: Value) -> Self {
        Self {
            kind: ConstStateKind::Loaded(value),
            visibility: ConstVisibility::Public,
            deprecated: false,
        }
    }

    pub(crate) fn autoload(path: std::path::PathBuf) -> Self {
        Self {
            kind: ConstStateKind::Autoload(AutoloadEntry::new(path)),
            visibility: ConstVisibility::Public,
            deprecated: false,
        }
    }

    pub(crate) fn loaded_value(&self) -> Option<Value> {
        match self.kind {
            ConstStateKind::Loaded(v) => Some(v),
            ConstStateKind::Autoload(_) => None,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn is_loaded(&self) -> bool {
        matches!(self.kind, ConstStateKind::Loaded(_))
    }

    pub(crate) fn is_autoload(&self) -> bool {
        matches!(self.kind, ConstStateKind::Autoload(_))
    }

    pub(crate) fn is_private(&self) -> bool {
        self.visibility.is_private()
    }

    pub(crate) fn set_private(&mut self) {
        self.visibility = ConstVisibility::Private;
    }

    pub(crate) fn set_public(&mut self) {
        self.visibility = ConstVisibility::Public;
    }

    pub(crate) fn is_deprecated(&self) -> bool {
        self.deprecated
    }

    pub(crate) fn set_deprecated(&mut self) {
        self.deprecated = true;
    }
}

/// Walk up the singleton chain until we hit a non-singleton or until
/// the attached object stops being a class/module. Used by class-var
/// access so that `class C; class << self; @@x = …; end; end` lands
/// on `C`'s class-var table rather than the singleton's, matching
/// CRuby's `rb_cvar_set` /`rb_cvar_get`.
fn singleton_attached_class(classes: &ClassInfoTable, mut class_id: ClassId) -> ClassId {
    loop {
        let module = classes.get_module(class_id);
        match module.is_singleton() {
            Some(attached) => match attached.is_class_or_module() {
                Some(m) => class_id = m.id(),
                None => return class_id,
            },
            None => return class_id,
        }
    }
}

impl ClassInfoTable {
    pub(crate) fn set_constant_autoload(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        file_name: String,
    ) {
        match self[class_id].constants.get_mut(&name) {
            Some(state) => match &mut state.kind {
                // If the constant is already loaded, autoload is a no-op
                // (matches CRuby behavior).
                ConstStateKind::Loaded(_) => {}
                // Re-registering autoload updates the path; visibility is
                // preserved. CRuby leaves an in-progress load alone here
                // and only swaps the feature, so we mirror that by keeping
                // an in-flight `Loading` state but reviving a previously
                // `Consumed` slot back to `Idle` so the next reference
                // re-triggers the autoload (matching the "after the
                // autoload is triggered by require, a new autoload with
                // the same path is considered" spec).
                ConstStateKind::Autoload(entry) => {
                    entry.feature = file_name.into();
                    if let AutoloadState::Consumed = entry.state {
                        entry.state = AutoloadState::Idle;
                    }
                }
            },
            None => {
                self[class_id]
                    .constants
                    .insert(name, ConstState::autoload(file_name.into()));
            }
        }
    }

    /// Transition an Autoload entry's `state`. Used by the const-lookup
    /// path to mark a slot `Loading` for the duration of the `require`,
    /// and to revert it on `require` failure. No-op if the slot is gone
    /// or has been replaced by a Loaded value (e.g. the require defined
    /// the constant via `set_constant`).
    pub(crate) fn set_autoload_state(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        state: AutoloadState,
    ) {
        if let Some(slot) = self[class_id].constants.get_mut(&name) {
            if let ConstStateKind::Autoload(entry) = &mut slot.kind {
                entry.state = state;
            }
        }
    }

    /// Walk every class's constant table looking for `Autoload` entries
    /// whose `feature` matches one of `paths`. Returns a list of
    /// `(ClassId, IdentId)` pairs identifying those entries. Used by
    /// `Executor::require` so that direct `require <file>` calls can
    /// flip every matching autoload slot to `Loading` for the duration
    /// of the load and clean them up afterwards (matching CRuby's
    /// behaviour: a same-file direct require "consumes" the autoload).
    pub(crate) fn find_autoload_entries_for_paths(
        &self,
        paths: &[std::path::PathBuf],
    ) -> Vec<(ClassId, IdentId)> {
        let mut result = Vec::new();
        for idx in 1..self.classinfo_len() {
            let class_id = ClassId::new(idx as u32);
            for (name, state) in self[class_id].constants.iter() {
                if let ConstStateKind::Autoload(entry) = &state.kind {
                    if Self::autoload_feature_matches_path(&entry.feature, paths) {
                        result.push((class_id, *name));
                    }
                }
            }
        }
        result
    }

    fn autoload_feature_matches_path(
        feature: &std::path::Path,
        paths: &[std::path::PathBuf],
    ) -> bool {
        // Direct match: feature == path.
        if paths.iter().any(|p| p == feature) {
            return true;
        }
        // Canonicalised match: try resolving the feature path.
        if let Ok(canon) = feature.canonicalize() {
            if paths.iter().any(|p| p == &canon) {
                return true;
            }
        }
        // Extension-less feature: try `.rb` and `.so` variants.
        if feature.extension().is_none() {
            for ext in ["rb", "so"] {
                let mut with_ext = feature.to_path_buf();
                with_ext.set_extension(ext);
                if paths.iter().any(|p| p == &with_ext) {
                    return true;
                }
                if let Ok(canon) = with_ext.canonicalize() {
                    if paths.iter().any(|p| p == &canon) {
                        return true;
                    }
                }
            }
        }
        false
    }

    ///
    /// Get a value of a constant with *name* in the class of *class_id*.
    ///
    /// If not found, return None.
    ///
    pub(crate) fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<&ConstState> {
        self[class_id].constants.get(&name)
    }

    ///
    /// Get a value of a constant with *name* in the class of *class_id*.
    ///
    /// If not found, return None.
    ///
    pub(crate) fn get_constant_noautoload(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<Value> {
        match &self.get_constant(class_id, name)?.kind {
            ConstStateKind::Loaded(v) => Some(*v),
            // Caller asked for a no-autoload lookup; pending-autoload
            // entries don't have a value to hand out without triggering
            // the load, so behave as "not yet defined".
            ConstStateKind::Autoload(_) => None,
        }
    }

    ///
    /// Get constant names in the class of *class_id*. Skips
    /// private constants (those marked via `Module#private_constant`)
    /// since CRuby's `Module#constants` reports public-only.
    pub fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id]
            .constants
            .iter()
            .filter_map(|(name, state)| {
                if state.is_private() {
                    None
                } else {
                    Some(*name)
                }
            })
            .collect()
    }

    ///
    /// Get constant names in the class of *class_id* and its
    /// superclasses and included Modules except Object class and its
    /// superclasses. Duplicates (the same constant name reachable
    /// through multiple ancestors via `include`) are reported once,
    /// matching CRuby — `Module#constants` keeps the closer-ancestor
    /// occurrence and discards farther duplicates. Private constants
    /// (from `Module#private_constant`) are also excluded.
    pub fn get_constant_names_inherit(&self, mut class: Module) -> Vec<IdentId> {
        let mut names = Vec::new();
        let mut seen = HashSet::default();
        loop {
            for (name, state) in self[class.id()].constants.iter() {
                if state.is_private() {
                    continue;
                }
                if seen.insert(*name) {
                    names.push(*name);
                }
            }
            match class.superclass() {
                Some(superclass) => {
                    if superclass.id() == OBJECT_CLASS {
                        break;
                    }
                    class = superclass;
                }
                None => break,
            }
        }
        names
    }

    pub(in crate::globals) fn set_constant(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        val: Value,
    ) {
        // Preserve the previous visibility / deprecation flags if we're
        // overwriting an existing entry, so that `M::X = 1;
        // private_constant :X; M::X = 2` keeps :X private (and likewise
        // for `deprecate_constant`). New entries default to public and
        // non-deprecated via `ConstState::loaded`.
        let (prev_visibility, prev_deprecated) = self[class_id]
            .constants
            .get(&name)
            .map(|s| (Some(s.visibility), s.deprecated))
            .unwrap_or((None, false));
        let mut new_state = ConstState::loaded(val);
        if let Some(vis) = prev_visibility {
            new_state.visibility = vis;
        }
        new_state.deprecated = prev_deprecated;
        // The "already initialized constant" warning for user-level
        // redefinitions is emitted by the upstream call sites
        // (`Executor::set_constant` for literal assignments,
        // `Module#const_set` for the reflective form, `Struct.new`'s
        // class-binding) so that the message goes through Ruby's
        // `$stderr.write` and uses the proper qualified path. We
        // intentionally do *not* re-emit here.
        let _prev = self[class_id].constants.insert(name, new_state);
        // Auto-naming: only for *non-singleton* anonymous classes/modules.
        // CRuby never names a singleton class from constant assignment
        // (`class << o; CONST = self; end` ⇒ `o.singleton_class.name == nil`).
        // Without this guard `CONST = self` set inside `class << o`
        // would store `target.parent = target` (the singleton class is
        // both the constant's owner and value), turning the parent
        // chain into a self-loop and hanging `get_parents`
        // (`#to_s`/`#inspect`/error formatting → OOM).
        if let Some(klass) = val.is_class_or_module()
            && klass.is_singleton().is_none()
        {
            // The new owner is "permanent" iff its parent's chain is
            // already reachable from a top-level constant. `Object`
            // counts as permanent at the root.
            let parent_permanent =
                class_id == OBJECT_CLASS || self[class_id].is_name_permanent();
            let target = klass.id();
            let target_was_anon = self[target].get_name().is_none();
            let target_was_non_permanent = !self[target].is_name_permanent();
            if target_was_anon {
                // First-time naming: install the leaf name and parent.
                self[target].set_parent(class_id);
                // Store the leaf name only; `get_parents` walks the
                // parent chain at lookup time and joins the segments.
                // Anonymous ancestors are rendered using their inspect
                // form during the walk (see `get_parents`).
                self[target].set_name(name.to_string());
                self[target].set_name_permanent(parent_permanent);
            } else if target_was_non_permanent && parent_permanent {
                // Promotion: an existing non-permanent name (anonymous
                // parent or `set_temporary_name`) gets replaced when
                // the module is bound under a permanent parent.
                // Discard any explicit-temporary marker so future
                // `Module#name` calls render the parent chain. Mirrors
                // CRuby's `rb_set_class_path` re-binding behaviour.
                self[target].set_parent(class_id);
                self[target].set_name(name.to_string());
                self[target].set_name_permanent(true);
            }
            // Whenever the target's permanence may have flipped, walk
            // its descendants and promote any of *their* non-permanent
            // names too — `module m::N; end; M = m` must promote both
            // `m` and `m::N`.
            if self[target].is_name_permanent() {
                self.propagate_permanent_name(target);
            }
        }
    }

    /// Walk the (parent, child) graph rooted at `root` and promote any
    /// descendant whose own name was non-permanent up to permanent,
    /// clearing the `name_explicit_temporary` flag in the process. The
    /// descendants' leaf names are kept as-is — only the rendered
    /// fully-qualified path changes (recomputed lazily via
    /// `get_parents`).
    fn propagate_permanent_name(&mut self, root: ClassId) {
        // Collect descendants iteratively. Walking the full class
        // table is acceptable because re-binding is rare; this avoids
        // maintaining a child-list per `ClassInfo`.
        let mut stack = vec![root];
        let mut to_promote: Vec<(ClassId, ClassId)> = Vec::new();
        while let Some(parent) = stack.pop() {
            // The class table is `ClassId`-indexed, so `table[idx]`
            // corresponds to `ClassId(idx)`; index 0 is unused
            // (`ClassId` is `NonZeroU32`).
            for idx in 1..self.classinfo_len() {
                let id = ClassId::new(idx as u32);
                if id == root {
                    continue;
                }
                if self[id].parent_id() != Some(parent) {
                    continue;
                }
                if self[id].get_name().is_none() {
                    // No own name — nothing to promote, and it can't
                    // have permanent named descendants either (a
                    // permanent name is propagated through named
                    // ancestors only).
                    continue;
                }
                if !self[id].is_name_permanent() {
                    to_promote.push((id, parent));
                }
                stack.push(id);
            }
        }
        for (id, parent) in to_promote {
            // If `set_temporary_name` had overridden the leaf, find
            // the original constant-bound leaf name on the parent and
            // restore it. Otherwise keep the existing leaf — it's
            // already the constant-bound one.
            if self[id].is_name_explicit_temporary()
                && let Some(original) = self.find_constant_binding(parent, id)
            {
                self[id].set_name(original.get_name().to_string());
            }
            self[id].set_name_permanent(true);
            // Drop the explicit-temporary flag too, otherwise
            // `get_class_name` would still short-circuit to the bare
            // leaf and skip the parent-chain rendering.
            self[id].clear_explicit_temporary();
            self[id].invalidate_cached_name_value();
        }
        // Also invalidate the root's cached name Value: it may have
        // been previously rendered as anonymous.
        self[root].invalidate_cached_name_value();
    }

    /// Drop the cached frozen-`String` returned by `Module#name` for
    /// every descendant of `root` whose lexical chain runs through
    /// `root`. Used when the qualified rendering may change without
    /// the descendant's own leaf having been rewritten — e.g. after
    /// `set_temporary_name` on `root`.
    pub(crate) fn invalidate_descendant_name_caches(&mut self, root: ClassId) {
        let mut stack = vec![root];
        while let Some(parent) = stack.pop() {
            for idx in 1..self.classinfo_len() {
                let id = ClassId::new(idx as u32);
                if id == root {
                    continue;
                }
                if self[id].parent_id() != Some(parent) {
                    continue;
                }
                self[id].invalidate_cached_name_value();
                stack.push(id);
            }
        }
        self[root].invalidate_cached_name_value();
    }

    /// Walk `parent`'s constant table looking for an entry whose
    /// loaded value is the class/module `child`. Returns the binding
    /// `IdentId` if found, used by `propagate_permanent_name` to
    /// restore the constant-bound leaf name when a temporary is
    /// overridden.
    fn find_constant_binding(&self, parent: ClassId, child: ClassId) -> Option<IdentId> {
        for (name, state) in self[parent].constants.iter() {
            if let ConstStateKind::Loaded(v) = state.kind
                && let Some(m) = v.is_class_or_module()
                && m.id() == child
            {
                return Some(*name);
            }
        }
        None
    }
}

impl Globals {
    pub fn set_constant_by_str(&mut self, class_id: ClassId, name: &str, val: Value) {
        let name = IdentId::get_id(name);
        self.set_constant(class_id, name, val);
    }

    pub(crate) fn set_class_variable(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        // Class variable assignments in a singleton class scope
        // (`class << self; @@x = …; end`) target the attached object's
        // class — when the attached is a Class/Module, the cvar lands
        // on it directly. CRuby's `rb_cvar_set` calls
        // `rb_singleton_class_attached`-aware logic; we replicate by
        // walking until the target is non-singleton (or the attached
        // is no longer a class/module).
        let target = singleton_attached_class(&self.store.classes, class_id);
        self.store.classes[target].set_cvar(name, val);
    }

    pub(crate) fn get_class_variable(
        &self,
        parent: Module,
        name: IdentId,
    ) -> Result<(Module, Value)> {
        let start_id = singleton_attached_class(&self.store.classes, parent.id());
        let start = self.store.classes[start_id].get_module();
        let mut module = start;
        let mut res: Option<(Module, Value)> = None;
        loop {
            if let Some(v) = self.store.classes[module.id()].get_cvar(name) {
                match res {
                    Some((under, _)) => {
                        if under.id() != module.id() {
                            return Err(MonorubyErr::runtimeerr(format!(
                                "class variable {name} of {} is overtaken by {}",
                                under.id().get_name(&self.store),
                                module.id().get_name(&self.store),
                            )));
                        }
                    }
                    None => {
                        res = Some((module, v));
                    }
                }
            };
            match module.superclass() {
                Some(superclass) => module = superclass,
                None => break,
            }
        }
        match res {
            Some(res) => Ok(res),
            None => Err(MonorubyErr::uninitialized_cvar(
                name,
                parent.id().get_name(&self.store),
            )),
        }
    }

    ///
    /// Search a class variable with *name* in the class of *module* and its superclasses.
    ///
    /// If found, return the value and the module which the class variable belongs to.
    /// If not found, return None.
    ///
    pub(crate) fn search_class_variables_superclass(
        &self,
        mut module: Module,
        name: IdentId,
    ) -> Option<(Module, Value)> {
        loop {
            match self.store.classes[module.id()].get_cvar(name) {
                Some(v) => return Some((module, v)),
                None => match module.superclass() {
                    Some(superclass) => module = superclass,
                    None => break,
                },
            };
        }
        None
    }
}

impl Globals {}
