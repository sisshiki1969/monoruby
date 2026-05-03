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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AutoloadState {
    Idle,
    Loading,
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
                // the existing `state` untouched.
                ConstStateKind::Autoload(entry) => {
                    entry.feature = file_name.into();
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
    /// Get constant names in the class of *class_id*.
    ///
    pub fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].constants.keys().cloned().collect()
    }

    ///
    /// Get constant names in the class of *class_id* and its superclasses and included Modules except Object class and its superclasses.
    ///
    pub fn get_constant_names_inherit(&self, mut class: Module) -> Vec<IdentId> {
        let mut names = vec![];
        loop {
            names.extend(self[class.id()].constants.keys().cloned());
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
        let prev = self[class_id].constants.insert(name, new_state);
        if prev.as_ref().is_some_and(|s| s.is_loaded())
            && WARNING.load(std::sync::atomic::Ordering::Relaxed) >= 1
        {
            eprintln!("warning: already initialized constant {name}")
        }
        if let Some(klass) = val.is_class_or_module() {
            if self[klass.id()].get_name().is_none() {
                self[klass.id()].set_parent(class_id);
                // Store the leaf name only; `get_parents` walks the parent
                // chain at lookup time and joins the segments. Anonymous
                // ancestors are rendered using their inspect form during the
                // walk (see `get_parents`).
                self[klass.id()].set_name(name.to_string());
                // The new module is permanently named only if its parent
                // is itself permanent (so the full chain back to a
                // top-level constant is reachable). Otherwise the leaf
                // name is "borrowed" through an anonymous ancestor and
                // CRuby still allows `set_temporary_name` on it.
                let parent_permanent =
                    class_id == OBJECT_CLASS || self[class_id].is_name_permanent();
                self[klass.id()].set_name_permanent(parent_permanent);
            }
        }
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
