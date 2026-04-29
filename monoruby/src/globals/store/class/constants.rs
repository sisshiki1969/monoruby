use super::*;

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
                // preserved.
                ConstStateKind::Autoload(path) => {
                    *path = file_name.into();
                }
            },
            None => {
                self[class_id]
                    .constants
                    .insert(name, ConstState::autoload(file_name.into()));
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
        // Preserve the previous visibility if we're overwriting an existing
        // entry, so that `M::X = 1; private_constant :X; M::X = 2` keeps :X
        // private. New entries default to public via `ConstState::loaded`.
        let prev_visibility = self[class_id]
            .constants
            .get(&name)
            .map(|s| s.visibility);
        let mut new_state = ConstState::loaded(val);
        if let Some(vis) = prev_visibility {
            new_state.visibility = vis;
        }
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
