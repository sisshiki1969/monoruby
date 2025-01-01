use super::*;

impl ClassInfoTable {
    pub(crate) fn set_constant_autoload(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        file_name: String,
    ) {
        match self[class_id].constants.get_mut(&name) {
            Some(state) => match state {
                ConstState::Loaded(_) => {}
                ConstState::Autoload(path) => {
                    *path = file_name.into();
                }
            },
            None => {
                self[class_id]
                    .constants
                    .insert(name, ConstState::Autoload(file_name.into()));
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
        match self.get_constant(class_id, name)? {
            ConstState::Loaded(v) => Some(*v),
            _ => unreachable!(),
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
        if let Some(ConstState::Loaded(_)) = self[class_id]
            .constants
            .insert(name, ConstState::Loaded(val))
            && WARNING.load(std::sync::atomic::Ordering::Relaxed) >= 1
        {
            eprintln!("warning: already initialized constant {name}")
        }
        if let Some(klass) = val.is_class() {
            self[klass.id()].set_name_id(name)
        }
    }
}

impl Globals {
    pub fn set_constant_by_str(&mut self, class_id: ClassId, name: &str, val: Value) {
        let name = IdentId::get_id(name);
        self.store.classes.set_constant(class_id, name, val);
    }

    pub(crate) fn set_class_variable(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        self.store.classes[class_id].set_cvar(name, val);
    }

    pub(crate) fn get_class_variable(
        &self,
        parent: Module,
        name: IdentId,
    ) -> Result<(Module, Value)> {
        let mut module = parent;
        let mut res: Option<(Module, Value)> = None;
        loop {
            if let Some(v) = self.store.classes[module.id()].get_cvar(name) {
                match res {
                    Some((under, _)) => {
                        return Err(MonorubyErr::runtimeerr(format!(
                            "class variable {name} of {} is overtaken by {}",
                            under.id().get_name_id(&self.store),
                            module.id().get_name_id(&self.store),
                        )));
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
                parent.id().get_name_id(&self.store),
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
