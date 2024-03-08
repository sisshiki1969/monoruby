use super::*;

impl Globals {
    /*pub fn dump_superclass(&self, mut module: Module) {
        loop {
            eprint!("{} ", module.id().get_name_id(self).unwrap());
            match module.superclass() {
                Some(superclass) => module = superclass,
                None => break,
            }
        }
        eprintln!();
    }*/

    pub(crate) fn set_class_variable(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        self.store[class_id].set_cvar(name, val);
    }

    pub(crate) fn get_class_variable(
        &self,
        parent: Module,
        name: IdentId,
    ) -> Result<(Module, Value)> {
        let mut module = parent;
        let mut res: Option<(Module, Value)> = None;
        loop {
            if let Some(v) = self.store[module.id()].get_cvar(name) {
                match res {
                    Some((under, _)) => {
                        return Err(MonorubyErr::runtimeerr(format!(
                            "class variable {name} of {} is overtaken by {}",
                            under.id().get_name_id(self).unwrap(),
                            module.id().get_name_id(self).unwrap(),
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
                parent.id().get_name_id(self).unwrap(),
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
            match self.store[module.id()].get_cvar(name) {
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

impl Globals {
    pub fn set_constant_by_str(&mut self, class_id: ClassId, name: &str, val: Value) {
        let name = IdentId::get_id(name);
        self.set_constant(class_id, name, val);
    }

    pub(crate) fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        if self.store[class_id].constants.insert(name, val).is_some() && self.warning >= 1 {
            eprintln!("warning: already initialized constant {name}")
        }
        if let Some(id) = val.is_class() {
            self.store[id].set_name_id(name)
        }
    }

    ///
    /// Get a value of a constant with *name* in the class of *class_id*.
    ///
    /// If not found, return None.
    ///
    pub(crate) fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self.store[class_id].constants.get(&name).cloned()
    }

    pub fn search_constant_superclass(
        &self,
        mut module: Module,
        name: IdentId,
    ) -> Option<(Module, Value)> {
        loop {
            match self.get_constant(module.id(), name) {
                Some(v) => return Some((module, v)),
                None => match module.superclass() {
                    Some(superclass) => module = superclass,
                    None => break,
                },
            };
        }
        None
    }

    pub(crate) fn get_qualified_constant(&mut self, base: ClassId, name: &[&str]) -> Result<Value> {
        let mut class = base;
        for name in name {
            let name = IdentId::get_id(name);
            class = match self.get_constant(class, name) {
                Some(val) => Ok(val),
                None => Err(MonorubyErr::uninitialized_constant(name)),
            }?
            .expect_class_or_module(self)?;
        }
        Ok(class.get_obj(self))
    }

    ///
    /// Get constant names in the class of *class_id*.
    ///
    pub fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.store[class_id].constants.keys().cloned().collect()
    }

    ///
    /// Get constant names in the class of *class_id* and its superclasses and included Modules except Object class and its superclasses.
    ///
    pub fn get_constant_names_inherit(&self, mut class_id: ClassId) -> Vec<IdentId> {
        let mut names = vec![];
        loop {
            names.extend(self.store[class_id].constants.keys().cloned());
            match class_id.get_module(self).superclass_id() {
                Some(superclass) => {
                    if superclass == OBJECT_CLASS {
                        break;
                    }
                    class_id = superclass;
                }
                None => break,
            }
        }
        names
    }

    ///
    /// Get a value of a constant specified by ConstSiteId *id*.
    ///
    /// If not found, return uninitialized constant error.
    ///
    pub(crate) fn find_constant(
        &mut self,
        id: ConstSiteId,
        current_func: FuncId,
        base: Option<Value>,
    ) -> Result<(Value, Option<Value>)> {
        let ConstSiteInfo {
            name,
            toplevel,
            mut prefix,
            ..
        } = self.store[id].clone();
        let mut parent = if let Some(base) = base {
            base.expect_class_or_module(self)?
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            let v = self.search_constant_checked(name, current_func)?;
            return Ok((v, None));
        } else {
            let parent = prefix.remove(0);
            self.search_constant_checked(parent, current_func)?
                .expect_class_or_module(self)?
        };
        for constant in prefix {
            parent = self
                .get_constant_checked(parent, constant)?
                .expect_class_or_module(self)?;
        }
        let v = self.get_constant_checked(parent, name)?;
        Ok((v, base))
    }

    ///
    /// Get constant with *name* and parent class *class_id*.
    ///
    /// If not found, set uninitialized constant error and return None.
    ///
    fn get_constant_checked(&self, class_id: ClassId, name: IdentId) -> Result<Value> {
        match self.get_constant(class_id, name) {
            Some(v) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn search_constant_checked(&self, name: IdentId, current_func: FuncId) -> Result<Value> {
        if let Some(v) = self.search_lexical_stack(name, current_func) {
            return Ok(v);
        }
        let module = self[current_func]
            .as_ruby_func()
            .lexical_context
            .last()
            .unwrap_or(&OBJECT_CLASS.get_module(self))
            .to_owned();

        match self.search_constant_superclass(module, name) {
            Some((_, v)) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn search_lexical_stack(&self, name: IdentId, current_func: FuncId) -> Option<Value> {
        self.store[current_func]
            .as_ruby_func()
            .lexical_context
            .iter()
            .rev()
            .find_map(|module| self.get_constant(module.id(), name))
    }
}
