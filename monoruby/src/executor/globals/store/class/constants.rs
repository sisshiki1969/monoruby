use super::*;

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
    /// If not found, simply return None with no error.
    ///
    pub(crate) fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self.store[class_id].constants.get(&name).cloned()
    }

    pub fn search_constant_superclass(
        &self,
        mut class_id: ClassId,
        name: IdentId,
    ) -> Option<Value> {
        loop {
            match self.get_constant(class_id, name) {
                Some(v) => return Some(v),
                None => match class_id.get_module(self).superclass_id() {
                    Some(superclass) => class_id = superclass,
                    None => break,
                },
            };
        }
        None
    }

    pub(crate) fn get_qualified_constant(&mut self, name: &[&str]) -> Result<Value> {
        let mut class = OBJECT_CLASS;
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
    /// If not found, set uninitialized constant error and return None.
    ///
    pub(crate) fn find_constant(&mut self, id: ConstSiteId, current_func: FuncId) -> Result<Value> {
        let ConstSiteInfo {
            toplevel,
            mut prefix,
            name,
            ..
        } = self.store[id].clone();
        let mut parent = if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            return self.search_constant_checked(name, current_func);
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
        self.get_constant_checked(parent, name)
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
        let class_id = self[current_func]
            .as_ruby_func()
            .lexical_context
            .last()
            .map_or(OBJECT_CLASS, |m| m.superclass_id().unwrap_or(OBJECT_CLASS));

        match self.search_constant_superclass(class_id, name) {
            Some(v) => Ok(v),
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
