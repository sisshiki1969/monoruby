use super::*;

impl Globals {
    pub(crate) fn set_constant_by_str(&mut self, class_id: ClassId, name: &str, val: Value) {
        let name = IdentId::get_id(name);
        self.set_constant(class_id, name, val);
    }

    pub(crate) fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        if self.class[class_id].constants.insert(name, val).is_some() && self.warning >= 1 {
            eprintln!("warning: already initialized constant {name}")
        }
    }

    ///
    /// Get a value of a constant with *name* in the class of *class_id*.
    ///
    /// If not found, simply return None with no error.
    ///
    pub(crate) fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self.class[class_id].constants.get(&name).cloned()
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
        Ok(class.get_obj(self).as_val())
    }

    ///
    /// Get constant names in the class of *class_id*.
    ///
    pub(crate) fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.class[class_id].constants.keys().cloned().collect()
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
        } = self.func[id].clone();
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
}

impl Globals {
    ///
    /// Get constant with *name* and parent class *class_id*.
    ///
    /// If not found, set uninitialized constant error and return None.
    ///
    fn get_constant_checked(&mut self, class_id: ClassId, name: IdentId) -> Result<Value> {
        match self.get_constant(class_id, name) {
            Some(v) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn search_constant_checked(&mut self, name: IdentId, current_func: FuncId) -> Result<Value> {
        if let Some(v) = self.search_lexical_stack(name, current_func) {
            return Ok(v);
        }
        match self.search_superclass(name, current_func) {
            Some(v) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn search_superclass(&self, name: IdentId, current_func: FuncId) -> Option<Value> {
        let mut class_id = self[current_func]
            .as_ruby_func()
            .lexical_context
            .last()
            .map_or(OBJECT_CLASS, |m| m.superclass_id().unwrap_or(OBJECT_CLASS));
        loop {
            match self.get_constant(class_id, name) {
                Some(v) => return Some(v),
                None => match class_id.get_obj(self).superclass_id() {
                    Some(superclass) => class_id = superclass,
                    None => break,
                },
            };
        }
        None
    }

    fn search_lexical_stack(&self, name: IdentId, current_func: FuncId) -> Option<Value> {
        self.func[current_func]
            .as_ruby_func()
            .lexical_context
            .iter()
            .rev()
            .find_map(|module| self.get_constant(module.class_id(), name))
    }
}
