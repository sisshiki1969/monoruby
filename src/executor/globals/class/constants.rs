use super::*;

impl Globals {
    pub(crate) fn set_constant_by_str(&mut self, class_id: ClassId, name: &str, val: Value) {
        let name = IdentId::get_ident_id(name);
        self.set_constant(class_id, name, val);
    }

    pub(crate) fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        if self.class[class_id].constants.insert(name, val).is_some() && self.warning >= 1 {
            eprintln!(
                "warning: already initialized constant {}",
                IdentId::get_name(name)
            )
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

    pub(crate) fn get_qualified_constant(&mut self, name: &[&str]) -> Option<Value> {
        let mut class = OBJECT_CLASS;
        for name in name {
            class = self
                .get_constant(class, IdentId::get_ident_id(name))?
                .expect_class_or_module(self)?;
        }
        Some(class.get_obj(self).as_val())
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
    pub(crate) fn find_constant(&mut self, id: ConstSiteId, current_func: FuncId) -> Option<Value> {
        let ConstSiteInfo {
            toplevel,
            mut prefix,
            name,
            ..
        } = self.func[id].clone();
        let mut parent = if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            return self.search_constant(name, current_func);
        } else {
            let parent = prefix.remove(0);
            self.search_constant(parent, current_func)?
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
    fn get_constant_checked(&mut self, class_id: ClassId, name: IdentId) -> Option<Value> {
        match self.get_constant(class_id, name) {
            Some(v) => Some(v),
            None => {
                self.err_uninitialized_constant(name);
                None
            }
        }
    }

    fn search_constant(&mut self, name: IdentId, current_func: FuncId) -> Option<Value> {
        if let Some(v) = self.search_lexical_stack(name, current_func) {
            return Some(v);
        }
        match self.search_superclass(name, current_func) {
            Some(v) => Some(v),
            None => {
                self.err_uninitialized_constant(name);
                None
            }
        }
    }

    fn search_superclass(&self, name: IdentId, current_func: FuncId) -> Option<Value> {
        let mut class_id = self.func[current_func]
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
