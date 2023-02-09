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
        if toplevel {
            let mut parent = OBJECT_CLASS;
            for constant in prefix {
                parent = self
                    .get_constant_checked(parent, constant)?
                    .expect_class_or_module(self)?;
            }
            self.get_constant_checked(parent, name)
        } else if prefix.is_empty() {
            match self.search_lexical_stack(name, current_func) {
                Some(v) => Some(v),
                _ => {
                    let class = self.func[current_func]
                        .as_ruby_func()
                        .lexical_context
                        .last()
                        .map_or(OBJECT_CLASS, |m| m.class_id());
                    self.search_superclass(class, name)
                }
            }
        } else {
            let parent = prefix.remove(0);
            let mut parent = match self.search_lexical_stack(parent, current_func) {
                Some(v) => v,
                None => self.get_constant_checked(OBJECT_CLASS, parent)?,
            }
            .expect_class_or_module(self)?;
            for constant in prefix {
                parent = self
                    .get_constant_checked(parent, constant)?
                    .expect_class_or_module(self)?;
            }
            self.get_constant_checked(parent, name)
        }
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

    fn search_superclass(&mut self, mut class_id: ClassId, name: IdentId) -> Option<Value> {
        loop {
            match self.get_constant(class_id, name) {
                Some(v) => return Some(v),
                None => match class_id.get_obj(self).superclass() {
                    Some(class) => class_id = class.class_id(),
                    None => break,
                },
            };
        }
        self.err_uninitialized_constant(name);
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
