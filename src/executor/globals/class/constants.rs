use super::*;

impl Globals {
    pub(crate) fn set_constant(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        val: Value,
    ) -> Option<Value> {
        self.class[class_id].constants.insert(name, val)
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
    pub(crate) fn find_constant(
        &mut self,
        id: ConstSiteId,
        class_context: &[ClassId],
    ) -> Option<Value> {
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
                    .expect_class(name, self)?;
            }
            self.get_constant_checked(parent, name)
        } else if prefix.is_empty() {
            match self.search_lexical_stack(name, class_context) {
                Some(v) => Some(v),
                _ => self.get_constant_checked(OBJECT_CLASS, name),
            }
        } else {
            let parent = prefix.remove(0);
            let mut parent = match self.search_lexical_stack(parent, class_context) {
                Some(v) => v,
                None => self.get_constant_checked(OBJECT_CLASS, parent)?,
            }
            .expect_class(name, self)?;
            for constant in prefix {
                parent = self
                    .get_constant_checked(parent, constant)?
                    .expect_class(name, self)?;
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

    fn search_lexical_stack(&self, name: IdentId, class_context: &[ClassId]) -> Option<Value> {
        class_context
            .iter()
            .rev()
            .find_map(|class| self.get_constant(*class, name))
    }
}
