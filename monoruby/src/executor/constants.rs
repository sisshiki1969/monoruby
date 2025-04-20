use super::*;

impl Executor {
    pub(crate) fn const_get(
        &mut self,
        globals: &mut Globals,
        module: Module,
        name: IdentId,
        inherit: bool,
    ) -> Result<Value> {
        if inherit {
            self.search_constant_superclass_checked(globals, module, name)
        } else {
            self.get_constant_checked(globals, module.id(), name)
        }
    }

    pub(crate) fn get_qualified_constant(
        &mut self,
        globals: &mut Globals,
        base: ClassId,
        name: &[&str],
    ) -> Result<Value> {
        let mut class = globals.store[base].get_module();
        for name in name {
            let name = IdentId::get_id(name);
            class = self
                .get_constant_checked(globals, class.id(), name)?
                .expect_class_or_module(&globals.store)?;
        }
        Ok(class.as_val())
    }

    ///
    /// Try to get a value of the constant `name` of the class `class_id`.
    ///
    /// If an error occurs (in autoload), return Err.
    ///
    pub(super) fn get_constant(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> Result<Option<Value>> {
        match globals.get_constant(class_id, name) {
            None => return Ok(None),
            Some(ConstState::Loaded(v)) => return Ok(Some(*v)),
            Some(ConstState::Autoload(file_name)) => {
                let file_name = file_name.clone();
                globals.remove_constant(class_id, name);
                self.require(globals, &file_name, false)?;
            }
        };
        match globals.get_constant(class_id, name) {
            None => Ok(None),
            Some(ConstState::Loaded(v)) => Ok(Some(*v)),
            Some(ConstState::Autoload(_)) => Ok(None),
        }
    }

    ///
    /// Get constant with *name* and parent class *class_id*.
    ///
    /// If not found, set uninitialized constant error and return None.
    ///
    pub(crate) fn get_constant_checked(
        &mut self,
        globals: &mut Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> Result<Value> {
        match self.get_constant(globals, class_id, name)? {
            Some(v) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn search_constant_superclass_checked(
        &mut self,
        globals: &mut Globals,
        mut module: Module,
        name: IdentId,
    ) -> Result<Value> {
        loop {
            match self.get_constant(globals, module.id(), name)? {
                Some(v) => return Ok(v),
                None => match module.superclass() {
                    Some(superclass) => module = superclass,
                    None => break,
                },
            };
        }
        Err(MonorubyErr::uninitialized_constant(name))
    }

    pub(super) fn search_constant_checked(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        current_func: FuncId,
    ) -> Result<Value> {
        if let Some(v) = self.search_lexical_stack(globals, name, current_func)? {
            return Ok(v);
        }
        let module = globals
            .store
            .iseq(current_func)
            .lexical_context
            .last()
            .cloned()
            .unwrap_or(globals.store.object_class());

        self.search_constant_superclass_checked(globals, module, name)
    }

    fn search_lexical_stack(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        current_func: FuncId,
    ) -> Result<Option<Value>> {
        let stack = globals
            .store
            .iseq(current_func)
            .lexical_context
            .iter()
            .rev();
        for m in stack {
            if globals.store.get_constant(m.id(), name).is_some() {
                return self.get_constant(globals, m.id(), name);
            }
        }
        Ok(None)
    }
}
