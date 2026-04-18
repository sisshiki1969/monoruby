use super::*;

impl Executor {
    pub(crate) fn const_get(
        &mut self,
        globals: &mut Globals,
        module: Module,
        name: IdentId,
        inherit: bool,
    ) -> Result<Value> {
        // Note: `Module#const_get` is a reflection API and (since Ruby 3.0+)
        // intentionally does *not* enforce constant visibility — private
        // constants are readable through `const_get`. Visibility is only
        // checked for syntactic qualified access (`Foo::Bar`, `::Bar`,
        // `obj::Bar`) in `Executor::find_constant`.
        if inherit {
            self.get_constant_superclass_checked(globals, module, name)
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
            Some(state) => match &state.kind {
                ConstStateKind::Loaded(v) => return Ok(Some(*v)),
                ConstStateKind::Autoload(file_name) => {
                    let file_name = file_name.clone();
                    globals.remove_constant(class_id, name);
                    let _level = self.inc_require_level();

                    #[cfg(feature = "dump-require")]
                    eprintln!("{} > Autoload:{:?}", "  ".repeat(_level), name);

                    let res = self.require(globals, &file_name, false);

                    #[cfg(feature = "dump-require")]
                    eprintln!("{} < Autoload:{:?}", "  ".repeat(_level), name);

                    self.dec_require_level();
                    res?;
                }
            },
        };
        match globals.get_constant(class_id, name) {
            None => Ok(None),
            Some(state) => match &state.kind {
                ConstStateKind::Loaded(v) => Ok(Some(*v)),
                ConstStateKind::Autoload(_) => Ok(None),
            },
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

    pub(crate) fn get_constant_superclass_checked(
        &mut self,
        globals: &mut Globals,
        module: Module,
        name: IdentId,
    ) -> Result<Value> {
        match self.get_constant_superclass(globals, module, name)? {
            Some(v) => Ok(v),
            None => Err(MonorubyErr::uninitialized_constant(name)),
        }
    }

    fn get_constant_superclass(
        &mut self,
        globals: &mut Globals,
        mut module: Module,
        name: IdentId,
    ) -> Result<Option<Value>> {
        loop {
            match self.get_constant(globals, module.id(), name)? {
                Some(v) => return Ok(Some(v)),
                None => match module.superclass() {
                    Some(superclass) => module = superclass,
                    None => break,
                },
            };
        }
        Ok(None)
    }

    /// Walk the ancestor chain of *module* to find the constant *name* and
    /// return both its value and the class on which it is defined. Used by
    /// callers that need to enforce constant visibility, since visibility is
    /// stored on the *defining* class.
    pub(crate) fn get_constant_superclass_with_class(
        &mut self,
        globals: &mut Globals,
        mut module: Module,
        name: IdentId,
    ) -> Result<(Value, ClassId)> {
        loop {
            // Snapshot whether the class has the constant before triggering
            // autoload, since `get_constant` may transition the entry from
            // `Autoload` to `Loaded` and we still want to attribute the
            // visibility to the defining class.
            let owns = globals.store[module.id()].has_own_constant(name);
            match self.get_constant(globals, module.id(), name)? {
                Some(v) => {
                    if owns {
                        return Ok((v, module.id()));
                    } else {
                        // Should not normally happen since `get_constant`
                        // only returns Some when the entry exists on this
                        // class, but be defensive.
                        return Ok((v, module.id()));
                    }
                }
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
        // Search the current frame's lexical_context first (covers string
        // eval where the eval's ISeqInfo has the receiver's class set).
        let frame_func = self.cfp().lfp().func_id();
        if frame_func != current_func {
            if let Some(v) = self.search_lexical_stack(globals, name, frame_func)? {
                return Ok(v);
            }
        }
        // Then search the enclosing method's lexical_context.
        if let Some(v) = self.search_lexical_stack(globals, name, current_func)? {
            return Ok(v);
        }
        // For superclass fallback, prefer the frame's lexical class if the
        // frame has its own lexical_context (i.e. string eval), otherwise
        // use the enclosing method's lexical class.
        let lexical_class = if frame_func != current_func {
            let fc = frame_func.lexical_class(globals);
            if fc != OBJECT_CLASS {
                fc
            } else {
                current_func.lexical_class(globals)
            }
        } else {
            current_func.lexical_class(globals)
        };
        let module = globals[lexical_class].get_module();
        match self.get_constant_superclass(globals, module, name)? {
            Some(v) => return Ok(v),
            None => {
                // Fall back to Object class, matching CRuby's implicit
                // top-level cref.  This ensures that classes inheriting from
                // BasicObject can still resolve top-level constants.
                if lexical_class == BASIC_OBJECT_CLASS {
                    if let Some(v) = self.get_constant(globals, OBJECT_CLASS, name)? {
                        return Ok(v);
                    }
                }
            }
        }
        Err(MonorubyErr::uninitialized_constant(name))
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
            .rev()
            .cloned();
        for module in stack {
            if globals.store.get_constant(module, name).is_some() {
                return self.get_constant(globals, module, name);
            }
        }
        Ok(None)
    }
}
