use super::*;

impl Executor {
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
        let feature = match globals.get_constant(class_id, name) {
            None => return Ok(None),
            Some(state) => match &state.kind {
                ConstStateKind::Loaded(v) => {
                    let v = *v;
                    let deprecated = state.is_deprecated();
                    if deprecated {
                        let qualified =
                            format!("{}::{}", class_id.get_name(&globals.store), name);
                        crate::value::emit_deprecated_constant_warning(
                            self, globals, &qualified,
                        )?;
                    }
                    return Ok(Some(v));
                }
                ConstStateKind::Autoload(entry) => match entry.state {
                    // Same-thread re-entry while the autoload's own
                    // require is in flight: behave as "not yet
                    // defined" without re-triggering, matching CRuby's
                    // recursion guard. The caller will surface
                    // NameError or const_missing.
                    AutoloadState::Loading => return Ok(None),
                    AutoloadState::Idle => entry.feature.clone(),
                },
            },
        };

        // Mark the slot Loading for the duration of the require so a
        // recursive reference from inside the file we're about to
        // load is rejected by the branch above.
        globals
            .store
            .set_autoload_state(class_id, name, AutoloadState::Loading);

        let _level = self.inc_require_level();

        #[cfg(feature = "dump-require")]
        eprintln!("{} > Autoload:{:?}", "  ".repeat(_level), name);

        let res = self.require(globals, &feature, false);

        #[cfg(feature = "dump-require")]
        eprintln!("{} < Autoload:{:?}", "  ".repeat(_level), name);

        self.dec_require_level();

        if let Err(e) = res {
            // require failed (LoadError, syntax error, …). Revert
            // the slot to Idle so a future reference may retry,
            // matching CRuby's behaviour of leaving the autoload
            // registration intact across a failed load.
            globals
                .store
                .set_autoload_state(class_id, name, AutoloadState::Idle);
            return Err(e);
        }

        // require returned successfully. Either the file defined the
        // constant (slot is now Loaded — typically via `set_constant`
        // or a `class`/`module` keyword) or it didn't (slot is still
        // Autoload + Loading). The "still Autoload" case means CRuby
        // would surface NameError; drop the entry so subsequent
        // references see "missing" rather than re-entering require.
        match globals.get_constant(class_id, name) {
            None => Ok(None),
            Some(state) => match &state.kind {
                ConstStateKind::Loaded(v) => Ok(Some(*v)),
                ConstStateKind::Autoload(_) => {
                    globals.remove_constant(class_id, name);
                    Ok(None)
                }
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

    /// Non-triggering probe of a constant directly on `class_id` —
    /// returns true if a value exists or an autoload is registered
    /// (Idle), false for missing entries and for entries currently
    /// `Loading` on this thread (same-thread recursion sees the slot
    /// as not-yet-defined). Mirrors CRuby's `rb_const_defined_at`
    /// with `autoload_load = FALSE`.
    pub(crate) fn probe_constant_at(
        globals: &Globals,
        class_id: ClassId,
        name: IdentId,
    ) -> bool {
        match globals.store.get_constant(class_id, name) {
            None => false,
            Some(state) => match &state.kind {
                ConstStateKind::Loaded(_) => true,
                ConstStateKind::Autoload(entry) => match entry.state {
                    AutoloadState::Idle => true,
                    AutoloadState::Loading => false,
                },
            },
        }
    }

    /// Non-triggering walk of the ancestor chain of `module`. Used by
    /// `defined?` / `Module#const_defined?` for the final lookup
    /// segment so that autoload-registered constants register as
    /// "defined" without invoking `require`.
    pub(crate) fn probe_constant_superclass(
        globals: &Globals,
        mut module: Module,
        name: IdentId,
    ) -> bool {
        loop {
            if Self::probe_constant_at(globals, module.id(), name) {
                return true;
            }
            match module.superclass() {
                Some(superclass) => module = superclass,
                None => return false,
            }
        }
    }

    /// Non-triggering walk of the ancestor chain that also returns the
    /// defining class so a caller can enforce visibility (private
    /// constants are still considered defined, but qualified-access
    /// callers may want to raise `NameError: private constant` —
    /// matching `find_constant`'s behaviour but without firing
    /// autoload).
    pub(crate) fn probe_constant_superclass_with_class(
        globals: &Globals,
        mut module: Module,
        name: IdentId,
    ) -> Option<ClassId> {
        loop {
            if Self::probe_constant_at(globals, module.id(), name) {
                return Some(module.id());
            }
            match module.superclass() {
                Some(superclass) => module = superclass,
                None => return None,
            }
        }
    }

    /// Non-triggering search of the *lexical* scope chain (innermost
    /// first). Used by `defined?(Foo)` for unqualified references.
    fn probe_lexical_stack(
        &self,
        globals: &Globals,
        name: IdentId,
        current_func: FuncId,
    ) -> bool {
        let iseq = match globals.store[current_func].is_iseq() {
            Some(iseq) => iseq,
            None => return false,
        };
        for module in globals.store[iseq].lexical_context.iter().rev().copied() {
            if Self::probe_constant_at(globals, module, name) {
                return true;
            }
        }
        false
    }

    /// Resolve a `ConstSiteId` for `defined?` semantics: intermediate
    /// segments are looked up with autoload triggering (CRuby resolves
    /// them via the regular `getconstant` path), but the *final*
    /// segment is probed without firing the autoload. Returns true if
    /// the constant is reachable.
    ///
    /// Errors from intermediate-segment resolution are swallowed —
    /// `defined?` reports `nil`, never propagates.
    pub(crate) fn probe_constant(
        &mut self,
        globals: &mut Globals,
        site_id: ConstSiteId,
    ) -> bool {
        let ConstSiteInfo {
            name,
            toplevel,
            mut prefix,
            base,
            ..
        } = globals.store[site_id].clone();
        // SAFETY: `base` slot was populated by the bytecode compiler.
        let base = base.map(|base| unsafe { self.get_slot(base) }.unwrap());
        let current_func = self.method_func_id();
        let parent = if let Some(base) = base {
            match base.is_class_or_module() {
                Some(m) => m.id(),
                None => return false,
            }
        } else if toplevel {
            OBJECT_CLASS
        } else if prefix.is_empty() {
            // Single-segment unqualified reference: walk lexical
            // contexts (innermost first), then the enclosing class's
            // ancestor chain — all non-triggering.
            let frame_func = self.cfp().lfp().func_id();
            if frame_func != current_func && self.probe_lexical_stack(globals, name, frame_func) {
                return true;
            }
            if self.probe_lexical_stack(globals, name, current_func) {
                return true;
            }
            let lexical_class = if frame_func != current_func {
                let fc = frame_func.lexical_class(&globals.store);
                if fc != OBJECT_CLASS {
                    fc
                } else {
                    current_func.lexical_class(&globals.store)
                }
            } else {
                current_func.lexical_class(&globals.store)
            };
            let module = globals.store[lexical_class].get_module();
            if Self::probe_constant_superclass(globals, module, name) {
                return true;
            }
            // BasicObject subclasses fall through to Object.
            if lexical_class == BASIC_OBJECT_CLASS
                && Self::probe_constant_at(globals, OBJECT_CLASS, name)
            {
                return true;
            }
            return false;
        } else {
            // Resolve the leading qualifier with the normal triggering
            // path; we need the actual class to walk further.
            let parent = prefix.remove(0);
            match self.search_constant_checked(globals, parent, current_func) {
                Ok(v) => match v.is_class_or_module() {
                    Some(m) => m.id(),
                    None => return false,
                },
                Err(_) => return false,
            }
        };
        let mut parent = parent;
        for constant in prefix {
            match self.get_constant_superclass_with_class(
                globals,
                globals.store[parent].get_module(),
                constant,
            ) {
                Ok((val, _)) => match val.is_class_or_module() {
                    Some(m) => parent = m.id(),
                    None => return false,
                },
                Err(_) => return false,
            }
        }
        Self::probe_constant_superclass(globals, globals.store[parent].get_module(), name)
    }

    fn search_lexical_stack(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        current_func: FuncId,
    ) -> Result<Option<Value>> {
        // `current_func` can legitimately be a builtin frame when the
        // `Module#class_eval` / `instance_eval` / `Kernel#eval` site
        // walked the cfp chain to find the nearest Ruby frame for the
        // outer scope but dispatch is now happening *inside* a
        // bytecode block whose containing method is still the builtin.
        // No iseq -> nothing to walk; return None and let the caller
        // fall through to the ancestor-chain lookup.
        let iseq = match globals.store[current_func].is_iseq() {
            Some(iseq) => iseq,
            None => return Ok(None),
        };
        let stack = globals.store[iseq]
            .lexical_context
            .iter()
            .rev()
            .cloned()
            .collect::<Vec<_>>();
        for module in stack {
            if globals.store.get_constant(module, name).is_some() {
                return self.get_constant(globals, module, name);
            }
        }
        Ok(None)
    }
}
