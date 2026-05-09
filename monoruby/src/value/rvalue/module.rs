use super::*;

#[monoruby_object]
pub struct Module(Value);

impl Module {
    pub(crate) fn get(self) -> Value {
        self.0
    }

    pub(crate) fn is_module(&self) -> bool {
        match self.0.try_rvalue() {
            Some(rvalue) => rvalue.ty() == ObjTy::MODULE,
            None => false,
        }
    }

    ///
    /// Check whether `self` is an ancestor of `module`.
    ///
    pub(crate) fn is_ancestor_of(self, mut module: Module) -> bool {
        while let Some(superclass) = module.superclass() {
            if superclass.id() == self.id() {
                return true;
            }
            module = superclass;
        }
        false
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        self.0.change_class(new_class_id);
    }

    ///
    /// Include `module` to `self`.
    ///
    /// We must ensure `module` does not include `self` cyclically.
    ///
    /// class_version is incremented.
    ///
    pub(crate) fn include_module(&mut self, module: Module) -> Result<()> {
        self.include_or_prepend_module(module, false)
    }

    fn include_or_prepend_module(
        &mut self,
        mut module: Module,
        for_prepend: bool,
    ) -> Result<()> {
        if self.check_cyclic(module) {
            return Err(MonorubyErr::argumenterr("cyclic include detected"));
        }
        Globals::class_version_inc();
        // The newly-inserted iclass exposes `module`'s constants to
        // anything that previously resolved unqualified constants
        // against `self`'s ancestor chain. Invalidate the constant
        // cache so cached lookups re-walk the new chain.
        Globals::const_version_inc();
        // If the module being included/prepended has its own origin
        // (i.e. one or more modules were prepended into it), the head
        // node represents the prepend-most position — its method table
        // is intentionally empty in CRuby; the module's actual content
        // lives at the origin iclass. Skip the head and start the walk
        // at the first iclass of its prepend chain so the resulting
        // chain order matches CRuby's: prepended modules first, the
        // module itself last.
        if module.has_origin() {
            if let Some(superclass) = module.superclass()
                && superclass.is_iclass()
            {
                module = superclass;
            } else {
                return Ok(());
            }
        }
        let mut base = *self;
        loop {
            // For prepend: only treat the module as already-an-ancestor
            // if it's in the prepend region (between head and origin).
            // CRuby intentionally lets you prepend a module that's also
            // included down the chain — both iclass entries appear in
            // ancestors, and method dispatch sees the prepend first.
            let already_present = if for_prepend {
                module.is_ancestor_of_before_origin(*self)
            } else {
                module.is_ancestor_of(*self)
            };
            if !already_present {
                base.include(module);
            }
            base = base.superclass().unwrap();
            if let Some(superclass) = module.superclass()
                && superclass.is_iclass()
            {
                module = superclass;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Like `is_ancestor_of`, but only checks the prepend region of
    /// `class` (the chain segment between `class`'s head and its
    /// origin iclass). Used by `prepend_module` to avoid duplicate
    /// inserts in the prepend region while still allowing the same
    /// module to surface again via an existing include further down
    /// the chain.
    ///
    /// Returns true iff `self` (a module) is reachable from `class`
    /// via super-walk, stopping at `class`'s origin iclass (exclusive).
    fn is_ancestor_of_before_origin(self, class: Module) -> bool {
        let origin = class.origin();
        let mut cur = class;
        while let Some(superclass) = cur.superclass() {
            if let Some(o) = origin
                && superclass.as_val() == o.as_val()
            {
                // Reached the origin iclass — stop. Anything past this
                // point is either `class`'s own methods (origin) or
                // included modules below origin.
                return false;
            }
            if superclass.id() == self.id() {
                return true;
            }
            cur = superclass;
        }
        false
    }

    fn include(&mut self, module: Module) {
        let include_module = module.make_iclass(self.superclass());
        self.superclass = Some(include_module);
    }

    ///
    /// Check whether `self` is an ancestor of `module`.
    ///
    fn check_cyclic(self, mut module: Module) -> bool {
        if module.id() == self.id() {
            return true;
        }
        while let Some(superclass) = module.superclass()
            && superclass.is_module()
        {
            if superclass.id() == self.id() {
                return true;
            }
            module = superclass;
        }
        false
    }

    ///
    /// Prepend `module` to `self`
    ///
    /// ```text
    ///
    ///  +-------+      +-------+
    ///  | super |      | super |
    ///  +-------+      +-------+
    ///      |              |
    ///  +-------+      +-------+
    ///  | _self |-->m  | origin|-->m
    ///  +-------+      +-------+ <-+
    ///                     |       |
    ///                 +-------+   |
    ///                 |prepend|   |
    ///                 +-------+   |
    ///                     |     origin
    ///                 +-------+   |
    ///                 | _self |---+
    ///                 +-------+
    ///
    /// ```
    pub(crate) fn prepend_module(&mut self, module: Module) -> Result<()> {
        if self.origin.is_none() {
            let origin = self.make_iclass(self.superclass());
            self.superclass = Some(origin);
            self.origin = Some(origin);
        }
        self.include_or_prepend_module(module, true)
    }

    pub fn get_real_class(&self) -> Module {
        let mut class = *self;
        while class.is_singleton().is_some() || class.is_iclass() {
            class = class.superclass().unwrap();
        }
        class
    }

    pub fn get_real_superclass(&self) -> Option<Module> {
        let mut class = self.superclass()?;
        while class.is_iclass() {
            class = class.superclass()?;
        }
        Some(class)
    }

    pub fn is_exception(&self) -> bool {
        let mut module = *self;
        if module.id() == EXCEPTION_CLASS {
            return true;
        }
        while let Some(superclass) = module.superclass() {
            if superclass.id() == EXCEPTION_CLASS {
                return true;
            } else {
                module = superclass;
            }
        }
        false
    }

    fn make_iclass(&self, superclass: Option<Module>) -> Module {
        Value::iclass(self.id(), superclass).as_class()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleType {
    RealClass,
    //Singleton(Value),
    IClass,
}

#[derive(Debug, Clone)]
pub struct ModuleInner {
    /// ClassId of thiis module/class.
    class_id: ClassId,
    /// super class object of this module.
    superclass: Option<Module>,
    /// is singleton class?
    singleton: Option<Value>,
    /// origin of this module/class (for prepend)
    origin: Option<Module>,
    /// the type of this module/class
    class_type: ModuleType,
}

impl GC<RValue> for ModuleInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(class) = self.superclass {
            class.mark(alloc)
        }
        if let Some(val) = self.singleton {
            val.mark(alloc)
        }
        if let Some(val) = self.origin {
            val.mark(alloc)
        }
    }
}

impl ModuleInner {
    pub fn new(
        class_id: ClassId,
        superclass: Option<Module>,
        singleton: Option<Value>,
        class_type: ModuleType,
    ) -> Self {
        Self {
            class_id,
            superclass,
            singleton,
            origin: None,
            class_type,
        }
    }

    pub fn id(&self) -> ClassId {
        self.class_id
    }

    pub fn superclass(&self) -> Option<Module> {
        self.superclass
    }

    pub fn set_superclass(&mut self, super_class: Option<Module>) {
        self.superclass = super_class;
    }

    pub fn superclass_value(&self) -> Option<Value> {
        self.superclass.map(|m| m.0)
    }

    pub fn superclass_id(&self) -> Option<ClassId> {
        self.superclass.map(|m| m.id())
    }

    pub fn class_type(&self) -> &ModuleType {
        &self.class_type
    }

    pub fn is_iclass(&self) -> bool {
        self.class_type == ModuleType::IClass
    }

    pub fn is_singleton(&self) -> Option<Value> {
        self.singleton
    }

    pub fn has_origin(&self) -> bool {
        self.origin.is_some()
    }

    pub fn origin(&self) -> Option<Module> {
        self.origin
    }
}
