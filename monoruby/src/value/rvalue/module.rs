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
    pub(crate) fn include_module(&mut self, mut module: Module) -> Result<()> {
        if self.check_cyclic(module) {
            return Err(MonorubyErr::argumenterr("cyclic include detected"));
        }
        let mut base = *self;
        loop {
            base.include(module);
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

    fn include(&mut self, module: Module) {
        let include_module = module.make_iclass(self.superclass());
        self.superclass = Some(include_module);
    }

    ///
    /// Check whether `self` is cyclically included in `module`.
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
        self.include_module(module)
    }

    pub fn get_real_class(&self) -> Module {
        let mut class = *self;
        while class.is_singleton().is_some() {
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
