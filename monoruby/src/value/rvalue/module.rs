use super::*;

#[monoruby_object]
pub struct Module(Value);

impl std::cmp::PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Module {
    pub(crate) fn new(val: Value) -> Self {
        match val.rvalue().ty() {
            ObjKind::CLASS | ObjKind::MODULE => Self(val),
            _ => unreachable!(),
        }
    }

    pub(crate) fn get(self) -> Value {
        self.0
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        self.0.change_class(new_class_id);
    }

    pub(crate) fn include_module(&mut self, module: Module) {
        let module = module.make_iclass(self.superclass());
        self.superclass = Some(module);
    }

    pub fn get_real_class(&self) -> Module {
        let mut class = *self;
        while !class.is_real_class() {
            class = class.superclass().unwrap();
        }
        class
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
    Singleton(Value),
    IClass,
}

#[derive(Debug, Clone)]
pub struct ModuleInner {
    class_id: ClassId,
    /// super class object.
    superclass: Option<Module>,
    /// is singleton class?
    class_type: ModuleType,
}

impl ModuleInner {
    pub fn new(class_id: ClassId, superclass: Option<Module>, class_type: ModuleType) -> Self {
        Self {
            class_id,
            superclass,
            class_type,
        }
    }

    pub fn id(&self) -> ClassId {
        self.class_id
    }

    pub fn superclass(&self) -> Option<Module> {
        self.superclass
    }

    pub fn superclass_value(&self) -> Option<Value> {
        self.superclass.map(|m| m.0)
    }

    pub fn superclass_id(&self) -> Option<ClassId> {
        self.superclass.map(|m| m.id())
    }

    pub fn class_type(&self) -> ModuleType {
        self.class_type.clone()
    }

    pub fn is_singleton(&self) -> Option<Value> {
        match self.class_type {
            ModuleType::Singleton(obj) => Some(obj),
            _ => None,
        }
    }

    pub fn is_real_class(&self) -> bool {
        matches!(self.class_type, ModuleType::RealClass)
    }
}
