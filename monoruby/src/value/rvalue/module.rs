use crate::*;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Module(Value);

impl std::ops::Deref for Module {
    type Target = ModuleInner;
    fn deref(&self) -> &Self::Target {
        self.as_ref_val().rvalue().as_class()
    }
}

impl std::ops::DerefMut for Module {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_ref_val().rvalue_mut().as_class_mut()
    }
}

impl alloc::GC<RValue> for Module {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.0.mark(alloc);
    }
}

impl Module {
    pub(crate) fn new(val: Value) -> Self {
        match val.rvalue().ty() {
            ObjKind::CLASS | ObjKind::MODULE => Self(val),
            _ => unreachable!(),
        }
    }

    pub fn as_val(&self) -> Value {
        self.0
    }

    pub fn as_ref_val(&self) -> &Value {
        &self.0
    }

    pub fn as_mut_ref_val(&mut self) -> &mut Value {
        &mut self.0
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        self.0.change_class(new_class_id);
    }

    pub fn get_real_class(&self) -> Module {
        let mut class = *self;
        while !class.is_real_class() {
            class = class.superclass().unwrap();
        }
        class
    }

    pub fn make_iclass(&self, superclass: Option<Module>) -> Module {
        Value::iclass(self.id(), superclass).as_class()
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

    pub fn change_superclass(&mut self, superclass: Module) {
        self.superclass = Some(superclass);
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
