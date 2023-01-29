use crate::*;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Module(Value);

impl std::ops::Deref for Module {
    type Target = ModuleInner;
    fn deref(&self) -> &Self::Target {
        self.0.as_class()
    }
}

impl Module {
    pub fn new(val: Value) -> Self {
        match val.rvalue().kind() {
            ObjKind::CLASS | ObjKind::MODULE => Self(val),
            _ => unreachable!(),
        }
    }

    pub fn as_val(&self) -> Value {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInner {
    class: ClassId,
    /// super class object.
    superclass: Option<Module>,
}

impl ModuleInner {
    pub fn new(class: ClassId, superclass: Option<Module>) -> Self {
        Self { class, superclass }
    }

    pub fn class_id(&self) -> ClassId {
        self.class
    }

    pub fn superclass(&self) -> Option<Module> {
        self.superclass
    }

    pub fn superclass_value(&self) -> Option<Value> {
        self.superclass.map(|m| m.as_val())
    }
}
