use crate::*;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Module(Value);

impl std::ops::Deref for Module {
    type Target = ModuleInner;
    fn deref(&self) -> &Self::Target {
        &self.as_ref_val().rvalue().as_class()
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

    pub fn as_ref_val(&self) -> &Value {
        &self.0
    }

    pub fn get_real_class(&self) -> Module {
        let mut class = *self;
        while !class.is_real_class() {
            class = class.superclass().unwrap();
        }
        class
    }
}

#[derive(Debug, Clone)]
pub enum ModuleType {
    RealClass,
    Singleton(Value),
    //IClass,
}

#[derive(Debug, Clone)]
pub struct ModuleInner {
    class: ClassId,
    /// super class object.
    superclass: Option<Module>,
    /// is singleton class?
    class_type: ModuleType,
}

impl ModuleInner {
    pub fn new(class: ClassId, superclass: Option<Module>, class_type: ModuleType) -> Self {
        Self {
            class,
            superclass,
            class_type,
        }
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
