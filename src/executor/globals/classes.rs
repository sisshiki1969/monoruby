use super::*;

pub const OBJECT_CLASS: ClassId = ClassId::new(1);
pub const CLASS_CLASS: ClassId = ClassId::new(2);
pub const NIL_CLASS: ClassId = ClassId::new(3);
pub const TRUE_CLASS: ClassId = ClassId::new(4);
pub const FALSE_CLASS: ClassId = ClassId::new(5);
pub const INTEGER_CLASS: ClassId = ClassId::new(6);
pub const FLOAT_CLASS: ClassId = ClassId::new(7);
pub const STRING_CLASS: ClassId = ClassId::new(8);
pub const SYMBOL_CLASS: ClassId = ClassId::new(9);
pub const TIME_CLASS: ClassId = ClassId::new(10);
pub const ARRAY_CLASS: ClassId = ClassId::new(11);

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ClassId(u32);

impl ClassId {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn get(&self) -> u32 {
        self.0
    }
}

impl std::fmt::Debug for ClassId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            1 => write!(f, "OBJECT"),
            2 => write!(f, "CLASS"),
            3 => write!(f, "NIL"),
            4 => write!(f, "TRUE"),
            5 => write!(f, "FALSE"),
            6 => write!(f, "INTEGER"),
            7 => write!(f, "FLOAT"),
            8 => write!(f, "STRING"),
            9 => write!(f, "SYMBOL"),
            10 => write!(f, "TIME"),
            n => write!(f, "ClassId({})", n),
        }
    }
}

impl std::default::Default for ClassId {
    fn default() -> Self {
        Self(0)
    }
}

impl Into<u32> for ClassId {
    fn into(self) -> u32 {
        self.0
    }
}

impl ClassId {
    /// Get *ClassId* of the super classof *self*.
    pub fn super_class(self, globals: &Globals) -> Option<ClassId> {
        globals.get_super_class(self)
    }

    /// Get object for *ClassId*.
    pub fn get_obj(self, globals: &Globals) -> Value {
        globals.get_class_obj(self)
    }

    /// Get class name of *ClassId*.
    pub fn get_name(self, globals: &Globals) -> String {
        if self.0 == 0 {
            return "<INVALID>".to_string();
        }
        let val = self.get_obj(globals);
        match globals.class[self].get_name() {
            Some(id) => IdentId::get_name(id),
            None => match globals.class[self].is_singleton {
                None => format!("#<Class:{:016x}>", val.get()),
                Some(base) => format!("#<Class:{}>", globals.val_tos(base)),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfo {
    /// the constant name which this class object is bound.
    name: Option<IdentId>,
    /// corresponding class object.
    object: Option<Value>,
    /// super class.
    super_class_id: Option<ClassId>,
    /// is singleton class?
    pub(super) is_singleton: Option<Value>,
    /// method table.
    methods: HashMap<IdentId, FuncId>,
    /// constants table.
    constants: HashMap<IdentId, Value>,
}

impl ClassInfo {
    fn new(super_class_id: Option<ClassId>) -> Self {
        Self {
            name: None,
            object: None,
            super_class_id,
            is_singleton: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    fn new_singleton(super_class_id: Option<ClassId>, base: Value) -> Self {
        Self {
            name: None,
            object: None,
            super_class_id,
            is_singleton: Some(base),
            methods: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    pub(super) fn set_class_obj(&mut self, class_obj: Value) {
        self.object = Some(class_obj);
    }

    pub(super) fn get_obj(&self) -> Value {
        self.object.unwrap()
    }

    pub fn set_name(&mut self, name: IdentId) {
        self.name = Some(name);
    }

    pub fn get_name(&self) -> Option<IdentId> {
        self.name
    }

    pub(super) fn super_class(&self) -> Option<ClassId> {
        self.super_class_id
    }

    pub fn is_singleton(&self) -> bool {
        self.is_singleton.is_some()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassStore {
    /// class table.
    classes: Vec<ClassInfo>,
}

impl std::ops::Index<ClassId> for ClassStore {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &Self::Output {
        &self.classes[index.0 as usize]
    }
}

impl std::ops::IndexMut<ClassId> for ClassStore {
    fn index_mut(&mut self, index: ClassId) -> &mut Self::Output {
        &mut self.classes[index.0 as usize]
    }
}

impl ClassStore {
    pub fn new() -> Self {
        Self {
            classes: vec![ClassInfo::new(None)],
        }
    }

    pub(super) fn add_class(&mut self, super_class: Option<ClassId>) -> ClassId {
        let id = self.classes.len();
        self.classes.push(ClassInfo::new(super_class));
        ClassId(id as u32)
    }

    pub(super) fn add_singleton_class(
        &mut self,
        super_class: Option<ClassId>,
        base: Value,
    ) -> ClassId {
        let id = self.classes.len();
        self.classes
            .push(ClassInfo::new_singleton(super_class, base));
        ClassId(id as u32)
    }

    pub fn add_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        self[class_id].methods.insert(name, func);
    }

    pub fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self[class_id].methods.get(&name).cloned()
    }

    pub fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) -> Option<Value> {
        self[class_id].constants.insert(name, val)
    }

    pub fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self[class_id].constants.get(&name).cloned()
    }

    pub fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].constants.keys().cloned().collect()
    }

    pub fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].methods.keys().cloned().collect()
    }

    pub(super) fn get_real_class_obj(&self, val: Value) -> Value {
        let mut id = val.class_id();
        while self[id].is_singleton() {
            id = self[id].get_obj().class_id();
        }
        self[id].get_obj()
    }
}
