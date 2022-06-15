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

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct ClassId(u32);

impl ClassId {
    pub const fn new(id: u32) -> Self {
        Self(id)
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

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfo {
    /// class object.
    class_object: Option<Value>,
    /// method table.
    methods: HashMap<IdentId, FuncId>,
    /// constants table.
    constants: HashMap<IdentId, Value>,
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            class_object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    pub fn set_class_obj(&mut self, class_obj: Value) {
        self.class_object = Some(class_obj);
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
            classes: vec![ClassInfo::new()],
        }
    }

    pub fn add_class(&mut self) -> ClassId {
        let id = self.classes.len();
        self.classes.push(ClassInfo::new());
        ClassId(id as u32)
    }

    pub fn add_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        self[class_id].methods.insert(name, func);
    }

    pub fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self[class_id].methods.get(&name).cloned()
    }

    pub fn set_constants(&mut self, name: IdentId, val: Value) -> Option<Value> {
        self.classes[0].constants.insert(name, val)
    }

    pub fn get_constants(&self, name: IdentId) -> Option<Value> {
        self.classes[0].constants.get(&name).cloned()
    }
}
