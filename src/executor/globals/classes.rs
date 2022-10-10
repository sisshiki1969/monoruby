use super::*;

impl Globals {
    pub fn get_class_obj(&self, class_id: ClassId) -> Value {
        self.class[class_id].get_obj()
    }

    pub fn get_super_class(&self, class_id: ClassId) -> Option<ClassId> {
        self.class[class_id].super_class()
    }

    pub fn define_class_under_obj(&mut self, name: &str) -> Value {
        self.define_class(name, Some(OBJECT_CLASS), OBJECT_CLASS)
    }

    pub fn define_class(
        &mut self,
        name: &str,
        super_class: impl Into<Option<ClassId>>,
        parent: ClassId,
    ) -> Value {
        let name_id = IdentId::get_ident_id(name);
        self.define_class_by_ident_id(name_id, super_class, parent)
    }

    pub fn define_class_by_ident_id(
        &mut self,
        name_id: IdentId,
        super_class: impl Into<Option<ClassId>>,
        parent: ClassId,
    ) -> Value {
        let id = self.class.add_class(super_class.into());
        let class_obj = Value::new_empty_class(id);
        self.class[id].set_class_obj(class_obj);
        self.class[id].set_name(name_id);
        self.set_constant(parent, name_id, class_obj);
        class_obj
    }

    pub fn get_real_class_obj(&self, val: Value) -> Value {
        self.class.get_real_class_obj(val)
    }

    pub fn get_singleton_id(&mut self, original_id: ClassId) -> ClassId {
        let mut original = self.get_class_obj(original_id);
        let original_class_id = original.class_id();
        if self.class[original_class_id].is_singleton() {
            return original_class_id;
        }
        let super_singleton_id = match original_id.super_class(self) {
            Some(id) => self.get_singleton_id(id),
            None => CLASS_CLASS,
        };

        let (mut singleton, singleton_id) =
            self.new_singleton_class(Some(super_singleton_id), original);
        original.change_class(singleton_id);
        singleton.change_class(original_class_id);
        #[cfg(debug_assertions)]
        {
            assert_eq!(original.class_id(), singleton_id);
            assert!(self.class[singleton_id].is_singleton());
        }
        singleton_id
    }

    pub fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) -> Option<Value> {
        self.class.set_constant(class_id, name, val)
    }

    ///
    /// Get constant with *name* and parent class *class_id*.
    ///
    /// If not found, simply return None with no error.
    ///
    pub fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self.class.get_constant(class_id, name)
    }

    pub fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.class.get_constant_names(class_id)
    }

    ///
    /// Get constant for ConstSiteId *id*.
    ///
    /// If not found, set uninitialized constant error and return None.
    ///
    pub fn find_constant(&mut self, id: ConstSiteId, class_context: &[ClassId]) -> Option<Value> {
        let ConstSiteInfo {
            toplevel,
            mut prefix,
            name,
            ..
        } = self.func[id].clone();
        if toplevel {
            let mut parent = OBJECT_CLASS;
            for constant in prefix {
                parent = self
                    .get_constant_checked(parent, constant)?
                    .expect_class(name, self)?;
            }
            self.get_constant_checked(parent, name)
        } else if prefix.is_empty() {
            match self.search_lexical_stack(name, class_context) {
                Some(v) => Some(v),
                _ => self.get_constant_checked(OBJECT_CLASS, name),
            }
        } else {
            let parent = prefix.remove(0);
            let mut parent = match self.search_lexical_stack(parent, class_context) {
                Some(v) => v,
                None => self.get_constant_checked(OBJECT_CLASS, parent)?,
            }
            .expect_class(name, self)?;
            for constant in prefix {
                parent = self
                    .get_constant_checked(parent, constant)?
                    .expect_class(name, self)?;
            }
            self.get_constant_checked(parent, name)
        }
    }

    pub fn add_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        self.class.add_method(class_id, name, func)
    }

    pub fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self.class.get_method(class_id, name)
    }

    pub fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.class.get_method_names(class_id)
    }
}

impl Globals {
    fn new_singleton_class(
        &mut self,
        super_class: impl Into<Option<ClassId>>,
        base: Value,
    ) -> (Value, ClassId) {
        let id = self.class.add_singleton_class(super_class.into(), base);
        let class_obj = Value::new_empty_class(id);
        self.class[id].set_class_obj(class_obj);
        (class_obj, id)
    }

    ///
    /// Get constant with *name* and parent class *class_id*.
    ///
    /// If not found, set uninitialized constant error and return None.
    ///
    fn get_constant_checked(&mut self, class_id: ClassId, name: IdentId) -> Option<Value> {
        match self.get_constant(class_id, name) {
            Some(v) => Some(v),
            None => {
                self.err_uninitialized_constant(name);
                None
            }
        }
    }

    fn search_lexical_stack(&self, name: IdentId, class_context: &[ClassId]) -> Option<Value> {
        class_context
            .iter()
            .rev()
            .find_map(|class| self.get_constant(*class, name))
    }
}

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
pub(super) struct ClassInfo {
    /// the constant name which this class object is bound.
    name: Option<IdentId>,
    /// corresponding class object.
    object: Option<Value>,
    /// super class.
    super_class_id: Option<ClassId>,
    /// is singleton class?
    is_singleton: Option<Value>,
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

    fn set_class_obj(&mut self, class_obj: Value) {
        self.object = Some(class_obj);
    }

    fn get_obj(&self) -> Value {
        self.object.unwrap()
    }

    fn set_name(&mut self, name: IdentId) {
        self.name = Some(name);
    }

    fn get_name(&self) -> Option<IdentId> {
        self.name
    }

    fn super_class(&self) -> Option<ClassId> {
        self.super_class_id
    }

    fn is_singleton(&self) -> bool {
        self.is_singleton.is_some()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ClassStore {
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

    fn add_class(&mut self, super_class: Option<ClassId>) -> ClassId {
        let id = self.classes.len();
        self.classes.push(ClassInfo::new(super_class));
        ClassId(id as u32)
    }

    fn add_singleton_class(&mut self, super_class: Option<ClassId>, base: Value) -> ClassId {
        let id = self.classes.len();
        self.classes
            .push(ClassInfo::new_singleton(super_class, base));
        ClassId(id as u32)
    }

    fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) -> Option<Value> {
        self[class_id].constants.insert(name, val)
    }

    fn get_constant(&self, class_id: ClassId, name: IdentId) -> Option<Value> {
        self[class_id].constants.get(&name).cloned()
    }

    fn get_constant_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].constants.keys().cloned().collect()
    }

    fn add_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        self[class_id].methods.insert(name, func);
    }

    fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self[class_id].methods.get(&name).cloned()
    }

    fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].methods.keys().cloned().collect()
    }

    fn get_real_class_obj(&self, val: Value) -> Value {
        let mut id = val.class_id();
        while self[id].is_singleton() {
            id = self[id].get_obj().class_id();
        }
        self[id].get_obj()
    }
}
