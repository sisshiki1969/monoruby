use super::*;

mod constants;
mod instance_var;
pub(crate) use instance_var::*;

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
pub const RANGE_CLASS: ClassId = ClassId::new(12);
pub const PROC_CLASS: ClassId = ClassId::new(13);
pub const HASH_CLASS: ClassId = ClassId::new(14);
pub const REGEXP_CLASS: ClassId = ClassId::new(15);
pub const MODULE_CLASS: ClassId = ClassId::new(16);

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash)]
#[repr(transparent)]
pub struct ClassId(pub u32);

impl ClassId {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub(crate) fn is_always_frozen(&self) -> bool {
        matches!(
            *self,
            NIL_CLASS
                | TRUE_CLASS
                | FALSE_CLASS
                | INTEGER_CLASS
                | FLOAT_CLASS
                | SYMBOL_CLASS
                | RANGE_CLASS
        )
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
            11 => write!(f, "ARRAY"),
            12 => write!(f, "RANGE"),
            13 => write!(f, "PROC"),
            14 => write!(f, "HASH"),
            15 => write!(f, "REGEXP"),
            16 => write!(f, "MODULE"),
            n => write!(f, "ClassId({})", n),
        }
    }
}

impl From<ClassId> for u32 {
    fn from(val: ClassId) -> Self {
        val.0
    }
}

impl ClassId {
    /// Get object for *ClassId*.
    pub(crate) fn get_obj(self, globals: &Globals) -> Module {
        globals.get_class_obj(self)
    }

    /// Get class name of *ClassId*.
    pub(crate) fn get_name(self, globals: &Globals) -> String {
        if self.0 == 0 {
            return "<INVALID>".to_string();
        }
        let class = self.get_obj(globals);
        match globals.class[self].name {
            Some(id) => IdentId::get_name(id),
            None => match class.is_singleton() {
                None => format!("#<Class:{:016x}>", class.as_val().get()),
                Some(base) => format!("#<Class:{}>", globals.val_tos(base)),
            },
        }
    }
}

impl Globals {
    fn get_class_obj(&self, class_id: ClassId) -> Module {
        self.class[class_id].object.unwrap()
    }

    pub(crate) fn define_class_under_obj(&mut self, name: &str) -> Module {
        let object_class = OBJECT_CLASS.get_obj(self);
        self.define_class_by_str(name, Some(object_class), OBJECT_CLASS)
    }

    pub(crate) fn define_module(&mut self, name: &str) -> Module {
        let object_class = OBJECT_CLASS.get_obj(self);
        let name_id = IdentId::get_ident_id(name);
        self.define_class(name_id, Some(object_class), OBJECT_CLASS, true)
    }

    pub(in crate::executor) fn define_builtin_class_under_obj(
        &mut self,
        name: &str,
        class_id: ClassId,
    ) -> Module {
        let object_class = OBJECT_CLASS.get_obj(self);
        self.define_builtin_class_by_str(name, class_id, Some(object_class), OBJECT_CLASS)
    }

    pub(crate) fn define_class_by_str(
        &mut self,
        name: &str,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_ident_id(name);
        self.define_class(name_id, superclass, parent, false)
    }

    pub(in crate::executor) fn define_builtin_class_by_str(
        &mut self,
        name: &str,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_ident_id(name);
        self.define_builtin_class(name_id, class_id, superclass, parent)
    }

    pub(crate) fn define_class(
        &mut self,
        name_id: IdentId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
        is_module: bool,
    ) -> Module {
        let class_id = self.class.add_class();
        let obj = self.generate_class_obj(name_id, class_id, superclass.into(), parent, is_module);
        self.get_metaclass(class_id);
        obj
    }

    // TODO: we must name the unnamed class when the class object is assigned to constant later.
    pub(crate) fn new_unnamed_class(&mut self, superclass: Option<Module>) -> Value {
        let class_id = self.class.add_class();
        let superclass = match superclass {
            Some(class) => class,
            None => OBJECT_CLASS.get_obj(self),
        };
        let class_obj = Value::new_empty_class(class_id, Some(superclass));
        self.class[class_id].object = Some(class_obj.as_class());
        class_obj
    }

    fn define_builtin_class(
        &mut self,
        name_id: IdentId,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        self.class.def_builtin_class(class_id);
        self.generate_class_obj(name_id, class_id, superclass.into(), parent, false)
    }

    fn generate_class_obj(
        &mut self,
        name_id: IdentId,
        class_id: ClassId,
        superclass: Option<Module>,
        parent: ClassId,
        is_module: bool,
    ) -> Module {
        let class_obj = if is_module {
            Value::new_empty_module(class_id, superclass)
        } else {
            Value::new_empty_class(class_id, superclass)
        };
        self.class[class_id].object = Some(class_obj.as_class());
        self.class[class_id].name = Some(name_id);
        self.set_constant(parent, name_id, class_obj);
        class_obj.as_class()
    }

    fn new_singleton_class(
        &mut self,
        super_class: impl Into<Option<Module>>,
        base: Value,
    ) -> Module {
        let id = self.class.add_class();
        let class_obj = Value::new_empty_singleton_class(id, super_class.into(), base).as_class();
        self.class[id].object = Some(class_obj);
        class_obj
    }

    ///
    /// Get a metaclass of *original*: ClassId.
    ///
    /// If not exists, create a new metaclass.
    ///
    pub(crate) fn get_metaclass(&mut self, original: ClassId) -> Module {
        let original_obj = self.get_class_obj(original);
        if original_obj.as_val().kind() == Some(ObjKind::CLASS) {
            let class = original_obj.as_val().get_class_obj(self);
            if original_obj.is_singleton().is_none() && class.is_singleton().is_some() {
                return class;
            }
            let super_singleton = match original_obj.superclass() {
                Some(id) => self.get_metaclass(id.class_id()),
                None => CLASS_CLASS.get_obj(self),
            };
            let mut original_obj = original_obj.as_val();
            let singleton = self.new_singleton_class(Some(super_singleton), original_obj);
            original_obj.change_class(singleton.class_id());
            let class_class = if class.class_id() == original {
                singleton.class_id()
            } else {
                self.get_class_obj(class.class_id()).class_id()
            };
            let mut singleton_obj = singleton.as_val();
            singleton_obj.change_class(class_class);
            #[cfg(debug_assertions)]
            {
                assert_eq!(singleton.class_id(), original_obj.class());
                assert_eq!(Some(original_obj), singleton.is_singleton());
            }
            singleton
        } else {
            self.get_singleton(original_obj.as_val())
        }
    }

    ///
    /// Get a singleton class of *original*: Value.
    ///
    /// If not exists, create a new singleton class.
    ///
    pub(crate) fn get_singleton(&mut self, mut original: Value) -> Module {
        let class = original.get_class_obj(self);
        if class.is_singleton().is_some() {
            return class;
        }
        let singleton = self.new_singleton_class(Some(class), original);
        original.change_class(singleton.class_id());
        #[cfg(debug_assertions)]
        {
            assert_eq!(singleton.class_id(), original.class());
        }
        singleton
    }

    pub(crate) fn include_module(&mut self, mut class: Module, module: Module) {
        let module = module.make_iclass(class.superclass());
        class.change_superclass(Some(module));
    }

    ///
    /// Add a new method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        self.class[class_id].methods.insert(name, func);
    }

    ///
    /// Add a new singleton method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_singleton_method(&mut self, class_id: ClassId, name: IdentId, func: FuncId) {
        let singleton = self.get_metaclass(class_id).class_id();
        self.class[singleton].methods.insert(name, func);
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// This fn checks whole superclass chain.
    ///
    pub(crate) fn find_method(&mut self, obj: Value, name: IdentId) -> Option<FuncId> {
        let class_id = obj.class();
        self.find_method_for_class(class_id, name)
    }

    ///
    /// Find method *name* for object *obj*. If not found, return MethodNotFound error.
    ///
    /// This fn checks whole superclass chain.
    ///
    pub(crate) fn find_method_checked(
        &mut self,
        obj: Value,
        func_name: IdentId,
        args_len: usize,
    ) -> Option<FuncId> {
        let func_id = match self.find_method(obj, func_name) {
            Some(id) => id,
            None => {
                self.err_method_not_found(func_name, obj);
                return None;
            }
        };
        self.check_arg(func_id, args_len)?;
        Some(func_id)
    }

    ///
    /// Find method *name* of class with *class_id*.
    ///
    /// This fn checks whole superclass chain.
    ///
    pub(crate) fn find_method_for_class(
        &mut self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<FuncId> {
        #[cfg(feature = "log-jit")]
        {
            match self.method_cache_stats.get_mut(&(class_id, name)) {
                Some(c) => *c = *c + 1,
                None => {
                    self.method_cache_stats.insert((class_id, name), 1);
                }
            };
        }
        let class_version = self.class_version();
        if let Some((version, func)) = self.global_method_cache.get(&(name, class_id)) {
            if *version == class_version {
                return *func;
            }
        };
        let func_id = self.find_method_main(class_id, name);
        self.global_method_cache
            .insert((name, class_id), (class_version, func_id));
        func_id
    }

    fn find_method_main(&mut self, mut class_id: ClassId, name: IdentId) -> Option<FuncId> {
        //eprintln!("find_method: {}", IdentId::get_name(name));
        //eprintln!("class: {}", class_id.get_obj(self).as_val().to_s(self));
        if let Some(func_id) = self.get_method(class_id, name) {
            return Some(func_id);
        }
        let mut class_obj = class_id.get_obj(self);
        while let Some(super_class) = class_obj.superclass() {
            class_obj = super_class;
            class_id = class_obj.class_id();
            //eprintln!("class: {}", class_id.get_obj(self).as_val().to_s(self));
            if let Some(func_id) = self.get_method(class_id, name) {
                return Some(func_id);
            }
        }
        None
    }

    ///
    /// Get a method with *name* in the class of *class_id*.
    ///   
    /// If not found, simply return None with no error.
    ///
    fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self.class[class_id].methods.get(&name).cloned()
    }

    ///
    /// Get method names in the class of *class_id*.
    ///  
    pub(crate) fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.class[class_id].methods.keys().cloned().collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ClassInfo {
    /// the constant name which this class object is bound.
    name: Option<IdentId>,
    /// corresponding class object.
    object: Option<Module>,
    /// method table.
    methods: HashMap<IdentId, FuncId>,
    /// constants table.
    constants: HashMap<IdentId, Value>,
    /// instance variable table.
    ivar_names: HashMap<IdentId, IvarId>,
}

impl GC<RValue> for ClassInfo {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(v) = self.object {
            v.as_val().mark(alloc);
        }
        self.constants.values().for_each(|v| v.mark(alloc));
    }
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            name: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            ivar_names: HashMap::default(),
        }
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

impl GC<RValue> for ClassStore {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.classes.iter().for_each(|info| info.mark(alloc));
    }
}

impl ClassStore {
    pub(crate) fn new() -> Self {
        Self {
            classes: vec![ClassInfo::new(); 20],
        }
    }

    fn add_class(&mut self) -> ClassId {
        let id = self.classes.len();
        self.classes.push(ClassInfo::new());
        ClassId(id as u32)
    }

    fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
    }
}
