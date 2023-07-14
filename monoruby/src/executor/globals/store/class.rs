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
pub const IO_CLASS: ClassId = ClassId::new(17);
pub const EXCEPTION_CLASS: ClassId = ClassId::new(18);
pub const STRUCT_CLASS: ClassId = ClassId::new(19);
pub const METHOD_CLASS: ClassId = ClassId::new(20);
pub const FIBER_CLASS: ClassId = ClassId::new(21);
pub const ENUMERATOR_CLASS: ClassId = ClassId::new(22);

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash)]
#[repr(transparent)]
pub struct ClassId(pub u32);

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
            17 => write!(f, "IO"),
            18 => write!(f, "EXCEPTION"),
            19 => write!(f, "STRUCT"),
            20 => write!(f, "METHOD"),
            21 => write!(f, "FIBER"),
            22 => write!(f, "ENUMERATOR"),
            n => write!(f, "ClassId({n})"),
        }
    }
}

impl From<ClassId> for u32 {
    fn from(val: ClassId) -> Self {
        val.0
    }
}

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

    /// Get *Module* for *ClassId*.
    pub(crate) fn get_module(self, globals: &Globals) -> Module {
        globals.get_class_obj(self)
    }

    /// Get Class object for *ClassId*.
    pub(crate) fn get_obj(self, globals: &Globals) -> Value {
        self.get_module(globals).as_val()
    }

    /// Get class name of *ClassId*.
    pub(crate) fn get_name(self, globals: &Globals) -> String {
        if self.0 == 0 {
            return "<INVALID>".to_string();
        }
        let class = self.get_module(globals);
        match globals.store[self].name {
            Some(id) => id.to_string(),
            None => match class.is_singleton() {
                None => format!("#<Class:{:016x}>", class.as_val().get()),
                Some(base) => format!("#<Class:{}>", globals.to_s(base)),
            },
        }
    }

    /// Get class name(IdentId) of *ClassId*.
    pub(crate) fn get_name_id(self, globals: &Globals) -> Option<IdentId> {
        if self.0 == 0 {
            return None;
        }
        let class = self.get_module(globals);
        match globals.store[self].name {
            Some(id) => Some(id),
            None => Some(IdentId::get_id_from_string(match class.is_singleton() {
                None => format!("#<Class:{:016x}>", class.as_val().get()),
                Some(base) => format!("#<Class:{}>", globals.to_s(base)),
            })),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ClassInfo {
    /// the constant name which this class object is bound.
    name: Option<IdentId>,
    /// corresponding class object.
    object: Option<Module>,
    /// method table.
    methods: HashMap<IdentId, MethodTableEntry>,
    /// constants table.
    constants: HashMap<IdentId, Value>,
    /// instance variable table.
    ivar_names: HashMap<IdentId, IvarId>,
}

impl alloc::GC<RValue> for ClassInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = self.object {
            v.as_val().mark(alloc);
        }
        self.constants.values().for_each(|v| v.mark(alloc));
    }
}

impl ClassInfo {
    pub(super) fn new() -> Self {
        Self {
            name: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            ivar_names: HashMap::default(),
        }
    }

    pub(super) fn copy(&self) -> Self {
        Self {
            name: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            ivar_names: self.ivar_names.clone(),
        }
    }

    pub(crate) fn get_ivarid(&self, name: IdentId) -> Option<IvarId> {
        self.ivar_names.get(&name).cloned()
    }

    pub(crate) fn set_name(&mut self, name: &str) {
        self.name = Some(IdentId::get_id(name));
    }

    pub(crate) fn set_name_id(&mut self, name: IdentId) {
        self.name = Some(name);
    }

    pub(crate) fn get_name_id(&self) -> Option<IdentId> {
        self.name
    }
}

impl Globals {
    fn get_class_obj(&self, class_id: ClassId) -> Module {
        self.store[class_id].object.unwrap()
    }

    pub fn object_class(&self) -> Module {
        self.get_class_obj(OBJECT_CLASS)
    }

    pub(crate) fn define_module(&mut self, name: &str) -> Module {
        let object_class = self.object_class();
        let name_id = IdentId::get_id(name);
        self.define_class(name_id, Some(object_class), OBJECT_CLASS, true)
    }

    pub(in crate::executor) fn define_builtin_class_under_obj(
        &mut self,
        name: &str,
        class_id: ClassId,
    ) -> Module {
        let object_class = self.object_class();
        self.define_builtin_class_by_str(name, class_id, Some(object_class), OBJECT_CLASS)
    }

    pub(crate) fn define_class_by_str(
        &mut self,
        name: &str,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.define_class(name_id, superclass, parent, false)
    }

    pub(crate) fn define_class_under_obj(&mut self, name: &str) -> Module {
        let name_id = IdentId::get_id(name);
        let object_class = self.object_class();
        self.define_class(name_id, Some(object_class), OBJECT_CLASS, false)
    }

    pub(in crate::executor) fn define_builtin_class_by_str(
        &mut self,
        name: &str,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.define_builtin_class(name_id, class_id, superclass, parent)
    }

    pub(crate) fn define_class(
        &mut self,
        name_id: IdentId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
        is_module: bool,
    ) -> Module {
        let class_id = self.store.add_class();
        let obj = self.generate_class_obj(name_id, class_id, superclass.into(), parent, is_module);
        self.get_metaclass(class_id);
        obj
    }

    // TODO: we must name the unnamed class when the class object is assigned to constant later.
    pub(crate) fn new_unnamed_class(&mut self, superclass: Option<Module>) -> Value {
        let class_id = self.store.add_class();
        let superclass = match superclass {
            Some(class) => class,
            None => self.object_class(),
        };
        let class_obj = Value::class_empty(class_id, Some(superclass));
        self.store[class_id].object = Some(class_obj.as_class());
        class_obj
    }

    fn define_builtin_class(
        &mut self,
        name_id: IdentId,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        self.store.def_builtin_class(class_id);
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
            Value::module_empty(class_id, superclass)
        } else {
            Value::class_empty(class_id, superclass)
        };
        self.store[class_id].object = Some(class_obj.as_class());
        self.store[class_id].name = Some(name_id);
        self.set_constant(parent, name_id, class_obj);
        class_obj.as_class()
    }

    fn new_singleton_class(
        &mut self,
        super_class: impl Into<Option<Module>>,
        base: Value,
        original_class: ClassId,
    ) -> Module {
        let id = self.store.copy_class(original_class);
        let class_obj = Value::singleton_class_empty(id, super_class.into(), base).as_class();
        self.store[id].object = Some(class_obj);
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
            let super_singleton = match original_obj.superclass_id() {
                Some(id) => self.get_metaclass(id),
                None => CLASS_CLASS.get_module(self),
            };
            let mut original_obj = original_obj.as_val();
            let mut singleton = self.new_singleton_class(super_singleton, original_obj, class.id());
            original_obj.change_class(singleton.id());
            let class_class = if class.id() == original {
                singleton.id()
            } else {
                self.get_class_obj(class.id()).id()
            };
            singleton.change_class(class_class);
            #[cfg(debug_assertions)]
            {
                assert_eq!(singleton.id(), original_obj.class());
                assert_eq!(Some(original_obj), singleton.is_singleton());
            }
            singleton
        } else {
            self.get_singleton(original_obj.as_val())
        }
    }

    ///
    /// Get a singleton class of *obj*: Value.
    ///
    /// If not exists, create a new singleton class.
    ///
    pub(crate) fn get_singleton(&mut self, mut obj: Value) -> Module {
        let org_class = obj.get_class_obj(self);
        if org_class.is_singleton().is_some() {
            return org_class;
        }
        let mut singleton = self.new_singleton_class(org_class, obj, org_class.id());
        obj.change_class(singleton.id());
        let singleton_meta = org_class.get_real_class().as_val().class();
        singleton.change_class(singleton_meta);
        #[cfg(debug_assertions)]
        {
            assert_eq!(singleton.id(), obj.class());
        }
        singleton
    }

    pub(crate) fn include_module(&mut self, mut class: Module, module: Module) {
        let module = module.make_iclass(class.superclass());
        class.change_superclass(module);
    }

    pub(crate) fn get_error_class(&self, err: &MonorubyErr) -> ClassId {
        let name = err.get_class_name();
        self.get_constant(OBJECT_CLASS, IdentId::get_id(name))
            .expect(&format!("{name}"))
            .as_class_id()
    }

    ///
    /// Add a new method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_method(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
    ) {
        self.store[class_id].methods.insert(
            name,
            MethodTableEntry {
                owner: class_id,
                func_id: Some(func_id),
                visibility,
            },
        );
    }

    pub(crate) fn add_empty_method(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        visibility: Visibility,
    ) {
        self.store[class_id].methods.insert(
            name,
            MethodTableEntry {
                owner: class_id,
                func_id: None,
                visibility,
            },
        );
    }

    ///
    /// Add a new singleton method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_singleton_method(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
    ) {
        let singleton = self.get_metaclass(class_id).id();
        self.store[singleton].methods.insert(
            name,
            MethodTableEntry {
                owner: singleton,
                func_id: Some(func_id),
                visibility,
            },
        );
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub(in crate::executor) fn find_method(
        &mut self,
        recv: Value,
        func_name: IdentId,
        is_func_call: bool,
    ) -> Result<FuncId> {
        let class_id = recv.class();
        match self.check_method_for_class(class_id, func_name) {
            Some(entry) => {
                match entry.visibility {
                    Visibility::Private => {
                        if !is_func_call {
                            return Err(MonorubyErr::private_method_called(self, func_name, recv));
                        }
                    }
                    Visibility::Protected => {
                        //if !is_func_call {
                        //    self.err_protected_method_called(func_name, obj);
                        //    return None;
                        //}
                    }
                    _ => {}
                }
                Ok(entry.func_id())
            }
            None => Err(MonorubyErr::method_not_found(self, func_name, recv)),
        }
    }

    ///
    /// Check whether public/protected method *name* is defined for *class_id* or its superclasses.
    ///
    pub(in crate::executor) fn method_defined(
        &mut self,
        class_id: ClassId,
        func_name: IdentId,
    ) -> bool {
        match self.check_method_for_class(class_id, func_name) {
            Some(entry) => match entry.visibility {
                Visibility::Private => false,
                _ => true,
            },
            None => false,
        }
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub(in crate::executor) fn find_method_entry_for_class(
        &mut self,
        class: ClassId,
        func_name: IdentId,
    ) -> Result<MethodTableEntry> {
        match self.check_method_for_class(class, func_name) {
            Some(entry) => Ok(entry),
            None => Err(MonorubyErr::method_not_found_for_class(
                self, func_name, class,
            )),
        }
    }

    /*pub fn find_super(&mut self, self_val: Value, func_name: IdentId) -> Option<MethodTableEntry> {
        match self.check_super(self_val, func_name) {
            Some(entry) => Some(entry),
            None => {
                self.err_method_not_found(func_name, self_val);
                None
            }
        }
    }*/

    ///
    /// Check whether a method *name* for object *obj* exists.
    ///
    pub(in crate::executor) fn check_method(
        &mut self,
        obj: Value,
        name: IdentId,
    ) -> Option<FuncId> {
        let class_id = obj.class();
        self.check_method_for_class(class_id, name)
            .map(|e| e.func_id())
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    pub(in crate::executor) fn check_super(
        &mut self,
        self_val: Value,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let class_id = self_val.class();
        let MethodTableEntry { owner, .. } = self.check_method_for_class(class_id, name)?;
        let superclass = owner.get_module(self).superclass_id()?;
        self.check_method_for_class(superclass, name)
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    pub(in crate::executor) fn check_method_for_class(
        &mut self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        #[cfg(feature = "log-jit")]
        {
            match self.method_cache_stats.get_mut(&(class_id, name)) {
                Some(c) => *c += 1,
                None => {
                    self.method_cache_stats.insert((class_id, name), 1);
                }
            };
        }
        let class_version = self.class_version();
        if let Some((version, entry)) = self.global_method_cache.get(&(name, class_id)) {
            if *version == class_version {
                return entry.clone();
            }
        };
        let entry = self.search_method(class_id, name);
        self.global_method_cache
            .insert((name, class_id), (class_version, entry.clone()));
        entry
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    /// This fn checks whole superclass chain everytime called.
    ///
    fn search_method(&mut self, class_id: ClassId, name: IdentId) -> Option<MethodTableEntry> {
        let mut visi = None;
        let mut module = class_id.get_module(self);
        loop {
            if let Some(entry) = self.get_method(module.id(), name) {
                if entry.func_id.is_some() {
                    let visibility = if let Some(visi) = visi {
                        visi
                    } else {
                        entry.visibility
                    };
                    return Some(MethodTableEntry {
                        visibility,
                        ..entry.clone()
                    });
                } else if visi.is_none() {
                    visi = Some(entry.visibility);
                }
            }
            module = module.superclass()?;
        }
    }

    pub(in crate::executor) fn change_method_visibility_for_class(
        &mut self,
        class_id: ClassId,
        names: &[IdentId],
        visi: Visibility,
    ) {
        for name in names {
            match self.store[class_id].methods.get_mut(name) {
                Some(entry) => {
                    entry.visibility = visi;
                }
                None => {
                    self.add_empty_method(class_id, *name, visi);
                }
            };
        }
        self.class_version_inc();
    }

    ///
    /// Get method names in the class of *class_id*.
    ///  
    pub(crate) fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.store[class_id].methods.keys().cloned().collect()
    }

    ///
    /// Get a method with *name* in the class of *class_id*.
    ///   
    /// If not found, simply return None with no error.
    ///
    fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<&MethodTableEntry> {
        self.store[class_id].methods.get(&name)
    }
}
