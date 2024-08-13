use std::num::NonZeroU32;

use super::*;

mod constants;
mod instance_var;
pub(crate) use instance_var::*;

pub const BASIC_OBJECT_CLASS: ClassId = ClassId::new(1);
pub const OBJECT_CLASS: ClassId = ClassId::new(2);
pub const CLASS_CLASS: ClassId = ClassId::new(3);
pub const NIL_CLASS: ClassId = ClassId::new(4);
pub const TRUE_CLASS: ClassId = ClassId::new(5);
pub const FALSE_CLASS: ClassId = ClassId::new(6);
pub const INTEGER_CLASS: ClassId = ClassId::new(7);
pub const FLOAT_CLASS: ClassId = ClassId::new(8);
pub const STRING_CLASS: ClassId = ClassId::new(9);
pub const SYMBOL_CLASS: ClassId = ClassId::new(10);
pub const TIME_CLASS: ClassId = ClassId::new(11);
pub const ARRAY_CLASS: ClassId = ClassId::new(12);
pub const RANGE_CLASS: ClassId = ClassId::new(13);
pub const PROC_CLASS: ClassId = ClassId::new(14);
pub const HASH_CLASS: ClassId = ClassId::new(15);
pub const REGEXP_CLASS: ClassId = ClassId::new(16);
pub const MODULE_CLASS: ClassId = ClassId::new(17);
pub const IO_CLASS: ClassId = ClassId::new(18);
pub const EXCEPTION_CLASS: ClassId = ClassId::new(19);
pub const STRUCT_CLASS: ClassId = ClassId::new(20);
pub const METHOD_CLASS: ClassId = ClassId::new(21);
pub const FIBER_CLASS: ClassId = ClassId::new(22);
pub const ENUMERATOR_CLASS: ClassId = ClassId::new(23);
pub const GENERATOR_CLASS: ClassId = ClassId::new(24);
pub const COMPLEX_CLASS: ClassId = ClassId::new(25);
pub const NUMERIC_CLASS: ClassId = ClassId::new(26);
pub const BINDING_CLASS: ClassId = ClassId::new(27);
pub const UMETHOD_CLASS: ClassId = ClassId::new(28);
pub const FILE_CLASS: ClassId = ClassId::new(29);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ClassId(NonZeroU32);

impl std::fmt::Debug for ClassId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.u32() {
            1 => write!(f, "BASICOBJECT"),
            2 => write!(f, "OBJECT"),
            3 => write!(f, "CLASS"),
            4 => write!(f, "NIL"),
            5 => write!(f, "TRUE"),
            6 => write!(f, "FALSE"),
            7 => write!(f, "INTEGER"),
            8 => write!(f, "FLOAT"),
            9 => write!(f, "STRING"),
            10 => write!(f, "SYMBOL"),
            11 => write!(f, "TIME"),
            12 => write!(f, "ARRAY"),
            13 => write!(f, "RANGE"),
            14 => write!(f, "PROC"),
            15 => write!(f, "HASH"),
            16 => write!(f, "REGEXP"),
            17 => write!(f, "MODULE"),
            18 => write!(f, "IO"),
            19 => write!(f, "EXCEPTION"),
            20 => write!(f, "STRUCT"),
            21 => write!(f, "METHOD"),
            22 => write!(f, "FIBER"),
            23 => write!(f, "ENUMERATOR"),
            24 => write!(f, "GENERATOR"),
            25 => write!(f, "COMPLEX"),
            26 => write!(f, "NUMERIC"),
            27 => write!(f, "BINDING"),
            28 => write!(f, "UMETHOD"),
            29 => write!(f, "FILE"),
            n => write!(f, "ClassId({n})"),
        }
    }
}

impl From<ClassId> for u32 {
    fn from(val: ClassId) -> Self {
        val.0.get()
    }
}

impl ClassId {
    pub const fn new(id: u32) -> Self {
        Self(NonZeroU32::new(id).unwrap())
    }

    pub const fn from(id: u32) -> Option<Self> {
        if id == 0 {
            None
        } else {
            Some(Self::new(id))
        }
    }

    pub fn u32(&self) -> u32 {
        self.0.get()
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

    /// Get class name(IdentId) of *ClassId*.
    pub(crate) fn get_name_id(self, globals: &Globals) -> IdentId {
        let class = globals.store.classes.get_module(self);
        match globals.store.classes[self].name {
            Some(id) => id,
            None => IdentId::get_id_from_string(match class.is_singleton() {
                None => format!("#<Class:{:016x}>", class.as_val().id()),
                Some(base) => format!("#<Class:{}>", base.to_s(globals)),
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ConstState {
    Loaded(Value),
    Autoload(std::path::PathBuf),
}

#[derive(Debug, Clone)]
pub(crate) struct ClassInfo {
    ///
    /// the constant name which this class object is bound.
    /// if this class object is not bound to any constant, this is None.
    ///
    name: Option<IdentId>,
    ///
    /// the parent class of this class.
    ///
    parent: Option<ClassId>,
    ///
    /// corresponding class object.
    ///
    object: Option<Module>,
    ///
    /// method table.
    ///
    methods: HashMap<IdentId, MethodTableEntry>,
    ///
    /// constants table.
    ///
    constants: HashMap<IdentId, ConstState>,
    ///
    /// class variable table.
    ///
    class_variables: Option<HashMap<IdentId, Value>>,
    ///
    /// instance variable table.
    ///
    ivar_names: HashMap<IdentId, IvarId>,
}

impl alloc::GC<RValue> for ClassInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = self.object {
            v.as_val().mark(alloc);
        }
        self.constants.values().for_each(|v| {
            if let ConstState::Loaded(v) = v {
                v.mark(alloc)
            }
        });
        if let Some(cv) = &self.class_variables {
            cv.values().for_each(|v| v.mark(alloc));
        }
    }
}

impl ClassInfo {
    pub(super) fn new() -> Self {
        Self {
            name: None,
            parent: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            class_variables: None,
            ivar_names: HashMap::default(),
        }
    }

    pub(super) fn copy(&self) -> Self {
        Self {
            name: None,
            parent: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            class_variables: None,
            ivar_names: self.ivar_names.clone(),
        }
    }

    pub(crate) fn get_module(&self) -> Module {
        self.object.unwrap()
    }

    pub(crate) fn get_ivarid(&self, name: IdentId) -> Option<IvarId> {
        self.ivar_names.get(&name).cloned()
    }

    pub(crate) fn ivar_names(&self) -> impl Iterator<Item = (&IdentId, &IvarId)> {
        self.ivar_names.iter()
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

    fn set_cvar(&mut self, name: IdentId, val: Value) {
        if let Some(cv) = &mut self.class_variables {
            cv.insert(name, val);
        } else {
            let mut cv = HashMap::default();
            cv.insert(name, val);
            self.class_variables = Some(cv);
        }
    }

    fn get_cvar(&self, name: IdentId) -> Option<Value> {
        self.class_variables.as_ref()?.get(&name).cloned()
    }
}

impl ClassInfoTable {
    fn get_module(&self, class_id: ClassId) -> Module {
        self[class_id].object.unwrap()
    }

    pub fn object_class(&self) -> Module {
        self.get_module(OBJECT_CLASS)
    }

    fn new_singleton_class(
        &mut self,
        super_class: impl Into<Option<Module>>,
        base: Value,
        original_class: ClassId,
    ) -> Module {
        let id = self.copy_class(original_class);
        let class_obj = Value::singleton_class_empty(id, super_class.into(), base).as_class();
        self[id].object = Some(class_obj);
        class_obj
    }

    // TODO: we must name the unnamed class when the class object is assigned to constant later.
    pub(crate) fn new_unnamed_class(&mut self, superclass: Option<Module>) -> Value {
        let class_id = self.add_class();
        let superclass = match superclass {
            Some(class) => class,
            None => self.object_class(),
        };
        let class_obj = Value::class_empty(class_id, Some(superclass));
        self[class_id].object = Some(class_obj.as_class());
        class_obj
    }

    pub(crate) fn get_parents(&self, mut class: ClassId) -> Vec<IdentId> {
        let mut parents = vec![self[class].name.unwrap()];
        while let Some(parent) = self[class].parent {
            if parent == OBJECT_CLASS {
                break;
            }
            parents.push(self[parent].name.unwrap());
            class = parent;
        }
        parents
    }

    ///
    /// Get a metaclass of *original*: ClassId.
    ///
    /// If not exists, create a new metaclass.
    ///
    pub(crate) fn get_metaclass(&mut self, original: ClassId) -> Module {
        let original_obj = self.get_module(original);
        if original_obj.as_val().ty() == Some(ObjKind::CLASS) {
            let class = self[original_obj.as_val().class()].get_module();
            if original_obj.is_singleton().is_none() && class.is_singleton().is_some() {
                return class;
            }
            let super_singleton = match original_obj.superclass_id() {
                Some(id) => self.get_metaclass(id),
                None => self.get_module(CLASS_CLASS),
            };
            let mut original_obj = original_obj.as_val();
            let mut singleton = self.new_singleton_class(super_singleton, original_obj, class.id());
            original_obj.change_class(singleton.id());
            let class_class = if class.id() == original {
                singleton.id()
            } else {
                self.get_module(class.id()).id()
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
        let org_class = self[obj.class()].get_module();
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

    ///
    /// Get a method with *name* in the class of *class_id*.
    ///   
    /// If not found, simply return None with no error.
    ///
    fn get_method(&self, class_id: ClassId, name: IdentId) -> Option<&MethodTableEntry> {
        self[class_id].methods.get(&name)
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    /// This fn checks whole superclass chain everytime called.
    ///
    fn search_method(&self, mut module: Module, name: IdentId) -> Option<MethodTableEntry> {
        let mut visi = None;
        loop {
            if !module.has_origin()
                && let Some(entry) = self.get_method(module.id(), name)
            {
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

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    pub(crate) fn check_super(
        &mut self,
        self_val: Value,
        class_context: ClassId,
        name: IdentId,
    ) -> Option<FuncId> {
        let class_id = self_val.class();

        let mut module = self.get_module(class_id);
        loop {
            if module.id() == class_context {
                module = module.superclass().unwrap();
                break;
            }
            module = module.superclass().unwrap();
        }
        let MethodTableEntry { func_id, .. } = self.search_method(module, name)?;
        func_id
    }

    ///
    /// Get method names in the class of *class_id*.
    ///  
    pub(crate) fn get_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].methods.keys().cloned().collect()
    }

    pub(crate) fn get_method_names_inherit(&self, mut class_id: ClassId) -> Vec<IdentId> {
        let mut names = vec![];
        loop {
            names.extend(self[class_id].methods.keys().cloned());
            match self.get_module(class_id).superclass_id() {
                Some(superclass) => {
                    if superclass == OBJECT_CLASS {
                        break;
                    }
                    class_id = superclass;
                }
                None => break,
            }
        }
        names
    }

    pub(crate) fn define_class(
        &mut self,
        name_id: IdentId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
        is_module: bool,
    ) -> Module {
        let class_id = self.add_class();
        let obj = self.generate_class_obj(name_id, class_id, superclass.into(), parent, is_module);
        self.get_metaclass(class_id);
        obj
    }

    fn define_builtin_class(
        &mut self,
        name_id: IdentId,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        self.def_builtin_class(class_id);
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
        self[class_id].object = Some(class_obj.as_class());
        self[class_id].name = Some(name_id);
        self[class_id].parent = Some(parent);
        self.set_constant(parent, name_id, class_obj);
        class_obj.as_class()
    }
}

impl Globals {
    pub(crate) fn define_module(&mut self, name: &str) -> Module {
        let object_class = self.store.classes.object_class();
        let name_id = IdentId::get_id(name);
        self.store
            .classes
            .define_class(name_id, Some(object_class), OBJECT_CLASS, true)
    }

    pub(crate) fn define_builtin_class_under_obj(
        &mut self,
        name: &str,
        class_id: ClassId,
    ) -> Module {
        let object_class = self.store.classes.object_class();
        self.define_builtin_class_by_str(name, class_id, Some(object_class), OBJECT_CLASS)
    }

    pub(crate) fn define_class_by_str(
        &mut self,
        name: &str,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.store
            .classes
            .define_class(name_id, superclass, parent, false)
    }

    pub(crate) fn define_class_under_obj(&mut self, name: &str) -> Module {
        let name_id = IdentId::get_id(name);
        let object_class = self.store.classes.object_class();
        self.store
            .classes
            .define_class(name_id, Some(object_class), OBJECT_CLASS, false)
    }

    pub(crate) fn define_builtin_class_by_str(
        &mut self,
        name: &str,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.store
            .classes
            .define_builtin_class(name_id, class_id, superclass, parent)
    }

    pub fn include_module(&mut self, mut base: Module, module: Module) -> Result<()> {
        self.class_version_inc();
        base.include_module(module)
    }

    pub fn prepend_module(&mut self, mut base: Module, module: Module) -> Result<()> {
        self.class_version_inc();
        base.prepend_module(module)
    }

    ///
    /// Add a new public method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_public_method(&mut self, class_id: ClassId, name: IdentId, func_id: FuncId) {
        self.add_method(class_id, name, func_id, Visibility::Public)
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
        self.add_method_inner(class_id, name, func_id, visibility, false)
    }

    ///
    /// Add a new basic operation method *func* with *name* to the class of *class_id*.
    ///
    pub(crate) fn add_basic_op_method(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
    ) {
        self.add_method_inner(class_id, name, func_id, visibility, true)
    }

    ///
    /// Add a new method *func* with *name* to the class of *class_id*.
    ///
    fn add_method_inner(
        &mut self,
        owner: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
        is_basic_op: bool,
    ) {
        self.store[func_id].set_owner_class(owner);
        self.insert_method(
            owner,
            name,
            MethodTableEntry {
                owner,
                func_id: Some(func_id),
                visibility,
                is_basic_op,
            },
        );
        #[cfg(feature = "perf")]
        {
            let info = self.store[func_id].get_wrapper_info();
            let desc = self.func_description(func_id);
            self.codegen.perf_write(info, &desc);
        }
    }

    pub(crate) fn add_empty_method(
        &mut self,
        owner: ClassId,
        name: IdentId,
        visibility: Visibility,
    ) {
        self.insert_method(
            owner,
            name,
            MethodTableEntry {
                owner,
                func_id: None,
                visibility,
                is_basic_op: false,
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
        #[cfg(feature = "perf")]
        {
            let info = self.store[func_id].get_wrapper_info();
            let desc = self.func_description(func_id);
            self.codegen.perf_write(info, &desc);
        }
        let singleton = self.store.classes.get_metaclass(class_id).id();
        self.store[func_id].set_owner_class(class_id);
        self.insert_method(
            singleton,
            name,
            MethodTableEntry {
                owner: singleton,
                func_id: Some(func_id),
                visibility,
                is_basic_op: false,
            },
        );
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub fn find_method(
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
                            return Err(MonorubyErr::private_method_called(func_name, recv));
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
                match entry.func_id() {
                    Some(func_id) => Ok(func_id),
                    None => Err(MonorubyErr::method_not_found(func_name, recv)),
                }
            }
            None => Err(MonorubyErr::method_not_found(func_name, recv)),
        }
    }

    ///
    /// remove method.
    ///
    pub(crate) fn remove_method(&mut self, class_id: ClassId, func_name: IdentId) -> Option<()> {
        self.store.classes[class_id].methods.remove(&func_name)?;
        Some(())
    }

    ///
    /// Check whether public/protected method *name* is defined for *class_id* or its superclasses.
    ///
    pub(crate) fn method_defined(
        &mut self,
        class_id: ClassId,
        func_name: IdentId,
        inherit: bool,
    ) -> Option<Visibility> {
        Some(if inherit {
            self.check_method_for_class(class_id, func_name)?.visibility
        } else {
            self.store
                .classes
                .get_method(class_id, func_name)?
                .visibility
        })
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub(crate) fn find_method_entry_for_class(
        &mut self,
        class: ClassId,
        func_name: IdentId,
    ) -> Result<MethodTableEntry> {
        match self.check_method_for_class(class, func_name) {
            Some(entry) => Ok(entry),
            None => Err(MonorubyErr::method_not_found_for_class(func_name, class)),
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
    pub(crate) fn check_method(&mut self, obj: Value, name: IdentId) -> Option<FuncId> {
        let class_id = obj.class();
        self.check_method_for_class(class_id, name)?.func_id()
    }

    ///
    /// Check whether a public method *name* for object *obj* exists.
    ///
    pub(crate) fn check_public_method(&mut self, obj: Value, name: IdentId) -> Option<FuncId> {
        let class_id = obj.class();
        let entry = self.check_method_for_class(class_id, name)?;
        if entry.visibility == Visibility::Public {
            entry.func_id()
        } else {
            None
        }
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    pub(crate) fn check_method_for_class(
        &mut self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        #[cfg(feature = "profile")]
        {
            match self.global_method_cache_stats.get_mut(&(class_id, name)) {
                Some(c) => *c += 1,
                None => {
                    self.global_method_cache_stats.insert((class_id, name), 1);
                }
            };
        }
        let class_version = self.class_version();
        if let Some(entry) = self.global_method_cache.get(class_id, name, class_version) {
            return entry.cloned();
        };
        #[cfg(feature = "profile")]
        {
            match self.method_exploration_stats.get_mut(&(class_id, name)) {
                Some(c) => *c += 1,
                None => {
                    self.method_exploration_stats.insert((class_id, name), 1);
                }
            };
        }
        let entry = self
            .store
            .classes
            .search_method(self.store.classes.get_module(class_id), name);
        self.global_method_cache
            .insert((name, class_id), class_version, entry.clone());
        entry
    }

    pub(crate) fn change_method_visibility_for_class(
        &mut self,
        class_id: ClassId,
        names: &[IdentId],
        visi: Visibility,
    ) -> Result<()> {
        for name in names {
            self.find_method_entry_for_class(class_id, *name)?;
            match self.store.classes[class_id].methods.get_mut(name) {
                Some(entry) => {
                    entry.visibility = visi;
                }
                None => {
                    self.add_empty_method(class_id, *name, visi);
                }
            };
        }
        self.class_version_inc();
        Ok(())
    }

    ///
    /// If the re-defined method is "basic operation", return true.
    ///
    fn insert_method(&mut self, class_id: ClassId, name: IdentId, entry: MethodTableEntry) {
        if let Some(old) = self.store.classes[class_id].methods.insert(name, entry)
            && old.is_basic_op
        {
            self.set_bop_redefine();
        }
    }

    fn set_bop_redefine(&mut self) {
        self.codegen.set_bop_redefine();
        self.store.functions.invalidate_jit_code();
        let vm_entry = self.codegen.vm_entry;
        for func in self.store.functions.functions() {
            match func.kind {
                FuncKind::ISeq(_) => {
                    let entry = func.entry_label();
                    self.codegen.jit.apply_jmp_patch(entry, vm_entry);
                }
                _ => {}
            }
        }
    }

    #[cfg(feature = "profile")]
    pub(crate) fn jit_class_guard_failed(&mut self, func_id: FuncId, class_id: ClassId) {
        {
            match self.jit_class_unmatched_stats.get_mut(&(func_id, class_id)) {
                Some(c) => *c += 1,
                None => {
                    self.jit_class_unmatched_stats
                        .insert((func_id, class_id), 1);
                }
            };
        }
    }
}

#[derive(Default)]
pub(in crate::globals) struct GlobalMethodCache {
    version: u32,
    cache: HashMap<(IdentId, ClassId), Option<MethodTableEntry>>,
}

impl GlobalMethodCache {
    fn get(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        class_version: u32,
    ) -> Option<Option<&MethodTableEntry>> {
        if self.version != class_version {
            self.cache.clear();
            self.version = class_version;
            return None;
        }
        self.cache.get(&(name, class_id)).map(|e| e.as_ref())
    }

    fn insert(
        &mut self,
        key: (IdentId, ClassId),
        class_version: u32,
        entry: Option<MethodTableEntry>,
    ) {
        self.version = class_version;
        self.cache.insert(key, entry);
    }
}
