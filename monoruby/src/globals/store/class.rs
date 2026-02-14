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
pub const FILE_CLASS: ClassId = ClassId::new(19);
pub const STRUCT_CLASS: ClassId = ClassId::new(20);
pub const METHOD_CLASS: ClassId = ClassId::new(21);
pub const FIBER_CLASS: ClassId = ClassId::new(22);
pub const ENUMERATOR_CLASS: ClassId = ClassId::new(23);
pub const GENERATOR_CLASS: ClassId = ClassId::new(24);
pub const COMPLEX_CLASS: ClassId = ClassId::new(25);
pub const NUMERIC_CLASS: ClassId = ClassId::new(26);
pub const BINDING_CLASS: ClassId = ClassId::new(27);
pub const UMETHOD_CLASS: ClassId = ClassId::new(28);

pub const EXCEPTION_CLASS: ClassId = ClassId::new(29);
pub const NO_METHOD_ERROR_CLASS: ClassId = ClassId::new(30);
pub const ARGUMENTS_ERROR_CLASS: ClassId = ClassId::new(31);
pub const SYNTAX_ERROR_CLASS: ClassId = ClassId::new(32);
pub const UNIMPLEMENTED_ERROR_CLASS: ClassId = ClassId::new(33);
pub const NAME_ERROR_CLASS: ClassId = ClassId::new(34);
pub const ZERO_DIVISION_ERROR_CLASS: ClassId = ClassId::new(35);
pub const LOCAL_JUMP_ERROR_CLASS: ClassId = ClassId::new(36);
pub const RANGE_ERROR_CLASS: ClassId = ClassId::new(37);
pub const TYPE_ERROR_CLASS: ClassId = ClassId::new(38);
pub const INDEX_ERROR_CLASS: ClassId = ClassId::new(39);
pub const FROZEN_ERROR_CLASS: ClassId = ClassId::new(40);
pub const LOAD_ERROR_CLASS: ClassId = ClassId::new(41);
//pub const INTERNAL_ERROR_CLASS: ClassId = ClassId::new(42);
pub const REGEX_ERROR_CLASS: ClassId = ClassId::new(43);
pub const RUNTIME_ERROR_CLASS: ClassId = ClassId::new(44);
pub const KEY_ERROR_CLASS: ClassId = ClassId::new(45);
pub const FIBER_ERROR_CLASS: ClassId = ClassId::new(46);
pub const STOP_ITERATION_CLASS: ClassId = ClassId::new(47);
pub const SYSTEM_EXIT_ERROR_CLASS: ClassId = ClassId::new(48);

pub const MATCHDATA_CLASS: ClassId = ClassId::new(49);

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
            19 => write!(f, "FILE"),
            20 => write!(f, "STRUCT"),
            21 => write!(f, "METHOD"),
            22 => write!(f, "FIBER"),
            23 => write!(f, "ENUMERATOR"),
            24 => write!(f, "GENERATOR"),
            25 => write!(f, "COMPLEX"),
            26 => write!(f, "NUMERIC"),
            27 => write!(f, "BINDING"),
            28 => write!(f, "UMETHOD"),

            29 => write!(f, "EXCEPTION"),
            30 => write!(f, "NO_METHOD_ERROR"),
            31 => write!(f, "ARGUMENTS_ERROR"),
            32 => write!(f, "SYNTAX_ERROR"),
            33 => write!(f, "UNIMPLEMENTED_ERROR"),
            34 => write!(f, "NAME_ERROR"),
            35 => write!(f, "ZERO_DIVISION_ERROR"),
            36 => write!(f, "LOCAL_JUMP_ERROR"),
            37 => write!(f, "RANGE_ERROR"),
            38 => write!(f, "TYPE_ERROR"),
            39 => write!(f, "INDEX_ERROR"),
            40 => write!(f, "FROZEN_ERROR"),
            41 => write!(f, "LOAD_ERROR"),
            42 => write!(f, "INTERNAL_ERROR"),
            43 => write!(f, "REGEX_ERROR"),
            44 => write!(f, "RUNTIME_ERROR"),
            45 => write!(f, "KEY_ERROR"),
            46 => write!(f, "FIBER_ERROR"),
            47 => write!(f, "STOP_ITERATION"),
            48 => write!(f, "SYSTEM_EXIT_ERROR"),

            49 => write!(f, "MATCHDATA"),
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
        if id == 0 { None } else { Some(Self::new(id)) }
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
                | COMPLEX_CLASS
        )
    }

    pub(crate) fn is_nil(&self) -> bool {
        *self == NIL_CLASS
    }

    pub(crate) fn is_falsy(&self) -> bool {
        *self == NIL_CLASS || *self == FALSE_CLASS
    }

    /// Get class name(IdentId) of *ClassId*.
    pub(crate) fn get_name(self, store: &Store) -> String {
        let class = store.classes.get_module(self);
        match &store.classes[self].name {
            Some(id) => id.clone(),
            None => match class.is_singleton() {
                None => format!("#<Class:{:016x}>", class.as_val().id()),
                Some(base) => format!("#<Class:{}>", base.to_s(store)),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ConstState {
    Loaded(Value),
    Autoload(std::path::PathBuf),
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    ///
    /// the constant name which this class object is bound.
    /// if this class object is not bound to any constant, this is None.
    ///
    name: Option<String>,
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
    ///
    /// Object type of instances of this class.
    ///
    /// None for modules.
    ///
    instance_ty: Option<ObjTy>,
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
            instance_ty: None,
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
            instance_ty: self.instance_ty,
        }
    }

    pub(crate) fn get_module(&self) -> Module {
        self.object.unwrap()
    }

    pub(crate) fn get_ivarid(&self, name: IdentId) -> Option<IvarId> {
        self.ivar_names.get(&name).cloned()
    }

    pub(crate) fn ivar_len(&self) -> usize {
        self.ivar_names.len()
    }

    pub(crate) fn ivar_names(&self) -> impl Iterator<Item = (&IdentId, &IvarId)> {
        self.ivar_names.iter()
    }

    pub(crate) fn set_parent(&mut self, parent: ClassId) {
        self.parent = Some(parent);
    }

    pub(crate) fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub(crate) fn get_name(&self) -> Option<&str> {
        self.name.as_ref().map(|x| x.as_str())
    }

    pub(crate) fn instance_ty(&self) -> Option<ObjTy> {
        self.instance_ty
    }

    pub(crate) fn is_object_ty_instance(&self) -> bool {
        self.instance_ty == Some(ObjTy::OBJECT)
    }

    pub(crate) fn is_array_ty_instance(&self) -> bool {
        self.instance_ty == Some(ObjTy::ARRAY)
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

    ///
    /// Remove a constant with *name* in the class of *class_id*.
    ///
    /// If not found, return None.
    ///
    pub(in crate::globals) fn remove_constant(&mut self, name: IdentId) -> Option<Value> {
        self.constants.remove(&name).map(|state| match state {
            ConstState::Loaded(v) => v,
            ConstState::Autoload(..) => Value::nil(),
        })
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

    pub(crate) fn get_parents(&self, mut class: ClassId) -> Vec<String> {
        let mut parents = vec![self[class].name.clone().unwrap()];
        while let Some(parent) = self[class].parent {
            if parent == OBJECT_CLASS {
                break;
            }
            parents.push(self[parent].name.as_ref().unwrap().to_string());
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
        if original_obj.as_val().ty() == Some(ObjTy::CLASS) {
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
    /// Get a singleton class of *obj*: Value.
    ///
    /// If not exists, create a new singleton class.
    ///
    pub(crate) fn has_singleton(&self, obj: Value) -> Option<Module> {
        let org_class = self[obj.class()].get_module();
        if org_class.is_singleton().is_some() {
            Some(org_class)
        } else {
            None
        }
    }

    ///
    /// Get a method with *name* in the class of *class_id*.
    ///   
    /// If not found, simply return None with no error.
    ///
    fn get_method(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<(FuncId, Visibility, ClassId)> {
        let entry = self[class_id].methods.get(&name)?;
        Some((entry.func_id()?, entry.visibility, entry.owner))
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    /// This fn checks whole superclass chain everytime called.
    ///
    pub(super) fn search_method(
        &self,
        mut module: Module,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let mut visi = None;
        loop {
            if !module.has_origin()
                && let Some(entry) = self[module.id()].methods.get(&name)
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
                } else if entry.visibility == Visibility::Undefined {
                    return None;
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
        &self,
        self_class: ClassId,
        owner: ClassId,
        name: IdentId,
    ) -> Option<FuncId> {
        let mut module = self.get_module(self_class);
        loop {
            if !module.has_origin() && module.id() == owner {
                break;
            }
            module = module.superclass().unwrap();
        }
        let module = module.superclass()?;
        self.search_method(module, name)?.func_id()
    }

    ///
    /// Get public and protected method names in the class of *class_id*.
    ///  
    pub(crate) fn get_method_names(&self, class_id: ClassId) -> Vec<Value> {
        self[class_id]
            .methods
            .iter()
            .filter_map(|(name, entry)| {
                if entry.is_public_protected() {
                    Some(Value::symbol(*name))
                } else {
                    None
                }
            })
            .collect()
    }

    ///
    /// Get private method names in the class of *class_id*.
    ///  
    pub(crate) fn get_private_method_names(&self, class_id: ClassId) -> Vec<Value> {
        self[class_id]
            .methods
            .iter()
            .filter_map(|(name, entry)| {
                if entry.is_private() {
                    Some(Value::symbol(*name))
                } else {
                    None
                }
            })
            .collect()
    }

    ///
    /// Get public and protected method names in the class of *class_id* and its ancesters.
    ///
    pub(crate) fn get_method_names_inherit(
        &self,
        class_id: ClassId,
        only_singleton: bool,
    ) -> Vec<Value> {
        let mut names = HashSet::default();
        let mut module = self.get_module(class_id);
        let mut exclude = HashSet::default();
        loop {
            if !only_singleton || module.is_singleton().is_some() || module.is_iclass() {
                for (name, entry) in &self[module.id()].methods {
                    if matches!(
                        entry.visibility,
                        Visibility::Undefined | Visibility::Private
                    ) {
                        exclude.insert(*name);
                    } else if entry.is_public_protected() && !exclude.contains(name) {
                        names.insert(*name);
                    }
                }
            }
            match module.superclass() {
                Some(superclass) => {
                    if superclass.id() == OBJECT_CLASS {
                        break;
                    }
                    module = superclass;
                }
                None => break,
            }
        }
        names.into_iter().map(|sym| Value::symbol(sym)).collect()
    }

    ///
    /// Get public and protected method names in the class of *class_id* and its ancesters.
    ///
    pub(crate) fn get_private_method_names_inherit(&self, class_id: ClassId) -> Vec<Value> {
        let mut names = HashSet::default();
        let mut module = self.get_module(class_id);
        let mut exclude = HashSet::default();
        loop {
            for (name, entry) in &self[module.id()].methods {
                if matches!(entry.visibility, Visibility::Undefined) {
                    exclude.insert(*name);
                } else if entry.is_private() && !exclude.contains(name) {
                    names.insert(*name);
                }
            }
            match module.superclass() {
                Some(superclass) => {
                    if superclass.id() == OBJECT_CLASS {
                        break;
                    }
                    module = superclass;
                }
                None => break,
            }
        }
        names.into_iter().map(|sym| Value::symbol(sym)).collect()
    }

    fn generate_class_obj(
        &mut self,
        name: Option<IdentId>,
        class_id: ClassId,
        superclass: Option<Module>,
        parent: Option<ClassId>,
        is_module: bool,
        instance_ty: Option<ObjTy>,
    ) -> Module {
        let instance_ty = if is_module {
            None
        } else if let Some(ty) = instance_ty {
            Some(ty)
        } else if let Some(superclass) = superclass {
            self[superclass.id()].instance_ty
        } else {
            Some(ObjTy::OBJECT)
        };
        let class_obj = if is_module {
            Value::module_empty(class_id, superclass)
        } else {
            Value::class_empty(class_id, superclass)
        };
        self[class_id].object = Some(class_obj.as_class());
        self[class_id].name = name.map(|id| id.to_string());
        self[class_id].parent = parent;
        self[class_id].instance_ty = instance_ty;
        if let Some(name) = name {
            self.set_constant(parent.unwrap(), name, class_obj);
        }
        class_obj.as_class()
    }
}

impl ClassInfoTable {
    pub(crate) fn define_module_with_identid(
        &mut self,
        name_id: IdentId,
        parent: ClassId,
    ) -> Module {
        let object_class = self.object_class();
        self.define_class_inner(Some(name_id), Some(object_class), Some(parent), true, None)
    }

    pub(crate) fn define_toplevel_module(&mut self, name: &str) -> Module {
        let name_id = IdentId::get_id(name);
        self.define_module_with_identid(name_id, OBJECT_CLASS)
    }

    pub(crate) fn define_class_with_identid(
        &mut self,
        name_id: IdentId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        self.define_class_inner(Some(name_id), superclass, Some(parent), false, None)
    }

    pub(crate) fn define_class(
        &mut self,
        name: &str,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.define_class_with_identid(name_id, superclass, parent)
    }

    // TODO: we must name the unnamed class when the class object is assigned to constant later.
    pub(crate) fn define_unnamed_class(&mut self, superclass: Option<Module>) -> Module {
        let superclass = match superclass {
            Some(class) => class,
            None => self.object_class(),
        };
        self.define_class_inner(None, superclass, None, false, Some(ObjTy::OBJECT))
    }

    pub(crate) fn define_struct_class(
        &mut self,
        name: Option<IdentId>,
        superclass: Module,
    ) -> Module {
        let parent = if name.is_some() {
            Some(STRUCT_CLASS)
        } else {
            None
        };
        self.define_class_inner(name, superclass, parent, false, Some(ObjTy::OBJECT))
    }

    fn define_class_inner(
        &mut self,
        name_id: Option<IdentId>,
        superclass: impl Into<Option<Module>>,
        parent: Option<ClassId>,
        is_module: bool,
        instance_ty: Option<ObjTy>,
    ) -> Module {
        let class_id = self.add_class();
        let obj = self.generate_class_obj(
            name_id,
            class_id,
            superclass.into(),
            parent,
            is_module,
            instance_ty,
        );
        self.get_metaclass(class_id);
        obj
    }

    pub(crate) fn define_builtin_class(
        &mut self,
        name: &str,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
        parent: ClassId,
        instance_ty: impl Into<Option<ObjTy>>,
    ) -> Module {
        let name_id = IdentId::get_id(name);
        self.def_builtin_class(class_id);
        self.generate_class_obj(
            Some(name_id),
            class_id,
            superclass.into(),
            Some(parent),
            false,
            instance_ty.into(),
        )
    }

    pub(crate) fn define_builtin_exception_class(
        &mut self,
        name: &str,
        class_id: ClassId,
        superclass: impl Into<Option<Module>>,
    ) -> Module {
        let m =
            self.define_builtin_class(name, class_id, superclass, OBJECT_CLASS, ObjTy::EXCEPTION);
        self.get_metaclass(class_id);
        m
    }

    pub(crate) fn define_builtin_class_under_obj(
        &mut self,
        name: &str,
        class_id: ClassId,
        instance_ty: impl Into<Option<ObjTy>>,
    ) -> Module {
        let object_class = self.object_class();
        self.define_builtin_class(
            name,
            class_id,
            Some(object_class),
            OBJECT_CLASS,
            instance_ty,
        )
    }

    pub(crate) fn define_class_under_obj(&mut self, name: &str) -> Module {
        let name_id = IdentId::get_id(name);
        let object_class = self.object_class();
        self.define_class_inner(
            Some(name_id),
            Some(object_class),
            Some(OBJECT_CLASS),
            false,
            None,
        )
    }
}

impl Store {
    ///
    /// Add a new public method *func* with *name* to the class of *class_id*.
    ///
    /// This fn increments class version.
    ///
    pub(crate) fn add_public_method(&mut self, class_id: ClassId, name: IdentId, func_id: FuncId) {
        self.add_method(class_id, name, func_id, Visibility::Public)
    }

    ///
    /// Add a new method *func* with *name* to the class of *class_id*.
    ///
    /// This fn increments class version.
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
    /// This fn increments class version.
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
    /// This fn increments class version.
    ///
    fn add_method_inner(
        &mut self,
        owner: ClassId,
        name: IdentId,
        func_id: FuncId,
        visibility: Visibility,
        is_basic_op: bool,
    ) {
        self[func_id].set_owner_class(owner);
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
            let info = self[func_id].get_wrapper_info();
            let desc = self.func_description(func_id);
            JitModule::perf_write(info, &desc);
        }
    }

    ///
    /// Add a new empty method with *name* to the class of *class_id*.
    ///
    /// This fn increments class version.
    ///
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
    /// This fn increments class version.
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
            let info = self[func_id].get_wrapper_info();
            let desc = self.func_description(func_id);
            JitModule::perf_write(info, &desc);
        }
        let singleton = self.classes.get_metaclass(class_id).id();
        self[func_id].set_owner_class(class_id);
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
    /// Remove method.
    ///
    /// This fn increments class version.
    ///
    pub(crate) fn remove_method(&mut self, class_id: ClassId, func_name: IdentId) -> Result<()> {
        Globals::class_version_inc();
        if self.classes[class_id].methods.remove(&func_name).is_none() {
            Err(MonorubyErr::nameerr(format!(
                "method `{}' not defined in {}",
                func_name,
                self.get_class_name(class_id)
            )))
        } else {
            Ok(())
        }
    }

    ///
    /// Check whether public/protected method *name* is defined for *class_id* or its superclasses.
    ///
    pub(crate) fn method_defined(
        &self,
        class_id: ClassId,
        func_name: IdentId,
        inherit: bool,
    ) -> Option<Visibility> {
        Some(if inherit {
            self.check_method_for_class(class_id, func_name)?.visibility
        } else {
            self.classes.get_method(class_id, func_name)?.1
        })
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub(crate) fn find_method_for_object(
        &self,
        obj: Value,
        func_name: IdentId,
    ) -> Result<(FuncId, Visibility, ClassId)> {
        let class = obj.class();
        if let Some(entry) = self.check_method_for_class(class, func_name)
            && let Some(func_id) = entry.func_id()
        {
            Ok((func_id, entry.visibility, entry.owner))
        } else {
            Err(MonorubyErr::method_not_found(self, func_name, obj))
        }
    }

    ///
    /// Find method *name* for object *obj*.
    ///
    /// If not found, return MethodNotFound error.
    ///
    pub(crate) fn find_method_for_class(
        &self,
        class: ClassId,
        func_name: IdentId,
    ) -> Result<(FuncId, Visibility, ClassId)> {
        if let Some(entry) = self.check_method_for_class(class, func_name)
            && let Some(func_id) = entry.func_id()
        {
            Ok((func_id, entry.visibility, entry.owner))
        } else {
            Err(MonorubyErr::method_not_found_for_class(
                self, func_name, class,
            ))
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
    pub(crate) fn check_method(&self, obj: Value, name: IdentId) -> Option<FuncId> {
        let class_id = obj.class();
        self.check_method_for_class(class_id, name)?.func_id()
    }

    ///
    /// Check whether a public method *name* for object *obj* exists.
    ///
    pub(crate) fn check_public_method(&self, obj: Value, name: IdentId) -> Option<FuncId> {
        let class_id = obj.class();
        let entry = self.check_method_for_class(class_id, name)?;
        if entry.visibility == Visibility::Public {
            entry.func_id()
        } else {
            None
        }
    }

    ///
    /// Change visibility of methods `names` class *class_id*.
    ///
    /// This fn increments class version.
    ///
    pub(crate) fn change_method_visibility_for_class(
        &mut self,
        class_id: ClassId,
        names: &[IdentId],
        visibility: Visibility,
    ) -> Result<()> {
        for name in names {
            self.find_method_for_class(class_id, *name)?;
            match self.classes[class_id].methods.get_mut(name) {
                Some(entry) => {
                    entry.visibility = visibility;
                    Globals::class_version_inc();
                }
                None => {
                    self.add_empty_method(class_id, *name, visibility);
                }
            };
        }
        Ok(())
    }

    ///
    /// If the re-defined method is "basic operation", return true.
    ///
    /// This fn increments class version.
    ///
    fn insert_method(&mut self, class_id: ClassId, name: IdentId, entry: MethodTableEntry) {
        Globals::class_version_inc();
        if let Some(old) = self.classes[class_id].methods.insert(name, entry)
            && old.is_basic_op
        {
            self.set_bop_redefine();
        }
    }

    fn set_bop_redefine(&mut self) {
        CODEGEN.with(|codegen| {
            let mut codegen = codegen.borrow_mut();
            codegen.set_bop_redefine();
            self.invalidate_jit_code();
            let vm_entry = codegen.vm_entry();
            for func in self.functions.functions() {
                if func.is_iseq().is_some() {
                    let entry = codegen.jit.get_label_address(&func.entry_label());
                    codegen.jit.apply_jmp_patch_address(entry, &vm_entry);
                }
            }
        });
    }

    fn check_method_for_name(
        &self,
        lfp: Lfp,
        recv_class: ClassId,
        name: Option<IdentId>,
    ) -> Option<FuncId> {
        if let Some(method_name) = name {
            self.check_method_for_class(recv_class, method_name)
                .map(|entry| entry.func_id())
                .flatten()
        } else {
            let func_id = lfp.method_func_id();
            let owner = self[func_id].owner_class()?;
            let name = self[func_id].name().unwrap();
            self.check_super(recv_class, owner, name)
        }
    }

    pub(crate) fn update_inline_cache(&mut self, lfp: Lfp) -> bool {
        let class_version = Globals::class_version();
        let func_id = lfp.func_id();
        let self_class = lfp.self_val().class();
        let iseq_id = self[func_id].as_iseq();
        let iseq = &self[iseq_id];
        for (recv_class, name, comptime_fid) in iseq.get_cache_map(self_class) {
            let func_id = self.check_method_for_name(lfp, *recv_class, *name);
            if func_id != Some(*comptime_fid) {
                return false;
            }
        }
        let version_label = self[iseq_id]
            .get_jit_class_version(lfp.self_val().class())
            .unwrap();
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .set_class_version(class_version, &version_label);
        });

        true
    }
}

impl Globals {
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
