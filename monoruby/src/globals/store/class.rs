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
pub const IO_ERROR_CLASS: ClassId = ClassId::new(49);

pub const MATCHDATA_CLASS: ClassId = ClassId::new(50);
pub const SET_CLASS: ClassId = ClassId::new(51);
pub const RATIONAL_CLASS: ClassId = ClassId::new(52);
pub const FATAL_ERROR_CLASS: ClassId = ClassId::new(53);

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
            49 => write!(f, "IO_ERROR"),

            50 => write!(f, "MATCHDATA"),
            51 => write!(f, "SET"),
            52 => write!(f, "RATIONAL"),
            53 => write!(f, "FATAL_ERROR"),
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
                | REGEXP_CLASS
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
                None => {
                    let kind = if class.as_val().ty() == Some(ObjTy::MODULE) {
                        "Module"
                    } else {
                        "Class"
                    };
                    format!("#<{kind}:0x{:016x}>", class.as_val().id())
                }
                Some(base) => format!("#<Class:{}>", base.to_s(store)),
            },
        }
    }
}

/// Visibility of a constant. Constants are public by default;
/// `Module#private_constant` makes them private. Visibility is stored
/// alongside the constant's value/autoload-path inside `ConstState`, so it
/// persists across an autoload-to-loaded transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum ConstVisibility {
    #[default]
    Public,
    Private,
}

impl ConstVisibility {
    pub(crate) fn is_private(self) -> bool {
        matches!(self, ConstVisibility::Private)
    }
}

/// State of a constant slot in a `ClassInfo`'s constant table.
///
/// `kind` distinguishes a fully loaded constant from a registered autoload,
/// and `visibility` records whether the constant is public or private. The
/// two are kept independent so that toggling visibility on an autoload entry
/// is preserved when the autoload is later triggered.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ConstState {
    pub kind: ConstStateKind,
    pub visibility: ConstVisibility,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ConstStateKind {
    Loaded(Value),
    Autoload(std::path::PathBuf),
}

impl ConstState {
    pub(crate) fn loaded(value: Value) -> Self {
        Self {
            kind: ConstStateKind::Loaded(value),
            visibility: ConstVisibility::Public,
        }
    }

    pub(crate) fn autoload(path: std::path::PathBuf) -> Self {
        Self {
            kind: ConstStateKind::Autoload(path),
            visibility: ConstVisibility::Public,
        }
    }

    pub(crate) fn loaded_value(&self) -> Option<Value> {
        match self.kind {
            ConstStateKind::Loaded(v) => Some(v),
            ConstStateKind::Autoload(_) => None,
        }
    }

    pub(crate) fn is_loaded(&self) -> bool {
        matches!(self.kind, ConstStateKind::Loaded(_))
    }

    pub(crate) fn is_autoload(&self) -> bool {
        matches!(self.kind, ConstStateKind::Autoload(_))
    }

    pub(crate) fn is_private(&self) -> bool {
        self.visibility.is_private()
    }

    pub(crate) fn set_private(&mut self) {
        self.visibility = ConstVisibility::Private;
    }

    pub(crate) fn set_public(&mut self) {
        self.visibility = ConstVisibility::Public;
    }
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    ///
    /// the constant name which this class object is bound.
    /// if this class object is not bound to any constant, this is None.
    ///
    name: Option<String>,
    ///
    /// Cached frozen `String` Value of `name`. Lazily populated by
    /// `Module#name` so that repeated calls return the same identical
    /// (frozen) string object, matching CRuby's behavior.
    ///
    name_value: Option<Value>,
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
    /// constants table. Each entry stores the constant's value (or autoload
    /// path) plus its visibility (public/private).
    ///
    constants: HashMap<IdentId, ConstState>,
    ///
    /// Per-constant source location, recorded when a constant is first
    /// stored. Used by `Module#const_source_location`. Constants defined in
    /// the runtime (e.g. via `define_class_inner` from Rust builtins) have
    /// no entry and `const_source_location` returns `nil` for them.
    ///
    constant_locations: HashMap<IdentId, (String, u32)>,
    ///
    /// class variable table.
    ///
    class_variables: Option<HashMap<IdentId, Value>>,
    ///
    /// instance variable table (insertion-ordered).
    ///
    ivar_names: indexmap::IndexMap<IdentId, IvarId>,
    ///
    /// Object type of instances of this class.
    ///
    /// None for modules.
    ///
    instance_ty: Option<ObjTy>,
    ///
    /// C-level allocator (CRuby's `rb_alloc_func_t` equivalent). Invoked by
    /// `Class#new` and the default `Class#allocate` to produce an
    /// uninitialized instance. Inherited from the superclass at class
    /// definition time; `None` means "allocator undefined" (raise TypeError).
    ///
    alloc_func: Option<AllocFunc>,
}

/// C-level allocator function pointer. Given a class id (and a globals
/// handle, used by a few allocators that need e.g. the class name), returns
/// a freshly allocated (uninitialized) instance of that class.
///
/// The second parameter is always passed (matching a fixed calling
/// convention so the JIT can install a single call sequence), but many
/// allocators ignore it.
pub type AllocFunc = extern "C" fn(ClassId, &mut crate::Globals) -> Value;

/// Default allocator used by `BasicObject` and inherited by any class that
/// does not override it. Produces a plain `RValue::Object` tagged with the
/// given class id.
pub extern "C" fn default_alloc_func(class_id: ClassId, _: &mut crate::Globals) -> Value {
    Value::object(class_id)
}

/// Allocator for `Struct.new(:a, :b, ...)`-derived classes. Reads
/// the class-level `/members` ivar (set during `Struct#initialize` of
/// the subclass) to size the per-instance slot vector. All slots start
/// as `nil`; `Struct#initialize` overwrites them positionally.
///
/// If the class has no `/members` set yet (e.g. someone called
/// `allocate` before `Struct.new` finished initialising the subclass)
/// we fall back to a zero-length slot vector.
pub extern "C" fn struct_alloc_func(class_id: ClassId, globals: &mut crate::Globals) -> Value {
    use crate::IdentId;
    use crate::value::Value;
    // Walk the class chain looking for `/members` (set on the immediate
    // subclass produced by `Struct.new(...)`, not propagated to deeper
    // descendants like `Class.new(MyStruct)`).
    let mut cls = globals.store[class_id].get_module();
    let len = loop {
        if let Some(m) = globals.store.get_ivar(cls.as_val(), IdentId::get_id("/members"))
            && let Some(arr) = m.try_array_ty()
        {
            break arr.len();
        }
        match cls.superclass() {
            Some(s) => cls = s,
            None => break 0,
        }
    };
    Value::struct_object(class_id, len)
}

impl alloc::GC<RValue> for ClassInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = self.object {
            v.as_val().mark(alloc);
        }
        if let Some(v) = self.name_value {
            v.mark(alloc);
        }
        self.constants.values().for_each(|state| {
            if let Some(v) = state.loaded_value() {
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
            name_value: None,
            parent: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            constant_locations: HashMap::default(),
            class_variables: None,
            ivar_names: indexmap::IndexMap::default(),
            instance_ty: None,
            alloc_func: None,
        }
    }

    pub(super) fn copy(&self) -> Self {
        Self {
            name: None,
            name_value: None,
            parent: None,
            object: None,
            methods: HashMap::default(),
            constants: HashMap::default(),
            constant_locations: HashMap::default(),
            class_variables: None,
            ivar_names: self.ivar_names.clone(),
            instance_ty: self.instance_ty,
            alloc_func: self.alloc_func,
        }
    }

    pub(crate) fn alloc_func(&self) -> Option<AllocFunc> {
        self.alloc_func
    }

    pub(crate) fn set_alloc_func(&mut self, f: AllocFunc) {
        self.alloc_func = Some(f);
    }

    pub(crate) fn clear_alloc_func(&mut self) {
        self.alloc_func = None;
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
        // Invalidate any cached frozen name Value so the next call to
        // `Module#name` rebuilds it from the current `name`.
        self.name_value = None;
    }

    pub(crate) fn clear_name(&mut self) {
        self.name = None;
        self.name_value = None;
    }

    pub(crate) fn get_name(&self) -> Option<&str> {
        self.name.as_ref().map(|x| x.as_str())
    }

    pub(crate) fn cached_name_value(&self) -> Option<Value> {
        self.name_value
    }

    pub(crate) fn set_cached_name_value(&mut self, val: Value) {
        self.name_value = Some(val);
    }

    pub(crate) fn record_constant_location(&mut self, name: IdentId, file: String, line: u32) {
        self.constant_locations.insert(name, (file, line));
    }

    pub(crate) fn get_constant_location(&self, name: IdentId) -> Option<(&str, u32)> {
        self.constant_locations
            .get(&name)
            .map(|(f, l)| (f.as_str(), *l))
    }

    /// Returns true if a constant `name` is defined directly on this
    /// class/module (not inherited).
    pub(crate) fn has_own_constant(&self, name: IdentId) -> bool {
        self.constants.contains_key(&name)
    }

    /// Returns true if the constant `name` defined on this class/module is
    /// marked as private. Constants are public by default. Returns false if
    /// the constant is not defined on this class.
    pub(crate) fn is_constant_private(&self, name: IdentId) -> bool {
        self.constants
            .get(&name)
            .is_some_and(|state| state.is_private())
    }

    /// Marks the constant `name` as private. The caller is responsible for
    /// verifying that the constant is defined on this class.
    pub(crate) fn set_constant_private(&mut self, name: IdentId) {
        if let Some(state) = self.constants.get_mut(&name) {
            state.set_private();
        }
    }

    /// Marks the constant `name` as public. No-op if not currently private
    /// or not defined on this class.
    pub(crate) fn set_constant_public(&mut self, name: IdentId) {
        if let Some(state) = self.constants.get_mut(&name) {
            state.set_public();
        }
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

    pub(crate) fn remove_cvar(&mut self, name: IdentId) -> Option<Value> {
        self.class_variables.as_mut()?.remove(&name)
    }

    fn cvar_names(&self) -> Vec<IdentId> {
        match &self.class_variables {
            Some(cv) => cv.keys().cloned().collect(),
            None => vec![],
        }
    }

    ///
    /// Remove a constant with *name* in the class of *class_id*.
    ///
    /// If not found, return None. Visibility metadata is cleared
    /// automatically since it lives inside `ConstState`. Also clears any
    /// associated source-location entry.
    ///
    pub(in crate::globals) fn remove_constant(&mut self, name: IdentId) -> Option<Value> {
        let removed = self.constants.remove(&name).map(|state| match state.kind {
            ConstStateKind::Loaded(v) => v,
            ConstStateKind::Autoload(..) => Value::nil(),
        });
        if removed.is_some() {
            self.constant_locations.remove(&name);
        }
        removed
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
            match self[parent].name.as_ref() {
                Some(name) => parents.push(name.to_string()),
                None => {
                    // Anonymous ancestor: render its inspect form so the
                    // child still has a non-nil, qualifying name (e.g.
                    // `#<Module:0x..>::Inner`).
                    let parent_obj = self[parent].get_module().as_val();
                    let kind = if parent_obj.ty() == Some(ObjTy::MODULE) {
                        "Module"
                    } else {
                        "Class"
                    };
                    parents.push(format!("#<{kind}:0x{:016x}>", parent_obj.id()));
                    break;
                }
            }
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
            // only happen when original_obj is Module, so we can unwrap() safely.
            self.get_singleton(original_obj.as_val()).unwrap()
        }
    }

    ///
    /// Get a singleton class of *obj*: Value.
    ///
    /// If not exists, create a new singleton class.
    ///
    pub(crate) fn get_singleton(&mut self, mut obj: Value) -> Result<Module> {
        let org_class = self[obj.class()].get_module();
        if org_class.is_singleton().is_some() {
            return Ok(org_class);
        }
        match org_class.id() {
            INTEGER_CLASS | FLOAT_CLASS | SYMBOL_CLASS => {
                return Err(MonorubyErr::typeerr("can't define singleton"));
            }
            RANGE_CLASS | COMPLEX_CLASS => {
                return Err(MonorubyErr::frozenerr("can't define singleton"));
            }
            _ => {}
        }
        let mut singleton = self.new_singleton_class(org_class, obj, org_class.id());
        obj.change_class(singleton.id());
        let singleton_meta = org_class.get_real_class().as_val().class();
        singleton.change_class(singleton_meta);
        #[cfg(debug_assertions)]
        {
            assert_eq!(singleton.id(), obj.class());
        }
        Ok(singleton)
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
    /// Get the immediate (direct) subclasses of *class_id*.
    /// Returns only real classes (not iclass entries, not singleton classes,
    /// not modules) whose direct superclass is *class_id*. The walk skips over
    /// any intermediate iclass nodes inserted by `include`/`prepend`.
    pub(crate) fn get_direct_subclasses(&self, class_id: ClassId) -> Vec<Value> {
        let mut result = Vec::new();
        for (idx, info) in self.table.iter().enumerate() {
            let module = match info.object {
                Some(m) => m,
                None => continue,
            };
            if module.is_singleton().is_some() || module.is_iclass() {
                continue;
            }
            let cid = ClassId::new(idx as u32);
            if cid == class_id {
                continue;
            }
            // Skip non-class objects (e.g. modules) — only Class entries can
            // have *class_id* as a parent in the inheritance chain.
            if module.as_val().ty() != Some(ObjTy::CLASS) {
                continue;
            }
            if let Some(superclass) = module.get_real_superclass() {
                if superclass.id() == class_id {
                    result.push(module.as_val());
                }
            }
        }
        result
    }

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
    /// Get private method names in the class of *class_id* and its ancestors.
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

    ///
    /// Get protected method names in the class of *class_id*.
    ///
    pub(crate) fn get_protected_method_names(&self, class_id: ClassId) -> Vec<Value> {
        self[class_id]
            .methods
            .iter()
            .filter_map(|(name, entry)| {
                if entry.func_id().is_some() && entry.visibility() == Visibility::Protected {
                    Some(Value::symbol(*name))
                } else {
                    None
                }
            })
            .collect()
    }

    ///
    /// Get protected method names in the class of *class_id* and its ancestors.
    ///
    pub(crate) fn get_protected_method_names_inherit(&self, class_id: ClassId) -> Vec<Value> {
        let mut names = HashSet::default();
        let mut module = self.get_module(class_id);
        let mut exclude = HashSet::default();
        loop {
            for (name, entry) in &self[module.id()].methods {
                if matches!(entry.visibility, Visibility::Undefined) {
                    exclude.insert(*name);
                } else if entry.func_id().is_some()
                    && entry.visibility() == Visibility::Protected
                    && !exclude.contains(name)
                {
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
        // Inherit `alloc_func` from the superclass. Modules have no alloc_func.
        // Builtin classes that need a custom allocator will overwrite this
        // later via `ClassInfo::set_alloc_func`.
        let inherited_alloc = if is_module {
            None
        } else if let Some(sc) = superclass {
            self[sc.id()].alloc_func
        } else {
            None
        };
        self[class_id].object = Some(class_obj.as_class());
        self[class_id].name = name.map(|id| id.to_string());
        self[class_id].parent = parent;
        self[class_id].instance_ty = instance_ty;
        self[class_id].alloc_func = inherited_alloc;
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

    /// Allocate an uninitialized class object (no superclass).
    /// This corresponds to `Class.allocate` in Ruby.
    pub(crate) fn allocate_uninit_class(&mut self) -> Module {
        self.define_class_inner(None, None::<Module>, None, false, Some(ObjTy::OBJECT))
    }

    pub(crate) fn define_unnamed_module(&mut self) -> Module {
        let object_class = self.object_class();
        self.define_class_inner(None, Some(object_class), None, true, None)
    }

    /// Create an anonymous duplicate of `original_class` that owns its own
    /// method table. Used by `Module#dup` / `Module#clone` so that subsequent
    /// `undef_method`, `define_method`, constant assignment, etc. on the copy
    /// do not bleed into the original module/class.
    ///
    /// The new module inherits the same superclass as the original, has no
    /// name, and is registered under no parent. Its methods, constants,
    /// constant source locations, and class variables are deep-cloned. The
    /// new module is a plain module (ObjTy::MODULE) regardless of whether
    /// the original was a class or module — matching CRuby's behaviour of
    /// producing an anonymous duplicate usable for `include`.
    pub(crate) fn duplicate_module(&mut self, original_class: ClassId) -> Module {
        let orig = &self[original_class];
        let superclass = self.get_module(original_class).superclass();
        let is_module = orig.instance_ty.is_none();
        let instance_ty = orig.instance_ty;
        // Snapshot data we need to copy before borrowing mut.
        let methods = orig.methods.clone();
        let constants = orig.constants.clone();
        let constant_locations = orig.constant_locations.clone();
        let class_variables = orig.class_variables.clone();
        let alloc_func = orig.alloc_func;

        let new_id = self.add_class();
        let class_obj = if is_module {
            Value::module_empty(new_id, superclass)
        } else {
            Value::class_empty(new_id, superclass)
        };
        let info = &mut self[new_id];
        info.object = Some(class_obj.as_class());
        info.name = None;
        info.parent = None;
        info.instance_ty = instance_ty;
        info.methods = methods;
        info.constants = constants;
        info.constant_locations = constant_locations;
        info.class_variables = class_variables;
        info.alloc_func = alloc_func;

        // Duplicate the singleton class too: CRuby's Module#initialize_copy
        // clones the singleton class so `def self.foo` and `extend`-ed modules
        // on the original are reachable through the dup.
        let orig_meta = self.get_metaclass(original_class);
        let new_meta = self.get_metaclass(new_id);

        // Copy singleton-class method/constant/cvar tables.
        let orig_meta_info = &self[orig_meta.id()];
        let m_methods = orig_meta_info.methods.clone();
        let m_constants = orig_meta_info.constants.clone();
        let m_constant_locations = orig_meta_info.constant_locations.clone();
        let m_class_variables = orig_meta_info.class_variables.clone();
        let new_meta_info = &mut self[new_meta.id()];
        new_meta_info.methods = m_methods;
        new_meta_info.constants = m_constants;
        new_meta_info.constant_locations = m_constant_locations;
        new_meta_info.class_variables = m_class_variables;

        // Replicate `extend`-ed modules (iclasses hanging off the original
        // metaclass's superclass chain) on the new metaclass in the same
        // order. `include_module` prepends, so walk the original chain from
        // nearest to furthest and re-include in reverse.
        let mut included: Vec<ClassId> = vec![];
        let mut cur = orig_meta.superclass();
        while let Some(sc) = cur
            && sc.is_iclass()
        {
            included.push(sc.id());
            cur = sc.superclass();
        }
        let mut new_meta = new_meta;
        for mod_id in included.into_iter().rev() {
            let module = self.get_module(mod_id);
            let _ = new_meta.include_module(module);
        }

        class_obj.as_class()
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
        // Struct subclasses tag their RValue as STRUCT (slot-array
        // storage) rather than OBJECT (ivar-only storage). Members are
        // stored in the per-instance slot vector instead of leaking out
        // as ivars from `instance_variables` introspection.
        let m = self.define_class_inner(name, superclass, parent, false, Some(ObjTy::STRUCT));
        // Struct itself has no alloc_func, so a class created with `<` Struct
        // would inherit None. `Struct.new(...)` however produces instantiable
        // classes — install the struct-specific allocator that allocates a
        // slot vector sized to the class's `/members` array.
        self[m.id()].set_alloc_func(struct_alloc_func);
        m
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
    pub(crate) fn get_class_variable_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self[class_id].cvar_names()
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
                if let Some(iseq) = func.is_iseq() {
                    // Skip trivial methods (ConstReturn/SelfReturn) — their
                    // wrappers don't execute bytecode and contain no BOP usage,
                    // so patching them to vm_entry would break execution.
                    if self[iseq].hint != ISeqHint::Normal {
                        continue;
                    }
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
            let name = self[func_id].name().unwrap();
            self.check_super(recv_class, func_id, name)
        }
    }

    pub(crate) fn update_inline_cache(&mut self, lfp: Lfp) -> bool {
        let class_version = Globals::class_version();
        let func_id = lfp.func_id();
        let self_class = lfp.self_val().class();
        let iseq_id = self[func_id].as_iseq();
        let iseq = &self[iseq_id];
        let Some(cache_map) = iseq.get_cache_map(self_class) else {
            // JIT entry was invalidated (e.g. by BOP redefinition). Fall back to recompile.
            return false;
        };
        for (recv_class, name, comptime_fid) in cache_map {
            let func_id = self.check_method_for_name(lfp, *recv_class, *name);
            if func_id != Some(*comptime_fid) {
                return false;
            }
        }
        let Some(version_label) = self[iseq_id].get_jit_class_version(lfp.self_val().class())
        else {
            // JIT entry was invalidated between cache_map check and here.
            return false;
        };
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .set_class_version(class_version, &version_label);
        });

        true
    }

    ///
    /// Check whether a super method for *current_func_id* exists in the ancestor chain.
    ///
    /// Walks the ancestor chain of *self_class* to find the module with *owner* ClassId,
    /// then searches for the method *name* starting from its superclass.
    ///
    /// When the same module appears multiple times in the ancestor chain (e.g., through
    /// include + include-via-module, or include + prepend), this method skips duplicate
    /// entries by checking if the found super method has the same func_id as the current
    /// method being executed.
    ///
    pub(crate) fn check_super(
        &self,
        self_class: ClassId,
        current_func_id: FuncId,
        name: IdentId,
    ) -> Option<FuncId> {
        let owner = self[current_func_id].owner_class();
        let mut module = self.get_module(self_class);
        loop {
            if !module.has_origin() && owner.contains(&module.id()) {
                let super_module = module.superclass()?;
                let super_fid = self.search_method(super_module, name)?.func_id()?;
                if super_fid != current_func_id {
                    return Some(super_fid);
                }
                // The super method has the same func_id as the current method,
                // meaning we found a duplicate iclass of the same module in the chain.
                // Continue walking to find the next occurrence.
            }
            module = module.superclass()?;
        }
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
