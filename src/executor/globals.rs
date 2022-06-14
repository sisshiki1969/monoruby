use std::io::{stdout, BufWriter, Stdout};
use std::path::PathBuf;

use super::*;

pub const NIL_CLASS: ClassId = ClassId::new(1);
pub const TRUE_CLASS: ClassId = ClassId::new(2);
pub const FALSE_CLASS: ClassId = ClassId::new(3);
pub const INTEGER_CLASS: ClassId = ClassId::new(4);
pub const FLOAT_CLASS: ClassId = ClassId::new(5);
pub const STRING_CLASS: ClassId = ClassId::new(6);
pub const SYMBOL_CLASS: ClassId = ClassId::new(7);
pub const TIME_CLASS: ClassId = ClassId::new(8);

// Integer#chr
extern "C" fn chr(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let b = match arg.self_value().as_fixnum() {
        Some(i) => {
            if let Ok(res) = u8::try_from(i) {
                res
            } else {
                unreachable!()
            }
        }
        _ => unreachable!(),
    };
    Value::string(vec![b])
}

//
/// Store of functions.
///
pub struct Globals {
    /// Functions.
    pub func: FnStore,
    /// identifier table.
    pub id_store: IdentifierTable,
    /// class table.
    pub class: ClassStore,
    pub error: Option<MonorubyErr>,
    /// warning level.
    pub warning: u8,
    /// stdout.
    pub stdout: BufWriter<Stdout>,
}

impl Globals {
    pub fn new(warning: u8) -> Self {
        let mut globals = Self {
            func: FnStore::new(),
            id_store: IdentifierTable::new(),
            class: ClassStore::new(),
            error: None,
            warning,
            stdout: BufWriter::new(stdout()),
        };
        let name = globals.get_ident_id("Process");
        globals.set_constant(name, Value::int32(42));
        let name = globals.get_ident_id("Time");
        globals.set_constant(name, Value::nil());
        let name = globals.get_ident_id("File");
        builtins::init_builtins(&mut globals);
        globals.set_constant(name, Value::nil());
        assert_eq!(NIL_CLASS, globals.class.add_class());
        assert_eq!(TRUE_CLASS, globals.class.add_class());
        assert_eq!(FALSE_CLASS, globals.class.add_class());
        assert_eq!(INTEGER_CLASS, globals.class.add_class());
        assert_eq!(FLOAT_CLASS, globals.class.add_class());
        assert_eq!(STRING_CLASS, globals.class.add_class());
        globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
        globals
    }

    pub fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            id_store: self.id_store.clone(),
            class: self.class.clone(),
            error: None,
            warning: self.warning,
            stdout: BufWriter::new(stdout()),
        }
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        self.func.main.unwrap()
    }

    pub fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.id_store.get_name(id)
    }

    fn get_method_inner(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        if let Some(func_id) = self.class.get_method(class_id, name) {
            return Some(func_id);
        }
        self.class.get_method(ClassId::new(0), name)
    }

    pub fn vm_find_method(
        &mut self,
        callsite_id: CallsiteId,
        receiver: Value,
        class_version: usize,
    ) -> Option<(FuncId, u16, u16, u16)> {
        let CallsiteInfo {
            ret,
            name,
            args,
            len,
            cache: (version, cached_class_id, cached_func),
        } = self.func[callsite_id];
        let recv_class = receiver.class();
        let func_id = if version == class_version && cached_class_id == recv_class {
            cached_func
        } else {
            match self.get_method(recv_class, name, len as usize) {
                Some(id) => {
                    self.func[callsite_id].cache = (class_version, recv_class, id);
                    id
                }
                None => return None,
            }
        };
        Some((func_id, args, len, ret))
    }

    pub fn get_method(
        &mut self,
        class_id: ClassId,
        func_name: IdentId,
        args_len: usize,
    ) -> Option<FuncId> {
        let func_id = match self.get_method_inner(class_id, func_name) {
            Some(id) => id,
            None => {
                self.error = Some(MonorubyErr::method_not_found(func_name));
                return None;
            }
        };
        let arity = self.func[func_id].arity();
        if arity != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity, args_len));
            return None;
        }
        Some(func_id)
    }

    pub fn get_constant(&self, name: IdentId) -> Option<Value> {
        self.class.get_constants(name)
    }

    pub fn set_constant(&mut self, name: IdentId, val: Value) -> Option<Value> {
        self.class.set_constants(name, val)
    }

    pub fn define_global_builtin_func(
        &mut self,
        name: &str,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        self.define_builtin_func(ClassId(0), name, address, arity)
    }

    pub fn define_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = self.get_ident_id(name);
        self.class.add_method(class_id, name_id, func_id);
        func_id
    }

    pub fn compile_script(&mut self, code: String, path: impl Into<PathBuf>) -> Result<()> {
        let res = match Parser::parse_program(code, path.into()) {
            Ok(res) => self
                .func
                .compile_script(res.node, &mut self.id_store, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        };
        res
    }
}

impl Globals {
    pub fn get_error_message(&self, err: &MonorubyErr) -> String {
        match &err.kind {
            MonorubyErrKind::UndefinedLocal(ident) => {
                format!("undefined local variable or method `{}'", ident)
            }
            MonorubyErrKind::MethodNotFound(name) => {
                format!("undefined method `{}'", self.get_ident_name(*name))
            }
            MonorubyErrKind::WrongArguments(name) => name.to_string(),
            MonorubyErrKind::Syntax(kind) => format!("{:?}", kind),
            MonorubyErrKind::Syntax2(msg) => msg.to_string(),
            MonorubyErrKind::Unimplemented(msg) => msg.to_string(),
            MonorubyErrKind::UninitConst(name) => {
                format!("uninitialized constant {}", self.get_ident_name(*name))
            }
            MonorubyErrKind::DivideByZero => format!("divided by 0"),
        }
    }
}

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
    methods: HashMap<IdentId, FuncId>,
    /// constants table.
    constants: HashMap<IdentId, Value>,
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            methods: HashMap::default(),
            constants: HashMap::default(),
        }
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
    fn new() -> Self {
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
