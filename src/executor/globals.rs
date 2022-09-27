use std::io::{stdout, BufWriter, Stdout};
use std::path::PathBuf;

use super::*;

mod classes;
mod error;
mod functions;
pub use classes::*;
pub use error::*;
pub use functions::*;

///
/// Global state.
///
pub struct Globals {
    /// function info.
    pub func: FnStore,
    /// identifier table.
    pub id_store: IdentifierTable,
    /// class table.
    pub class: ClassStore,
    error: Option<MonorubyErr>,
    /// warning level.
    pub warning: u8,
    /// suppress jit compilation.
    pub no_jit: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool) -> Self {
        let mut globals = Self {
            func: FnStore::new(),
            id_store: IdentifierTable::new(),
            class: ClassStore::new(),
            error: None,
            warning,
            no_jit,
            stdout: BufWriter::new(stdout()),
        };
        builtins::init_builtins(&mut globals);
        globals
    }

    pub fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write(bytes).unwrap();
    }

    pub fn exec_startup(&mut self) {
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../../startup/startup.rb").to_string();
        let startup_fid = match self.compile_script(code, path) {
            Ok(func_id) => func_id,
            Err(err) => {
                eprintln!("error occured in startup.rb");
                eprintln!("{}", err.get_error_message(&self));
                err.show_loc();
                return;
            }
        };
        match Interp::eval_toplevel(self, startup_fid) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("error occured in startup.rb");
                eprintln!("{}", err.get_error_message(self));
                err.show_loc();
            }
        };
    }
}

//
// error handlers
//
impl Globals {
    fn set_error(&mut self, err: MonorubyErr) {
        self.error = Some(err);
    }

    pub fn err_method_not_found(&mut self, name: IdentId, class: ClassId) {
        self.set_error(MonorubyErr::method_not_found(name, class))
    }

    pub fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub fn err_uninitialized_constant(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::uninitialized_constant(name));
    }

    ///
    /// Set RangeError with message "*val* out of char range".
    ///
    pub fn err_char_out_of_range(&mut self, val: Value) {
        self.set_error(MonorubyErr::range(format!(
            "{} out of char range",
            self.val_tos(val)
        )));
    }

    ///
    /// Set TypeError with message "no implicit conversion of *actual* into *expected*".
    ///
    pub fn err_no_implict_conv(&mut self, actual: ClassId, expect: ClassId) {
        self.set_error(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            actual.get_name(self),
            expect.get_name(self),
        )));
    }

    ///
    /// Set TypeError with message "*name* is not a class".
    ///
    pub fn err_is_not_class(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::typeerr(format!(
            "{} is not a class",
            self.get_ident_name(name)
        )));
    }

    ///
    /// Set TypeError with message "*name* is not a class".
    ///
    pub fn err_is_not_symbol_nor_string(&mut self, val: Value) {
        self.set_error(MonorubyErr::typeerr(format!(
            "{} is not a symbol nor a string",
            self.val_tos(val)
        )));
    }

    ///
    /// Set IndexError with message "index *actual* too small for array; minimum: *minimum*".
    ///
    pub fn err_index_too_small(&mut self, actual: i64, minimum: i64) {
        self.set_error(MonorubyErr::indexerr(format!(
            "index {} too small for array; minimum: {}",
            actual, minimum,
        )));
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub fn err_cant_modify_frozen(&mut self, val: Value) {
        self.set_error(MonorubyErr::indexerr(format!(
            "can't modify frozen {}: {}",
            val.class_id().get_name(self),
            self.val_tos(val),
        )));
    }

    pub fn take_error(&mut self) -> Option<MonorubyErr> {
        std::mem::take(&mut self.error)
    }

    pub fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        match &mut self.error {
            Some(err) => {
                err.loc.push((loc, sourceinfo));
            }
            None => unreachable!(),
        };
    }
}

impl Globals {
    fn array_tos(&self, v: &Vec<Value>) -> String {
        match v.len() {
            0 => "[]".to_string(),
            1 => format!("[{}]", self.val_inspect(v[0])),
            _ => {
                let mut s = format!("[{}", self.val_inspect(v[0]));
                for val in v[1..].iter() {
                    s += &format!(", {}", self.val_inspect(*val));
                }
                s += "]";
                s
            }
        }
    }

    fn object_tos(&self, val: Value) -> String {
        if let Some(name) = val.rvalue().get_var(IdentId::_NAME) {
            self.val_tos(name)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                val.class_id().get_name(self),
                val.rvalue().id()
            )
        }
    }

    fn object_inspect(&self, val: Value) -> String {
        if let Some(name) = val.rvalue().get_var(IdentId::_NAME) {
            self.val_tos(name)
        } else {
            let mut ivars = String::new();
            match val.rvalue().get_varmap() {
                Some(vars) => {
                    for (id, v) in vars.iter() {
                        ivars += &format!(" {}={}", self.get_ident_name(*id), v.to_s(self));
                    }
                }
                None => {}
            };
            format!(
                "#<{}:0x{:016x}{ivars}>",
                val.class_id().get_name(self),
                val.rvalue().id()
            )
        }
    }

    pub fn val_tos(&self, val: Value) -> String {
        match val.unpack() {
            RV::Nil => format!("nil"),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => self.get_ident_name(id).to_string(),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => s,
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match &rvalue.kind {
                ObjKind::Class(class_id) => class_id.get_name(self),
                ObjKind::Time(time) => time.to_string(),
                ObjKind::Array(v) => self.array_tos(v),
                ObjKind::Object => self.object_tos(val),
                _ => format!("{:016x}", val.get()),
            },
        }
    }

    pub fn val_tobytes(&self, val: Value) -> Vec<u8> {
        match val.unpack() {
            RV::String(s) => return s.to_vec(),
            _ => {}
        };
        self.val_tos(val).into_bytes()
    }

    pub fn val_inspect(&self, val: Value) -> String {
        match val.unpack() {
            RV::Nil => format!("nil"),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => format!(":{}", self.get_ident_name(id)),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => format!("\"{}\"", escape_string::escape(&s)),
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match &rvalue.kind {
                ObjKind::Class(class_id) => class_id.get_name(self),
                ObjKind::Time(time) => time.to_string(),
                ObjKind::Array(v) => self.array_tos(v),
                ObjKind::Object => self.object_inspect(val),
                _ => unreachable!(),
            },
        }
    }

    pub fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.id_store.get_name(id)
    }

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
        let name_id = self.get_ident_id(name);
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

    pub fn find_method(&self, mut class_id: ClassId, name: IdentId) -> Option<FuncId> {
        if let Some(func_id) = self.class.get_method(class_id, name) {
            return Some(func_id);
        }
        while let Some(super_class) = class_id.super_class(self) {
            class_id = super_class;
            if let Some(func_id) = self.class.get_method(class_id, name) {
                return Some(func_id);
            }
        }
        None
    }

    pub fn get_method(
        &mut self,
        class_id: ClassId,
        func_name: IdentId,
        args_len: usize,
    ) -> Option<FuncId> {
        let func_id = match self.find_method(class_id, func_name) {
            Some(id) => id,
            None => {
                self.err_method_not_found(func_name, class_id);
                return None;
            }
        };
        self.check_arg(func_id, args_len)?;
        Some(func_id)
    }

    pub fn check_arg(&mut self, func_id: FuncId, args_len: usize) -> Option<()> {
        let arity = self.func[func_id].arity();
        if arity != -1 && (arity as usize) != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity as usize, args_len));
            return None;
        }
        Some(())
    }

    ///
    /// Get constant for *id*.
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

    fn search_lexical_stack(&self, name: IdentId, class_context: &[ClassId]) -> Option<Value> {
        class_context
            .iter()
            .rev()
            .find_map(|class| self.get_constant(*class, name))
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

    pub fn get_instance_method_names(&self, class_id: ClassId) -> Vec<IdentId> {
        self.class.get_method_names(class_id)
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

    pub fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) -> Option<Value> {
        self.class.set_constant(class_id, name, val)
    }

    pub fn define_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = self.get_ident_id(name);
        self.class.add_method(class_id, name_id, func_id);
        func_id
    }

    pub fn define_builtin_singleton_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let class_id = self.get_singleton_id(class_id);
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = self.get_ident_id(name);
        self.class.add_method(class_id, name_id, func_id);
        func_id
    }

    ///
    /// Define attribute reader for *class_id* and *ivar_name*.
    ///
    pub fn define_attr_reader(&mut self, class_id: ClassId, method_name: IdentId) -> IdentId {
        let ivar_name = self.id_store.add_ivar_prefix(method_name);
        let method_name_str = self.get_ident_name(method_name).to_string();
        let func_id = self.func.add_attr_reader(method_name_str, ivar_name);
        self.class.add_method(class_id, method_name, func_id);
        method_name
    }

    ///
    /// Define attribute writer for *class_id* and *ivar_name*.
    ///
    pub fn define_attr_writer(&mut self, class_id: ClassId, method_name: IdentId) -> IdentId {
        let ivar_name = self.id_store.add_ivar_prefix(method_name);
        let method_name = self.id_store.add_assign_postfix(method_name);
        let method_name_str = self.get_ident_name(method_name).to_string();
        let func_id = self.func.add_attr_writer(method_name_str, ivar_name);
        self.class.add_method(class_id, method_name, func_id);
        method_name
    }

    pub fn compile_script(&mut self, code: String, path: impl Into<PathBuf>) -> Result<FuncId> {
        let res = match Parser::parse_program(code, path.into()) {
            Ok(res) => self
                .func
                .compile_script(res.node, &mut self.id_store, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        };
        res
    }

    pub fn compile_script_with_binding(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        context: Option<LvarCollector>,
    ) -> Result<(FuncId, LvarCollector)> {
        match Parser::parse_program_binding(code, path.into(), context, None) {
            Ok(res) => {
                let collector = res.lvar_collector;
                let fid =
                    self.func
                        .compile_script(res.node, &mut self.id_store, res.source_info)?;
                return Ok((fid, collector));
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        }
    }
}

impl Globals {
    pub(self) fn get_error_message(&self, err: &MonorubyErr) -> String {
        match &err.kind {
            MonorubyErrKind::UndefinedLocal(ident) => {
                format!("undefined local variable or method `{}'", ident)
            }
            MonorubyErrKind::MethodNotFound(name, class) => {
                format!(
                    "undefined method `{}' for {}",
                    self.get_ident_name(*name),
                    class.get_name(self)
                )
            }
            MonorubyErrKind::WrongArguments(name) => name.to_string(),
            MonorubyErrKind::Syntax(kind) => format!("{:?}", kind),
            MonorubyErrKind::Syntax2(msg) => msg.to_string(),
            MonorubyErrKind::Unimplemented(msg) => msg.to_string(),
            MonorubyErrKind::UninitConst(name) => {
                format!("uninitialized constant {}", self.get_ident_name(*name))
            }
            MonorubyErrKind::DivideByZero => format!("divided by 0"),
            MonorubyErrKind::Range(msg) => msg.to_string(),
            MonorubyErrKind::Type(msg) => msg.to_string(),
            MonorubyErrKind::Index(msg) => msg.to_string(),
            MonorubyErrKind::Frozen(msg) => msg.to_string(),
        }
    }
}

impl Globals {
    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self) {
        self.func
            .functions()
            .iter()
            .skip(1)
            .for_each(|info| match &info.kind {
                FuncKind::Normal(_) => info.dump_bc(self),
                _ => {}
            });
    }
}
