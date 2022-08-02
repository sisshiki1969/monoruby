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

    pub fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            id_store: self.id_store.clone(),
            class: self.class.clone(),
            error: None,
            warning: self.warning,
            no_jit: self.no_jit,
            stdout: BufWriter::new(stdout()),
        }
    }

    pub fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write(bytes).unwrap();
    }
}

//
// error handlers
//
impl Globals {
    fn set_error(&mut self, err: MonorubyErr) {
        self.error = Some(err);
    }

    pub fn err_method_not_found(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::method_not_found(name))
    }

    pub fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub fn err_uninitialized_constant(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::uninitialized_constant(name));
    }

    pub fn err_char_out_of_range(&mut self, val: Value) {
        self.set_error(MonorubyErr::range(format!(
            "{} out of char range",
            self.val_tos(val)
        )));
    }

    pub fn err_no_implict_conv(&mut self, actual: ClassId, expect: ClassId) {
        self.set_error(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            actual.get_name(self),
            expect.get_name(self),
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
                ObjKind::Array(v) => match v.len() {
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
                },
                _ => format!("{:016x}", val.get()),
            },
        }
    }

    pub fn val_tobytes(&self, val: Value) -> Vec<u8> {
        match val.unpack() {
            RV::String(s) => return s.clone().into_vec(),
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
                ObjKind::Array(v) => match v.len() {
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
                },
                _ => unreachable!(),
            },
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

    pub fn get_class_obj(&self, class_id: ClassId) -> Value {
        self.class[class_id].get_obj()
    }

    pub fn get_super_class(&self, class_id: ClassId) -> Option<ClassId> {
        self.class[class_id].super_class()
    }

    pub fn define_class_under_obj(&mut self, name: &str) -> Value {
        self.define_class(name, Some(OBJECT_CLASS))
    }

    pub fn define_class(&mut self, name: &str, super_class: impl Into<Option<ClassId>>) -> Value {
        let name_id = self.get_ident_id(name);
        let id = self.class.add_class(super_class.into());
        let class_obj = Value::new_empty_class(id);
        self.class[id].set_class_obj(class_obj);
        self.class[id].set_name(name.to_string());
        self.set_constant(name_id, class_obj);
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

    pub fn get_method_inner(&self, mut class_id: ClassId, name: IdentId) -> Option<FuncId> {
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
        let func_id = match self.get_method_inner(class_id, func_name) {
            Some(id) => id,
            None => {
                self.error = Some(MonorubyErr::method_not_found(func_name));
                return None;
            }
        };
        let arity = self.func[func_id].arity();
        if arity != -1 && (arity as usize) != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity as usize, args_len));
            return None;
        }
        Some(func_id)
    }

    pub fn get_constant(&self, name: IdentId) -> Option<Value> {
        self.class.get_constants(name)
    }

    pub fn get_constant_checked(&mut self, name: IdentId) -> Option<Value> {
        match self.get_constant(name) {
            Some(v) => Some(v),
            None => {
                self.err_uninitialized_constant(name);
                None
            }
        }
    }

    pub fn set_constant(&mut self, name: IdentId, val: Value) -> Option<Value> {
        self.class.set_constants(name, val)
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
    pub(self) fn get_error_message(&self, err: &MonorubyErr) -> String {
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
            MonorubyErrKind::Range(msg) => msg.to_string(),
            MonorubyErrKind::Type(msg) => msg.to_string(),
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
                FuncKind::Normal(info) => info.dump_bc(self),
                _ => {}
            });
    }
}
