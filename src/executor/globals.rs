use monoasm::DestLabel;
use ruruby_parse::{
    BinOp, BlockInfo, Loc, LvarCollector, Node, NodeKind, ParamKind, ParseErr, ParseErrKind,
    Parser, SourceInfoRef,
};
use std::io::{stdout, BufWriter, Stdout};
use std::io::{Read, Write};
use std::path::PathBuf;

use super::*;

mod class;
mod compiler;
mod error;
mod functions;
pub use class::*;
pub use compiler::*;
pub use error::*;
pub use functions::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InlineMethod {
    IntegerTof,
    MathSqrt,
    MathCos,
    MathSin,
}

///
/// Global state.
///
pub struct Globals {
    pub codegen: Codegen,
    /// function info.
    pub(crate) func: FnStore,
    /// class table.
    class: ClassStore,
    error: Option<MonorubyErr>,
    /// warning level.
    pub warning: u8,
    /// suppress jit compilation.
    pub no_jit: bool,
    /// stdout.
    stdout: BufWriter<Stdout>,
    /// library directries.
    pub lib_directories: Vec<String>,
    #[cfg(feature = "log-jit")]
    pub deopt_stats: HashMap<(FuncId, usize), usize>,
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool) -> Self {
        let main_object = Value::new_object(OBJECT_CLASS);
        let mut globals = Self {
            codegen: Codegen::new(no_jit, main_object),
            func: FnStore::new(),
            class: ClassStore::new(),
            error: None,
            warning,
            no_jit,
            stdout: BufWriter::new(stdout()),
            lib_directories: vec![],
            #[cfg(feature = "log-jit")]
            deopt_stats: HashMap::default(),
        };
        builtins::init_builtins(&mut globals);
        globals.set_ivar(
            main_object,
            IdentId::_NAME,
            Value::new_string_from_str("main"),
        );
        globals
    }

    pub fn compile_and_run(&mut self, code: &str, path: &std::path::Path) -> Result<Value> {
        let mut executor = Executor::default();
        match executor.eval_script(self, code.to_string(), path) {
            Some(val) => Ok(val),
            None => Err(self.take_error().unwrap()),
        }
    }

    pub(crate) fn flush_stdout(&mut self) {
        self.stdout.flush().unwrap();
    }

    pub(crate) fn write_stdout(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).unwrap();
    }

    pub(crate) fn class_version_inc(&mut self) {
        unsafe { *self.codegen.class_version_addr += 1 }
    }

    pub fn exec_startup(&mut self) {
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../../startup/startup.rb").to_string();
        let mut executor = Executor::default();
        match executor.eval_script(self, code, path) {
            Some(_) => {}
            None => {
                let err = self.take_error().unwrap();
                err.show_error_message_and_all_loc(self);
                panic!("error occured in startup.");
            }
        };
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");
        let description = format!("{pcg_name} {pcg_version} [x86_64-linux]",);
        let val = Value::new_string_from_str(&description);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_DESCRIPTION"), val);
        let val = Value::new_string_from_str(pcg_name);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_ENGINE"), val);
        let val = Value::new_string_from_str(pcg_version);
        self.set_constant(OBJECT_CLASS, IdentId::get_ident_id("RUBY_VERSION"), val);
        self.set_constant(
            OBJECT_CLASS,
            IdentId::get_ident_id("RUBY_ENGINE_VERSION"),
            val,
        );
    }

    pub fn load_lib(&mut self, path: &std::path::Path) -> Option<(String, PathBuf)> {
        for lib in self.lib_directories.clone() {
            let mut lib = std::path::PathBuf::from(lib);
            lib.push(path);
            //if lib.is_absolute() {
            lib.set_extension("rb");
            if lib.exists() {
                return self.load_file(&lib);
            }
            lib.set_extension("so");
            if path.exists() {
                eprintln!("Warning: currently, can not require .so file. {:?}", lib);
            }
            //}
        }
        self.err_cant_load(None, path);
        None
    }

    pub fn load_file(&mut self, path: &std::path::Path) -> Option<(String, PathBuf)> {
        let mut file_body = String::new();
        match std::fs::OpenOptions::new().read(true).open(path) {
            Ok(mut file) => match file.read_to_string(&mut file_body) {
                Ok(_) => {}
                Err(err) => {
                    self.err_cant_load(Some(err), path);
                    return None;
                }
            },
            Err(err) => {
                self.err_cant_load(Some(err), path);
                return None;
            }
        };

        Some((file_body, path.into()))
    }

    ///
    /// ## stack layout for JIT-ed code (just after prologue).
    ///
    ///~~~text
    ///       +-------------+
    /// +0x08 | return addr |
    ///       +-------------+
    ///  0x00 |  prev rbp   | <- rbp
    ///       +-------------+  
    /// -0x08 |    meta     |  
    ///       +-------------+  
    /// -0x10 |     %0      |
    ///       +-------------+
    /// -0x18 |     %1      |
    ///       +-------------+
    ///       |      :      |
    ///       +-------------+
    /// -0xy0 |    %(n-1)   | <- rsp
    ///       +-------------+
    ///       |      :      |
    /// ~~~

    /// ## ABI of JIT-compiled code.
    ///
    /// ### argument registers:
    ///  - rdi: number pf args
    ///
    /// ### global registers:
    ///  - rbx: &mut Interp
    ///  - r12: &mut Globals
    ///  - r13: pc (dummy for JIT-ed code)
    ///
    /// ## stack layout when just after the code is called
    /// ~~~text
    ///       +-------------+
    /// -0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |  (old rbp)  |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |     %0      |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///       |             |
    /// ~~~~
    ///
    ///  - meta and arguments is set by caller.
    ///  - (old rbp) is to be set by callee.
    ///

    pub(super) fn compile_on_demand(&mut self, func_id: FuncId) -> &FuncData {
        //let func = &mut globals.func[func_id];
        if self.func[func_id].data.codeptr.is_none() {
            let codeptr = match self.func[func_id].kind {
                FuncKind::ISeq(_) => {
                    let codeptr = if !self.no_jit {
                        self.codegen.gen_jit_stub()
                    } else {
                        self.codegen.gen_vm_stub()
                    };
                    //func.data.meta.set_jit();
                    codeptr
                }
                FuncKind::Builtin { abs_address } => self.codegen.wrap_native_func(abs_address),
                FuncKind::AttrReader { ivar_name } => self.codegen.gen_attr_reader(ivar_name),
                FuncKind::AttrWriter { ivar_name } => self.codegen.gen_attr_writer(ivar_name),
            };
            self.codegen.jit.finalize();
            self.func[func_id].data.codeptr = Some(codeptr);
        }
        &self.func[func_id].data
    }
}

impl Globals {
    fn array_tos(&self, v: &ArrayInner) -> String {
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
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
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
        if let Some(name) = self.get_ivar(val, IdentId::_NAME) {
            self.val_tos(name)
        } else {
            let mut s = String::new();
            for (id, v) in self.get_ivars(val).into_iter() {
                s += &format!(" {}={}", IdentId::get_name(id), self.val_inspect(v));
            }
            format!(
                "#<{}:0x{:016x}{s}>",
                val.class_id().get_name(self),
                val.rvalue().id()
            )
        }
    }

    fn range_inspect(&self, val: Value) -> String {
        let range = val.as_range();
        format!(
            "{}{}{}",
            self.val_inspect(range.start),
            if range.exclude_end() { "..." } else { ".." },
            self.val_inspect(range.end),
        )
    }

    pub(crate) fn val_tos(&self, val: Value) -> String {
        match val.unpack() {
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => IdentId::get_name(id),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => s,
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match rvalue.kind() {
                ObjKind::CLASS => rvalue.as_class().get_name(self),
                ObjKind::TIME => rvalue.as_time().to_string(),
                ObjKind::ARRAY => self.array_tos(rvalue.as_array()),
                ObjKind::OBJECT => self.object_tos(val),
                _ => format!("{:016x}", val.get()),
            },
        }
    }

    pub(crate) fn val_tobytes(&self, val: Value) -> Vec<u8> {
        if let RV::String(s) = val.unpack() {
            return s.to_vec();
        }
        self.val_tos(val).into_bytes()
    }

    pub(crate) fn val_inspect(&self, val: Value) -> String {
        match val.unpack() {
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Integer(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Symbol(id) => format!(":{}", IdentId::get_name(id)),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => format!("\"{}\"", escape_string::escape(&s)),
                Err(_) => format!("{:?}", s),
            },
            RV::Object(rvalue) => match rvalue.kind() {
                ObjKind::CLASS => rvalue.as_class().get_name(self),
                ObjKind::TIME => rvalue.as_time().to_string(),
                ObjKind::ARRAY => self.array_tos(rvalue.as_array()),
                ObjKind::OBJECT => self.object_inspect(val),
                ObjKind::RANGE => self.range_inspect(val),
                _ => unreachable!(),
            },
        }
    }

    pub fn generate_range(&mut self, start: Value, end: Value, exclude_end: bool) -> Option<Value> {
        if start.get_real_class_id(self) != end.get_real_class_id(self) {
            self.err_bad_range(start, end);
            return None;
        }
        Some(Value::new_range(start, end, exclude_end))
    }

    pub(crate) fn find_method(&mut self, obj: Value, name: IdentId) -> Option<FuncId> {
        let mut class_id = obj.class_id();
        if let Some(func_id) = self.get_method(class_id, name) {
            return Some(func_id);
        }
        while let Some(super_class) = class_id.super_class(self) {
            class_id = super_class;
            if let Some(func_id) = self.get_method(class_id, name) {
                return Some(func_id);
            }
        }
        None
    }

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

    pub(crate) fn check_arg(&mut self, func_id: FuncId, args_len: usize) -> Option<()> {
        let arity = self.func[func_id].arity();
        if arity != -1 && (arity as usize) != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity as usize, args_len));
            return None;
        }
        Some(())
    }

    pub(crate) fn define_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id);
        func_id
    }

    pub(crate) fn define_builtin_func_inlinable(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
        inline_id: InlineMethod,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id);
        self.func.inline.insert(func_id, inline_id);
        func_id
    }

    pub(crate) fn define_builtin_singleton_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let class_id = self.get_singleton_id(class_id);
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id);
        func_id
    }

    pub(crate) fn define_builtin_singleton_func_inlinable(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
        inline_id: InlineMethod,
    ) -> FuncId {
        let class_id = self.get_singleton_id(class_id);
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id);
        self.func.inline.insert(func_id, inline_id);
        func_id
    }

    ///
    /// Define attribute reader for *class_id* and *ivar_name*.
    ///
    pub(crate) fn define_attr_reader(
        &mut self,
        class_id: ClassId,
        method_name: IdentId,
    ) -> IdentId {
        let ivar_name = IdentId::add_ivar_prefix(method_name);
        let method_name_str = IdentId::get_name(method_name);
        let func_id = self.func.add_attr_reader(method_name_str, ivar_name);
        self.add_method(class_id, method_name, func_id);
        self.class_version_inc();
        method_name
    }

    ///
    /// Define attribute writer for *class_id* and *ivar_name*.
    ///
    pub(crate) fn define_attr_writer(
        &mut self,
        class_id: ClassId,
        method_name: IdentId,
    ) -> IdentId {
        let ivar_name = IdentId::add_ivar_prefix(method_name);
        let method_name = IdentId::add_assign_postfix(method_name);
        let method_name_str = IdentId::get_name(method_name);
        let func_id = self.func.add_attr_writer(method_name_str, ivar_name);
        self.add_method(class_id, method_name, func_id);
        self.class_version_inc();
        method_name
    }

    pub fn compile_script(&mut self, code: String, path: impl Into<PathBuf>) -> Result<FuncId> {
        match Parser::parse_program(code, path.into()) {
            Ok(res) => self.func.compile_script(res.node, res.source_info),
            Err(err) => Err(MonorubyErr::parse(err)),
        }
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
                let fid = self.func.compile_script(res.node, res.source_info)?;
                Ok((fid, collector))
            }
            Err(err) => Err(MonorubyErr::parse(err)),
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
                FuncKind::ISeq(_) => info.dump_bc(self),
                _ => {}
            });
    }

    #[cfg(any(feature = "emit-asm"))]
    fn dump_disas(&mut self, sourcemap: Vec<(usize, usize)>, func_id: FuncId) {
        let (start, code_end, end) = self.codegen.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.codegen.jit.select_page(0);
        let dump = self.codegen.jit.dump_code().unwrap();
        //eprintln!("{}", dump);
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                let i = x.find(':').unwrap();
                (
                    match usize::from_str_radix(&x[0..i].trim(), 16) {
                        Ok(i) => i,
                        _ => {
                            panic!("{}", &x[0..i].trim());
                        }
                    },
                    x[28..].to_string(),
                )
            })
            .collect();
        let func = self.func[func_id].as_ruby_func();
        for (i, text) in dump {
            sourcemap
                .iter()
                .filter_map(
                    |(bc_pos, code_pos)| {
                        if *code_pos == i {
                            Some(*bc_pos)
                        } else {
                            None
                        }
                    },
                )
                .for_each(|bc_pos| {
                    let pc = BcPc::from(&func.bytecode()[bc_pos]);
                    eprintln!(
                        ":{:05} {}",
                        bc_pos,
                        match pc.format(self, bc_pos) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
        }
    }
}
