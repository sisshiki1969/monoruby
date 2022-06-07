use std::path::PathBuf;

use super::*;

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
#[derive(Clone, PartialEq)]
pub struct Globals {
    /// Functions.
    pub func: FnStore,
    /// identifier table.
    pub id_store: IdentifierTable,
    /// class table.
    pub class: ClassStore,
}

impl Globals {
    pub fn new() -> Self {
        let mut globals = Self {
            func: FnStore::new(),
            id_store: IdentifierTable::new(),
            class: ClassStore::new(),
        };
        builtins::init_builtins(&mut globals);
        assert_eq!(NIL_CLASS, globals.class.add_class());
        assert_eq!(TRUE_CLASS, globals.class.add_class());
        assert_eq!(FALSE_CLASS, globals.class.add_class());
        assert_eq!(INTEGER_CLASS, globals.class.add_class());
        assert_eq!(FLOAT_CLASS, globals.class.add_class());
        assert_eq!(STRING_CLASS, globals.class.add_class());
        globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
        globals
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        self.func.main.unwrap()
    }

    pub fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    /*pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.id_store.get_name(id)
    }*/

    pub fn get_method(&self, class_id: u32, name: IdentId) -> Option<FuncId> {
        if let Some(func_id) = self.class.get_method(class_id, name) {
            return Some(func_id);
        }
        self.class.get_method(0, name)
    }

    pub fn define_global_builtin_func(
        &mut self,
        name: &str,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        self.define_builtin_func(0, name, address, arity)
    }

    pub fn define_builtin_func(
        &mut self,
        class_id: u32,
        name: &str,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        let name_id = self.get_ident_id(name);
        let func_id = self.func.add_builtin_func(name_id, address, arity);
        self.class.add_method(class_id, name_id, func_id);
        func_id
    }

    pub fn compile_script(
        &mut self,
        code: String,
        path: impl Into<PathBuf>,
        context_name: &str,
    ) -> Result<()> {
        let id_table = std::mem::take(&mut self.id_store);
        let res = match Parser::parse_program(code, path.into(), context_name, id_table) {
            Ok(res) => {
                self.id_store = res.id_store;
                self.func
                    .compile_script(res.node, &self.id_store, res.source_info)
            }
            Err(err) => Err(MonorubyErr::parse(err)),
        };
        res
    }
}

#[derive(Clone, PartialEq)]
pub struct ClassStore {
    classes: Vec<HashMap<IdentId, FuncId>>,
}

impl ClassStore {
    fn new() -> Self {
        Self {
            classes: vec![HashMap::default()],
        }
    }

    pub fn add_class(&mut self) -> u32 {
        let id = self.classes.len();
        self.classes.push(HashMap::default());
        id as u32
    }

    pub fn add_method(&mut self, class_id: u32, name: IdentId, func: FuncId) {
        self.classes[class_id as usize].insert(name, func);
    }

    pub fn get_method(&self, class_id: u32, name: IdentId) -> Option<FuncId> {
        self.classes[class_id as usize].get(&name).cloned()
    }
}
