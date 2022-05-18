use super::*;

//
/// Store of functions.
///
#[derive(Clone, PartialEq)]
pub struct Globals {
    /// Functions.
    pub func: FnStore,
    /// identifier table.
    pub id_store: IdentifierTable,
}

impl Globals {
    pub fn new(id_store: IdentifierTable) -> Self {
        let mut globals = Self {
            func: FnStore::new(),
            id_store,
        };
        builtins::init_builtins(&mut globals);
        globals
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        self.func.main.unwrap()
    }

    fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    pub fn get_method(&self, name: IdentId) -> Option<FuncId> {
        self.func.get(name).cloned()
    }

    pub fn add_builtin_func(&mut self, name: &str, address: BuiltinFn, arity: usize) -> FuncId {
        let name_id = self.get_ident_id(name);
        self.func.add_builtin_func(name_id, address, arity)
    }

    pub fn compile_main(&mut self, ast: Node) -> Result<()> {
        self.func.compile_main(ast, &self.id_store)
    }
}
