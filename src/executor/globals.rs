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
    //ctx: Context,
}

impl Globals {
    pub fn new(id_store: IdentifierTable) -> Self {
        let mut store = Self {
            func: FnStore::new(),
            id_store,
        };
        store.add_builtin_func("puts".to_string(), builtins::puts, 1);
        store.add_builtin_func("assert".to_string(), builtins::assert, 2);
        store
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        *self.get_method(IdentId::_MAIN).unwrap()
    }

    pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.id_store.get_name(id)
    }

    fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    pub fn get_method(&self, name: IdentId) -> Option<&FuncId> {
        self.func.get(name)
    }

    fn add_builtin_func(&mut self, name: String, address: BuiltinFn, arity: usize) -> FuncId {
        let name_id = self.get_ident_id(&name);
        self.func.add_builtin_func(name_id, address, arity)
    }

    pub fn compile_main(&mut self, ast: Node) -> Result<()> {
        self.func.compile_main(ast, &self.id_store)
    }
}
