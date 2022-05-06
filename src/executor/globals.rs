use super::*;

//
/// Store of functions.
///
#[derive(Clone, PartialEq)]
pub struct Globals {
    /// Functions.
    pub functions: Vec<FuncInfo>,
    pub func_map: HashMap<IdentId, FuncId>,
    /// identifier table.
    id_store: IdentifierTable,
    //ctx: Context,
}

impl std::ops::Index<FuncId> for Globals {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index.0 as usize]
    }
}

impl std::ops::IndexMut<FuncId> for Globals {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index.0 as usize]
    }
}

impl Globals {
    pub fn new(id_store: IdentifierTable) -> Self {
        let mut store = Self {
            functions: vec![],
            func_map: HashMap::default(),
            id_store,
            //ctx: Context::new(),
        };
        store.add_builtin_func("puts".to_string(), builtins::puts, 1);
        store.add_builtin_func("assert".to_string(), builtins::assert, 2);
        store
    }

    pub fn from_ast(&mut self, ast: Node) -> Result<()> {
        let mut ctx = Context::new();
        ctx.push_remaining(IdentId::_MAIN, vec![], ast);

        let mut irs = vec![];
        #[cfg(debug_assertions)]
        let mut funcs = vec![];
        while let Some(TempInfo {
            temp_id,
            name,
            args,
            ast,
        }) = ctx.remaining.pop()
        {
            let ir = self.compile_func(&mut ctx, temp_id, name, args, ast)?;
            irs.push(ir.1);
            #[cfg(debug_assertions)]
            funcs.push(ir.0);
        }

        for ir in irs {
            match &mut self[ir.func_id].kind {
                FuncKind::Normal(info) => {
                    info.ir_to_bytecode(ir, &ctx.temp_map);
                }
                _ => unreachable!(),
            };
        }
        #[cfg(debug_assertions)]
        for f in funcs {
            self.dump_bytecode(f);
        }
        Ok(())
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        *self.get_method(IdentId::_MAIN).unwrap()
    }

    pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.id_store.get_name(id)
    }

    pub fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.id_store.get_ident_id(name)
    }

    pub fn get_method(&self, name: IdentId) -> Option<&FuncId> {
        self.func_map.get(&name)
    }

    fn add_builtin_func(&mut self, name: String, address: BuiltinFn, arity: usize) -> FuncId {
        let id = FuncId(self.functions.len() as u32);
        let name_id = self.get_ident_id(&name);
        self.func_map.insert(name_id, id);
        self.functions
            .push(FuncInfo::new_builtin(id, name, address, arity));
        id
    }

    fn dump_bytecode(&self, id: FuncId) {
        match &self[id].kind {
            FuncKind::Builtin { .. } => {}
            FuncKind::Normal(info) => {
                info.dump(self);
                eprintln!("------------------------------------")
            }
        }
    }
}

impl Globals {
    /// Generate bytecode Ir in a new function from [(Stmt, Span)].
    fn compile_func(
        &mut self,
        ctx: &mut Context,
        //temp_map: &mut HashMap<usize, FuncId>,
        temp_id: usize,
        name: IdentId,
        args: Vec<IdentId>,
        ast: Node,
    ) -> Result<(FuncId, IrContext)> {
        let func_id = FuncId(self.functions.len() as u32);
        self.func_map.insert(name, func_id);
        let mut info = NormalFuncInfo::new(func_id, args);
        let mut ir = IrContext::new(func_id);
        info.compile_ast(ctx, &mut ir, ast)?;
        let name = self.get_ident_name(name).to_string();
        self.functions.push(FuncInfo::new_normal(name, info));
        ctx.temp_map.insert(temp_id, func_id);
        Ok((func_id, ir))
    }
}
