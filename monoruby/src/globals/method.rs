use super::*;

// Define builtin functions.

impl Globals {
    fn new_builtin_fn(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        visi: Visibility,
        min: usize,
        max: usize,
        rest: bool,
        kw: &[&str],
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func(name, address, min, max, rest, kw);
        let method_name = IdentId::get_id(name);
        self.gen_wrapper(func_id);
        self.add_method(class_id, method_name, func_id, visi);
        func_id
    }

    fn new_builtin_fn_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        visi: Visibility,
    ) -> FuncId {
        self.new_builtin_fn(class_id, name, address, visi, 0, 0, true, &[])
    }

    fn new_basic_op(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        visi: Visibility,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let func_id = self
            .store
            .add_basic_op(name.to_string(), address, min, max, rest);
        let method_name = IdentId::get_id(name);
        self.gen_wrapper(func_id);
        self.add_basic_op_method(class_id, method_name, func_id, visi);
        func_id
    }

    fn new_builtin_fn_eval(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        visi: Visibility,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func_eval(name.to_string(), address, min, max, rest);
        let method_name = IdentId::get_id(name);
        self.gen_wrapper(func_id);
        self.add_method(class_id, method_name, func_id, visi);
        func_id
    }

    fn new_builtin_fns(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        visi: Visibility,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let fid = self.new_builtin_fn(class_id, name, address, visi, min, max, rest, &[]);
        for alias in alias {
            self.add_method(class_id, IdentId::get_id(alias), fid, visi);
        }
        fid
    }

    fn new_builtin_fns_eval(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        visi: Visibility,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let fid = self.new_builtin_fn_eval(class_id, name, address, visi, min, max, rest);
        for alias in alias {
            self.add_method(class_id, IdentId::get_id(alias), fid, visi);
        }
        fid
    }

    fn new_builtin_module_fn(&mut self, class_id: ClassId, name: &str, func_id: FuncId) -> FuncId {
        let method_name = IdentId::get_id(name);
        self.gen_wrapper(func_id);
        self.add_method(class_id, method_name, func_id, Visibility::Private);
        let class_id = self.store.get_metaclass(class_id).id();
        self.add_public_method(class_id, method_name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            arg_num,
            arg_num,
            false,
            &[],
        )
    }

    pub(crate) fn define_basic_op(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        self.new_basic_op(
            class_id,
            name,
            address,
            Visibility::Public,
            arg_num,
            arg_num,
            false,
        )
    }

    pub(crate) fn define_builtin_funcs(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        self.new_builtin_fns(
            class_id,
            name,
            alias,
            address,
            Visibility::Public,
            arg_num,
            arg_num,
            false,
        )
    }

    pub(crate) fn define_builtin_funcs_eval_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.new_builtin_fns_eval(
            class_id,
            name,
            alias,
            address,
            Visibility::Public,
            min,
            max,
            rest,
        )
    }

    pub(crate) fn define_builtin_func_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
    ) -> FuncId {
        self.new_builtin_fn_rest(class_id, name, address, Visibility::Public)
    }

    pub(crate) fn define_builtin_funcs_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
    ) -> FuncId {
        self.new_builtin_fns(
            class_id,
            name,
            alias,
            address,
            Visibility::Public,
            0,
            0,
            true,
        )
    }

    pub(crate) fn define_builtin_func_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            min,
            max,
            rest,
            &[],
        )
    }

    pub(crate) fn define_builtin_funcs_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.new_builtin_fns(
            class_id,
            name,
            alias,
            address,
            Visibility::Public,
            min,
            max,
            rest,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn define_builtin_func_with_kw(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw_names: &[&str],
    ) -> FuncId {
        self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            min,
            max,
            rest,
            kw_names,
        )
    }

    pub(crate) fn define_private_builtin_func_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
    ) -> FuncId {
        self.new_builtin_fn_rest(class_id, name, address, Visibility::Private)
    }

    pub(crate) fn define_builtin_module_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func(name, address, arg_num, arg_num, false, &[]);
        self.new_builtin_module_fn(class_id, name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_module_func_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
    ) -> FuncId {
        let func_id = self.store.add_builtin_func(name, address, 0, 0, true, &[]);
        self.new_builtin_module_fn(class_id, name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_module_func_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func(name, address, min, max, rest, &[]);
        self.new_builtin_module_fn(class_id, name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_module_func_with_kw(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw: &[&str],
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func(name, address, min, max, rest, kw);
        self.new_builtin_module_fn(class_id, name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_module_func_eval_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let func_id = self
            .store
            .add_builtin_func_eval(name.to_string(), address, min, max, rest);
        self.new_builtin_module_fn(class_id, name, func_id);
        func_id
    }

    pub(crate) fn define_builtin_inline_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        arg_num: usize,
    ) -> FuncId {
        let fid = self.define_builtin_func(class_id, name, address, arg_num);
        let info = inline::InlineFuncInfo { inline_gen };
        self.store.inline_info.add_inline(fid, info);
        fid
    }

    pub(crate) fn define_builtin_inline_funcs(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        arg_num: usize,
    ) -> FuncId {
        let fid = self.define_builtin_funcs(class_id, name, alias, address, arg_num);
        let info = inline::InlineFuncInfo { inline_gen };
        self.store.inline_info.add_inline(fid, info);
        fid
    }

    pub(crate) fn define_builtin_inline_func_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let fid = self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            min,
            max,
            rest,
            &[],
        );
        let info = inline::InlineFuncInfo { inline_gen };
        self.store.inline_info.add_inline(fid, info);
        fid
    }

    pub(crate) fn define_builtin_inline_funcs_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let fid = self
            .define_builtin_inline_func_with(class_id, name, address, inline_gen, min, max, rest);
        for alias in alias {
            self.add_method(class_id, IdentId::get_id(alias), fid, Visibility::Public);
        }
        fid
    }

    pub(crate) fn define_builtin_class_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_func(class_id, name, address, arg_num)
    }

    pub(crate) fn define_builtin_class_funcs(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_funcs(class_id, name, alias, address, arg_num)
    }

    pub(crate) fn define_builtin_class_func_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.new_builtin_fn_rest(class_id, name, address, Visibility::Public)
    }

    pub(crate) fn define_builtin_class_func_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            min,
            max,
            rest,
            &[],
        )
    }

    pub(crate) fn define_builtin_class_func_with_kw(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw_names: &[&str],
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.new_builtin_fn(
            class_id,
            name,
            address,
            Visibility::Public,
            min,
            max,
            rest,
            kw_names,
        )
    }

    pub(crate) fn define_builtin_class_funcs_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.new_builtin_fns(
            class_id,
            name,
            alias,
            address,
            Visibility::Public,
            min,
            max,
            rest,
        )
    }

    /*pub(crate) fn define_builtin_class_inline_func_with(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        max: usize,
        min: usize,
        rest: bool,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_inline_func_with(class_id, name, address, inline_gen, min, max, rest)
    }*/

    pub(crate) fn define_builtin_class_inline_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        arg_num: usize,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_inline_func_with(
            class_id, name, address, inline_gen, arg_num, arg_num, false,
        )
    }

    pub(crate) fn define_builtin_class_inline_func_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_inline_func_with(class_id, name, address, inline_gen, 0, 0, true)
    }

    pub(crate) fn define_builtin_class_inline_funcs_rest(
        &mut self,
        class_id: ClassId,
        name: &str,
        alias: &[&str],
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
    ) -> FuncId {
        let class_id = self.store.get_metaclass(class_id).id();
        self.define_builtin_inline_funcs_with(
            class_id, name, alias, address, inline_gen, 0, 0, true,
        )
    }

    pub(crate) fn define_builtin_singleton_func(
        &mut self,
        obj: Value,
        name: &str,
        address: BuiltinFn,
        arg_num: usize,
    ) -> FuncId {
        let class_id = self.store.get_singleton(obj).id();
        self.define_builtin_func(class_id, name, address, arg_num)
    }

    pub(crate) fn define_builtin_singleton_func_with(
        &mut self,
        obj: Value,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        let class_id = self.store.get_singleton(obj).id();
        self.define_builtin_func_with(class_id, name, address, min, max, rest)
    }

    pub(crate) fn define_builtin_module_inline_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        inline_gen: Box<InlineGen>,
        arg_num: usize,
    ) -> FuncId {
        let fid = self.define_builtin_module_func(class_id, name, address, arg_num);
        let info = inline::InlineFuncInfo { inline_gen };
        self.store.inline_info.add_inline(fid, info);
        fid
    }

    ///
    /// Define attribute reader for *class_id* and *ivar_name*.
    ///
    pub(crate) fn define_attr_reader(
        &mut self,
        class_id: ClassId,
        method_name: IdentId,
        visi: Visibility,
    ) -> IdentId {
        let ivar_name = IdentId::add_ivar_prefix(method_name);
        let func_id = self.store.add_attr_reader(method_name, ivar_name);
        self.gen_wrapper(func_id);
        self.add_method(class_id, method_name, func_id, visi);
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
        visi: Visibility,
    ) -> IdentId {
        let ivar_name = IdentId::add_ivar_prefix(method_name);
        let method_name = IdentId::add_assign_postfix(method_name);
        let func_id = self.store.add_attr_writer(method_name, ivar_name);
        self.gen_wrapper(func_id);
        self.add_method(class_id, method_name, func_id, visi);
        self.class_version_inc();
        method_name
    }

    ///
    /// Give alias *new_name* to the method *old_name* for object *obj*.
    ///
    pub(crate) fn alias_method(
        &mut self,
        obj: Value,
        new_name: IdentId,
        old_name: IdentId,
    ) -> Result<()> {
        let class_id = obj.class();
        let entry = match self.check_method_for_class(class_id, old_name) {
            Some(func) => func,
            None => return Err(MonorubyErr::method_not_found(old_name, obj)),
        };
        self.add_method(
            obj.class(),
            new_name,
            entry.func_id().unwrap(),
            entry.visibility,
        );
        Ok(())
    }

    ///
    /// Give alias *new_name* to the instance method *old_name* of class *class_id*.
    ///
    pub(crate) fn alias_method_for_class(
        &mut self,
        class_id: ClassId,
        new_name: IdentId,
        old_name: IdentId,
    ) -> Result<()> {
        let entry = self.find_method_entry_for_class(class_id, old_name)?;
        self.add_method(
            class_id,
            new_name,
            entry.func_id().unwrap(),
            entry.visibility,
        );
        Ok(())
    }
}
