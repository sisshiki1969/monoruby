use super::*;

impl Globals {
    pub(crate) fn define_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id, Visibility::Public);
        func_id
    }

    pub(crate) fn define_private_builtin_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id, Visibility::Private);
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
        self.add_method(class_id, name_id, func_id, Visibility::Public);
        self.func.add_inline(func_id, inline_id);
        func_id
    }

    pub(crate) fn define_builtin_singleton_func(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        let class_id = self.get_metaclass(class_id).class_id();
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id, Visibility::Public);
        func_id
    }

    /*pub(crate) fn define_builtin_singleton_func_inlinable(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
        inline_id: InlineMethod,
    ) -> FuncId {
        let class_id = self.get_metaclass(class_id).class_id();
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id, Visibility::Public);
        self.func.add_inline(func_id, inline_id);
        func_id
    }*/

    pub(crate) fn define_builtin_module_func_inlinable(
        &mut self,
        class_id: ClassId,
        name: &str,
        address: BuiltinFn,
        arity: i32,
        inline_id: InlineMethod,
    ) -> FuncId {
        let func_id = self.func.add_builtin_func(name.to_string(), address, arity);
        let name_id = IdentId::get_ident_id(name);
        self.add_method(class_id, name_id, func_id, Visibility::Private);
        let class_id = self.get_metaclass(class_id).class_id();
        self.add_method(class_id, name_id, func_id, Visibility::Public);
        self.func.add_inline(func_id, inline_id);
        func_id
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
        let method_name_str = IdentId::get_name(method_name);
        let func_id = self.func.add_attr_reader(method_name_str, ivar_name);
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
        let method_name_str = IdentId::get_name(method_name);
        let func_id = self.func.add_attr_writer(method_name_str, ivar_name);
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
    ) -> Option<()> {
        let entry = match self.check_method_entry(obj, old_name) {
            Some(func) => func,
            None => {
                self.err_method_not_found(old_name, obj);
                return None;
            }
        };
        self.add_method(
            obj.class(),
            new_name,
            entry.func_id.unwrap(),
            entry.visibility,
        );
        Some(())
    }

    ///
    /// Give alias *new_name* to the instance method *old_name* of class *class_id*.
    ///
    pub(crate) fn alias_method_for_class(
        &mut self,
        class_id: ClassId,
        new_name: IdentId,
        old_name: IdentId,
    ) -> Option<()> {
        let entry = self.find_method_entry_for_class(class_id, old_name)?;
        self.add_method(class_id, new_name, entry.func_id.unwrap(), entry.visibility);
        Some(())
    }
}
