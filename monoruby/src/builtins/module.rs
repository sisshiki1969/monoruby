use super::*;

//
// Module class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Module", MODULE_CLASS);
    // instance methods
    globals.define_builtin_func(MODULE_CLASS, "==", eq, 1);
    globals.define_builtin_func(MODULE_CLASS, "===", teq, 1);
    globals.define_builtin_func(MODULE_CLASS, "<", lt, 1);
    globals.define_builtin_func(MODULE_CLASS, ">", gt, 1);
    globals.define_builtin_func(MODULE_CLASS, "alias_method", alias_method, 2);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_accessor", attr_accessor);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_reader", attr_reader);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_writer", attr_writer);
    globals.define_builtin_func(MODULE_CLASS, "autoload", autoload, 2);
    globals.define_builtin_funcs_eval_with(
        MODULE_CLASS,
        "class_eval",
        &["module_eval"],
        class_eval,
        0,
        2,
        false,
    );
    globals.define_builtin_func_with(MODULE_CLASS, "const_defined?", const_defined, 1, 2, false);
    globals.define_builtin_func_with(MODULE_CLASS, "const_get", const_get, 1, 2, false);
    globals.define_builtin_func(MODULE_CLASS, "const_set", const_set, 2);
    globals.define_builtin_func_with(MODULE_CLASS, "constants", constants, 0, 1, false);
    globals.define_builtin_func_with(MODULE_CLASS, "define_method", define_method, 1, 2, false);
    globals.define_builtin_func_rest(MODULE_CLASS, "deprecate_constant", deprecate_constant);
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "instance_methods",
        instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "include", include);
    globals.define_builtin_func_rest(MODULE_CLASS, "prepend", prepend);
    globals.define_builtin_func(MODULE_CLASS, "prepend_features", prepend_features, 1);
    globals.define_builtin_func(MODULE_CLASS, "instance_method", instance_method, 1);
    globals.define_builtin_func(MODULE_CLASS, "remove_method", remove_method, 1);
    globals.define_builtin_func_with(MODULE_CLASS, "method_defined?", method_defined, 1, 2, false);
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "private_method_defined?",
        private_method_defined,
        1,
        2,
        false,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "private_class_method", private_class_method);
    globals.define_builtin_func(MODULE_CLASS, "to_s", tos, 0);
    // private methods
    globals.define_private_builtin_func_rest(MODULE_CLASS, "module_function", module_function);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "private", private);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "protected", protected);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "public", public);
}

///
/// ### Module#==
///
/// - self == obj -> bool
///
/// []
#[monoruby_builtin]
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let rhs = match lfp.arg(0).is_class_or_module() {
        Some(class) => class.id(),
        None => return Ok(Value::bool(false)),
    };
    let lhs = lfp.self_val().as_class_id();
    Ok(Value::bool(lhs == rhs))
}

fn less_than(lhs: Module, rhs: Module) -> Value {
    if rhs.id() == lhs.id() {
        Value::bool(false)
    } else if rhs.is_ancestor_of(lhs) {
        Value::bool(true)
    } else if lhs.is_ancestor_of(rhs) {
        Value::bool(false)
    } else {
        Value::nil()
    }
}

///
/// ### Module#<
///
/// - self < other -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3c.html]
#[monoruby_builtin]
fn lt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(globals)?;
    let lhs = lfp.self_val().as_class();
    Ok(less_than(lhs, rhs))
}

///
/// ### Module#>
///
/// - self > other -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3e.html]
#[monoruby_builtin]
fn gt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(globals)?;
    let lhs = lfp.self_val().as_class();
    Ok(less_than(rhs, lhs))
}

///
/// ### Module#===
///
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    Ok(Value::bool(lfp.arg(0).is_kind_of(globals, class)))
}

///
/// ### Module#alias_method
///
/// - alias_method(new, original) -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/alias_method.html]
#[monoruby_builtin]
fn alias_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let new_name = lfp.arg(0).expect_symbol_or_string()?;
    let old_name = lfp.arg(1).expect_symbol_or_string()?;
    globals.alias_method_for_class(class_id, new_name, old_name)?;
    Ok(Value::symbol(new_name))
}

///
/// ### Module#attr_accessor
///
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
#[monoruby_builtin]
fn attr_accessor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#attr_reader
///
/// - attr_reader(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_reader.html]
#[monoruby_builtin]
fn attr_reader(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#attr_writer
///
/// - attr_writer(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_writer.html]
#[monoruby_builtin]
fn attr_writer(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#autoload
///
/// - autoload(const_name, feature) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/autoload.html]
#[monoruby_builtin]
fn autoload(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let const_name = lfp.arg(0).expect_symbol_or_string()?;
    let feature = lfp.arg(1).expect_string()?;
    globals
        .store
        .classes
        .set_constant_autoload(lfp.self_val().as_class_id(), const_name, feature);
    Ok(Value::nil())
}

///
/// ### Module#class_eval
///
/// - module_eval(expr, fname = "(eval)", [NOT SUPPORTED] lineno = 1) -> object
/// - module_eval {|mod| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_eval.html]
#[monoruby_builtin]
fn class_eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let module = lfp.self_val().as_class();

    if let Some(bh) = lfp.block() {
        if lfp.try_arg(0).is_some() {
            return Err(MonorubyErr::wrong_number_of_arg(0, lfp.args_count(3)));
        }
        let data = vm.get_block_data(globals, bh)?;
        vm.push_class_context(module.id());
        let res = vm.invoke_block_with_self(globals, &data, module.get(), &[module.get()]);
        vm.pop_class_context();
        res
    } else {
        if let Some(arg0) = lfp.try_arg(0) {
            let expr = arg0.expect_string()?;
            let cfp = vm.cfp();
            let caller_cfp = cfp.prev().unwrap();
            let path = if let Some(arg1) = lfp.try_arg(1) {
                arg1.expect_string()?
            } else {
                "(eval)".into()
            };

            let fid = globals.compile_script_eval(expr, path, caller_cfp)?;
            let proc = ProcInner::from(caller_cfp.lfp(), fid);
            vm.push_class_context(module.id());
            let res = vm.invoke_block_with_self(globals, &proc, module.get(), &[]);
            vm.pop_class_context();
            res
        } else {
            Err(MonorubyErr::wrong_number_of_arg_range(0, 1..=3))
        }
    }
}

///
/// ### Module#const_defined?
///
/// - const_defined?(name, inherit = true) -> bool
///
/// https://docs.ruby-lang.org/ja/latest/method/Module/i/const_defined=3f.html]
#[monoruby_builtin]
fn const_defined(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string()?;
    let module = lfp.self_val().as_class();
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    Ok(Value::bool(
        vm.const_get(globals, module, name, inherit).is_ok(),
    ))
}

///
/// ### Module#const_get
///
/// - const_get(name, inherit = true) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_get.html]
#[monoruby_builtin]
fn const_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string()?;
    let module = lfp.self_val().as_class();
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    vm.const_get(globals, module, name, inherit)
}

///
/// ### Module#const_set
///
/// - const_set(name, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_set.html]
#[monoruby_builtin]
fn const_set(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string()?;
    let module = lfp.self_val().as_class();
    let val = lfp.arg(1);
    globals.store.classes.set_constant(module.id(), name, val);
    Ok(val)
}

///
/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
#[monoruby_builtin]
fn constants(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().expect_class_or_module(globals)?;
    let v = if lfp.try_arg(0).is_none() || lfp.arg(0).as_bool() {
        globals.store.classes.get_constant_names_inherit(class)
    } else {
        globals.store.classes.get_constant_names(class.id())
    };
    let iter = v.into_iter().map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

///
/// ### Module#define_method
/// - define_method(name, method) -> Symbol
/// - define_method(name) { ... } -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/define_method.html]
#[monoruby_builtin]
fn define_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name = lfp.arg(0).expect_symbol_or_string()?;
    let func_id = if let Some(method) = lfp.try_arg(1) {
        if let Some(proc) = method.is_proc() {
            proc.func_id()
        } else if let Some(method) = method.is_method() {
            method.func_id()
        } else {
            return Err(MonorubyErr::typeerr(
                "",
                TypeErrKind::WrongArgumentType {
                    val: method,
                    expected: "Proc/Method/UnboundMethod",
                },
            ));
        }
    } else if let Some(bh) = lfp.block() {
        bh.func_id()
    } else {
        return Err(MonorubyErr::wrong_number_of_arg(2, 1));
    };
    globals.store[func_id].set_method_style();
    globals.add_public_method(class_id, name, func_id);
    Ok(Value::symbol(name))
}

///
/// Module#deprecate_constant
///
/// - deprecate_constant(*name) -> self
///
/// TODO: implement
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/deprecate_constant.html]
#[monoruby_builtin]
fn deprecate_constant(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Module#instance_methods
/// - instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_methods.html]
#[monoruby_builtin]
fn instance_methods(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    let iter = if !inherited_too {
        globals.store.classes.get_method_names(class_id)
    } else {
        globals.store.classes.get_method_names_inherit(class_id)
    }
    .into_iter()
    .map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

///
/// ### Module#include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let class = lfp.self_val().as_class();
    for v in args.iter().cloned().rev() {
        globals.include_module(class, v.expect_module(globals)?)?;
    }
    Ok(lfp.self_val())
}

///
/// ### Module#prepend
/// - prepend(*modules) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/prepend.html]
#[monoruby_builtin]
fn prepend(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let self_ = lfp.self_val();
    for v in args.iter().cloned().rev() {
        vm.invoke_method_inner(
            globals,
            IdentId::get_id("prepend_features"),
            v,
            &[self_],
            None,
        )?;
    }
    Ok(lfp.self_val())
}

///
/// ### Module#prepend_features
/// - prepend_features(mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/prepend_features.html]
#[monoruby_builtin]
fn prepend_features(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let base = lfp.arg(0).expect_class_or_module(globals)?;
    let prepend_module = lfp.self_val().as_class();
    globals.prepend_module(base, prepend_module)?;
    Ok(lfp.self_val())
}

///
/// ### Module#instance_method
///
/// - instance_method(name) -> UnboundMethod
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_method.html]
#[monoruby_builtin]
fn instance_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let klass = lfp.self_val().as_class();
    let method_name = lfp.arg(0).expect_symbol_or_string()?;
    let entry = match globals.check_method_for_class(klass.id(), method_name) {
        Some(entry) => entry,
        None => {
            return Err(MonorubyErr::undefined_method(
                method_name,
                klass.id().get_name_id(globals),
            ))
        }
    };
    let func_id = match entry.func_id() {
        Some(id) => id,
        None => {
            return Err(MonorubyErr::undefined_method(
                method_name,
                klass.id().get_name_id(globals),
            ))
        }
    };
    Ok(Value::new_unbound_method(func_id, entry.owner()))
}

///
/// ### Module#instance_method
///
/// - remove_method(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/remove_method.html]
#[monoruby_builtin]
fn remove_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let func_name = lfp.arg(0).expect_symbol_or_string()?;
    match globals.remove_method(class_id, func_name) {
        Some(_) => Ok(lfp.self_val()),
        None => Err(MonorubyErr::nameerr(format!(
            "method `{}' not defined in {}",
            func_name,
            globals.get_class_name(class_id)
        ))),
    }
}

///
/// ### Module#method_defined?
///
/// - method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/method_defined=3f.html]
#[monoruby_builtin]
fn method_defined(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let func_name = lfp.arg(0).expect_symbol_or_string()?;
    let inherit = if let Some(arg) = lfp.try_arg(1) {
        arg.as_bool()
    } else {
        true
    };
    Ok(Value::bool(
        match globals.method_defined(class_id, func_name, inherit) {
            Some(v) => matches!(v, Visibility::Public | Visibility::Protected),
            None => false,
        },
    ))
}

///
/// ### Module#private_method_defined?
///
/// - private_method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_method_defined=3f.html]
#[monoruby_builtin]
fn private_method_defined(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let func_name = lfp.arg(0).expect_symbol_or_string()?;
    let inherit = if let Some(arg) = lfp.try_arg(1) {
        arg.as_bool()
    } else {
        true
    };
    Ok(Value::bool(
        globals.method_defined(class_id, func_name, inherit) == Some(Visibility::Private),
    ))
}

///
/// ### Module#private_class_method
/// - private_class_method(*name) -> self
/// - private_class_method(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_class_method.html]
#[monoruby_builtin]
fn private_class_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let singleton = globals.store.classes.get_singleton(lfp.self_val());
    let arg = lfp.arg(0).as_array();
    let (_, names) = extract_names(arg)?;
    globals.change_method_visibility_for_class(singleton.id(), &names, Visibility::Private)?;
    Ok(lfp.self_val())
}

///
/// ### Module#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn tos(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_name = globals.get_class_name(lfp.self_val().as_class_id());
    let res = Value::string(class_name);
    Ok(res)
}

///
/// ### Module#module_function
/// - module_function(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/module_function.html]
#[monoruby_builtin]
fn module_function(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let len = arg0.as_array().len();
    if len == 0 {
        vm.set_module_function();
        Ok(Value::nil())
    } else {
        let class_id = lfp.self_val().as_class_id();
        let visi = vm.context_visibility();
        for v in arg0.as_array().iter() {
            let name = v.expect_symbol_or_string()?;
            let func_id = globals
                .find_method_entry_for_class(class_id, name)?
                .func_id()
                .unwrap();
            globals.add_singleton_method(class_id, name, func_id, visi);
        }
        Ok(arg0)
    }
}

///
/// ### Module#private
/// - private(*name) -> self
/// - private(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private.html]
#[monoruby_builtin]
fn private(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = lfp.arg(0).as_array();
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Private)
}

///
/// ### Module#protected
/// - protected(*name) -> self
/// - protected(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected.html]
#[monoruby_builtin]
fn protected(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = lfp.arg(0).as_array();
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Protected)
}

///
/// ### Module#public
/// - public(*name) -> self
/// - public(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public.html]
#[monoruby_builtin]
fn public(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = lfp.arg(0).as_array();
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Public)
}

fn change_visi(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Array,
    visi: Visibility,
) -> Result<Value> {
    if arg.len() == 0 {
        vm.set_context_visibility(visi);
        return Ok(Value::nil());
    }
    let (res, names) = extract_names(arg)?;
    let class_id = self_val.as_class_id();
    globals.change_method_visibility_for_class(class_id, &names, visi)?;
    Ok(res)
}

fn extract_names(arg: Array) -> Result<(Value, Vec<IdentId>)> {
    if arg.len() == 1 {
        let mut names = vec![];
        if let Some(ary) = arg[0].try_array_ty() {
            for v in ary.iter() {
                names.push(v.expect_symbol_or_string()?);
            }
        } else {
            names.push(arg[0].expect_symbol_or_string()?);
        }
        Ok((arg[0], names))
    } else {
        let mut names = vec![];
        for v in arg.iter() {
            names.push(v.expect_symbol_or_string()?);
        }
        Ok((arg.as_val(), names))
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_teq() {
        run_test("Integer === 4");
        run_test("Integer === 4.5");
        run_test("Integer === 'Ruby'");
        run_test("Float === 4.5");
        run_test("Float === 'Ruby'");
        run_test(
            r#"
        class C
        end
        C === C.new"#,
        );
        run_test(
            r#"
        class S
        end
        class C < S
        end
        S === C.new"#,
        );
    }

    #[test]
    fn compare() {
        run_test(
            r#"
        module Foo
        end
        class Bar
          include Foo
        end
        class Baz < Bar
        end
        class Qux
        end
        res = []
        res << (Bar < Foo)     # => true
        res << (Baz < Bar)     # => true
        res << (Baz < Foo)     # => true
        res << (Baz < Qux)     # => nil
        res << (Baz > Qux)     # => nil
        res
        "#,
        );
        run_test_error(
            r##"
        module Foo; end
        Foo < Object.new
        "##,
        )
    }

    #[test]
    fn attr_reader() {
        run_test(
            r#"
        c = class A
          def f(x,y)
            @v = x
            @w = y
          end
          attr_reader :v, "w"
        end
        a = A.new
        a.f(7,11)
        [a.v, a.w, c]
        "#,
        );
    }

    #[test]
    fn attr_writer() {
        run_test(
            r#"
        c = class A
          def f(x,y)
            @v = x
            @w = y
          end
          attr_writer :v, "w"
          attr_reader :v, "w"
        end
        a = A.new
        a.v = 7
        a.w = 11
        [a.v, a.w, c]
        "#,
        );
    }

    #[test]
    fn attr_accessor() {
        run_test(
            r#"
        c = class A
          def f(x,y)
            @v = x
            @w = y
          end
          attr_accessor :v, "w"
        end
        a = A.new
        a.v = 7
        a.w = 11
        [a.v, a.w, c]
        "#,
        );
    }

    #[test]
    fn attr_accessor_assign_op() {
        run_test(
            r#"
        class A
          attr_accessor :v
        end
        a = A.new
        a.v = 7
        a.v *= 3
        a.v
        "#,
        );
    }

    #[test]
    fn define_method() {
        run_test_with_prelude(
            r#"
            C.new.foo(17)
        "#,
            r#"
            class C
              def bar
                "bar"
              end
              p = Proc.new { |a| a * 100 }
              define_method "foo", p
            end
            "#,
        );
        run_test_with_prelude(
            r#"
            3.baz(4)
        "#,
            r#"
            class Integer
              define_method "baz" do |other|
                self * other
              end
            end
            "#,
        );
    }

    #[test]
    fn module_eval() {
        run_test_with_prelude(
            r#"
        a = 1
        C.class_eval %Q{
          def m                   # メソッドを動的に定義できる。
            return :m, #{a}, self.class
          end
        }
        C.new.m
        "#,
            r#"
        class C
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        a = 1
        C.class_eval do |m|
            def f
                self.class
            end
        end
        C.new.f
        "#,
            r#"
        class C
        end
        "#,
        );
    }

    #[test]
    fn module_function() {
        run_test_with_prelude(
            "[M.c]",
            r#"
            module M
              def a; 1; end
              def b; 2; end
              module_function
              def c; 3; end
            end
            "#,
        );
        run_test_with_prelude(
            "[M.a, M.b]",
            r#"
            module M
              def a; 1; end
              def b; 2; end
              def c; 3; end
              module_function :a, "b"
            end
            "#,
        );
        run_test(
            r#"
            module M
              def a; 1; end
              def b; 2; end
              module_function :a, "b"
            end
            "#,
        );
        run_test_error(
            r#"
            module M
              module_function 100
            end
            "#,
        );
    }

    #[test]
    fn include() {
        run_test_with_prelude(
            "C.new.f",
            r#"
            module M1
              def f; "M1"; end
            end
            module M2
              def f; "M2"; end
            end
            class C
              include M1, M2 
            end
            "#,
        );
        run_test_with_prelude(
            r#"
            $res = []
            C.new.f
            $res
            "#,
            r#"
        module M1
          def f
            $res << "M1"
          end
        end

        module M2
          def f
            $res << "M2"
            super
          end
        end

        class C
          include M1
          include M2
          def f
            $res << "C"
            super
          end
        end
        "#,
        );
        run_test_with_prelude(
            r#"
            $res = []
            C.new.f
            $res
            "#,
            r#"
        module M1
          def f
            $res << "M1"
          end
        end

        module M2
          def f
            $res << "M2"
            super
          end
        end

        class C
          include M2,M1
          def f
            $res << "C"
            super
          end
        end
        "#,
        );
        run_test_with_prelude(
            r#"
            C.new.f
            "#,
            r#"
            module M
              def f
                "M"
              end
            end

            module M1
              include M
            end

            module M2
              include M1
            end

            class C
              include M2
            end
        "#,
        );
        run_test_error(
            r#"
            module M
            end
            module M2
              include M
            end
            module M
              include M2
            end
            "#,
        );
        run_test_error(
            r#"
            class M; end
            class C
              include M 
            end
            "#,
        );
        run_test_error(
            r#"
            class C
              include
            end
            "#,
        );
    }

    #[test]
    fn prepend() {
        run_test(
            r##"
        class Recorder
          RECORDS = []
        end

        module X
          def self.prepend_features(mod)
            Recorder::RECORDS << mod
          end
        end

        class A
          prepend X
        end

        class B
          include X
        end

        class C
          prepend X
        end

        Recorder::RECORDS
        "##,
        );
        run_test_with_prelude(
            r##"
            $res = []
            a = A.new
            a.foo
            # (1x) (2x)(ここの super で A#foo を呼びだす) (1a) (3x) の順に実行される
            # >> X1
            # >> A
            # >> X2
            $res
            "##,
            r##"
            # super と prepend の組み合わせの例
            module X
              def foo
                $res << "X1" # (1x)
                super # (2x)
                $res << "X2" # (3x)
              end
            end

            class A
              prepend X

              def foo
                $res << "A" #(1a)
              end
            end
        "##,
        );
        run_test_error(
            r#"
            module M
            end
            module M2
              prepend M
            end
            module M
              prepend M2
            end
            "#,
        );
    }

    #[test]
    fn constants() {
        run_test_with_prelude(
            r#"
            [C.constants.sort!, C.constants(false).sort!]
            "#,
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
        "#,
        );
    }

    #[test]
    fn instance_methods() {
        run_test_with_prelude(
            r#"
            [(C.instance_methods - S.instance_methods).sort!, C.instance_methods(false).sort!]
            "#,
            r#"
            class S
              def s; end
            end
            class C < S
              def c1; end
              def c2; end
            end
        "#,
        );
    }

    #[test]
    fn remove_method() {
        run_test(
            r#"
            class S
              def f; end
            end
            class C < S
            end
            S.remove_method(:f)
            "#,
        );
        run_test_error(
            r#"
            class S
              def f; end
            end
            class C < S
            end
            C.remove_method(:f)
            "#,
        );
        run_test_error(
            r#"
            class S
              def f; end
            end
            class C < S
            end
            S.remove_method(:f)
            S.new.f
            "#,
        );
    }

    #[test]
    fn const_defined() {
        run_test_with_prelude(
            r#"
            [C.const_defined?(:C1), C.const_defined?(:C3), C.const_defined?(:S1), C.const_defined?(:S1, true), C.const_defined?(:S1, false)]
            "#,
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
        "#,
        );
    }

    #[test]
    fn const_get() {
        run_test_with_prelude(
            r#"
            C.const_get(:S1)
            "#,
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
        "#,
        );
        run_test_error(
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
            C.const_get(:S1, false)
            "#,
        );
    }

    #[test]
    fn const_set() {
        run_test_with_prelude(
            r#"
            res = []
            res << Foo.const_set(:FOO, 123)
            res << Foo::FOO
            res << Foo.const_set('BAR', 'abc')
            res << Foo::BAR
            res << Foo.const_set('BAR', '123')
            res
            "#,
            r#"
            module Foo; end
        "#,
        );
    }

    #[test]
    fn private() {
        run_test_with_prelude(
            r#"
            C.new.g
            "#,
            r#"
            class C
              private def f; 5; end
              def g; f; end
            end
        "#,
        );
        run_test_error(
            r#"
            class C
              private def f; 5; end
              def g; f; end
            end
            C.new.f
        "#,
        );
    }

    #[test]
    fn private_class_method() {
        run_test_error(
            r#"
            module Foo
                def self.foo; end
            end
            Foo.private_class_method(:foo)
            Foo.foo
            "#,
        )
    }

    #[test]
    fn public() {
        run_test_with_prelude(
            r#"
            D.new.f
            "#,
            r#"
            class C
              private def f; 5; end
            end

            class D < C
              public :f
            end
        "#,
        );
        run_test_with_prelude(
            r#"
            $res = []
            class C
                $res << public(:f)
                $res << public("f")
                $res << public
                $res << public(["f"])
                $res << public(:f, :g, :h)
            end
            $res
            "#,
            r#"
            class C
              def f; end
              def g; end
              def h; end
            end
        "#,
        );
        run_test_error(r#"class C; public(1); end"#);
        run_test_error(r#"class C; public(:k); end"#);
        run_test_error(r#"class C; public(1,2); end"#);
        run_test_error(r#"class C; public([1,2]); end"#);
    }

    #[test]
    fn alias_method() {
        run_test_once(
            r#"
            class String
              alias_method :foo, :upcase
            end
            "abdG&0[]nim".foo
            "#,
        );
    }

    #[test]
    fn method_defined() {
        run_test_once(
            r##"
        module A
          def method1()  end
          def protected_method1()  end
          protected :protected_method1
        end
        class B
          def method2()  end
          def private_method2()  end
          private :private_method2
        end
        class C < B
          include A
          def method3()  end
        end
        x = []
        x << A.method_defined?(:method1)                        #=> true
        x << A.private_method_defined?(:method1)                #=> false
        x << C.method_defined?("method1")                       #=> true
        x << C.private_method_defined?("method1")               #=> false
        x << C.method_defined?("method2")                       #=> true
        x << C.method_defined?("method2", true)                 #=> true
        x << C.method_defined?("method2", false)                #=> false
        x << C.method_defined?("method3")                       #=> true
        x << C.method_defined?("protected_method1")             #=> true
        x << C.method_defined?("method4")                       #=> false
        x << C.method_defined?("private_method2")               #=> false
        x << C.private_method_defined?("private_method2")       #=> true
        x << C.private_method_defined?("private_method2", true) #=> true
        x << C.private_method_defined?("private_method2", false)#=> false
        x << B.private_method_defined?("private_method2", true) #=> true
        x << B.private_method_defined?("private_method2", false)#=> true
        x
        "##,
        )
    }
}
