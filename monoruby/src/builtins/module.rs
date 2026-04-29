use super::*;

//
// Module class
//

pub(super) fn init(globals: &mut Globals) {
    // Module.allocate raises (CRuby behavior); Module.new uses an internal
    // allocator inside `module_new`, not Class#new. Class subclasses Module
    // and reinstalls its own alloc_func in `class.rs::init`.
    globals.store[MODULE_CLASS].clear_alloc_func();
    // class methods
    globals.define_builtin_class_func_with_kw(
        MODULE_CLASS,
        "new",
        module_new,
        0,
        0,
        true,
        &[],
        true,
    );
    // Default `Module#initialize`: yields to the block. Private, so it's
    // only callable via Module.new/allocate+initialize. Subclasses of
    // Module (e.g. AcceptsMultiparameterTime in ActiveModel) override
    // this; `module_new` above invokes whichever `initialize` is in
    // effect for the receiver class.
    globals.define_private_builtin_func_with_kw(
        MODULE_CLASS,
        "initialize",
        module_initialize,
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_class_func(MODULE_CLASS, "nesting", module_nesting, 0);

    // instance methods
    globals.define_builtin_func(MODULE_CLASS, "==", eq, 1);
    globals.define_builtin_func(MODULE_CLASS, "===", teq, 1);
    globals.define_builtin_func(MODULE_CLASS, "<", lt, 1);
    globals.define_builtin_func(MODULE_CLASS, ">", gt, 1);
    globals.define_builtin_func(MODULE_CLASS, "<=", le, 1);
    globals.define_builtin_func(MODULE_CLASS, ">=", ge, 1);
    globals.define_builtin_func(MODULE_CLASS, "singleton_class?", singleton_class_, 0);
    globals.define_builtin_func(MODULE_CLASS, "alias_method", alias_method, 2);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr", attr);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_accessor", attr_accessor);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_reader", attr_reader);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_writer", attr_writer);
    globals.define_builtin_func(MODULE_CLASS, "autoload", autoload, 2);
    globals.define_builtin_func_with(MODULE_CLASS, "autoload?", autoload_query, 1, 2, false);
    globals.define_builtin_funcs_with_effect(
        MODULE_CLASS,
        "class_eval",
        &["module_eval"],
        class_eval,
        0,
        3,
        false,
        Effect::EVAL,
    );
    globals.define_builtin_funcs_with_effect(
        MODULE_CLASS,
        "class_exec",
        &["module_exec"],
        class_exec,
        0,
        0,
        true,
        Effect::EVAL,
    );
    globals.define_builtin_func(MODULE_CLASS, "ancestors", ancestors, 0);
    globals.define_builtin_func(MODULE_CLASS, "subclasses", subclasses, 0);
    globals.define_builtin_func_with(MODULE_CLASS, "const_defined?", const_defined, 1, 2, false);
    globals.define_builtin_func_with(MODULE_CLASS, "const_get", const_get, 1, 2, false);
    globals.define_builtin_func(MODULE_CLASS, "const_set", const_set, 2);
    globals.define_private_builtin_func(MODULE_CLASS, "remove_const", remove_const, 1);
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "const_source_location",
        const_source_location,
        1,
        2,
        false,
    );
    globals.define_builtin_func_with(MODULE_CLASS, "constants", constants, 0, 1, false);
    globals.define_builtin_funcs_with_effect(
        MODULE_CLASS,
        "define_method",
        &[],
        define_method,
        1,
        2,
        false,
        Effect::CAPTURE,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "instance_methods",
        instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "private_instance_methods",
        private_instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "protected_instance_methods",
        protected_instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "public_instance_methods",
        public_instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "include", include);
    globals.define_private_builtin_func(MODULE_CLASS, "append_features", append_features, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "extend_object", extend_object, 1);
    globals.define_builtin_func_rest(MODULE_CLASS, "prepend", prepend);
    globals.define_private_builtin_func(MODULE_CLASS, "prepend_features", prepend_features, 1);
    globals.define_builtin_func(MODULE_CLASS, "instance_method", instance_method, 1);
    // `public_instance_method` is identical to `instance_method` except it
    // raises NameError when the resolved method is private or protected.
    globals.define_builtin_func(
        MODULE_CLASS,
        "public_instance_method",
        public_instance_method,
        1,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "remove_method", remove_method);
    globals.define_builtin_func_rest(MODULE_CLASS, "undef_method", undef_method);
    globals.define_builtin_func_with(MODULE_CLASS, "method_defined?", method_defined, 1, 2, false);
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "private_method_defined?",
        private_method_defined,
        1,
        2,
        false,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "public_method_defined?",
        public_method_defined,
        1,
        2,
        false,
    );
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "protected_method_defined?",
        protected_method_defined,
        1,
        2,
        false,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "private_class_method", private_class_method);
    globals.define_builtin_func_rest(MODULE_CLASS, "public_class_method", public_class_method);
    globals.define_builtin_func(MODULE_CLASS, "class_variable_set", class_variable_set, 2);
    globals.define_builtin_func(MODULE_CLASS, "class_variable_get", class_variable_get, 1);
    globals.define_builtin_func(
        MODULE_CLASS,
        "class_variable_defined?",
        class_variable_defined,
        1,
    );
    globals.define_builtin_func(
        MODULE_CLASS,
        "remove_class_variable",
        remove_class_variable,
        1,
    );
    globals.define_builtin_func_with(MODULE_CLASS, "class_variables", class_variables, 0, 1, false);
    globals.define_builtin_func(MODULE_CLASS, "included_modules", included_modules, 0);
    globals.define_builtin_func_rest(MODULE_CLASS, "public_constant", public_constant);
    globals.define_builtin_func_rest(MODULE_CLASS, "private_constant", private_constant);
    // `used_refinements` (both class and instance forms) is mocked in
    // Ruby — see `Module#used_refinements` / `Module.used_refinements`
    // in `builtins/startup.rb`. Both return `[]` since refinements
    // aren't implemented; defining in Ruby lets specs that actually
    // exercise refinements override the implementation.
    globals.define_builtin_funcs(MODULE_CLASS, "to_s", &["inspect"], tos, 0);
    globals.define_builtin_func(MODULE_CLASS, "name", name, 0);
    globals.define_builtin_func(MODULE_CLASS, "set_temporary_name", set_temporary_name, 1);
    // private methods
    globals.define_private_builtin_func_rest(MODULE_CLASS, "module_function", module_function);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "private", private);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "protected", protected);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "public", public);
    // hook methods (no-op by default, overridden by startup.rb)
    globals.define_private_builtin_func(MODULE_CLASS, "method_added", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "method_removed", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "method_undefined", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "included", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "prepended", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "extended", module_noop_hook, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "const_added", module_noop_hook, 1);
}

/// No-op hook for method_added/removed/undefined, included/prepended/extended, const_added.
/// Required during bootstrap before startup.rb overrides them.
#[monoruby_builtin]
fn module_noop_hook(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

/// ### Module.new
/// - new -> Module
/// - new {|mod| ... } -> Module
///
/// Also handles subclasses of Module (e.g. `class Foo < Module`). In that
/// case the receiver is the subclass, the returned value is an instance of
/// the subclass (its header class is the subclass's id), and the subclass's
/// `#initialize` is invoked with the forwarded arguments and block.
/// ActiveModel's `AcceptsMultiparameterTime < Module` relies on this.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/s/new.html]
#[monoruby_builtin]
fn module_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_class_id = lfp.self_val().as_class_id();
    let module = globals.store.define_unnamed_module();
    let mut module_val = module.as_val();
    // When called on a subclass of Module, re-tag the header so `#class`
    // reports the subclass rather than the plain `Module` class.
    if self_class_id != MODULE_CLASS {
        module_val.change_class(self_class_id);
    }
    // Invoke `#initialize` so subclasses that override it (with args and
    // block) run their setup. For plain `Module.new`, the default
    // initialize is a no-op that just yields to the block, which the
    // builtin `module_initialize` below emulates.
    //
    // Signature: rest=true, kw_rest=true → lfp.arg(0) is the rest array,
    // lfp.arg(1) is the kwargs hash (matching `Class#new`'s convention).
    let args = lfp.arg(0).as_array();
    let kw = if let Some(kw) = lfp.try_arg(1)
        && let Some(kw) = kw.try_hash_ty()
        && !kw.is_empty()
    {
        Some(kw)
    } else {
        None
    };
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        module_val,
        &args,
        lfp.block(),
        kw,
    )?;
    Ok(module_val)
}

///
/// Default `Module#initialize` — called by `Module.new` (and subclasses
/// that don't override it). Yields the new module to the given block if
/// one is present.
#[monoruby_builtin]
fn module_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let module = lfp.self_val().as_class();
        vm.module_eval(globals, module, bh)?;
    }
    Ok(Value::nil())
}

/// ### Module.nesting
/// - nesting -> [Module]
///
/// Returns the lexically enclosing class/module nesting at the call site,
/// from innermost to outermost. monoruby tracks lexical nesting through the
/// active class/module body, so the result is accurate at module/class body
/// scope but is best-effort inside method bodies (where CRuby uses the lexical
/// scope captured at definition time).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/s/nesting.html]
#[monoruby_builtin]
fn module_nesting(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let nesting = vm.current_class_nesting();
    let ary: Vec<Value> = nesting
        .into_iter()
        .map(|class_id| globals.store[class_id].get_module().as_val())
        .collect();
    Ok(Value::array_from_vec(ary))
}

///
/// ### Module#==
///
/// - self == obj -> bool
///
/// []
#[monoruby_builtin]
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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

fn less_than_or_equal(lhs: Module, rhs: Module) -> Value {
    if lhs.id() == rhs.id() {
        Value::bool(true)
    } else {
        less_than(lhs, rhs)
    }
}

///
/// ### Module#<
///
/// - self < other -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3c.html]
#[monoruby_builtin]
fn lt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(&globals.store)?;
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
fn gt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(&globals.store)?;
    let lhs = lfp.self_val().as_class();
    Ok(less_than(rhs, lhs))
}

///
/// ### Module#<=
///
/// - self <= other -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3c=3d.html]
#[monoruby_builtin]
fn le(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(&globals.store)?;
    let lhs = lfp.self_val().as_class();
    Ok(less_than_or_equal(lhs, rhs))
}

///
/// ### Module#>=
///
/// - self >= other -> bool | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3e=3d.html]
#[monoruby_builtin]
fn ge(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rhs = lfp.arg(0).expect_class_or_module(&globals.store)?;
    let lhs = lfp.self_val().as_class();
    if lhs.id() == rhs.id() {
        Ok(Value::bool(true))
    } else {
        Ok(less_than(rhs, lhs))
    }
}

///
/// ### Module#singleton_class?
///
/// - singleton_class? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/singleton_class=3f.html]
#[monoruby_builtin]
fn singleton_class_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class = lfp.self_val().as_class();
    Ok(Value::bool(class.is_singleton().is_some()))
}

///
/// ### Module#===
///
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    Ok(Value::bool(lfp.arg(0).is_kind_of(&globals.store, class)))
}

///
/// ### Module#alias_method
///
/// - alias_method(new, original) -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/alias_method.html]
#[monoruby_builtin]
fn alias_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let class_id = self_val.as_class_id();
    let new_name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let old_name = lfp.arg(1).coerce_to_symbol_or_string(vm, globals)?;
    vm.alias_method_for_class(globals, class_id, new_name, old_name)?;
    Ok(Value::symbol(new_name))
}

///
/// ### Module#attr
///
/// - attr(*name) -> [Symbol]
/// - attr(name, true) -> [Symbol]   # legacy form
///
/// Defines accessor methods. The legacy two-argument form `attr(name, true)`
/// defines both a reader and writer; `attr(name, false)` defines only a
/// reader. Otherwise (modern form) every argument is treated as a name and
/// only readers are defined.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr.html]
#[monoruby_builtin]
fn attr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    let args = lfp.arg(0).as_array();
    let mut ary = Array::new_empty();

    // Detect the legacy `attr(name, true|false)` form.
    let legacy_writable = if args.len() == 2 {
        let second = args[1];
        if second.id() == Value::bool(true).id() {
            Some(true)
        } else if second.id() == Value::bool(false).id() {
            Some(false)
        } else {
            None
        }
    } else {
        None
    };

    if let Some(writable) = legacy_writable {
        let arg_name = args[0].coerce_to_symbol_or_string(vm, globals)?;
        let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
        ary.push(Value::symbol(method_name));
        if writable {
            let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
            ary.push(Value::symbol(method_name));
        }
    } else {
        for v in args.iter() {
            let arg_name = v.coerce_to_symbol_or_string(vm, globals)?;
            let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
            ary.push(Value::symbol(method_name));
        }
    }
    Ok(ary.into())
}

///
/// ### Module#attr_accessor
///
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
#[monoruby_builtin]
fn attr_accessor(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.coerce_to_symbol_or_string(vm, globals)?;
        let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
        ary.push(Value::symbol(method_name));
        let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
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
fn attr_reader(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.coerce_to_symbol_or_string(vm, globals)?;
        let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
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
fn attr_writer(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.coerce_to_symbol_or_string(vm, globals)?;
        let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
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
fn autoload(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let const_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    validate_constant_name(const_name)?;
    let feature = lfp.arg(1).coerce_to_path_rstring(vm, globals)?;
    let feature = feature.to_str()?.to_string();
    if feature.is_empty() {
        return Err(MonorubyErr::argumenterr("empty file name"));
    }
    let class_id = lfp.self_val().as_class_id();
    let was_autoload = match globals.store.get_constant(class_id, const_name) {
        None => true,
        Some(state) => state.is_autoload(),
    };
    globals
        .store
        .set_constant_autoload(class_id, const_name, feature);
    if was_autoload {
        let receiver = globals.store[class_id].get_module().into();
        vm.invoke_method_inner(
            globals,
            IdentId::CONST_ADDED,
            receiver,
            &[Value::symbol(const_name)],
            None,
            None,
        )?;
    }
    Ok(Value::nil())
}

///
/// ### Module#autoload?
///
/// - autoload?(name, inherit = true) -> String | nil
///
/// Returns the file path that would be autoloaded for the given constant
/// `name`, or `nil` if no autoload is registered (or the constant has already
/// been loaded). When `inherit` is true (the default), the receiver's
/// ancestors are also searched.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/autoload=3f.html]
#[monoruby_builtin]
fn autoload_query(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    let mut module = lfp.self_val().as_class();
    loop {
        if let Some(state) = globals.store.get_constant(module.id(), name) {
            match &state.kind {
                ConstStateKind::Autoload(path) => {
                    return Ok(Value::string(path.to_string_lossy().into_owned()));
                }
                ConstStateKind::Loaded(_) => return Ok(Value::nil()),
            }
        }
        if !inherit {
            return Ok(Value::nil());
        }
        match module.superclass() {
            Some(superclass) => module = superclass,
            None => return Ok(Value::nil()),
        }
    }
}

/// Validate that *name* is a syntactically valid Ruby constant name.
/// CRuby raises `NameError` for autoload/const_set when the symbol cannot be
/// a constant.
fn validate_constant_name(name: IdentId) -> Result<()> {
    let s = name.to_string();
    let mut chars = s.chars();
    let first = chars.next();
    let valid = matches!(first, Some(c) if c.is_ascii_uppercase())
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !valid {
        return Err(MonorubyErr::nameerr(format!(
            "wrong constant name {s}"
        )));
    }
    Ok(())
}

///
/// ### Module#class_eval
///
/// - module_eval(expr, fname = "(eval)", lineno = 1) -> object
/// - module_eval {|mod| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_eval.html]
#[monoruby_builtin]
fn class_eval(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let module = lfp.self_val().as_class();

    // CRuby: `class_eval` accepts at most 3 args (expr, fname, lineno);
    // anything more is an ArgumentError, even before classifying the
    // string-vs-block dispatch. The arity is also reported as 0..3 to
    // match the spec's regex.
    let supplied = lfp.args_count(3);
    if supplied > 3 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {supplied}, expected 0..3)"
        )));
    }

    if let Some(bh) = lfp.block() {
        if lfp.try_arg(0).is_some() {
            return Err(MonorubyErr::wrong_number_of_arg(0, supplied));
        }
        vm.module_eval(globals, module, bh)
    } else if let Some(arg0) = lfp.try_arg(0) {
        let expr = arg0.coerce_to_string(vm, globals)?;
        let cfp = vm.cfp();
        let caller_cfp = cfp.prev().unwrap();
        let path = if let Some(arg1) = lfp.try_arg(1) {
            arg1.coerce_to_str(vm, globals)?
        } else {
            let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
            format!("(eval at {})", caller_loc)
        };
        let lineno: i64 = if let Some(arg2) = lfp.try_arg(2) {
            arg2.coerce_to_int_i64(vm, globals)?
        } else {
            1
        };

        let fid = globals.compile_script_eval(expr, path, caller_cfp, Some(module.id()), lineno)?;
        let proc = ProcData::new(caller_cfp.lfp(), fid);
        vm.push_class_context(module.id());
        let res = vm.invoke_block_with_self(globals, &proc, module.get(), &[]);
        vm.pop_class_context();
        res
    } else {
        Err(MonorubyErr::wrong_number_of_arg_range(0, 1..=3))
    }
}

///
/// ### Module#class_exec
///
/// - class_exec(*args) {|*args| ... } -> object
/// - module_exec(*args) {|*args| ... } -> object
///
/// Evaluates the given block in the context of the module, passing any
/// arguments through to the block. Unlike `class_eval`, this form does not
/// accept a string.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_exec.html]
#[monoruby_builtin]
fn class_exec(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let module = lfp.self_val().as_class();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
    let args = lfp.arg(0).as_array();
    vm.push_class_context(module.id());
    let res = vm.invoke_block_with_self(globals, &data, module.get(), &args);
    vm.pop_class_context();
    res
}

///
/// ### Module#ancestors
///
/// - ancestors -> [Class, Module]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/ancestors.html]
#[monoruby_builtin]
fn ancestors(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let v = globals
        .ancestors(class_id)
        .into_iter()
        .map(|m| m.into())
        .collect();
    Ok(Value::array_from_vec(v))
}

///
/// ### Module#subclasses
///
/// - subclasses -> [Class]
///
/// Returns an array of the immediate (direct) subclasses of the receiver. Does
/// not include modules included via `include`/`prepend`, nor singleton classes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/subclasses.html]
#[monoruby_builtin]
fn subclasses(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(Value::array_from_vec(
        globals.store.get_direct_subclasses(class_id),
    ))
}

///
/// ### Module#const_defined?
///
/// - const_defined?(name, inherit = true) -> bool
///
/// Returns true if the named constant is defined on the receiver (or on an
/// ancestor when `inherit` is true). Visibility is *not* checked: private
/// constants are still considered defined.
///
/// https://docs.ruby-lang.org/ja/latest/method/Module/i/const_defined=3f.html]
#[monoruby_builtin]
fn const_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let module = lfp.self_val().as_class();
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    let found = lookup_constant_path(vm, globals, module, lfp.arg(0), inherit)?;
    Ok(Value::bool(found.is_some()))
}

///
/// ### Module#const_get
///
/// - const_get(name, inherit = true) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_get.html]
#[monoruby_builtin]
fn const_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let module = lfp.self_val().as_class();
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    match lookup_constant_path(vm, globals, module, lfp.arg(0), inherit)? {
        Some(v) => Ok(v),
        None => {
            // For a single-segment lookup, fall back to `const_missing` so
            // user-defined `const_missing` hooks fire (matching CRuby's
            // `rb_const_get` / `rb_funcall(klass, idConst_missing, …)`).
            // If the default `const_missing` raises NameError, we re-wrap
            // it with `nameerr_with_name` so `e.name` is the missing
            // Symbol — `raise ex_obj` doesn't preserve the original
            // exception's ivars in monoruby, so the constructor-set
            // `/name` would otherwise be lost.
            let single_sym = if let Some(sym) = lfp.arg(0).try_symbol() {
                Some(sym)
            } else if let Some(s) = lfp.arg(0).is_str()
                && !s.contains("::")
            {
                Some(IdentId::get_id(s))
            } else {
                None
            };
            if let Some(sym) = single_sym {
                return match vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("const_missing"),
                    module.as_val(),
                    &[Value::symbol(sym)],
                    None,
                    None,
                ) {
                    Ok(v) => Ok(v),
                    Err(e) if matches!(e.kind, MonorubyErrKind::Name(_)) => {
                        Err(MonorubyErr::nameerr_with_name(e.message, sym))
                    }
                    Err(e) => Err(e),
                };
            }
            // Multi-segment paths or weird inputs: raise straight away
            // with the CRuby-style wording.
            let display = if let Some(s) = lfp.arg(0).is_str() {
                s.to_string()
            } else {
                "?".to_string()
            };
            Err(MonorubyErr::nameerr(format!(
                "uninitialized constant {display}"
            )))
        }
    }
}

/// True if `name` is a syntactically valid constant identifier — first
/// character is uppercase ASCII, the rest are word characters (`\w`).
/// Used by `const_defined?` / `const_get` / `const_set` to raise a
/// NameError for `"name"`, `"_X"`, `"@X"`, etc., matching CRuby's
/// `rb_check_id_without_pindown` validation.
fn is_valid_const_name(name: &str) -> bool {
    let mut chars = name.chars();
    let first = match chars.next() {
        Some(c) => c,
        None => return false,
    };
    if !first.is_ascii_uppercase() {
        return false;
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Walk a (possibly scoped) constant path. Coerces *name_arg* to a
/// Symbol or String (via `#to_str`); a Symbol is treated as a bare
/// name (a `::`-containing Symbol is a NameError). A String may be
/// fully-qualified (`"::A::B"` -> start at Object) or relative
/// (`"A::B"` -> start at *receiver*).
///
/// Each segment is validated by `is_valid_const_name`. Returns
/// `Ok(Some(v))` on hit, `Ok(None)` if any segment is missing,
/// or `Err(NameError)` for malformed names.
fn lookup_constant_path(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Module,
    name_arg: Value,
    inherit: bool,
) -> Result<Option<Value>> {
    // Coerce to a String we can split, but remember whether the input
    // was a Symbol (Symbols may not contain `::`).
    let (path, was_symbol) = if let Some(sym) = name_arg.try_symbol() {
        (sym.get_name().to_string(), true)
    } else if let Some(s) = name_arg.is_str() {
        (s.to_string(), false)
    } else if let Some(func_id) = globals.check_method(name_arg, IdentId::TO_STR) {
        let result = vm.invoke_func_inner(globals, func_id, name_arg, &[], None, None)?;
        if let Some(s) = result.is_str() {
            (s.to_string(), false)
        } else {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into String",
                name_arg.get_real_class_name(&globals.store)
            )));
        }
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into String",
            name_arg.get_real_class_name(&globals.store)
        )));
    };

    let (start_at_object, segs): (bool, Vec<&str>) = if was_symbol {
        if path.contains("::") {
            return Err(MonorubyErr::nameerr(format!(
                "wrong constant name {path}"
            )));
        }
        (false, vec![path.as_str()])
    } else if let Some(rest) = path.strip_prefix("::") {
        (true, rest.split("::").collect())
    } else {
        (false, path.split("::").collect())
    };

    if segs.iter().any(|s| !is_valid_const_name(s)) {
        return Err(MonorubyErr::nameerr(format!(
            "wrong constant name {path}"
        )));
    }

    // Walk the scope chain. The first segment honours `inherit`; nested
    // segments are looked up directly on the resolved class (CRuby
    // scopes `Foo::Bar::Baz` like the parser would, so each `::` step
    // is a non-inherited lookup *unless* the user asked for inherited
    // and we're still at the receiver).
    let mut current = if start_at_object {
        globals.store[OBJECT_CLASS].get_module()
    } else {
        receiver
    };
    let last_idx = segs.len() - 1;
    for (i, seg) in segs.iter().enumerate() {
        let id = IdentId::get_id(seg);
        // For the *first* segment, defer to the inherit flag (and
        // implicitly walk Object for modules / superclasses for
        // classes, as `get_constant_superclass_with_class` does). For
        // later segments, only the resolved class itself is checked.
        let val = if i == 0 && inherit {
            match vm.get_constant_superclass_with_class(globals, current, id) {
                Ok((v, _)) => Some(v),
                Err(_) => None,
            }
        } else if globals.store[current.id()].has_own_constant(id) {
            // For nested segments, restrict to *direct* lookup on the
            // resolved class (no superclass walk). Use the
            // `get_constant_checked` path so autoload triggers fire
            // correctly; an error means "missing constant" -> None.
            match vm.get_constant_checked(globals, current.id(), id) {
                Ok(v) => Some(v),
                Err(_) => None,
            }
        } else {
            None
        };
        match val {
            Some(v) => {
                if i == last_idx {
                    return Ok(Some(v));
                }
                match v.is_class_or_module() {
                    Some(m) => current = m,
                    None => return Ok(None),
                }
            }
            None => return Ok(None),
        }
    }
    Ok(None)
}

///
/// ### Module#const_set
///
/// - const_set(name, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_set.html]
#[monoruby_builtin]
fn const_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let name_str = name.get_name();
    if !is_valid_const_name(&name_str) {
        return Err(MonorubyErr::nameerr(format!(
            "wrong constant name {name_str}"
        )));
    }
    let module = self_val.as_class().id();
    let val = lfp.arg(1);
    // Warn (via Ruby's `$stderr` so mspec's `complain` matcher captures it)
    // when overwriting an existing same-name constant on the receiver,
    // matching CRuby's redefinition warning.
    if globals.store.get_constant_noautoload(module, name).is_some() {
        let parent_name = globals.store[module]
            .get_name()
            .unwrap_or_default()
            .to_string();
        let qual = if parent_name.is_empty() {
            name.get_name().to_string()
        } else {
            format!("{parent_name}::{}", name.get_name())
        };
        let msg = format!("warning: already initialized constant {qual}\n");
        let stderr_id = IdentId::get_id("$stderr");
        let stderr = globals.get_gvar(stderr_id).unwrap_or(Value::nil());
        let write_id = IdentId::get_id("write");
        let _ = vm.invoke_method_inner(
            globals,
            write_id,
            stderr,
            &[Value::string(msg)],
            None,
            None,
        );
    }
    globals.set_constant(module, name, val);
    // Record the call-site as the constant's source location so
    // `Module#const_source_location(:Foo)` returns `[__FILE__,
    // __LINE__]` of the `const_set` call. Walks the CFP chain to skip
    // builtin frames; falls back to no location if we can't find a
    // Ruby frame (in which case `const_source_location` will return
    // `[]`, the "C-defined" sentinel).
    if let Some(caller) = vm.cfp().prev()
        && let Some((file, line)) = caller_source_location_with_pc(globals, caller, pc)
    {
        globals.store[module].record_constant_location(name, file, line);
    }
    let receiver = globals.store[module].get_module().into();
    vm.invoke_method_inner(
        globals,
        IdentId::CONST_ADDED,
        receiver,
        &[Value::symbol(name)],
        None,
        None,
    )?;
    Ok(val)
}

///
/// ### Module#const_source_location
///
/// - const_source_location(name, inherit = true) -> [String, Integer] | nil
///
/// Returns the `[file, line]` source location where the named constant was
/// last assigned, walking the ancestor chain when `inherit` is true. Returns
/// `nil` if the constant exists but has no recorded location (e.g. constants
/// defined by Rust builtins) or if the constant is not defined.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_source_location.html]
#[monoruby_builtin]
fn const_source_location(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    let module = lfp.self_val().as_class();
    let result = lookup_constant_with_owner(vm, globals, module, lfp.arg(0), inherit)?;
    match result {
        // Constant found and recorded by user code. Return [file, line].
        Some((owner_id, Some((file, line)))) => {
            let _ = owner_id;
            Ok(Value::array_from_vec(vec![
                Value::string(file),
                Value::integer(line as i64),
            ]))
        }
        // Constant found but has no recorded location (defined in
        // Rust, e.g. `Object::String`). CRuby returns an empty array
        // here ("[] for C-defined constants").
        Some((_, None)) => Ok(Value::array_empty()),
        // Constant not found.
        None => Ok(Value::nil()),
    }
}

/// Walk the CFP chain starting at `cfp` for the nearest Ruby (iseq)
/// frame and return the (file, line) source location *of the current
/// PC* in that frame. Used by `Module#const_set` to record
/// `const_source_location` for dynamically assigned constants —
/// `mod.const_set :Foo, 1` must report the line of the `const_set`
/// call, not the start of the enclosing iseq. The `pc` argument is the
/// builtin call site's bytecode pointer, used only when the *immediate*
/// caller is itself an iseq (i.e. the receiver of `const_set`); deeper
/// iseq frames fall back to their own loc.
fn caller_source_location_with_pc(
    globals: &Globals,
    cfp: Cfp,
    pc: BytecodePtr,
) -> Option<(String, u32)> {
    let mut frame = Some(cfp);
    let mut first = true;
    while let Some(c) = frame {
        let fid = c.lfp().func_id();
        if let Some(iseq_id) = globals.store[fid].is_iseq() {
            let iseq = &globals.store[iseq_id];
            let loc = if first {
                let bc_index = iseq.get_pc_index(Some(pc));
                iseq.sourcemap[bc_index.to_usize()]
            } else {
                iseq.loc
            };
            let line = iseq.sourceinfo.get_line(&loc) as u32;
            let file = iseq.sourceinfo.file_name().to_string();
            return Some((file, line));
        }
        frame = c.prev();
        first = false;
    }
    None
}

/// Walk a (possibly scoped) constant path and return the (defining class
/// id, optional source location) of the resolved constant. Returns
/// `Ok(None)` when no segment in the path resolves. The argument is
/// coerced through `#to_str` like `Module#const_get`, with the same
/// `::`-split rules.
fn lookup_constant_with_owner(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Module,
    name_arg: Value,
    inherit: bool,
) -> Result<Option<(ClassId, Option<(String, u32)>)>> {
    let (path, was_symbol) = if let Some(sym) = name_arg.try_symbol() {
        (sym.get_name().to_string(), true)
    } else if let Some(s) = name_arg.is_str() {
        (s.to_string(), false)
    } else if let Some(func_id) = globals.check_method(name_arg, IdentId::TO_STR) {
        let result = vm.invoke_func_inner(globals, func_id, name_arg, &[], None, None)?;
        if let Some(s) = result.is_str() {
            (s.to_string(), false)
        } else {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into String",
                name_arg.get_real_class_name(&globals.store)
            )));
        }
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into String",
            name_arg.get_real_class_name(&globals.store)
        )));
    };

    let (start_at_object, segs): (bool, Vec<&str>) = if was_symbol {
        // CRuby rejects `::`-containing Symbols outright (Symbols are
        // bare names; they can't carry scope qualifiers).
        if path.contains("::") {
            return Err(MonorubyErr::nameerr(format!(
                "wrong constant name {path}"
            )));
        }
        (false, vec![path.as_str()])
    } else if let Some(rest) = path.strip_prefix("::") {
        (true, rest.split("::").collect())
    } else {
        (false, path.split("::").collect())
    };
    if segs.iter().any(|s| !is_valid_const_name(s)) {
        return Err(MonorubyErr::nameerr(format!("wrong constant name {path}")));
    }

    let mut current = if start_at_object {
        globals.store[OBJECT_CLASS].get_module()
    } else {
        receiver
    };
    let last_idx = segs.len() - 1;
    for (i, seg) in segs.iter().enumerate() {
        let id = IdentId::get_id(seg);
        // First segment honours `inherit`; later segments are looked
        // up directly on the resolved class, matching CRuby's scoped
        // resolution.
        let owner_with_loc: Option<(ClassId, Option<(String, u32)>)> = if i == 0 && inherit {
            let mut module = current;
            loop {
                if globals.store[module.id()].has_own_constant(id) {
                    let loc = globals.store[module.id()]
                        .get_constant_location(id)
                        .map(|(f, l)| (f.to_string(), l));
                    break Some((module.id(), loc));
                }
                match module.superclass() {
                    Some(superclass) => module = superclass,
                    None => break None,
                }
            }
        } else if globals.store[current.id()].has_own_constant(id) {
            let loc = globals.store[current.id()]
                .get_constant_location(id)
                .map(|(f, l)| (f.to_string(), l));
            Some((current.id(), loc))
        } else {
            None
        };
        let (owner, loc) = match owner_with_loc {
            Some(p) => p,
            None => return Ok(None),
        };
        if i == last_idx {
            return Ok(Some((owner, loc)));
        }
        // Resolve the value to descend into for the next segment.
        let val = match globals.store.get_constant_noautoload(owner, id) {
            Some(v) => v,
            None => return Ok(None),
        };
        match val.is_class_or_module() {
            Some(m) => current = m,
            None => return Ok(None),
        }
    }
    Ok(None)
}

///
/// ### Module#cremove_const
///
/// - remove_const(name) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/remove_const.html]
#[monoruby_builtin]
fn remove_const(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let module = lfp.self_val().as_class().id();
    globals
        .remove_constant(module, name)
        .ok_or(MonorubyErr::uninitialized_constant(name))
}

///
/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
#[monoruby_builtin]
fn constants(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().expect_class_or_module(&globals.store)?;
    let v = if lfp.try_arg(0).is_none() || lfp.arg(0).as_bool() {
        globals.store.get_constant_names_inherit(class)
    } else {
        globals.store.get_constant_names(class.id())
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
fn define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let func_id = if let Some(method) = lfp.try_arg(1) {
        if let Some(proc) = method.is_proc() {
            globals.define_proc_method(proc)
        } else if let Some(method) = method.is_method() {
            method.func_id()
        } else if let Some(umethod) = method.is_umethod() {
            umethod.func_id()
        } else {
            return Err(MonorubyErr::wrong_argument_type(
                globals,
                method,
                "Proc/Method/UnboundMethod",
            ));
        }
    } else if let Some(bh) = lfp.block() {
        let proc = vm.generate_proc(globals, bh, pc)?;
        globals.define_proc_method(proc)
    } else {
        return Err(MonorubyErr::wrong_number_of_arg(2, 1));
    };
    vm.add_public_method(globals, class_id, name, func_id)?;
    Ok(Value::symbol(name))
}

///
/// ### Module#instance_methods
///
/// - instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_methods.html]
#[monoruby_builtin]
fn instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_method_names(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, false)
    }))
}

///
/// ### Module#private_instance_methods
///
/// - private_instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_instance_methods.html]
#[monoruby_builtin]
fn private_instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_private_method_names(class_id)
    } else {
        globals.store.get_private_method_names_inherit(class_id)
    }))
}

///
/// ### Module#protected_instance_methods
///
/// - protected_instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected_instance_methods.html]
#[monoruby_builtin]
fn protected_instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_protected_method_names(class_id)
    } else {
        globals.store.get_protected_method_names_inherit(class_id)
    }))
}

///
/// ### Module#public_instance_methods
///
/// - public_instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public_instance_methods.html]
#[monoruby_builtin]
fn public_instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_method_names(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, false)
    }))
}

///
/// ### Module#include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    for v in args.iter().cloned() {
        require_module_argument(globals, v, "include")?;
    }
    let self_ = lfp.self_val();
    for v in args.iter().cloned().rev() {
        vm.invoke_method_inner(globals, IdentId::APPEND_FEATURES, v, &[self_], None, None)?;
        vm.invoke_method_inner(globals, IdentId::INCLUDED, v, &[self_], None, None)?;
    }
    Ok(lfp.self_val())
}

///
/// ### Module#append_features
/// - append_features(mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/append_features.html]
#[monoruby_builtin]
fn append_features(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    require_module_receiver(globals, lfp.self_val(), "append_features")?;
    let mut base = lfp.arg(0).expect_class_or_module(&globals.store)?;
    base.as_val().ensure_not_frozen(&globals.store)?;
    let include_module = lfp.self_val().expect_module(globals)?;
    base.include_module(include_module)?;
    Ok(lfp.self_val())
}

///
/// ### Module#extend_object
/// - extend_object(obj) -> obj
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/extend_object.html]
#[monoruby_builtin]
fn extend_object(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    require_module_receiver(globals, lfp.self_val(), "extend_object")?;
    let mut obj = lfp.arg(0);
    // Refuse to extend a frozen object — CRuby raises `RuntimeError`
    // (a FrozenError, which is a RuntimeError subclass) before
    // mutating anything.
    obj.ensure_not_frozen(&globals.store)?;
    let mut class = globals.store.get_singleton(obj)?;
    let include_module = lfp.self_val().expect_module(globals)?;
    class.include_module(include_module)?;
    Ok(obj)
}

///
/// ### Module#prepend
/// - prepend(*modules) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/prepend.html]
#[monoruby_builtin]
fn prepend(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    // Validate up front: every argument must be a true Module (not a
    // Class, not anything else). CRuby raises TypeError before invoking
    // any `prepend_features` hook. This also keeps a non-Module from
    // reaching `Module#prepend_features` (which we removed from Class)
    // and bottoming out as NoMethodError.
    for v in args.iter().cloned() {
        require_module_argument(globals, v, "prepend")?;
    }
    let self_ = lfp.self_val();
    for v in args.iter().cloned().rev() {
        vm.invoke_method_inner(
            globals,
            IdentId::get_id("prepend_features"),
            v,
            &[self_],
            None,
            None,
        )?;
        vm.invoke_method_inner(globals, IdentId::PREPENDED, v, &[self_], None, None)?;
    }
    Ok(lfp.self_val())
}

/// Reject a non-Module argument (a Class, an instance, …). Used by
/// `Module#prepend` and `Module#include` for their up-front
/// `TypeError` checks before invoking the per-argument hooks.
fn require_module_argument(globals: &Globals, arg: Value, op: &str) -> Result<()> {
    if arg.ty() == Some(ObjTy::MODULE) {
        Ok(())
    } else {
        Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Module) in {op}",
            arg.get_real_class_name(&globals.store),
        )))
    }
}

///
/// ### Module#prepend_features
/// - prepend_features(mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/prepend_features.html]
#[monoruby_builtin]
fn prepend_features(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    require_module_receiver(globals, lfp.self_val(), "prepend_features")?;
    let mut base = lfp.arg(0).expect_class_or_module(&globals.store)?;
    base.as_val().ensure_not_frozen(&globals.store)?;
    let prepend_module = lfp.self_val().as_class();
    base.prepend_module(prepend_module)?;
    Ok(lfp.self_val())
}

/// Reject Class receivers. `Module#module_function`, `prepend_features`,
/// `append_features`, `extend_object` are conceptually module-only; CRuby
/// raises TypeError when they're invoked on a Class via
/// `Module.instance_method(:foo).bind(Class.new).call`. Removing them
/// from `Class`'s method table covers normal dispatch but not the
/// rebound-UnboundMethod path, which still routes back through Module's
/// implementation.
fn require_module_receiver(globals: &Globals, recv: Value, name: &str) -> Result<()> {
    if recv.ty() == Some(ObjTy::CLASS) {
        Err(MonorubyErr::typeerr(format!(
            "Module#{name} cannot be called on a Class"
        )))
    } else {
        let _ = globals;
        Ok(())
    }
}

///
/// ### Module#instance_method
///
/// - instance_method(name) -> UnboundMethod
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_method.html]
#[monoruby_builtin]
fn instance_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let klass = lfp.self_val().as_class();
    let method_name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let (func_id, _, owner) = globals
        .find_method_for_class(klass.id(), method_name)
        .map_err(|_| {
            MonorubyErr::nameerr_with_name(
                format!(
                    "undefined method `{}' for class `{}'",
                    method_name.get_name(),
                    klass.id().get_name(&globals.store),
                ),
                method_name,
            )
        })?;
    Ok(Value::new_unbound_method(func_id, owner))
}

#[monoruby_builtin]
fn public_instance_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let klass = lfp.self_val().as_class();
    let method_name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let (func_id, visibility, owner) =
        globals
            .find_method_for_class(klass.id(), method_name)
            .map_err(|_| {
                MonorubyErr::nameerr_with_name(
                    format!(
                        "undefined method `{}' for class `{}'",
                        method_name.get_name(),
                        klass.id().get_name(&globals.store),
                    ),
                    method_name,
                )
            })?;
    // CRuby raises a NameError (kind, not NoMethodError) when the
    // looked-up method exists but isn't public.
    match visibility {
        Visibility::Private => {
            return Err(MonorubyErr::nameerr_with_name(
                format!(
                    "method `{}' for class `{}' is private",
                    method_name.get_name(),
                    klass.id().get_name(&globals.store),
                ),
                method_name,
            ));
        }
        Visibility::Protected => {
            return Err(MonorubyErr::nameerr_with_name(
                format!(
                    "method `{}' for class `{}' is protected",
                    method_name.get_name(),
                    klass.id().get_name(&globals.store),
                ),
                method_name,
            ));
        }
        _ => {}
    }
    Ok(Value::new_unbound_method(func_id, owner))
}

///
/// ### Module#undef_method
///
/// - undef_method(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/undef_method.html]
#[monoruby_builtin]
fn undef_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    let names = lfp.arg(0).as_array();
    // Coerce all names *before* the frozen check so that a non-name
    // argument raises TypeError (not FrozenError) on a frozen receiver,
    // matching CRuby's error precedence and the
    // "raises a TypeError when passed a not name" spec.
    let resolved: Vec<IdentId> = names
        .iter()
        .map(|n| n.coerce_to_symbol_or_string(vm, globals))
        .collect::<Result<_>>()?;
    if !resolved.is_empty() {
        self_val.ensure_not_frozen(&globals.store)?;
    }
    for name in resolved {
        if globals.find_method_for_class(class_id, name).is_err() {
            return Err(undefined_method_for_kind(globals, class_id, name));
        }
        globals.undef_method_for_class(class_id, name)?;
        vm.invoke_method_undefined(globals, class_id, name)?;
    }
    Ok(lfp.self_val())
}

///
/// ### Module#remove_method
///
/// - remove_method(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/remove_method.html]
#[monoruby_builtin]
fn remove_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    let names = lfp.arg(0).as_array();
    let resolved: Vec<IdentId> = names
        .iter()
        .map(|n| n.coerce_to_symbol_or_string(vm, globals))
        .collect::<Result<_>>()?;
    if !resolved.is_empty() {
        self_val.ensure_not_frozen(&globals.store)?;
    }
    for name in resolved {
        globals.remove_method(class_id, name)?;
        vm.invoke_method_removed(globals, class_id, name)?;
    }
    Ok(lfp.self_val())
}

/// CRuby renders `undef_method`'s missing-name NameError as
/// "undefined method `<name>' for module `<X>'" or `... for class
/// `<X>'` depending on whether the receiver is a Module or a Class.
/// For a singleton class, the name string is the attached object
/// rather than `#<Class:X>` so e.g.
/// `String.singleton_class.send(:undef_method, :foo)` reads
/// `... for class `String'`.
fn undefined_method_for_kind(
    globals: &Globals,
    class_id: ClassId,
    method: IdentId,
) -> MonorubyErr {
    let module = globals.store[class_id].get_module();
    let kind = if module.as_val().ty() == Some(ObjTy::MODULE) {
        "module"
    } else {
        "class"
    };
    // Singleton class display: when the attached object is itself a
    // class/module (a metaclass like `String.singleton_class`), CRuby
    // unwraps the display to the attached's own name (`String`). For an
    // instance's singleton, fall back to the singleton class's own
    // `#<Class:#<Foo:0x...>>` form.
    let display = if let Some(attached) = module.is_singleton() {
        if attached.is_class_or_module().is_some() {
            attached.to_s(&globals.store)
        } else {
            class_id.get_name(&globals.store)
        }
    } else {
        class_id.get_name(&globals.store)
    };
    MonorubyErr::nameerr(format!(
        "undefined method `{}' for {kind} `{display}'",
        method.get_name(),
    ))
}

fn check_method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Option<Visibility>> {
    let class_id = lfp.self_val().as_class_id();
    let func_name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let inherit = if let Some(arg) = lfp.try_arg(1) {
        arg.as_bool()
    } else {
        true
    };
    Ok(globals.method_defined(class_id, func_name, inherit))
}

///
/// ### Module#method_defined?
///
/// - method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/method_defined=3f.html]
#[monoruby_builtin]
fn method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(match check_method_defined(vm, globals, lfp)? {
        Some(v) => matches!(v, Visibility::Public | Visibility::Protected),
        None => false,
    }))
}

///
/// ### Module#private_method_defined?
///
/// - private_method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_method_defined=3f.html]
#[monoruby_builtin]
fn private_method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(
        check_method_defined(vm, globals, lfp)? == Some(Visibility::Private),
    ))
}

///
/// ### Module#public_method_defined?
///
/// - public_method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public_method_defined=3f.html]
#[monoruby_builtin]
fn public_method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(
        check_method_defined(vm, globals, lfp)? == Some(Visibility::Public),
    ))
}

///
/// ### Module#protected_method_defined?
///
/// - protected_method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected_method_defined=3f.html]
#[monoruby_builtin]
fn protected_method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(
        check_method_defined(vm, globals, lfp)? == Some(Visibility::Protected),
    ))
}

///
/// ### Module#private_class_method
/// - private_class_method(*name) -> self
/// - private_class_method(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_class_method.html]
#[monoruby_builtin]
fn private_class_method(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let singleton = globals.store.get_singleton(lfp.self_val())?;
    let arg = lfp.arg(0).as_array();
    let (_, names) = extract_names(globals, arg)?;
    globals.change_method_visibility_for_class(singleton.id(), &names, Visibility::Private)?;
    Ok(lfp.self_val())
}

///
/// ### Module#public_class_method
///
/// - public_class_method(*name) -> self
///
/// Makes the named class methods public.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public_class_method.html]
#[monoruby_builtin]
fn public_class_method(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let singleton = globals.store.get_singleton(lfp.self_val())?;
    let arg = lfp.arg(0).as_array();
    let (_, names) = extract_names(globals, arg)?;
    globals.change_method_visibility_for_class(singleton.id(), &names, Visibility::Public)?;
    Ok(lfp.self_val())
}

///
/// ### Module#const_source_location
///
/// - const_source_location(name, inherit = true) -> [String, Integer] | nil
///
/// ### Module#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn tos(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(module) = self_val.is_class_or_module() {
        let class_name = globals.store.get_class_name(module.id());
        Ok(Value::string(class_name))
    } else {
        let class_name = globals.store.get_class_name(self_val.class());
        Ok(Value::string(format!(
            "#<{}:0x{:016x}>",
            class_name,
            self_val.id()
        )))
    }
}

///
/// ### Module#name
/// - name -> String | nil
///
/// Returns the name of the module/class. The returned string is frozen and
/// is cached so that repeated calls return the identical string object.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/name.html]
#[monoruby_builtin]
fn name(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    if globals.store[class_id].get_name().is_none() {
        return Ok(Value::nil());
    }
    if let Some(cached) = globals.store[class_id].cached_name_value() {
        return Ok(cached);
    }
    let class_name = globals.store.get_class_name(class_id);
    let mut val = Value::string(class_name);
    val.set_frozen();
    globals.store[class_id].set_cached_name_value(val);
    Ok(val)
}

///
/// ### Module#class_variable_set
///
/// - class_variable_set(name, val) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_variable_set.html]
#[monoruby_builtin]
fn class_variable_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let class_id = self_val.as_class_id();
    let name = coerce_to_class_var_name(vm, globals, lfp.arg(0))?;
    let val = lfp.arg(1);
    globals.set_class_variable(class_id, name, val);
    Ok(val)
}

///
/// ### Module#class_variable_get
///
/// - class_variable_get(name) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_variable_get.html]
#[monoruby_builtin]
fn class_variable_get(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name = coerce_to_class_var_name(vm, globals, lfp.arg(0))?;
    let module = globals.store[class_id].get_module();
    globals.get_class_variable(module, name).map(|(_, v)| v)
}

///
/// ### Module#class_variable_defined?
///
/// - class_variable_defined?(name) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_variable_defined=3f.html]
#[monoruby_builtin]
fn class_variable_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name = coerce_to_class_var_name(vm, globals, lfp.arg(0))?;
    let module = globals.store[class_id].get_module();
    Ok(Value::bool(globals.get_class_variable(module, name).is_ok()))
}

/// Coerce a name argument for `Module#class_variable_*` to an `IdentId`,
/// validating that the resulting string starts with `@@` (CRuby raises
/// NameError otherwise). `#to_str` is invoked for non-Symbol/non-String
/// arguments.
fn coerce_to_class_var_name(
    vm: &mut Executor,
    globals: &mut Globals,
    name_arg: Value,
) -> Result<IdentId> {
    let id = name_arg.coerce_to_symbol_or_string(vm, globals)?;
    let s = id.get_name();
    if !s.starts_with("@@") || s.len() <= 2 {
        return Err(MonorubyErr::nameerr(format!(
            "`{s}' is not allowed as a class variable name"
        )));
    }
    Ok(id)
}

///
/// ### Module#remove_class_variable
///
/// - remove_class_variable(name) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/remove_class_variable.html]
#[monoruby_builtin]
fn remove_class_variable(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    match globals.store[class_id].remove_cvar(name) {
        Some(val) => Ok(val),
        None => Err(MonorubyErr::nameerr(format!(
            "class variable {name} not defined for {}",
            class_id.get_name(&globals.store)
        ))),
    }
}

///
/// ### Module#class_variables
///
/// - class_variables -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_variables.html]
#[monoruby_builtin]
fn class_variables(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inherit = lfp.try_arg(0).map(|v| v.as_bool()).unwrap_or(true);
    let mut seen = HashSet::default();
    let mut names: Vec<IdentId> = Vec::new();
    let mut module = Some(globals.store[class_id].get_module());
    while let Some(m) = module {
        for name in globals.store.get_class_variable_names(m.id()) {
            if seen.insert(name) {
                names.push(name);
            }
        }
        if !inherit {
            break;
        }
        module = m.superclass();
    }
    Ok(Value::array_from_vec(
        names.into_iter().map(Value::symbol).collect(),
    ))
}

/// ### Module#set_temporary_name
/// - set_temporary_name(name) -> self
///
///
/// ### Module#included_modules
///
/// - included_modules -> [Module]
///
/// Returns an array of all modules in the receiver's ancestor chain that are
/// modules (not classes). This includes inherited included modules.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/included_modules.html]
#[monoruby_builtin]
fn included_modules(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    // Skip the receiver itself: `included_modules` lists the modules
    // *mixed in* via `include` / `prepend`, not the receiver. CRuby:
    // `ModuleSpecs.included_modules == []`,
    // `Child.included_modules == [Super, Basic, Kernel]`.
    let v: Vec<Value> = globals
        .ancestors(class_id)
        .into_iter()
        .skip(1)
        .filter_map(|m| {
            let val: Value = m.into();
            if val.ty() == Some(ObjTy::MODULE) {
                Some(val)
            } else {
                None
            }
        })
        .collect();
    Ok(Value::array_from_vec(v))
}

///
/// ### Module#public_constant
///
/// - public_constant(*name) -> self
///
/// Marks each named constant as public. Each name must be a constant defined
/// directly on the receiver (not inherited); otherwise `NameError` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public_constant.html]
#[monoruby_builtin]
fn public_constant(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    set_constants_visibility(globals, lfp, /*private=*/ false)
}

///
/// ### Module#private_constant
///
/// - private_constant(*name) -> self
///
/// Marks each named constant as private so that qualified access
/// (`Foo::Bar`, `Foo.const_get(:Bar)`) raises `NameError`. Lexical access
/// from within the defining scope is unaffected. Each name must be a
/// constant defined directly on the receiver (not inherited); otherwise
/// `NameError` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_constant.html]
#[monoruby_builtin]
fn private_constant(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    set_constants_visibility(globals, lfp, /*private=*/ true)
}

/// Shared body of `Module#private_constant` and `Module#public_constant`.
fn set_constants_visibility(globals: &mut Globals, lfp: Lfp, make_private: bool) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let args = lfp.arg(0).as_array();
    // Validate every name first so a partial failure does not leave the
    // class in an inconsistent state.
    let mut names = Vec::with_capacity(args.len());
    for arg in args.iter() {
        let name = arg.expect_symbol_or_string(globals)?;
        if !globals.store[class_id].has_own_constant(name) {
            return Err(MonorubyErr::nameerr(format!(
                "constant {}::{} not defined",
                class_id.get_name(&globals.store),
                name
            )));
        }
        names.push(name);
    }
    for name in names {
        if make_private {
            globals.store[class_id].set_constant_private(name);
        } else {
            globals.store[class_id].set_constant_public(name);
        }
    }
    Ok(lfp.self_val())
}

/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/set_temporary_name.html]
#[monoruby_builtin]
fn set_temporary_name(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let name_val = lfp.arg(0);
    if name_val.is_nil() {
        globals.store[class_id].clear_name();
    } else {
        let name = name_val.coerce_to_string(vm, globals)?;
        globals.store[class_id].set_name(name);
    }
    Ok(lfp.self_val())
}

///
/// ### Module#module_function
/// - module_function(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/module_function.html]
#[monoruby_builtin]
fn module_function(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `module_function` is meaningful only on a Module. CRuby raises
    // TypeError when invoked on a Class (e.g. via
    // `Module.instance_method(:module_function).bind(Class.new).call`),
    // and we removed it from `Class`'s public method table elsewhere.
    require_module_receiver(globals, lfp.self_val(), "module_function")?;
    let arg0 = lfp.arg(0);
    let args = arg0.as_array();
    if args.is_empty() {
        vm.set_module_function();
        return Ok(Value::nil());
    }
    let class_id = lfp.self_val().as_class_id();
    let mut names: Vec<IdentId> = Vec::with_capacity(args.len());
    for v in args.iter() {
        names.push(v.coerce_to_symbol_or_string(vm, globals)?);
    }
    for &name in &names {
        let func_id = globals.find_method_for_class(class_id, name)?.0;
        // Module-method copy: install on the singleton (callable as `M.name`)
        // *and* mark the instance-method side private (matching CRuby).
        vm.add_singleton_method(globals, class_id, name, func_id, Visibility::Public)?;
        globals.add_method(class_id, name, func_id, Visibility::Private);
    }
    // CRuby return value: the single argument as-is when exactly one name
    // was passed, otherwise the original argument array (preserving the
    // caller's String/Symbol mix). `module_function(:foo)` -> `:foo`;
    // `module_function(:a, "b")` -> `[:a, "b"]`.
    if args.len() == 1 {
        Ok(args[0])
    } else {
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
fn private(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn protected(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn public(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    let (res, names) = extract_names(globals, arg)?;
    let class_id = self_val.as_class_id();
    globals.change_method_visibility_for_class(class_id, &names, visi)?;
    Ok(res)
}

fn extract_names(store: &Store, arg: Array) -> Result<(Value, Vec<IdentId>)> {
    if arg.len() == 1 {
        let mut names = vec![];
        if let Some(ary) = arg[0].try_array_ty() {
            for v in ary.iter() {
                names.push(v.expect_symbol_or_string(store)?);
            }
        } else {
            names.push(arg[0].expect_symbol_or_string(store)?);
        }
        Ok((arg[0], names))
    } else {
        let mut names = vec![];
        for v in arg.iter() {
            names.push(v.expect_symbol_or_string(store)?);
        }
        Ok((arg.as_val(), names))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
    fn compare_le_ge() {
        run_test(
            r#"
        module Foo; end
        class Bar; include Foo; end
        class Baz < Bar; end
        class Qux; end
        res = []
        # <=
        res << (Bar <= Foo)   # true (includes)
        res << (Baz <= Bar)   # true (subclass)
        res << (Baz <= Foo)   # true (transitive include)
        res << (Foo <= Foo)   # true (equal)
        res << (Bar <= Bar)   # true (equal)
        res << (Foo <= Bar)   # false (ancestor)
        res << (Bar <= Baz)   # false (ancestor)
        res << (Bar <= Qux)   # nil (unrelated)
        # >=
        res << (Foo >= Bar)   # true (included by)
        res << (Bar >= Baz)   # true (superclass)
        res << (Foo >= Baz)   # true (transitive)
        res << (Foo >= Foo)   # true (equal)
        res << (Bar >= Foo)   # false (descendant)
        res << (Baz >= Bar)   # false (descendant)
        res << (Bar >= Qux)   # nil (unrelated)
        res
        "#,
        );
        run_test_error(
            r##"
        module Foo; end
        Foo <= Object.new
        "##,
        );
        run_test_error(
            r##"
        module Foo; end
        Foo >= Object.new
        "##,
        );
    }

    #[test]
    fn singleton_class_q() {
        run_test(
            r#"
        class A; end
        [A.singleton_class.singleton_class?, A.singleton_class?,
         Class.singleton_class?, Object.singleton_class?]
        "#,
        );
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
        run_test(
            r##"
        class C
          a = 42
          define_method :foo do |x|
            a += x
          end
        end
        c = C.new
        g = 0
        100.times { |x|
            g += c.foo(x)
        }
        g
        "##,
        );
        run_test_with_prelude(
            r#"
            C.new.foo
        "#,
            r#"
            class C
              def bar
                42
              end
              define_method :foo, instance_method(:bar)
            end
            "#,
        );
    }

    #[test]
    fn define_method_return() {
        // `return` inside a `define_method` block acts like a lambda-style
        // method return: it exits the method created by `define_method`,
        // not the block's lexical enclosing scope.
        run_test(
            r#"
            k = Class.new { define_method(:m) { return 42 } }
            k.new.m
            "#,
        );
        // Multiple return values via `return a, b` — wraps as an Array.
        run_test(
            r#"
            k = Class.new { define_method(:m) { |a, b| return a, b } }
            k.new.m(10, 20)
            "#,
        );
        // `return` from a nested block still targets the proc-method frame.
        run_test(
            r#"
            k = Class.new { define_method(:m) { [1, 2, 3].each { |x| return x if x == 2 }; :no_return } }
            k.new.m
            "#,
        );
    }

    #[test]
    fn define_method_in_eval() {
        // `def` inside string class_eval inherits the receiver's lexical
        // context so that constants defined in C are visible.
        run_test_once(
            r#"
            class C
              X = 99
            end
            C.class_eval "def get_x; X; end"
            C.new.get_x
            "#,
        );
    }

    #[test]
    fn module_new() {
        // Module.new returns an anonymous module with nil name
        run_test(
            r#"
            m = Module.new
            m.name
            "#,
        );
        // Module.new creates a proper Module
        run_test(
            r#"
            Module.new.is_a?(Module)
            "#,
        );
    }

    #[test]
    fn module_new_with_block() {
        run_test(
            r#"
            m = Module.new do |mod|
              mod.define_method(:hello) { "world" }
            end
            Class.new { include m }.new.hello
            "#,
        );
    }

    #[test]
    fn module_new_subclass() {
        run_test_once(
            r#"
            class MyMod < Module
              def initialize(name)
                @name = name
                super()
              end
              def my_name; @name; end
            end
            m = MyMod.new("test")
            [m.is_a?(MyMod), m.is_a?(Module), m.my_name]
            "#,
        );
    }

    #[test]
    fn public_instance_method() {
        run_test_with_prelude(
            r##"
            Foo.public_instance_method(:bar).name
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn set_temporary_name() {
        // set_temporary_name returns self
        run_test(
            r#"
            m = Module.new
            m.set_temporary_name("foo").equal?(m)
            "#,
        );
        // set_temporary_name changes name
        run_test(
            r#"
            m = Module.new
            m.set_temporary_name("my_temp")
            m.name
            "#,
        );
        // set_temporary_name with nil resets name
        run_test(
            r#"
            m = Module.new
            m.set_temporary_name("temp")
            m.set_temporary_name(nil)
            m.name
            "#,
        );
        // set_temporary_name works on Class.new
        run_test(
            r#"
            c = Class.new
            c.set_temporary_name("my_cls")
            c.name
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
        // constant lookup in class_eval with string
        run_test_with_prelude(
            r#"C.class_eval("X")"#,
            r#"
        class C
          X = 99
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
        run_test(
            r#"
            module M
            end
            class C1
              include M
            end
            class C2 < C1
              include M   # この include は無視される
            end

            C2.ancestors.inspect  # => [C2, C1, M, Object, Kernel]
            "#,
        );
        run_test(
            r#"
            module M
            end
            class C1
            end
            class C2 < C1
              include M
            end
            class C1
              include M
            end

            C2.ancestors.inspect
            "#,
        );
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
    fn ancestors() {
        run_test_with_prelude(
            r#"
            (C.ancestors - S.ancestors).map(&:to_s)
            "#,
            r#"
            module M
            end
            class S
            end
            class C < S
              include M
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
        run_test_with_prelude(
            r##"
            [
              Foo.instance_methods(false).sort,
              Foo.instance_methods(true).sort - Object.instance_methods(true),
              Foo.private_instance_methods(false).sort,
              Foo.private_instance_methods(true).sort - Object.private_instance_methods(true)
            ]
        "##,
            r##"
        class Foo
          private;   def private_foo()   end
          protected; def protected_foo() end
          public;    def public_foo()    end
        end
        "##,
        );
    }

    #[test]
    fn instance_method() {
        run_test_with_prelude(
            r#"
            $res = []
            interpreter = Interpreter.new
            interpreter.interpret('dave')
            $res
            "#,
            r#"
            class Interpreter
              def do_a() $res << "there, "; end
              def do_d() $res << "Hello ";  end
              def do_e() $res << "!\n";     end
              def do_v() $res << "Dave";    end
              Dispatcher = {
                "a" => instance_method(:do_a),
                "d" => instance_method(:do_d),
                "e" => instance_method(:do_e),
                "v" => instance_method(:do_v)
              }
              def interpret(string)
                string.each_char {|b| Dispatcher[b].bind(self).call }
              end
            end
        "#,
        );
    }

    #[test]
    fn undef_method() {
        run_test_once(
            r#"
            class S
              def f; end
            end
            class C < S
            end
            C.undef_method(:f)
            [
                C.instance_methods - Object.instance_methods,
                S.instance_methods - Object.instance_methods
            ]
            "#,
        );
        run_test_error(
            r##"
            class S
              def f; end
            end
            class C < S
            end
            C.undef_method(:f)
            C.new.f
        "##,
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
        run_test(
            r#"
            class S
              def f; end
            end
            class C < S
            end
            S.remove_method(:f)
            S.instance_methods - Object.instance_methods
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
    fn const_remove() {
        run_test_once(
            r#"
            Bar = 100
            class Foo
              autoload :Bar, "./c"
              def self.remove
                remove_const(:Bar)
              end
              def self.define
                const_set(:Bar, 2)
              end
              def self.get
                Bar
              end
            end
            res = []
            res << Foo.get
            res << Foo.remove
            res << Foo.get
            res << Foo.define
            res << Foo.get
            res
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
        run_test(
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
        run_test(
            r##"
        module A
          def method1; end
          private def private_method1; end
          protected def protected_method1; end
        end
        class B
          def method2; end
          private def private_method2; end
          protected def protected_method2; end
        end
        class C < B
          include A
          def method3; end
        end
        x = []
        x << A.method_defined?(:method1)                         #=> true
        x << A.public_method_defined?(:method1)                  #=> true
        x << A.private_method_defined?(:method1)                 #=> false
        x << A.protected_method_defined?(:method1)               #=> false
        x << C.method_defined?("method1")                        #=> true
        x << C.public_method_defined?("method1")                 #=> true
        x << C.private_method_defined?("method1")                #=> false
        x << C.protected_method_defined?("method1")              #=> false
        x << C.method_defined?("method2")                        #=> true
        x << C.method_defined?("method2", true)                  #=> true
        x << C.method_defined?("method2", false)                 #=> false
        x << C.method_defined?("method3")                        #=> true
        x << C.method_defined?("protected_method1")              #=> true
        x << C.method_defined?("method4")                        #=> false
        x << C.method_defined?("private_method2")                #=> false
        x << C.private_method_defined?("private_method2")        #=> true
        x << C.public_method_defined?("private_method2")         #=> false
        x << C.protected_method_defined?("private_method2")      #=> false
        x << C.private_method_defined?("private_method2", true)  #=> true
        x << C.private_method_defined?("private_method2", false) #=> false
        x << B.private_method_defined?("private_method2", true)  #=> true
        x << B.private_method_defined?("private_method2", false) #=> true
        x
        "##,
        )
    }

    #[test]
    fn included_callback() {
        run_test(
            r##"
        $res = []
        module M
          def self.included(base)
            $res << "included:#{base}"
          end
        end
        class C
          include M
        end
        $res
        "##,
        );
    }

    #[test]
    fn extended_callback() {
        run_test(
            r##"
        $res = []
        module Foo
          def self.extended(base)
            $res << "extended:#{base}"
          end
        end
        module Bar
          extend Foo
        end
        $res
        "##,
        );
        run_test(
            r##"
        $res = []
        module M
          def self.extended(base)
            $res << "extended:#{base}"
          end
        end
        class C
          extend M
        end
        $res
        "##,
        );
    }

    #[test]
    fn prepended_callback() {
        run_test(
            r##"
        $res = []
        module M
          def self.prepended(base)
            $res << "prepended:#{base}"
          end
        end
        class C
          prepend M
        end
        $res
        "##,
        );
    }

    #[test]
    fn append_features_callback() {
        run_test(
            r##"
        $res = []
        module M
          def self.append_features(base)
            $res << "append_features:#{base}"
            super
          end
        end
        class C
          include M
        end
        $res
        "##,
        );
    }

    #[test]
    fn extend_object_callback() {
        run_test(
            r##"
        $res = []
        module M
          def self.extend_object(obj)
            $res << "extend_object:#{obj}"
            super
          end
          def hello
            "hello"
          end
        end
        class C
          extend M
        end
        $res << C.hello
        $res
        "##,
        );
    }

    #[test]
    fn method_added_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.method_added(name)
                $res << name
              end
              def foo; end
              def bar; end
            end
            $res
            "##,
        );
    }

    #[test]
    fn singleton_method_added_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.singleton_method_added(name)
                $res << name
              end
              def self.foo; end
              def self.bar; end
            end
            $res
            "##,
        );
    }

    #[test]
    fn method_removed_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.method_removed(name)
                $res << name
              end
              def foo; end
              def bar; end
              remove_method :foo
            end
            $res
            "##,
        );
    }

    #[test]
    fn method_undefined_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.method_undefined(name)
                $res << name
              end
              def foo; end
              undef_method :foo
            end
            $res
            "##,
        );
    }

    #[test]
    fn singleton_method_removed_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.singleton_method_removed(name)
                $res << name
              end
              def self.foo; end
              def self.bar; end
              class << self
                remove_method :foo
              end
            end
            $res
            "##,
        );
    }

    #[test]
    fn singleton_method_undefined_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.singleton_method_undefined(name)
                $res << name
              end
              def self.foo; end
              def self.bar; end
              class << self
                undef_method :foo, :bar
              end
            end
            $res
            "##,
        );
    }

    #[test]
    fn singleton_method_removed_on_object() {
        run_test(
            r##"
            $res = []
            obj = Object.new
            def obj.singleton_method_removed(name)
              $res << name
            end
            def obj.foo; end
            def obj.bar; end
            class << obj
              remove_method :foo, :bar
            end
            $res
            "##,
        );
    }

    #[test]
    fn singleton_method_undefined_on_object() {
        run_test(
            r##"
            $res = []
            obj = Object.new
            def obj.singleton_method_undefined(name)
              $res << name
            end
            def obj.foo; end
            def obj.bar; end
            class << obj
              undef_method :foo, :bar
            end
            $res
            "##,
        );
    }

    #[test]
    fn const_added_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.const_added(name)
                $res << name
              end
              FOO = 1
              BAR = 2
            end
            $res
            "##,
        );
    }

    #[test]
    fn const_added_const_set() {
        run_test(
            r##"
            $res = []
            class C
              def self.const_added(name)
                $res << name
              end
            end
            C.const_set(:FOO, 1)
            $res
            "##,
        );
    }

    #[test]
    fn define_method_hook() {
        run_test(
            r##"
            $res = []
            class C
              def self.method_added(name)
                $res << name
              end
              define_method(:foo) { 42 }
            end
            $res
            "##,
        );
    }

    #[test]
    fn module_function_hook() {
        run_test_once(
            r##"
            $res = []
            module M
              def self.method_added(name)
                $res << [:method_added, name]
              end
              def self.singleton_method_added(name)
                $res << [:singleton_method_added, name]
              end
              def foo; end
              module_function :foo
            end
            $res
            "##,
        );
    }

    #[test]
    fn scope_const_with_do_block() {
        run_test(
            r##"
        module Foo
          BAR = 42
        end
        def baz(x)
          [x, block_given?]
        end
        baz Foo::BAR do end
        "##,
        );
        run_test(
            r##"
        module Foo
          BAR = 42
        end
        def baz(x)
          x + yield
        end
        baz Foo::BAR do 10 end
        "##,
        );
        run_test(
            r##"
        module A
          module B
            C = 99
          end
        end
        def f(x)
          [x, block_given?]
        end
        f A::B::C do end
        "##,
        );
        run_test(
            r##"
        module Foo
          def self.bar; yield; end
        end
        Foo::bar { 55 }
        "##,
        );
        run_test(
            r##"
        module Foo
          def self.Bar; yield; end
        end
        Foo::Bar { 77 }
        "##,
        );
    }

    #[test]
    fn module_new2() {
        run_test(
            r##"
        m = Module.new
        m.class
        "##,
        );
        run_test(
            r##"
        m = Module.new
        m.is_a?(Module)
        "##,
        );
        run_test(
            r##"
        $res = []
        m = Module.new do |mod|
          $res << mod.class
          def self.foo; 42; end
        end
        $res << m.foo
        $res
        "##,
        );
        run_test(
            r##"
        m = Module.new
        m.module_eval do
          def hello; "hello"; end
        end
        c = Class.new
        c.include(m)
        c.new.hello
        "##,
        );
        run_test(
            r##"
        m = Module.new
        res = []
        res << m.is_a?(Module)
        res << m.class
        n = Module.new
        res << (m == n)
        res
        "##,
        );
        run_test(
            r##"
        m = Module.new do
          def foo; 42; end
        end
        Class.new { include m }.new.foo
        "##,
        );
        run_test(
            r##"
        m1 = Module.new do
          def foo; "m1"; end
        end
        m2 = Module.new do
          include m1
          def foo; super + "+m2"; end
        end
        Class.new { include m2 }.new.foo
        "##,
        );
    }

    #[test]
    fn module_to_s() {
        run_test("Integer.to_s");
        run_test("String.to_s");
        run_test("Module.to_s");
        run_test("Class.to_s");
        run_test(
            r##"
        module Foo; end
        Foo.to_s
        "##,
        );
        run_test(
            r##"
        class Bar; end
        Bar.to_s
        "##,
        );
    }

    #[test]
    fn public_instance_methods() {
        run_test(
            r#"
            class A
              def foo; end
              def bar; end
              private
              def baz; end
            end
            A.public_instance_methods(false).sort
            "#,
        );
        run_test(
            r#"
            class B
              def x; end
            end
            class C < B
              def y; end
            end
            C.public_instance_methods(false).sort
            "#,
        );
        run_test(
            r#"
            class D
              def m1; end
            end
            class E < D
              def m2; end
            end
            (E.public_instance_methods(true) & [:m1, :m2]).sort
            "#,
        );
    }

    #[test]
    fn class_variable_get_set() {
        run_test(
            r#"
            class Foo
              @@x = 42
              def self.get_x; class_variable_get(:@@x); end
            end
            Foo.get_x
            "#,
        );
        run_test(
            r#"
            class Bar
              class_variable_set(:@@val, "hello")
              class_variable_get(:@@val)
            end
            "#,
        );
        run_test(
            r#"
            class A
              @@shared = 10
            end
            class B < A
              def self.read; class_variable_get(:@@shared); end
            end
            B.read
            "#,
        );
        run_test(
            r#"
            class C
              class_variable_set(:@@a, 1)
              class_variable_set(:@@a, 2)
              class_variable_get(:@@a)
            end
            "#,
        );
        run_test_error(
            r#"
            class D; end
            D.class_variable_get(:@@missing)
            "#,
        );
    }

    #[test]
    fn public_class_method() {
        run_test(
            r#"
            module Foo
                def self.bar; 42; end
                private_class_method :bar
                public_class_method :bar
            end
            Foo.bar
            "#,
        );
    }

    // ----- attr_*/attr/alias_method/const_*/class_variable_* coercion paths -----

    #[test]
    fn attr_accessor_coerces_via_to_str() {
        // A non-Symbol/non-String name with a `#to_str` returning a String is
        // installed as the accessor name. Reading and writing both work.
        run_test(
            r#"
            o = Object.new
            def o.to_str; "value"; end
            c = Class.new { attr_accessor o }
            i = c.new
            i.value = 42
            [i.value, i.respond_to?(:value), i.respond_to?(:value=)]
            "#,
        );
    }

    #[test]
    fn attr_to_str_returning_non_string_raises() {
        // `#to_str` returning a non-String value -> TypeError, matching CRuby.
        run_test_error(
            r#"
            o = Object.new
            def o.to_str; 123; end
            Class.new { attr_reader o }
            "#,
        );
    }

    #[test]
    fn attr_no_to_str_raises() {
        // No `#to_str` at all -> TypeError.
        run_test_error(
            r#"
            Class.new { attr_writer Object.new }
            "#,
        );
    }

    #[test]
    fn alias_method_frozen_raises() {
        // `mod.freeze; mod.alias_method(...)` raises FrozenError before
        // installing the alias.
        run_test(
            r#"
            c = Class.new do
              def foo; :foo; end
            end
            c.freeze
            begin
              c.send(:alias_method, :bar, :foo)
              :no_error
            rescue FrozenError
              :ok
            end
            "#,
        );
    }

    #[test]
    fn alias_method_keeps_special_methods_private() {
        // Aliasing TO a special method name (`initialize`, `initialize_copy`,
        // `initialize_clone`, `initialize_dup`, `respond_to_missing?`) forces
        // the alias to be private regardless of the source method's
        // visibility — the spec exercises this for `alias_method`.
        run_test(
            r#"
            c = Class.new do
              def public_one; :one; end
            end
            c.send(:alias_method, :initialize, :public_one)
            c.send(:alias_method, :initialize_copy, :public_one)
            c.send(:alias_method, :initialize_clone, :public_one)
            c.send(:alias_method, :initialize_dup, :public_one)
            c.send(:alias_method, :respond_to_missing?, :public_one)
            [
              c.private_instance_methods(false).include?(:initialize),
              c.private_instance_methods(false).include?(:initialize_copy),
              c.private_instance_methods(false).include?(:initialize_clone),
              c.private_instance_methods(false).include?(:initialize_dup),
              c.private_instance_methods(false).include?(:respond_to_missing?),
            ]
            "#,
        );
    }

    #[test]
    fn alias_method_coerces_via_to_str() {
        run_test(
            r#"
            o = Object.new
            def o.to_str; "alias_target"; end
            n = Object.new
            def n.to_str; "alias_new"; end
            c = Class.new do
              def alias_target; :hit; end
            end
            c.send(:alias_method, n, o)
            c.new.alias_new
            "#,
        );
    }

    #[test]
    fn const_set_validates_name() {
        // Names not starting with an uppercase ASCII letter raise NameError,
        // matching CRuby. Tests cover lowercase, leading `_`, leading `@`,
        // and bare `!`.
        run_test_error(r#"Class.new.const_set("name", 1)"#);
        run_test_error(r#"Class.new.const_set("_X", 1)"#);
        run_test_error(r#"Class.new.const_set("@Name", 1)"#);
        run_test_error(r#"Class.new.const_set("!Name", 1)"#);
    }

    #[test]
    fn const_set_frozen_raises() {
        run_test(
            r#"
            m = Module.new
            m.freeze
            begin
              m.const_set(:Foo, 1)
              :no_error
            rescue FrozenError
              :ok
            end
            "#,
        );
    }

    #[test]
    fn const_defined_validates_name() {
        run_test_error(r#"Module.new.const_defined?("lower")"#);
        run_test_error(r#"Module.new.const_defined?("_X")"#);
    }

    #[test]
    fn const_defined_scoped_path() {
        // String path "A::B::C" walks scopes; leading "::" rebinds to
        // toplevel.
        run_test(
            r#"
            module M; class N; CONST = 1; end; end
            [
              M.const_defined?("N::CONST"),
              Object.const_defined?("M::N::CONST"),
              M.const_defined?("::M"),
            ]
            "#,
        );
    }

    #[test]
    fn const_defined_symbol_with_scope_separator_raises() {
        // A `::`-containing Symbol is NameError (Symbols are bare names).
        run_test_error(
            r#"
            class A; CONST = 1; end
            A.const_defined?(:"A::CONST")
            "#,
        );
    }

    #[test]
    fn const_get_scoped_path_resolves() {
        run_test(
            r#"
            module M; class N; CONST = 99; end; end
            M.const_get("N::CONST")
            "#,
        );
        run_test(
            r#"
            module M; class N; CONST = 99; end; end
            Object.const_get("M::N::CONST")
            "#,
        );
        run_test(
            r#"
            module M; class N; CONST = 99; end; end
            M.const_get("::M::N::CONST")
            "#,
        );
    }

    #[test]
    fn class_variable_set_validates_name() {
        // Class variable names must start with `@@`. Anything else raises
        // NameError.
        run_test_error(r#"Class.new.class_variable_set(:plain, 1)"#);
        run_test_error(r#"Class.new.class_variable_set(:@only_one_at, 1)"#);
        run_test_error(r#"Class.new.class_variable_set(:@@, 1)"#);
    }

    #[test]
    fn class_variable_set_frozen_raises() {
        run_test(
            r#"
            c = Class.new
            c.freeze
            begin
              c.class_variable_set(:@@x, 1)
              :no_error
            rescue FrozenError
              :ok
            end
            "#,
        );
    }

    #[test]
    fn module_function_returns_input_arg() {
        // `module_function(:foo)` returns `:foo`; `module_function(:a, :b)`
        // returns `[:a, :b]`. Single-arg returns the original Symbol/String
        // unchanged (CRuby preserves the caller-supplied form).
        run_test(
            r#"
            m = Module.new do
              def a; end
              def b; end
            end
            r1 = m.send(:module_function, :a)
            r2 = m.send(:module_function, :a, :b)
            [r1, r2]
            "#,
        );
    }

    #[test]
    fn module_function_makes_instance_methods_private() {
        // After `module_function :foo`, `:foo` is callable as a module
        // function (`M.foo`) AND its instance-method side is private (so
        // including `M` exposes `:foo` only via `send`).
        run_test(
            r#"
            m = Module.new do
              def hi; "hello"; end
              module_function :hi
            end
            o = Object.new
            o.extend(m)
            [
              m.respond_to?(:hi),
              m.private_instance_methods.include?(:hi),
              o.respond_to?(:hi),
              o.send(:hi),
            ]
            "#,
        );
    }

    // ----- NameError#name -----

    #[test]
    fn name_error_name_attribute() {
        // `NameError.new(msg, name)` records `name` as the exception's
        // `#name` attribute, so `rescue NameError => e; e.name` returns
        // the missing-name Symbol that the raiser supplied.
        run_test(
            r#"
            e = NameError.new("test message", :missing_constant)
            [e.name, e.message]
            "#,
        );
        // Default `Module#const_missing` (and `Module#const_get`'s
        // fallback through it) sets `e.name` to the missing constant
        // symbol. Round-trips through `rescue` correctly even though
        // monoruby's `raise ex_obj` rebuilds the MonorubyErr.
        run_test(
            r#"
            class CGetMissingNameErr; end
            begin
              CGetMissingNameErr.const_get(:NOPE)
            rescue NameError => e
              e.name
            end
            "#,
        );
        // `Module#instance_method` on a missing method also sets `name`.
        run_test(
            r#"
            begin
              Object.instance_method(:utterly_undefined_method_name)
            rescue NameError => e
              e.name
            end
            "#,
        );
    }

    #[test]
    fn name_error_name_default_nil() {
        // No-arg / default `NameError.new` leaves `#name` as `nil`.
        run_test(
            r#"
            [
              NameError.new.name,
              NameError.new("just a message").name,
            ]
            "#,
        );
    }

    // ----- Module#nesting -----

    #[test]
    fn module_nesting_lexical_chain() {
        // Lexically nested `module`/`class` definitions push entries on
        // `Module.nesting`, in inside-out order.
        run_test(
            r#"
            module ModNestOuter
              module ModNestMiddle
                class ModNestInner
                  $nesting = Module.nesting
                end
              end
            end
            $nesting
            "#,
        );
    }

    #[test]
    fn module_nesting_top_level_empty() {
        // At the top level, `Module.nesting` is empty.
        run_test(r#"Module.nesting"#);
    }

    // ----- Module#attr -----

    #[test]
    fn module_attr_legacy_reader_only() {
        // `attr :foo` (single Symbol) creates a reader, no writer.
        run_test(
            r#"
            c = Class.new { attr :foo }
            i = c.new
            i.instance_variable_set(:@foo, 42)
            [
              i.respond_to?(:foo),
              i.respond_to?(:foo=),
              i.foo,
            ]
            "#,
        );
    }

    #[test]
    fn module_attr_legacy_with_true_creates_writer() {
        // `attr :foo, true` creates BOTH reader and writer (the Ruby
        // 1.x compatibility form).
        run_test(
            r#"
            c = Class.new { attr :foo, true }
            i = c.new
            i.foo = 7
            [i.respond_to?(:foo), i.respond_to?(:foo=), i.foo]
            "#,
        );
    }

    #[test]
    fn module_attr_returns_symbol_array() {
        run_test(
            r#"
            c = Class.new { attr :a, :b }
            c.instance_methods(false).sort
            "#,
        );
    }

    #[test]
    fn module_attr_invalid_name_raises() {
        // A non-Symbol/non-String/non-#to_str arg raises TypeError.
        run_test_error(r#"Class.new { attr 123 }"#);
        run_test_error(r#"Class.new { attr Object.new }"#);
    }

    // ----- Module#autoload? -----

    #[test]
    fn module_autoload_query_returns_path_or_nil() {
        // `autoload?` returns the registered path while the constant is
        // pending, `nil` once it's loaded (or never registered).
        run_test(
            r#"
            m = Module.new do
              autoload :Foo, "/no/such/path"
            end
            [m.autoload?(:Foo), m.autoload?(:Bar)]
            "#,
        );
    }

    #[test]
    fn module_autoload_query_string_name() {
        // `autoload?` accepts a String name as well as a Symbol.
        run_test(
            r#"
            m = Module.new { autoload :X, "/p" }
            [m.autoload?("X"), m.autoload?("Y")]
            "#,
        );
    }

    // ----- Module#class_exec -----

    #[test]
    fn class_exec_defines_methods_on_receiver() {
        // The block runs with `self` set to the receiver class, so
        // `def`s inside become instance methods of the receiver.
        run_test(
            r#"
            c = Class.new
            c.class_exec do
              def hello
                "hello"
              end
            end
            c.new.hello
            "#,
        );
    }

    #[test]
    fn class_exec_passes_args_to_block() {
        // Positional args to `class_exec` are forwarded to the block.
        run_test(
            r#"
            c = Class.new
            captured = nil
            c.class_exec(1, 2, 3) { |*args| captured = args }
            captured
            "#,
        );
    }

    #[test]
    fn class_exec_self_is_receiver() {
        // Inside the block, `self` is the receiver — `self.name`,
        // `define_method`, etc. all act on the class.
        run_test(
            r#"
            c = Class.new
            c.class_exec do
              define_method(:answer) { 42 }
            end
            c.new.answer
            "#,
        );
    }

    // ----- Module#const_source_location -----

    #[test]
    fn const_source_location_records_file_and_line() {
        // Basic shape: returns `[file, line]` for an ordinary
        // user-defined constant.
        run_test(
            r#"
            class CSrcLocBasic
              MY_CONST = 1
            end
            loc = CSrcLocBasic.const_source_location(:MY_CONST)
            [loc.is_a?(Array), loc.size, loc[1].is_a?(Integer)]
            "#,
        );
    }

    #[test]
    fn const_source_location_unknown_returns_nil() {
        // CRuby returns `nil` for an undefined constant (no NameError).
        run_test(
            r#"
            class CSrcLocMissing; end
            CSrcLocMissing.const_source_location(:NOPE)
            "#,
        );
    }

    #[test]
    fn const_source_location_builtin_returns_empty_array() {
        // For a constant defined in C/Rust (e.g. `Object::String`),
        // CRuby returns `[]` to signal "exists but has no Ruby
        // source". Only an undefined constant returns `nil`.
        run_test(r#"Object.const_source_location(:String)"#);
    }

    #[test]
    fn const_source_location_via_const_set() {
        // `Module#const_set` records the call-site so
        // `const_source_location` later returns `[__FILE__, line]` of
        // the `const_set` call.
        run_test(
            r#"
            mod = Module.new
            mod.const_set(:MyConst, 1)
            loc = mod.const_source_location(:MyConst)
            [loc.is_a?(Array), loc.size == 2, loc[1].is_a?(Integer)]
            "#,
        );
    }

    #[test]
    fn const_source_location_scoped_path() {
        // `"A::B"` and `"::A::B"` qualified paths resolve and report
        // the inner constant's recorded location. Rejects malformed
        // paths with NameError.
        run_test(
            r#"
            class CSrcLocScopeOuter
              class Inner
                INNER_CONST = 42
              end
            end
            loc = CSrcLocScopeOuter.const_source_location("Inner::INNER_CONST")
            [loc.is_a?(Array), loc.size == 2]
            "#,
        );
    }

    #[test]
    fn const_source_location_invalid_name_raises() {
        run_test_error(r#"Module.new.const_source_location("lower")"#);
        run_test_error(r#"Module.new.const_source_location(:"A::B")"#);
        run_test_error(r#"Module.new.const_source_location(123)"#);
    }

    // ----- Module#protected_instance_methods -----

    #[test]
    fn protected_instance_methods_lists_only_protected() {
        run_test(
            r#"
            c = Class.new do
              def pub; end
              protected
              def pro1; end
              def pro2; end
              private
              def pri; end
            end
            c.protected_instance_methods(false).sort
            "#,
        );
    }

    #[test]
    fn protected_instance_methods_inherits_by_default() {
        // The inherit flag defaults to `true`. With `false`, only the
        // immediate class is queried.
        run_test(
            r#"
            parent = Class.new do
              protected; def parent_pro; end
            end
            child = Class.new(parent) do
              protected; def child_pro; end
            end
            [
              child.protected_instance_methods.include?(:parent_pro),
              child.protected_instance_methods(false).include?(:parent_pro),
              child.protected_instance_methods(false).include?(:child_pro),
            ]
            "#,
        );
    }

    // ----- Module#class_variable_defined? -----

    #[test]
    fn class_variable_defined_basic() {
        // Use `class_variable_set` to seed the var so the test body
        // doesn't need a `class … @@x = 1 … end` form (CRuby rejects
        // bare `@@x =` inside a `Class.new do … end` block as toplevel
        // class-var access).
        run_test(
            r#"
            c = Class.new
            c.class_variable_set(:@@x, 1)
            [
              c.class_variable_defined?(:@@x),
              c.class_variable_defined?(:@@y),
              c.class_variable_defined?("@@x"),
            ]
            "#,
        );
    }

    #[test]
    fn class_variable_defined_invalid_name_raises() {
        // Names not starting with `@@` raise NameError.
        run_test_error(r#"Class.new.class_variable_defined?(:foo)"#);
        run_test_error(r#"Class.new.class_variable_defined?(:@only_one_at)"#);
    }

    // ----- Module#remove_class_variable -----

    #[test]
    fn remove_class_variable_returns_old_value() {
        // Returns the value that was bound, then the var is gone.
        run_test(
            r#"
            c = Class.new
            c.class_variable_set(:@@x, 99)
            [
              c.remove_class_variable(:@@x),
              c.class_variable_defined?(:@@x),
            ]
            "#,
        );
    }

    #[test]
    fn remove_class_variable_missing_raises() {
        // Removing a non-existent class variable raises NameError.
        run_test_error(
            r#"
            Class.new.remove_class_variable(:@@nope)
            "#,
        );
    }

    // ----- Module#class_variables -----

    #[test]
    fn class_variables_basic() {
        run_test(
            r#"
            c = Class.new
            c.class_variable_set(:@@a, 1)
            c.class_variable_set(:@@b, 2)
            c.class_variables.sort
            "#,
        );
    }

    #[test]
    fn class_variables_includes_inherited_by_default() {
        // The default inherit flag `true` walks the superclass chain.
        run_test(
            r#"
            parent = Class.new
            parent.class_variable_set(:@@inherited, 1)
            child = Class.new(parent)
            child.class_variable_set(:@@own, 2)
            [
              child.class_variables.sort,
              child.class_variables(false).sort,
            ]
            "#,
        );
    }

    // ----- Module#included_modules -----

    #[test]
    fn included_modules_basic() {
        // The receiver itself is *not* listed; included modules are.
        run_test(
            r#"
            m1 = Module.new
            m2 = Module.new
            c = Class.new do
              include m1
              include m2
            end
            inc = c.included_modules
            [inc.include?(m1), inc.include?(m2), inc.include?(c)]
            "#,
        );
    }

    #[test]
    fn included_modules_empty_for_fresh_module() {
        // `Module.new.included_modules == []` — a Module with nothing
        // mixed in has no included modules (it isn't its own).
        run_test(r#"Module.new.included_modules"#);
    }

    // ----- Module#public_constant -----

    #[test]
    fn public_constant_unhides_private_constant() {
        // `private_constant` followed by `public_constant` restores
        // qualified access (`C::FOO`). Use `const_set` instead of a bare
        // `FOO = 1` literal so the constant lands on the new class
        // itself rather than the enclosing lexical scope.
        run_test(
            r#"
            c = Class.new
            c.const_set(:FOO, 1)
            c.send(:private_constant, :FOO)
            c.send(:public_constant, :FOO)
            c::FOO
            "#,
        );
    }

    #[test]
    fn public_constant_unknown_constant_raises() {
        // Calling on an undefined name raises NameError.
        run_test_error(
            r#"
            Class.new.send(:public_constant, :NOPE)
            "#,
        );
    }

    // ----- Module.used_refinements (Ruby-side mock) -----

    #[test]
    fn used_refinements_mock_returns_empty_array() {
        // monoruby has no refinement support; `Module.used_refinements`
        // is mocked in `builtins/startup.rb` to return `[]` so callers
        // (RSpec, Sorbet, …) that defensively read the list don't
        // crash. Only the class form is present, matching CRuby.
        run_test(r#"Module.used_refinements"#);
    }

    #[test]
    fn used_refinements_no_instance_method() {
        // CRuby exposes only the class form, so calling
        // `Module.new.used_refinements` raises NoMethodError. Our mock
        // follows the same shape.
        run_test_error(r#"Module.new.used_refinements"#);
    }

    // ----- eval through builtin frames -----

    #[test]
    fn class_eval_string_through_builtin_caller() {
        // `Module#class_eval` is reachable through a builtin frame
        // (here `Array#each`, `Array#map`). The eval used to require
        // the immediate caller to be a Ruby method; it now walks the
        // CFP chain to find the nearest Ruby frame, so this works.
        run_test(
            r#"
            c = Class.new
            [1].each do |_|
              c.class_eval "def hi; 'hi'; end"
            end
            c.new.hi
            "#,
        );
    }

    #[test]
    fn module_eval_string_through_builtin_caller() {
        run_test(
            r#"
            m = Module.new
            [m].map { |mod| mod.module_eval "def greet; 'hey'; end" }
            o = Object.new
            o.extend(m)
            o.greet
            "#,
        );
    }

    #[test]
    fn class_eval_too_many_args_raises() {
        // `Module#class_eval` takes at most 3 args (expr, fname, lineno);
        // passing 4 raises ArgumentError ("wrong number of arguments
        // (given 4, expected 0..3)").
        run_test_error(
            r#"
            Class.new.class_eval("nil", "f", 1, :extra)
            "#,
        );
    }
}
