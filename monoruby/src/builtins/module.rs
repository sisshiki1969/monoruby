use super::*;
use crate::builtins::kernel::{CheckedName, checked_name, name_error_unknown};

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
    globals.define_builtin_class_func_with(
        MODULE_CLASS,
        "constants",
        module_class_constants,
        0,
        1,
        false,
    );

    // instance methods
    // Two modules are equal only when they are the same object.
    globals.define_builtin_inline_func(
        MODULE_CLASS,
        "==",
        eq,
        inline_gen2!(super::symbol::symbol_eq),
        1,
    );
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
    // Registered as a rest-arg builtin so that the dispatch layer
    // doesn't pre-format the arity error with monoruby's default
    // "expected 0..3"; the function does its own arity check below
    // and produces the CRuby-compatible "expected 1..3" message that
    // ruby/spec asserts on.
    globals.define_builtin_funcs_with_effect(
        MODULE_CLASS,
        "class_eval",
        &["module_eval"],
        class_eval,
        0,
        0,
        true,
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
    globals.define_builtin_func(MODULE_CLASS, "const_missing", const_missing, 1);
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
    globals.define_builtin_func(
        MODULE_CLASS,
        "undefined_instance_methods",
        undefined_instance_methods,
        0,
    );
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
    globals.define_builtin_func_rest(MODULE_CLASS, "deprecate_constant", deprecate_constant);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "ruby2_keywords", ruby2_keywords);
    globals.define_builtin_funcs(MODULE_CLASS, "to_s", &["inspect"], tos, 0);
    globals.define_builtin_func(MODULE_CLASS, "name", name, 0);
    globals.define_builtin_func(MODULE_CLASS, "set_temporary_name", set_temporary_name, 1);
    // private methods
    globals.define_private_builtin_func_rest(MODULE_CLASS, "module_function", module_function);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "private", private);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "protected", protected);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "public", public);
    globals.define_private_builtin_func(MODULE_CLASS, "using", using, 1);
    globals.define_private_builtin_func(MODULE_CLASS, "refine", refine, 1);
    globals.define_builtin_func(MODULE_CLASS, "refinements", refinements, 0);
    globals.define_builtin_class_func(MODULE_CLASS, "used_refinements", used_refinements, 0);
    globals.define_builtin_class_func(MODULE_CLASS, "used_modules", used_modules, 0);
    // `Refinement` — a Module subclass; `refine` returns one.
    let module_obj = globals.store[MODULE_CLASS].get_module();
    globals.define_builtin_class(
        "Refinement",
        REFINEMENT_CLASS,
        module_obj,
        OBJECT_CLASS,
        ObjTy::MODULE,
    );
    globals.define_builtin_func(REFINEMENT_CLASS, "target", refinement_target, 0);
    globals.define_builtin_funcs(
        REFINEMENT_CLASS,
        "to_s",
        &["inspect"],
        refinement_tos,
        0,
    );
    // A refinement is not a mixin: CRuby undefines the hooks that would
    // let one be included / extended, and makes `include` / `prepend`
    // raise rather than silently building a broken ancestor chain.
    for m in ["append_features", "prepend_features", "extend_object"] {
        let _ = globals.undef_method_for_class(REFINEMENT_CLASS, IdentId::get_id(m));
    }
    globals.define_builtin_func_rest(REFINEMENT_CLASS, "include", refinement_include);
    globals.define_builtin_func_rest(REFINEMENT_CLASS, "prepend", refinement_include);
    globals.define_builtin_func_rest(REFINEMENT_CLASS, "import_methods", import_methods);
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
        module_val.change_class_fresh(self_class_id);
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
    let nesting = vm.current_class_nesting(globals);
    let ary: Vec<Value> = nesting
        .into_iter()
        .map(|class_id| globals.store[class_id].get_module().as_val())
        .collect();
    Ok(Value::array_from_vec(ary))
}

///
/// ### Module.constants
///
/// - constants -> [Symbol]
///
/// Returns the list of constants visible from the current lexical
/// scope. With no inner-class context this matches `Object.constants`
/// — the toplevel constant table.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/s/constants.html]
#[monoruby_builtin]
fn module_class_constants(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // No-arg: return toplevel (`Object`) constants — CRuby's
    // `Module.constants` returns the constants visible from the
    // current lexical scope; at top-level that's `Object`'s
    // constants table.
    //
    // With an argument: behave like `Module#constants(inherit)` on
    // the `Module` class itself (`Module.constants(true)` walks the
    // ancestor chain, `(false)` returns direct constants only). The
    // ruby/spec exercises both forms.
    let names = if lfp.try_arg(0).is_none() {
        globals.store.get_constant_names(OBJECT_CLASS)
    } else if lfp.arg(0).as_bool() {
        let class = globals.store[MODULE_CLASS].get_module();
        globals.store.get_constant_names_inherit(class)
    } else {
        globals.store.get_constant_names(MODULE_CLASS)
    };
    let iter = names.into_iter().map(Value::symbol);
    Ok(Value::array_from_iter(iter))
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
    let self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    globals.store.class_modify_check(class_id)?;
    let new_name = lfp.arg(0).coerce_to_symbol_or_string(vm, globals)?;
    let old_name = existing_method_name(vm, globals, class_id, lfp.arg(1))?;
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
    let visi = scope_visibility_for(vm, globals, class_id);
    let args = lfp.arg(0).as_array();
    let mut ary = Array::new_empty();
    // Root the fresh result Array: the warning and the
    // `#to_str`/`#to_sym` coercions below re-enter Ruby while it lives
    // only in this Rust local.
    vm.with_temp_scope(|vm| {
    vm.temp_push(ary.into());

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
        // CRuby emits "boolean argument is obsoleted" when $VERBOSE is
        // true (i.e. ruby -W2). $VERBOSE defaults to nil/false; only
        // `true` triggers the warning, matching CRuby's `rb_warning`
        // gating used here.
        if globals
            .get_gvar(IdentId::get_id("$VERBOSE"))
            .is_some_and(|v| v == Value::bool(true))
        {
            crate::value::emit_verbose_warning(
                vm,
                globals,
                "boolean argument is obsoleted",
            )?;
        }
        let arg_name = attr_name_id(vm, globals, lfp.self_val(), args[0])?;
        let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
        ary.push(Value::symbol(method_name));
        if writable {
            let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
            ary.push(Value::symbol(method_name));
        }
    } else {
        for v in args.iter() {
            let arg_name = attr_name_id(vm, globals, lfp.self_val(), *v)?;
            let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
            ary.push(Value::symbol(method_name));
        }
    }
    Ok(ary.into())
    })
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
    let class_id = lfp.self_val().as_class_id();
    let visi = scope_visibility_for(vm, globals, class_id);
    // The result array is a bare heap object held only by this native
    // frame, but define_attr_* runs the Ruby `method_added` hook, whose
    // poll points can GC — root the array on the temp stack for the
    // duration or a collection there frees it out from under us.
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(None);
        for v in lfp.arg(0).as_array().iter() {
            let arg_name = attr_name_id(vm, globals, lfp.self_val(), *v)?;
            let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
            vm.temp_array_push(Value::symbol(method_name));
            let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
            vm.temp_array_push(Value::symbol(method_name));
        }
        Ok(vm.temp_at(vm.temp_len() - 1))
    })
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
    let class_id = lfp.self_val().as_class_id();
    let visi = scope_visibility_for(vm, globals, class_id);
    // See attr_accessor: the result array must be temp-rooted across the
    // method_added hook.
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(None);
        for v in lfp.arg(0).as_array().iter() {
            let arg_name = attr_name_id(vm, globals, lfp.self_val(), *v)?;
            let method_name = vm.define_attr_reader(globals, class_id, arg_name, visi)?;
            vm.temp_array_push(Value::symbol(method_name));
        }
        Ok(vm.temp_at(vm.temp_len() - 1))
    })
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
    let class_id = lfp.self_val().as_class_id();
    let visi = scope_visibility_for(vm, globals, class_id);
    // See attr_accessor: the result array must be temp-rooted across the
    // method_added hook.
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(None);
        for v in lfp.arg(0).as_array().iter() {
            let arg_name = attr_name_id(vm, globals, lfp.self_val(), *v)?;
            let method_name = vm.define_attr_writer(globals, class_id, arg_name, visi)?;
            vm.temp_array_push(Value::symbol(method_name));
        }
        Ok(vm.temp_at(vm.temp_len() - 1))
    })
}

///
/// ### Module#autoload
///
/// - autoload(const_name, feature) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/autoload.html]
#[monoruby_builtin]
fn autoload(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    autoload_on(vm, globals, lfp.self_val(), lfp.arg(0), lfp.arg(1), pc)
}

/// `rb_str_quote_unprintable`: *s* as it is when it is printable and
/// in the result encoding (`default_internal`, else `default_external`)
/// or ASCII only, and escaped (`rb_str_escape`) otherwise — how CRuby's
/// `QUOTE_ID` shows a name in a message. Answered as UTF-8 text: a name
/// kept as it is in another encoding is valid UTF-8 or ASCII here only
/// when that encoding is UTF-8, so anything else is escaped.
pub(crate) fn quote_unprintable(store: &Store, s: &RStringInner) -> String {
    let enc = s.encoding();
    let resenc = if store.inspect_escape() {
        store.inspect_escape_encoding()
    } else {
        crate::value::Encoding::UTF8
    };
    let bytes = s.as_bytes();
    let printable = match std::str::from_utf8(bytes) {
        Ok(text) if bytes.is_ascii() => text.bytes().all(|b| (0x20..0x7f).contains(&b)),
        Ok(text) => {
            matches!(enc, crate::value::Encoding::Utf8(_))
                && enc == resenc
                && text.chars().all(|c| !c.is_control())
        }
        Err(_) => false,
    };
    if printable {
        // SAFETY: checked to be UTF-8 above.
        unsafe { std::str::from_utf8_unchecked(bytes) }.to_string()
    } else {
        crate::builtins::encoding::str_escape(s)
    }
}

/// Shared body of `Module#autoload` and `Kernel#autoload` (the latter
/// passes its caller's cbase as *target*).
pub(super) fn autoload_on(
    vm: &mut Executor,
    globals: &mut Globals,
    target: Value,
    name_arg: Value,
    feature_arg: Value,
    pc: BytecodePtr,
) -> Result<Value> {
    // `rb_to_id`: a String is interned as `String#to_sym` interns it, so
    // a US-ASCII name with 8-bit bytes is a Symbol, not an EncodingError.
    let const_name = match name_arg.try_symbol() {
        Some(sym) => sym,
        None => {
            let s = if name_arg.is_rstring().is_some() {
                name_arg
            } else {
                match name_arg.check_string_type(vm, globals)? {
                    Some(s) => s,
                    None => {
                        return Err(MonorubyErr::is_not_symbol_nor_string(
                            &globals.store,
                            name_arg,
                        ));
                    }
                }
            };
            crate::builtins::string::intern_string(&globals.store, s.as_rstring_inner())?
        }
    };
    {
        // `rb_autoload_str`'s own complaint, with no name / receiver:
        // the name `QUOTE_ID`'d.
        let name = crate::builtins::symbol::symbol_string_inner(const_name);
        if !is_const_name_bytes(name.as_bytes(), name.encoding()) {
            let shown = quote_unprintable(&globals.store, &name);
            return Err(MonorubyErr::nameerr(format!(
                "autoload must be constant name: {shown}"
            )));
        }
    }
    let feature = feature_arg.coerce_to_path_rstring(vm, globals)?;
    let feature_encoding = feature.encoding();
    let feature = feature.to_str()?.to_string();
    if feature.is_empty() {
        return Err(MonorubyErr::argumenterr("empty file name"));
    }
    // CRuby raises a `FrozenError` *before* recording any autoload state so
    // that `frozen_module.autoload :Foo, ...` neither installs the entry
    // nor stores the constant name.
    target.ensure_not_frozen(&globals.store)?;
    let class_id = target.as_class_id();
    // `autoload :Foo, path` naming a file that is already loaded (or is
    // the current `require`'s in-flight file) still *registers*: CRuby
    // lists the constant in `Module#constants`, fires `const_added`,
    // and brings the entry back to life if the feature ever leaves
    // `$LOADED_FEATURES`. What it does not do is leave anything to
    // load, so the entry is born retired — `autoload?` answers nil and
    // `const_defined?` false for as long as the file stays loaded.
    let already_loaded = autoload_feature_already_loaded(globals, &feature);
    let was_autoload = match globals.store.get_constant(class_id, const_name) {
        None => true,
        Some(state) => state.is_autoload(),
    };
    globals
        .store
        .set_constant_autoload(class_id, const_name, feature, feature_encoding);
    if already_loaded {
        globals
            .store
            .set_autoload_state(class_id, const_name, AutoloadState::Consumed);
    }
    // Record the autoload call-site as the constant's source
    // location so `Module#const_source_location(:Foo)` returns the
    // `autoload` line until the load actually happens — once the
    // loaded file assigns the constant, that assignment overwrites
    // this entry via `Executor::set_constant`'s recording.
    if let Some(caller) = vm.cfp().prev()
        && let Some((file, line)) = caller_source_location_with_pc(globals, caller, pc)
    {
        globals.store[class_id].record_constant_location(const_name, file, line);
    }
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // No symbol for the name means no autoload is registered under it;
    // answer without interning (a query must not grow the symbol table).
    let Some(name) = lfp.arg(0).expect_symbol_or_existing_to_str(vm, globals)? else {
        return Ok(Value::nil());
    };
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    autoload_query_on(globals, lfp.self_val().as_class(), name, inherit)
}

/// Shared body of `Module#autoload?` and `Kernel#autoload?` (the
/// latter passes its caller's cbase).
pub(super) fn autoload_query_on(
    globals: &Globals,
    module: Module,
    name: IdentId,
    inherit: bool,
) -> Result<Value> {
    let mut module = module;
    loop {
        if let Some(state) = globals.store.get_constant(module.id(), name) {
            match &state.kind {
                ConstStateKind::Autoload(entry) => match entry.state {
                    // CRuby 3.0+: to the thread running the load,
                    // `autoload?` is nil — that thread is past the point
                    // where an autoload is pending, there being nothing
                    // left for it to schedule. Every other thread still
                    // has one pending and gets the path, exactly as it
                    // would before the load started.
                    AutoloadState::Loading { .. } if entry.loading_here() => {
                        return Ok(Value::nil());
                    }
                    // The entry's file has already run while it was
                    // registered, so there is nothing left to
                    // schedule — unless the feature has since left
                    // `$LOADED_FEATURES`, which makes it live again.
                    AutoloadState::Consumed if !autoload_pending(globals, &entry.feature) => {
                        return Ok(Value::nil());
                    }
                    // The file is running on this thread — for another
                    // constant registered to it. CRuby finds the feature
                    // provided (`check_autoload_required`), so nothing is
                    // pending until the load is over (#2128).
                    AutoloadState::Idle if autoload_running_here(globals, &entry.feature) => {
                        return Ok(Value::nil());
                    }
                    AutoloadState::Idle
                    | AutoloadState::Consumed
                    | AutoloadState::Loading { .. } => {
                        return Ok(entry.feature_value());
                    }
                },
                // A loaded constant, and a `TOPLEVEL_BINDING` that
                // has not been built yet, both have no autoload to
                // report.
                ConstStateKind::Loaded(_) | ConstStateKind::LazyToplevelBinding => {
                    return Ok(Value::nil());
                }
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

/// Resolve `feature` to a candidate path (absolute, relative, or
/// $LOAD_PATH-searched) and check whether that path is already a member
/// of `$LOADED_FEATURES` *or* is currently being executed by an
/// in-flight `require`. This is what CRuby's `rb_autoload_str` checks
/// before installing an autoload entry — if the file is already
/// loaded/loading, the autoload registration is a no-op.
/// Is a *retired* autoload entry live again?
///
/// Retirement is not permanent: it says "this entry's file has already
/// run while the entry was registered", and CRuby re-checks that
/// against `$LOADED_FEATURES` on every read (`check_autoload_required`).
/// Drop the feature and the entry has something to load once more, so
/// `autoload?` answers the path again and `const_defined?` true.
///
/// Whether the file has run is what separates this from a plain `Idle`
/// entry, which stays pending even when the feature is loaded: a file
/// loaded by *another* constant's autoload never gave this one its
/// chance. See `Executor::require`, which retires only on a direct
/// `require`.
pub(crate) fn autoload_pending(globals: &Globals, feature: &std::path::Path) -> bool {
    !autoload_resolution_candidates(globals, &feature.to_string_lossy())
        .iter()
        .any(|path| globals.is_feature_loaded(path))
}

fn autoload_feature_already_loaded(globals: &Globals, feature: &str) -> bool {
    let candidates = autoload_resolution_candidates(globals, feature);
    for path in &candidates {
        if globals.is_feature_loaded(path) || globals.is_loading_here(path) {
            return true;
        }
    }
    false
}

/// Whether `feature` names a file this thread is in the middle of
/// `require`ing, as far as that can be told without running anything:
/// the candidates `$LOAD_PATH` gives it, and the stub tree's.
///
/// This is the question the non-triggering readers of an autoload ask
/// (`autoload?`, `const_defined?`, `defined?`). CRuby's
/// `check_autoload_required` finds a feature that is being loaded
/// "provided", so to the thread running the file an autoload registered
/// to it has nothing left to load: `autoload?` is nil and
/// `const_defined?` false until the load is over (#2128). A file `load`
/// runs provides no feature and does not count.
pub(crate) fn autoload_running_here(globals: &Globals, feature: &std::path::Path) -> bool {
    // Nearly every autoload is asked about while no file of its name is
    // running; tell by the file name alone, before resolving anything.
    let Some(stem) = feature.file_stem() else {
        return false;
    };
    if !globals.is_loading_stem_here(stem) {
        return false;
    }
    let name = feature.to_string_lossy();
    if autoload_resolution_candidates(globals, &name)
        .iter()
        .any(|path| globals.is_loading_here(path))
    {
        return true;
    }
    // A bare feature may also resolve into the stub tree, which is
    // searched ahead of `$LOAD_PATH` and is not listed on it.
    if feature.is_absolute() || name.starts_with("./") || name.starts_with("../") {
        return false;
    }
    crate::globals::stub_dirs().iter().any(|dir| {
        let path = dir.join(feature);
        if path.extension().is_some() && globals.is_loading_here(&path) {
            return true;
        }
        ["rb", "so"].into_iter().any(|ext| {
            let mut name = path.clone().into_os_string();
            name.push(".");
            name.push(ext);
            globals.is_loading_here(std::path::Path::new(&name))
        })
    })
}

/// Whether `feature` names a file this thread is in the middle of
/// loading.
///
/// CRuby's `check_autoload_required` finds such a feature "provided" and
/// leaves the autoload alone: the constant reads as undefined for now,
/// its registration stays, and the file — which is still running — may
/// yet define it. Dispatching `require` instead could only answer false,
/// with a "circular require" warning on the way and an "Expected … to
/// define …" one after it. The prism gem relies on this: `prism/visitor`
/// is the autoload target of both `BasicVisitor` and `Visitor`, and
/// `class Visitor < BasicVisitor` looks `Visitor` up while the file
/// runs for `BasicVisitor` (#2116).
pub(crate) fn autoload_feature_in_flight(
    vm: &mut Executor,
    globals: &mut Globals,
    feature: &std::path::Path,
) -> bool {
    if autoload_running_here(globals, feature) {
        return true;
    }
    // Whatever else the feature would resolve to: an installed gem's
    // file, a host bundler's.
    let Some(stem) = feature.file_stem() else {
        return false;
    };
    let name = feature.to_string_lossy();
    if !globals.is_loading_stem_here(stem)
        || feature.is_absolute()
        || name.starts_with("./")
        || name.starts_with("../")
    {
        return false;
    }
    globals.search_lib(vm, feature).is_some_and(|found| {
        let found =
            crate::globals::lexically_normalize(&std::path::absolute(&found).unwrap_or(found));
        globals.is_loading_here(&found)
    })
}

/// Build the list of candidate canonical paths that the given autoload
/// `feature` might resolve to. Mirrors the search rules in
/// `Globals::require_lib`/`search_lib` but only returns the paths so we
/// can probe for already-loadedness without performing I/O.
fn autoload_resolution_candidates(
    globals: &Globals,
    feature: &str,
) -> Vec<std::path::PathBuf> {
    use std::collections::HashSet;
    use std::path::PathBuf;
    let mut out: Vec<PathBuf> = Vec::new();
    // Keep the candidates unique, but not by re-scanning what is already
    // there: `PathBuf`'s `==` walks both paths component by component, so
    // this was quadratic in `$LOAD_PATH` — one candidate per entry per
    // extension, each compared against every candidate before it. A Rails
    // app's 166 entries make 332 candidates and ~55,000 whole-path
    // comparisons per resolution, and Zeitwerk asks for a resolution on
    // every missing constant. A callgrind run of ruby-bench's lobsters
    // (boot included) spent 42% of its instructions inside
    // `std::path::Components`. A `HashSet` answers the same question in
    // one hash of the path — `Hash` for `Path` is component-wise too, so
    // it agrees with `==` exactly.
    let mut seen: HashSet<PathBuf> = HashSet::new();
    let push = |out: &mut Vec<PathBuf>, seen: &mut HashSet<PathBuf>, p: PathBuf| {
        if seen.insert(p.clone()) {
            out.push(p);
        }
    };
    let raw = PathBuf::from(feature);
    let with_ext = |p: &std::path::Path| -> Vec<PathBuf> {
        if p.extension().is_some() {
            vec![p.to_path_buf()]
        } else {
            let mut rb = p.to_path_buf();
            rb.set_extension("rb");
            let mut so = p.to_path_buf();
            so.set_extension("so");
            vec![rb, so]
        }
    };
    // Normalise candidates with `path::absolute` (expand `.`/`..`, no
    // symlink resolution) — NOT `canonicalize`. `require_lib_file`
    // registers `$LOADED_FEATURES` entries and `Globals::loading_features`
    // in the `path::absolute` form, so the already-loaded / currently-
    // loading probes below must use the same representation or they miss
    // on macOS where `/tmp`→`/private/tmp` (and `/var/folders`→
    // `/private/var/folders`) symlinks make the two diverge.
    let norm = |p: PathBuf| std::path::absolute(&p).unwrap_or(p);
    if feature.starts_with('/') {
        for c in with_ext(&raw) {
            push(&mut out, &mut seen, norm(c));
        }
        return out;
    }
    if feature.starts_with("./") || feature.starts_with("../") {
        if let Ok(cwd) = std::env::current_dir() {
            let resolved = cwd.join(&raw);
            for c in with_ext(&resolved) {
                push(&mut out, &mut seen, norm(c));
            }
        }
        return out;
    }
    // Bare feature: search every $LOAD_PATH entry.
    for lib in globals.get_load_path().as_array().iter() {
        let lib = match lib.is_str() {
            Some(s) => s,
            None => continue,
        };
        let joined = PathBuf::from(lib).join(&raw);
        for c in with_ext(&joined) {
            push(&mut out, &mut seen, norm(c));
        }
    }
    out
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
    let args = lfp.arg(0).as_array();
    let supplied = args.len();

    // CRuby: `class_eval` accepts at most 3 args (expr, fname, lineno);
    // anything more is an ArgumentError. The reported range is `1..3`
    // because the string form requires the expression argument; the
    // no-arg block form is a separate dispatch and never reaches this
    // arity check.
    if supplied > 3 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {supplied}, expected 1..3)"
        )));
    }

    if let Some(bh) = lfp.block() {
        if supplied != 0 {
            return Err(MonorubyErr::wrong_number_of_arg(0, supplied));
        }
        vm.module_eval(globals, module, bh)
    } else if supplied >= 1 {
        let expr = crate::builtins::eval_source_bytes(vm, globals, args[0])?;
        let cfp = vm.cfp();
        let caller_cfp = cfp.prev().unwrap();
        // The eval body is anchored to the Ruby scope that *wrote* the
        // call — its locals and its lexical nesting — which is not the
        // immediate caller when we were reached through a builtin
        // (`send`, a Rust helper, mspec, …). The location below still
        // comes from the immediate caller, whose pc we were handed.
        let outer_cfp = caller_cfp.nearest_ruby_frame();
        crate::codegen::runtime::convert_frame_for_capture(vm, cfp, outer_cfp)?;
        let path = if supplied >= 2 {
            crate::builtins::eval_file_name(vm, globals, args[1])?
        } else {
            let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
            format!("(eval at {})", caller_loc)
        };
        let lineno: i64 = if supplied >= 3 {
            args[2].num2long(vm, globals)?
        } else {
            1
        };

        let src_encoding = crate::builtins::eval_src_encoding(args[0]);
        let fid = globals.compile_script_eval(
            expr,
            path,
            outer_cfp,
            Some(module.id()),
            lineno,
            src_encoding,
        );
        // The parse's warnings, before the body runs — as `eval` and
        // `instance_eval` print theirs — and before the SyntaxError of
        // a source that does not compile.
        vm.flush_compile_warnings(globals);
        let fid = fid?;
        let proc = ProcData::new(outer_cfp.lfp(), fid);
        // class_eval "..." string form: runtime-only push, just like
        // the block form. The compiled eval body itself receives the
        // module as its lexical context via `compile_script_eval`'s
        // 4th argument, so `module Foo; end` / `X = ...` inside the
        // string still see `module` as the lexical owner.
        vm.push_runtime_class_context(module.id());
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
    // class_exec(&block) is a runtime-only push: `def` inside lands
    // on the receiver, but `module Foo; end` / `X = ...` use the
    // block's captured lexical scope (matches CRuby).
    vm.push_runtime_class_context(module.id());
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
    // CRuby's `Module#const_defined?` does not trigger autoload for
    // the final segment (since Ruby 2.0 — `rb_const_defined` calls
    // `rb_const_defined_0` with `autoload_load = FALSE`). The
    // intermediate qualifiers in a path string like `"A::B::C"` are
    // still resolved through the regular triggering lookup so we can
    // walk into the right class.
    let found = with_constant_path(vm, globals, lfp.arg(0), |vm, globals, path| {
        probe_constant_path(vm, globals, module, path, inherit)
    })?;
    Ok(Value::bool(found))
}

///
/// ### Module#const_missing
///
/// - const_missing(name)
///
/// The default raises CRuby's `uninitialized_constant`: `uninitialized
/// constant Foo` on Object, `uninitialized constant M::Foo` on any other
/// module — `M` being its `#name`, or its `#inspect` when it has none —
/// with the name on `#name` and the module on `#receiver`. The name is
/// shown through `Symbol#to_s` each time the message is read
/// (`MonorubyErr::name_span`); a name given as anything else is shown
/// by its `to_s`, and is `#name` as it is.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_missing.html]
#[monoruby_builtin]
fn const_missing(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let name = lfp.arg(0);
    let prefix = if self_val.as_class_id() == OBJECT_CLASS {
        String::new()
    } else {
        let shown = match vm.invoke_method_inner(
            globals,
            IdentId::get_id("name"),
            self_val,
            &[],
            None,
            None,
        ) {
            Ok(v) if !v.is_nil() => Some(v),
            _ => vm
                .invoke_method_inner(globals, IdentId::INSPECT, self_val, &[], None, None)
                .ok(),
        };
        let shown = shown
            .and_then(|v| {
                v.is_rstring_inner()
                    .map(|s| String::from_utf8_lossy(s.as_bytes()).into_owned())
            })
            .unwrap_or_else(|| globals.store.get_class_name(self_val.as_class_id()));
        format!("{shown}::")
    };
    if let Some(sym) = name.try_symbol() {
        return Err(MonorubyErr::nameerr_showing(
            format_args!("uninitialized constant {prefix}"),
            sym,
            format_args!(""),
            Some(self_val),
        ));
    }
    // The String is formatted into the message as its own bytes, in its
    // own encoding (`const_get` with an EUC-JP name no Symbol exists for).
    let shown = vm.obj_as_string(globals, name)?;
    let shown = shown.as_rstring_inner();
    let (bytes, enc) = (shown.as_bytes().to_vec(), shown.encoding());
    Err(crate::builtins::kernel::name_error_reflection_bytes(
        globals,
        &format!("uninitialized constant {prefix}"),
        &bytes,
        enc,
        "",
        name,
        self_val,
    ))
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
    with_constant_path(vm, globals, lfp.arg(0), |vm, globals, path| {
        lookup_constant_path(vm, globals, module, path, inherit)
    })
}

/// `rb_is_const_name` of a name's bytes in *enc*: a character that
/// starts a constant name ([`is_const_start`]), then name characters
/// ([`ident_chars_from`]). Nothing in an encoding that is not
/// ASCII-compatible is a name.
pub(crate) fn is_const_name_bytes(bytes: &[u8], enc: crate::value::Encoding) -> bool {
    if !enc.is_ascii_compatible() || enc.is_cruby_dummy() {
        return false;
    }
    !bytes.is_empty() && is_const_start(bytes, enc) && ident_chars_from(bytes, enc, 0)
}

/// `is_identchar` over the characters of *bytes* from *start*: an ASCII
/// letter, digit or `_`, or any character that is not ASCII — a byte
/// that starts no character of *enc* included, which CRuby steps over
/// as one (`rb_enc_mbclen`), so a US-ASCII `"A\xE9b"` is a constant
/// name.
pub(crate) fn ident_chars_from(bytes: &[u8], enc: crate::value::Encoding, start: usize) -> bool {
    use crate::value::rvalue::PreciseLen;
    let mut pos = start;
    while pos < bytes.len() {
        let b = bytes[pos];
        if b.is_ascii() {
            if !(b.is_ascii_alphanumeric() || b == b'_') {
                return false;
            }
            pos += 1;
            continue;
        }
        match crate::value::rvalue::precise_mbclen(enc, bytes, pos) {
            PreciseLen::Char(len) => pos += len,
            _ => pos += 1,
        }
    }
    true
}

/// `rb_sym_constant_char_p`: whether the character *bytes* start with
/// starts a constant name in *enc*. An ASCII one is an uppercase letter;
/// any other is one the encoding calls uppercase (or titlecase), or —
/// in an encoding that is not Unicode — one its case folding changes.
///
/// Those tables are Onigmo's, per encoding. What they answer was asked
/// of CRuby 4.0.9 character by character: in a Unicode encoding the
/// Unicode properties; in the single-byte encodings with case tables
/// the bytes in [`single_byte_const_starts`]; in the EUC-JP and
/// Shift_JIS families the full-width Latin, Greek and Cyrillic capital
/// letters (the only letters with a case mapping there); and in every
/// other encoding (the Chinese and Korean sets, BINARY, the code pages
/// without case tables) no character that is not ASCII.
pub(crate) fn is_const_start(bytes: &[u8], enc: crate::value::Encoding) -> bool {
    let Some(&first) = bytes.first() else {
        return false;
    };
    if first.is_ascii() {
        return first.is_ascii_uppercase();
    }
    is_const_start_nonascii(bytes, enc)
}

#[cold]
fn is_const_start_nonascii(bytes: &[u8], enc: crate::value::Encoding) -> bool {
    use crate::value::Encoding as E;
    let first = bytes[0];
    let two = bytes
        .get(1)
        .map(|second| u16::from_be_bytes([first, *second]));
    match enc {
        // CESU-8 stores the Basic Multilingual Plane as UTF-8 does.
        E::Utf8(_) => unicode_const_start(bytes),
        _ if enc.name() == "CESU-8" => unicode_const_start(bytes),
        E::EucJp(_) => {
            two.is_some_and(|c| matches!(c, 0xA3C1..=0xA3DA | 0xA6A1..=0xA6B8 | 0xA7A1..=0xA7C1))
        }
        E::Sjis(_) => {
            two.is_some_and(|c| matches!(c, 0x8260..=0x8279 | 0x839F..=0x83B6 | 0x8440..=0x8460))
        }
        _ => single_byte_const_starts(enc.name()) >> (first - 0x80) & 1 != 0,
    }
}

/// The first character of a UTF-8 name: uppercase or titlecase.
fn unicode_const_start(bytes: &[u8]) -> bool {
    let len = bytes.len().min(4);
    let c = match std::str::from_utf8(&bytes[..len]) {
        Ok(s) => s.chars().next(),
        Err(e) => std::str::from_utf8(&bytes[..e.valid_up_to()])
            .ok()
            .and_then(|s| s.chars().next()),
    };
    c.is_some_and(|c| {
        c.is_uppercase()
            // General category Lt: the titlecase digraphs and the Greek
            // letters with a prosgegrammeni.
            || matches!(
                c,
                '\u{01C5}'
                    | '\u{01C8}'
                    | '\u{01CB}'
                    | '\u{01F2}'
                    | '\u{1F88}'..='\u{1F8F}'
                    | '\u{1F98}'..='\u{1F9F}'
                    | '\u{1FA8}'..='\u{1FAF}'
                    | '\u{1FBC}'
                    | '\u{1FCC}'
                    | '\u{1FFC}'
            )
    })
}

/// The bytes 0x80..=0xFF that start a constant name in a single-byte
/// encoding, bit `b - 0x80` set for byte `b` — as CRuby 4.0.9 answers
/// `const_defined?("<b>x")`. Some rows carry the quirks of Onigmo's
/// ctype tables (Windows-1252 0xAF, Windows-1250 0xD7).
fn single_byte_const_starts(name: &str) -> u128 {
    match name {
        "ISO-8859-1" => 0x000000007f7fffff0000000000000000,
        "ISO-8859-10" => 0x000000007fffffff0000df7e00000000,
        "ISO-8859-13" => 0x000000007f7fffff0000850000000000,
        "ISO-8859-14" => 0x000000007fffffff2895955200000000,
        "ISO-8859-15" => 0x000000007f7fffff5010004000000000,
        "ISO-8859-16" => 0x000000007fffffff5014945a00000000,
        "ISO-8859-2" => 0x000000007f7fffff0000de6a00000000,
        "ISO-8859-3" => 0x000000007f7efff700009e4200000000,
        "ISO-8859-4" => 0x000000007f7fffff20005e6a00000000,
        "ISO-8859-5" => 0x000000000000ffffffffdffe00000000,
        "ISO-8859-7" => 0x000000000ffbfffed740000000000000,
        "ISO-8859-9" => 0x000000007f7fffff0000000000000000,
        "KOI8-R" => 0xffffffff000000000008000000000000,
        "KOI8-U" => 0xffffffff0000000020d8000000000000,
        "Windows-1250" => 0x000000007fffffff100084280000f400,
        "Windows-1251" => 0x00000000ffffffff2004852a0000f403,
        "Windows-1252" => 0x000000007fffffff0000800080005400,
        "Windows-1253" => 0x000000000ffbfffed740000400000000,
        "Windows-1254" => 0x000000007f7fffff0000000080001400,
        "Windows-1257" => 0x000000007f7fffff0000850000000000,
        _ => 0,
    }
}

/// What `rb_mod_const_get` / `rb_mod_const_defined` refuse of a String
/// path before any segment of it is looked up: a path in an encoding
/// that is not ASCII-compatible (an `ArgumentError`), and — segment by
/// segment, each one a name handed to `rb_check_id_cstr` — a segment
/// that is broken in the path's encoding (`EncodingError: invalid
/// symbol in encoding …`, #1987). A well-formed segment ahead of the
/// broken one that is no constant name is refused first, as the walk
/// would have refused it.
fn check_constant_path_string(store: &Store, path: &RStringInner) -> Result<()> {
    let enc = path.encoding();
    if !enc.is_ascii_compatible() || enc.is_cruby_dummy() {
        return Err(MonorubyErr::argumenterr(
            "invalid class path encoding (non ASCII)",
        ));
    }
    if path.is_valid_encoding() {
        return Ok(());
    }
    let bytes = path.as_bytes();
    let mut rest = bytes.strip_prefix(b"::").unwrap_or(bytes);
    loop {
        let (seg, tail) = match rest.windows(2).position(|w| w == b"::") {
            Some(i) => (&rest[..i], Some(&rest[i + 2..])),
            None => (rest, None),
        };
        let seg_str = RStringInner::from_encoding(seg, enc);
        crate::builtins::string::check_symbol_bytes(store, &seg_str, true)?;
        if !is_const_name_bytes(seg, enc) {
            return Err(MonorubyErr::nameerr_with_bytes(
                "wrong constant name ",
                seg,
                enc,
                "",
            ));
        }
        match tail {
            Some(tail) => rest = tail,
            None => return Ok(()),
        }
    }
}

/// One segment of a [`ConstPath`] that is a constant name, as the walk
/// reaches it (`rb_check_id_cstr`'s answer for it then).
enum ConstSeg {
    /// The name, as the Symbol that exists for it.
    Id(IdentId),
    /// A name no Symbol exists for — so nothing is defined under it, and
    /// it is not interned to find that out. The segment as a String (the
    /// path itself when the segment is all of it), which is what a
    /// NameError about it carries as `#name`.
    Unknown(Value),
}

/// What stops the walk of a [`ConstPath`] after its segments.
enum ConstPathBad {
    /// The name given is the wrong one: a Symbol that is no constant
    /// name, or a String path malformed at that point (empty, an empty
    /// segment, a lone `:`, a trailing `::`).
    Whole,
    /// The segment at this byte range of the path, which is no constant
    /// name.
    Seg(std::ops::Range<usize>),
}

/// A constant path given to `const_get` / `const_defined?` /
/// `const_source_location`, as far as it can be walked.
///
/// The walk runs Ruby (an autoload's `require`, a `const_missing`), so
/// the only object this holds is the path itself, which the walk keeps
/// on the temp stack ([`with_constant_path`]). A segment's Symbol is
/// looked for, and its String or the NameError made, when the walk
/// reaches it, as in CRuby: one made earlier would be freed by a
/// collection in that Ruby, and a Symbol the Ruby made (an autoloaded
/// file defining the next segment) would be missed.
struct ConstPath {
    /// A String path that starts with `::` is looked up from Object.
    start_at_object: bool,
    /// The byte ranges, in `bytes`, of the leading segments that are
    /// constant names.
    segs: Vec<std::ops::Range<usize>>,
    /// What stops the walk after `segs`: the "wrong constant name"
    /// NameError of the first segment that is no constant name (or of
    /// the whole path, when it is malformed at that point). CRuby
    /// validates a segment only when the walk reaches it, so
    /// `"Nope::bad"` is an uninitialized `Nope` and `"X::y"` with a
    /// non-module `X` a TypeError before `y` is looked at.
    bad: Option<ConstPathBad>,
    /// The path as given (a Symbol, or the String), for messages.
    path: Value,
    /// The path's bytes (a Symbol's name), in its encoding.
    bytes: Vec<u8>,
    enc: crate::value::Encoding,
}

impl ConstPath {
    /// The index of the path's final segment, `None` when the walk
    /// ends in [`Self::bad`] instead.
    fn last_idx(&self) -> Option<usize> {
        match self.bad {
            Some(_) => None,
            None => Some(self.segs.len() - 1),
        }
    }

    /// Segment *i*: its Symbol when one of that name exists by now
    /// (the Symbol given, for a Symbol path), else the segment as a
    /// String.
    fn seg(&self, i: usize) -> ConstSeg {
        if let Some(sym) = self.path.try_symbol() {
            return ConstSeg::Id(sym);
        }
        let range = self.segs[i].clone();
        match existing_name_id(&self.bytes[range.clone()], self.enc) {
            Some(id) => ConstSeg::Id(id),
            None => ConstSeg::Unknown(self.seg_string(range)),
        }
    }

    /// The segment at *range* as a frozen String, or the path itself
    /// when the segment is all of it.
    fn seg_string(&self, range: std::ops::Range<usize>) -> Value {
        if range.len() == self.bytes.len() {
            return self.path;
        }
        let mut name =
            Value::string_from_inner(RStringInner::from_encoding(&self.bytes[range], self.enc));
        name.set_frozen();
        name
    }

    /// The NameError [`Self::bad`] stands for, carrying *receiver* and —
    /// as `#name` — the path given when it is wrong as a whole, else the
    /// segment's Symbol when one of that name exists, the segment itself
    /// otherwise.
    fn bad_error(&self, globals: &mut Globals, receiver: Value) -> MonorubyErr {
        let (bytes, name) = match &self.bad {
            Some(ConstPathBad::Seg(range)) => {
                let seg = &self.bytes[range.clone()];
                let name = match existing_name_id(seg, self.enc) {
                    Some(id) => Value::symbol(id),
                    None => self.seg_string(range.clone()),
                };
                (seg, name)
            }
            _ => (&self.bytes[..], self.path),
        };
        crate::builtins::kernel::name_error_reflection_bytes(
            globals,
            "wrong constant name ",
            bytes,
            self.enc,
            "",
            name,
            receiver,
        )
    }

    /// `"X::y" does not refer to class/module`: a segment that has
    /// more path after it resolved to something that is no module.
    fn not_a_module(&self, store: &Store) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} does not refer to class/module",
            self.path.to_s(store)
        ))
    }
}

/// The Symbol named *name* (in *enc*), when one exists
/// (`rb_check_id_cstr`).
fn existing_name_id(name: &[u8], enc: crate::value::Encoding) -> Option<IdentId> {
    match std::str::from_utf8(name) {
        Ok(s) => IdentId::try_get_id(s),
        Err(_) => IdentId::try_get_id_from_bytes(name, enc),
    }
}

/// Run *walk* over the [`ConstPath`] *name_arg* denotes, with the path
/// on the temp stack: it may be the String a `#to_str` answered, which
/// nothing else holds while the walk runs Ruby (an autoload's
/// `require`, a `const_missing`), and a collection there would free it.
fn with_constant_path<T>(
    vm: &mut Executor,
    globals: &mut Globals,
    name_arg: Value,
    walk: impl FnOnce(&mut Executor, &mut Globals, ConstPath) -> Result<T>,
) -> Result<T> {
    let path = constant_path_segments(vm, globals, name_arg)?;
    vm.with_temp_scope(|vm| {
        vm.temp_push(path.path);
        walk(vm, globals, path)
    })
}

/// Coerce *name_arg* to a Symbol or String (via `#to_str`) and split it
/// into the sequence of constant names the path denotes
/// (`rb_mod_const_get`'s reading). A Symbol is a bare name; a String
/// may be fully-qualified (`"::A::B"`: start at Object) or relative
/// (`"A::B"`).
///
/// A segment that is no constant name ends the list and is reported
/// through [`ConstPath::bad`] — as is a path malformed there (an empty
/// segment, a trailing `::`).
fn constant_path_segments(
    vm: &mut Executor,
    globals: &mut Globals,
    name_arg: Value,
) -> Result<ConstPath> {
    if let Some(sym) = name_arg.try_symbol() {
        // A Symbol is a bare name: it carries no scope qualifiers.
        let bytes = sym.get_ident_name_clone().as_bytes().to_vec();
        let enc = sym
            .symbol_encoding()
            .unwrap_or(crate::value::Encoding::UTF8);
        let (segs, bad) = if is_const_name_bytes(&bytes, enc) {
            (vec![0..bytes.len()], None)
        } else {
            (vec![], Some(ConstPathBad::Whole))
        };
        return Ok(ConstPath {
            start_at_object: false,
            segs,
            bad,
            path: name_arg,
            bytes,
            enc,
        });
    }
    let path = if name_arg.is_rstring().is_some() {
        name_arg
    } else if let Some(result) =
        crate::coerce::check_funcall(vm, globals, name_arg, IdentId::TO_STR)?
    {
        if result.is_rstring().is_none() {
            return Err(MonorubyErr::cant_convert_error(
                &globals.store,
                name_arg,
                result,
                "String",
                IdentId::TO_STR,
            ));
        }
        result
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into String",
            name_arg.builtin_class_name(&globals.store)
        )));
    };
    check_constant_path_string(&globals.store, path.as_rstring_inner())?;
    let inner = path.as_rstring_inner();
    let (bytes, enc) = (inner.as_bytes().to_vec(), inner.encoding());
    // `::` — being ASCII — never occurs inside a character of an
    // ASCII-compatible encoding, so the path splits bytewise.
    let start_at_object = bytes.len() > 2 && bytes.starts_with(b"::");
    let mut start = if start_at_object { 2 } else { 0 };
    let mut segs = vec![];
    let bad = loop {
        let end = bytes[start..]
            .iter()
            .position(|c| *c == b':')
            .map_or(bytes.len(), |i| start + i);
        // An empty segment (or an empty path), a lone `:` and a
        // trailing `::` make the whole path the wrong name, at the
        // segment they are met at.
        if start == end {
            break Some(ConstPathBad::Whole);
        }
        let next = if end == bytes.len() {
            None
        } else if bytes[end..].starts_with(b"::") && end + 2 < bytes.len() {
            Some(end + 2)
        } else {
            break Some(ConstPathBad::Whole);
        };
        if !is_const_name_bytes(&bytes[start..end], enc) {
            break Some(ConstPathBad::Seg(start..end));
        }
        segs.push(start..end);
        match next {
            Some(next) => start = next,
            None => break None,
        }
    };
    Ok(ConstPath {
        start_at_object,
        segs,
        bad,
        path,
        bytes,
        enc,
    })
}

/// `const_missing` for the segment *name* that *scope* does not have
/// (`rb_const_missing`); whatever it answers stands in for the
/// constant. A NameError it raises is about that name.
fn call_const_missing(
    vm: &mut Executor,
    globals: &mut Globals,
    scope: Module,
    name: IdentId,
) -> Result<Value> {
    match vm.invoke_method_inner(
        globals,
        IdentId::get_id("const_missing"),
        scope.as_val(),
        &[Value::symbol(name)],
        None,
        None,
    ) {
        Err(e) if matches!(e.kind, MonorubyErrKind::Name(..)) => Err(e.nameerr_about(name, None)),
        res => res,
    }
}

/// [`call_const_missing`] for a segment no Symbol exists for
/// (`rb_mod_const_get`): the default `const_missing` raises its
/// NameError about the String itself — no Symbol is made for a name
/// that names nothing — while a `const_missing` of the scope's own is
/// called with the Symbol.
fn call_const_missing_unknown(
    vm: &mut Executor,
    globals: &mut Globals,
    scope: Module,
    name: Value,
) -> Result<Value> {
    let const_missing = IdentId::get_id("const_missing");
    let basic = globals
        .check_method(scope.as_val(), const_missing)
        .is_none_or(|fid| matches!(globals.store[fid].kind, FuncKind::Builtin { .. }));
    if !basic {
        let id = crate::builtins::string::intern_name_string(
            &globals.store,
            name.as_rstring_inner(),
        )?;
        return call_const_missing(vm, globals, scope, id);
    }
    vm.invoke_method_inner(globals, const_missing, scope.as_val(), &[name], None, None)
}

/// Walk a (possibly scoped) constant path for `Module#const_get`
/// (`rb_mod_const_get`): a segment the scope reached so far does not
/// have goes to that scope's `const_missing`, a segment with more path
/// after it has to be a class or module (TypeError), and a malformed
/// path is a NameError where the walk meets it.
fn lookup_constant_path(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Module,
    path: ConstPath,
    inherit: bool,
) -> Result<Value> {
    // Walk the scope chain. The first segment honours `inherit`; nested
    // segments are looked up directly on the resolved class (CRuby
    // scopes `Foo::Bar::Baz` like the parser would, so each `::` step
    // is a non-inherited lookup *unless* the user asked for inherited
    // and we're still at the receiver).
    let mut current = if path.start_at_object {
        globals.store[OBJECT_CLASS].get_module()
    } else {
        receiver
    };
    let last_idx = path.last_idx();
    for i in 0..path.segs.len() {
        let id = match path.seg(i) {
            ConstSeg::Id(id) => id,
            ConstSeg::Unknown(name) => {
                let v = call_const_missing_unknown(vm, globals, current, name)?;
                if Some(i) == last_idx {
                    return Ok(v);
                }
                match v.is_class_or_module() {
                    Some(m) => current = m,
                    None => return Err(path.not_a_module(&globals.store)),
                }
                continue;
            }
        };
        // For the *first* segment, defer to the inherit flag (and
        // implicitly walk Object for modules / superclasses for
        // classes, as `get_constant_superclass_with_class` does). For
        // later segments, only the resolved class itself is checked.
        let val = if i == 0 && inherit {
            vm.get_constant_superclass(globals, current, id)?
        } else if globals.store[current.id()].has_own_constant(id) {
            // For nested segments, restrict to *direct* lookup on the
            // resolved class (no superclass walk). `get_constant` fires
            // an autoload if one is registered; an error the autoloaded
            // file raised propagates (CRuby lets it through too), and
            // only a genuinely missing constant answers None.
            vm.get_constant(globals, current.id(), id)?
        } else {
            None
        };
        let v = match val {
            Some(v) => v,
            None => call_const_missing(vm, globals, current, id)?,
        };
        if Some(i) == last_idx {
            return Ok(v);
        }
        match v.is_class_or_module() {
            Some(m) => current = m,
            None => return Err(path.not_a_module(&globals.store)),
        }
    }
    Err(path.bad_error(globals, receiver.as_val()))
}

/// Like `lookup_constant_path` but probes the *final* segment without
/// firing autoload — used by `Module#const_defined?` which (since
/// Ruby 2.0) reports an autoload-registered constant as defined
/// without invoking `require`. Intermediate qualifiers still trigger
/// autoload, since we need the actual class object to walk further.
fn probe_constant_path(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Module,
    path: ConstPath,
    inherit: bool,
) -> Result<bool> {
    let mut current = if path.start_at_object {
        globals.store[OBJECT_CLASS].get_module()
    } else {
        receiver
    };
    let last_idx = path.last_idx();
    for i in 0..path.segs.len() {
        // A name no Symbol exists for is not defined anywhere.
        let ConstSeg::Id(id) = path.seg(i) else {
            return Ok(false);
        };
        if Some(i) == last_idx {
            // Final segment: non-triggering probe. For the receiver
            // (i == 0) honour `inherit`; for nested resolved classes
            // (i > 0) restrict to direct lookup, matching the
            // triggering variant.
            return Ok(if i == 0 && inherit {
                Executor::probe_constant_superclass(globals, current, id)
            } else {
                Executor::probe_constant_at(globals, current.id(), id)
            });
        }
        // Intermediate segment: resolve via the triggering path so
        // we can walk into the named class. A miss short-circuits
        // to "not defined".
        let val = if i == 0 && inherit {
            vm.get_constant_superclass(globals, current, id)?
        } else if globals.store[current.id()].has_own_constant(id) {
            match vm.get_constant_checked(globals, current.id(), id) {
                Ok(v) => Some(v),
                Err(_) => None,
            }
        } else {
            None
        };
        match val {
            Some(v) => match v.is_class_or_module() {
                Some(m) => current = m,
                None => return Err(path.not_a_module(&globals.store)),
            },
            None => return Ok(false),
        }
    }
    Err(path.bad_error(globals, receiver.as_val()))
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
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let name = crate::builtins::kernel::setter_name_id(
        vm,
        globals,
        self_val,
        lfp.arg(0),
        is_const_name_bytes,
        "wrong constant name ",
        "",
    )?;
    let module = self_val.as_class().id();
    let val = lfp.arg(1);
    // Warn when overwriting an existing same-name constant on the
    // receiver, matching CRuby's redefinition warning.
    let location = vm.nearest_caller_location(&globals.store);
    vm.warn_already_initialized_constant(globals, module, name, location);
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
    let result = with_constant_path(vm, globals, lfp.arg(0), |_, globals, path| {
        lookup_constant_with_owner(globals, module, path, inherit)
    })?;
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
/// `Ok(None)` when no segment in the path resolves. The path is read as
/// `Module#const_get` reads it (see [`with_constant_path`]).
fn lookup_constant_with_owner(
    globals: &mut Globals,
    receiver: Module,
    path: ConstPath,
    inherit: bool,
) -> Result<Option<(ClassId, Option<(String, u32)>)>> {
    let mut current = if path.start_at_object {
        globals.store[OBJECT_CLASS].get_module()
    } else {
        receiver
    };
    let last_idx = path.last_idx();
    for i in 0..path.segs.len() {
        // A name no Symbol exists for is not defined anywhere.
        let ConstSeg::Id(id) = path.seg(i) else {
            return Ok(None);
        };
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
        if Some(i) == last_idx {
            return Ok(Some((owner, loc)));
        }
        // Resolve the value to descend into for the next segment.
        let val = match globals.store.get_constant_noautoload(owner, id) {
            Some(v) => v,
            None => return Ok(None),
        };
        match val.is_class_or_module() {
            Some(m) => current = m,
            None => return Err(path.not_a_module(&globals.store)),
        }
    }
    Err(path.bad_error(globals, receiver.as_val()))
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
    let name = crate::builtins::kernel::setter_name_check(
        vm,
        globals,
        lfp.self_val(),
        lfp.arg(0),
        is_const_name_bytes,
        "'",
        "' is not allowed as a constant variable name",
    )?;
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let module = lfp.self_val().as_class().id();
    let name = match name {
        CheckedName::Id(name) => name,
        CheckedName::Unknown(name) => {
            let before = format!("constant {}::", globals.store.get_class_name(module));
            return Err(crate::builtins::kernel::name_error_unknown(
                globals,
                &before,
                name,
                " not defined",
                lfp.self_val(),
            ));
        }
    };
    // CRuby 3.4+ emits the deprecation warning when removing a
    // deprecated constant (matches `remove_const` reading the slot
    // before deleting it). The message uses the receiver's full
    // qualified name even for anonymous modules.
    if globals
        .store
        .get_constant(module, name)
        .is_some_and(|s| s.is_deprecated())
    {
        let qualified = format!("{}::{}", module.get_name(&globals.store), name);
        crate::value::emit_deprecated_constant_warning(vm, globals, &qualified)?;
    }
    match globals.remove_constant(module, name) {
        Some(v) => Ok(v),
        None => Err(MonorubyErr::constant_not_defined(
            &globals.store,
            name,
            module,
        )),
    }
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
    define_method_in(vm, globals, lfp, pc, class_id, false)
}

/// The name of a `define_method` / `define_singleton_method` call:
/// `rb_check_id`, then the checks on the body, and only then a Symbol
/// made for a name that has none — so a call that is refused makes no
/// Symbol (CRuby's `rb_mod_define_method_with_visibility`).
/// *class_name* says whether a body of the wrong type is named by its
/// class (`define_method`) or as `wrong_argument_type` does.
pub(super) fn defined_method_name(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    class_name: bool,
) -> Result<IdentId> {
    let name = super::kernel::checked_name(vm, globals, lfp.arg(0))?;
    if let Some(body) = lfp.try_arg(1) {
        if body.is_proc().is_none() && body.is_method().is_none() && body.is_umethod().is_none() {
            return Err(if class_name {
                // `rb_mod_define_method` names the argument's *class*, so
                // `nil` reports `NilClass` rather than the
                // `wrong_argument_type` helper's keyword spelling.
                MonorubyErr::typeerr(format!(
                    "wrong argument type {} (expected Proc/Method/UnboundMethod)",
                    body.get_real_class_name(&globals.store),
                ))
            } else {
                MonorubyErr::wrong_argument_type(globals, body, "Proc/Method/UnboundMethod")
            });
        }
    } else if lfp.block().is_none() {
        // No body at all: CRuby's `rb_block_lambda` reports the missing
        // block rather than an arity mismatch.
        return Err(MonorubyErr::argumenterr(
            "tried to create Proc object without a block",
        ));
    }
    match name {
        super::kernel::CheckedName::Id(id) => Ok(id),
        super::kernel::CheckedName::Unknown(s) => {
            super::string::intern_name_string(&globals.store, s.as_rstring_inner())
        }
    }
}

/// `Module#define_method` on *class_id*, with the name in `arg(0)` and
/// the body in `arg(1)` or the block. `main.define_method` is this on
/// `Object` (CRuby's `top_define_method` calls `rb_mod_define_method`
/// with `rb_cObject`), where *toplevel* makes the method public whatever
/// the default visibility of the toplevel is.
pub(super) fn define_method_in(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    class_id: ClassId,
    toplevel: bool,
) -> Result<Value> {
    globals.store.class_modify_check(class_id)?;
    // The name accepts Symbol / String and anything `to_str`-convertible
    // (CRuby's `define_method(obj)` with `obj.to_str`).
    let name = defined_method_name(vm, globals, lfp, true)?;
    // For proc-based define_method, the runtime LFP has the *block*'s
    // FuncId (the wrapper jumps directly to the block code, only
    // tagging the meta with `PROC_METHOD_MASK`). For `super` to work
    // we need that block FuncId itself to (a) have a `name` we can
    // look up and (b) have its owner_class registered so
    // `check_super`'s walk recognises the method's home class.
    let block_fid_to_decorate: Option<FuncId>;
    // Original definition name to record on the new method entry.
    // Defaults to the installed name; a Method/UnboundMethod source
    // propagates its own original name so `Method#original_name`
    // recovers e.g. `Kernel#is_a?` behind `define_method(:my_is_a?, …)`.
    let mut original_name = name;
    let func_id = if let Some(method) = lfp.try_arg(1) {
        if let Some(proc) = method.is_proc() {
            let fid = globals.define_proc_method(proc);
            // A method defined via `define_method` enforces strict arity
            // (no destructuring of a single Array argument, ArgumentError
            // on count mismatch), unlike the underlying Proc.
            globals.store[fid].set_method_style();
            globals.store[fid].set_name(name);
            block_fid_to_decorate = Some(proc.func_id());
            fid
        } else if let Some(m) = method.is_method() {
            validate_bind_target(globals, m.owner(), class_id)?;
            original_name = m.original_name(&globals.store);
            block_fid_to_decorate = None;
            m.func_id()
        } else if let Some(umethod) = method.is_umethod() {
            validate_bind_target(globals, umethod.owner(), class_id)?;
            original_name = umethod.original_name(&globals.store);
            block_fid_to_decorate = None;
            umethod.func_id()
        } else {
            // `rb_mod_define_method` names the argument's *class*, so
            // `nil` reports `NilClass` rather than the `wrong_argument_type`
            // helper's keyword spelling.
            return Err(MonorubyErr::typeerr(format!(
                "wrong argument type {} (expected Proc/Method/UnboundMethod)",
                method.get_real_class_name(&globals.store),
            )));
        }
    } else if let Some(bh) = lfp.block() {
        let proc = vm.generate_proc(globals, bh, pc)?;
        let fid = globals.define_proc_method(proc);
        globals.store[fid].set_method_style();
        globals.store[fid].set_name(name);
        block_fid_to_decorate = Some(proc.func_id());
        fid
    } else {
        // No body at all: CRuby's `rb_block_lambda` reports the missing
        // block rather than an arity mismatch.
        return Err(MonorubyErr::argumenterr(
            "tried to create Proc object without a block",
        ));
    };
    // At the toplevel `self` is `main`, not `Object`, so there is no
    // cref to take a default visibility (or a `module_function` toggle)
    // from: the method is public, unlike a toplevel `def`.
    let visibility = if !toplevel {
        define_method_visibility(vm, globals, class_id, name)
    } else if is_special_private_method_name(name) {
        Visibility::Private
    } else {
        Visibility::Public
    };
    // Honor the `module_function` toggle (set via `module_function`
    // with no arguments earlier in the same body): when the target
    // matches the current lexical class AND the toggle is on, the
    // method is installed as a private instance method *and* a
    // module-level singleton method, exactly like a `def` in the
    // same scope. Cross-class `klass.send(:define_method, ...)`
    // never enters module-function mode (the toggle is per-cref).
    if !toplevel && vm.context_class_id() == class_id && vm.is_module_function() {
        vm.add_method_with_original(
            globals,
            class_id,
            name,
            func_id,
            Visibility::Private,
            original_name,
        )?;
        vm.add_singleton_method(globals, class_id, name, func_id, Visibility::Public)?;
    } else {
        vm.add_method_with_original(
            globals,
            class_id,
            name,
            func_id,
            visibility,
            original_name,
        )?;
    }
    // For proc-based define_method: also decorate the block iseq's
    // FuncInfo with the installed name and owner so `find_super`
    // (which looks at the runtime LFP's block FuncId, not the
    // proc-method wrapper FuncId) can resolve correctly.
    if let Some(block_fid) = block_fid_to_decorate {
        if globals.store[block_fid].name().is_none() {
            globals.store[block_fid].set_name(name);
        }
        // `set_owner_class` appends — but only push when this owner
        // isn't already recorded, to avoid an unbounded grow on
        // repeated re-registration of the same proc under the same
        // class (e.g. `define_method` inside a hot loop).
        if !globals.store[block_fid]
            .owner_class()
            .contains(&class_id)
        {
            globals.store[block_fid].set_owner_class(class_id);
        }
        // Tag the block's static Meta with the proc-method bit.
        // The dynamic OR in `gen_wrapper` only fires for the
        // unspecialised dispatch path; JIT-specialised calls copy
        // this static Meta verbatim into LFP_META, so without this
        // bit `Lfp::outermost` would walk past the method boundary
        // once JIT compilation kicks in (manifested as
        // `undefined method '/main'` from `super`).
        globals.store[block_fid].set_proc_method();
        // The installed body's cref is the scope the block/proc was
        // *written* in (its lexical class) — a nested `def` inside it
        // lands there, not on the define target (CRuby: a Proc created
        // in class T and installed on class C still defines nested
        // methods in T). See `ISeqInfo::nested_definee`.
        if let Some(iseq) = globals.store[block_fid].is_iseq() {
            // Block iseqs carry no lexical_context of their own; the
            // write-site scope is the mother iseq's (main, a class
            // body, or an enclosing method).
            let mother = globals.store[iseq].mother().0;
            let cref_class = globals.store[mother]
                .lexical_context
                .last()
                .copied()
                .unwrap_or(OBJECT_CLASS);
            globals.store[iseq].nested_definee = Some(cref_class);
        }
    }
    Ok(Value::symbol(name))
}

/// Validate that a `Method` / `UnboundMethod` from `owner` may be
/// reinstalled on `target` via `define_method`. Mirrors CRuby's
/// `bind_check`:
///
/// - **Module owner** (e.g. `mixin.instance_method(:bar)`): any
///   class/module may receive the rebound method, since the module
///   could be included into target. No further check.
/// - **Class owner**: `target`'s ancestor chain must include `owner`
///   (target inherits from or *is* `owner`). The singleton-class
///   chain mirrors the instance chain, so `child.singleton_class`
///   accepts methods from `parent.singleton_class`.
/// - **Singleton-class owner unrelated to target**: error message
///   uses CRuby's "can't bind singleton method to a different class"
///   wording instead of the generic "subclass of …" form.
pub(crate) fn validate_bind_target(
    globals: &Globals,
    owner: ClassId,
    target: ClassId,
) -> Result<()> {
    let owner_module = globals.store[owner].get_module();
    // Module owners (not Class) bind freely — the module can be
    // included on any receiver. CRuby's `rb_obj_method_arity` and
    // `method_def_iseq` skip the subclass check in that case.
    if owner_module.as_val().ty() == Some(ObjTy::MODULE) {
        return Ok(());
    }
    if globals
        .store
        .ancestors(target)
        .iter()
        .any(|m| m.id() == owner)
    {
        return Ok(());
    }
    if owner_module.is_singleton().is_some() {
        return Err(MonorubyErr::typeerr(
            "can't bind singleton method to a different class",
        ));
    }
    Err(MonorubyErr::typeerr(format!(
        "bind argument must be a subclass of {}",
        globals.store.get_class_name(owner)
    )))
}

/// The name argument of `attr` / `attr_reader` / `attr_writer` /
/// `attr_accessor` (`id_for_attr`): a local-variable or a constant
/// name, anything else — `"bad name"`, `:x?`, `:@x`, `""` — being
/// NameError "invalid attribute name '…'".
fn attr_name_id(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Value,
    arg: Value,
) -> Result<IdentId> {
    crate::builtins::kernel::setter_name_id(
        vm,
        globals,
        receiver,
        arg,
        crate::builtins::kernel::is_valid_attr_name,
        "invalid attribute name '",
        "'",
    )
}

/// The default visibility of the scope calling a builtin that defines
/// methods on `target` (`attr_*`, `define_method`), as far as it
/// applies to `target` — CRuby's `rb_vm_cref_in_context(klass, klass)`
/// followed by `vm_scope_visibility_get`.
///
/// The calling scope's `private` / `protected` counts only when that
/// scope is defining methods for `target` itself: the nearest Ruby
/// frame's `self` is `target`, its cref class is `target`, and the
/// frame carries a cref of its own, which a method body does not (its
/// default is always public). So `Foo.send(:attr_accessor, :x)` from
/// the top level (whose default is private), from a method called
/// there, or from another class's body after `private` defines public
/// accessors, and so does a class method that calls `attr_accessor`
/// after the class body said `private`.
fn scope_visibility_for(vm: &Executor, globals: &Globals, target: ClassId) -> Visibility {
    let Some(caller) = vm.cfp().prev() else {
        return Visibility::Public;
    };
    let lfp = caller.nearest_ruby_frame().lfp();
    if lfp.self_val().is_class_or_module().map(|m| m.id()) != Some(target) {
        return Visibility::Public;
    }
    // A method body itself (a block in one may still be run by
    // `class_eval`, which brings its own cref).
    let info = &globals.store[lfp.func_id()];
    if info.is_method() || info.meta().is_proc_method() {
        return Visibility::Public;
    }
    if vm.context_class_id() == target {
        vm.context_visibility()
    } else {
        Visibility::Public
    }
}

/// Compute the visibility for a `define_method` call.
///
/// `:initialize` (and the other "special private" names) is forced
/// private to match `def initialize`. Otherwise the calling scope's
/// default visibility applies when it applies to the receiver
/// ([`scope_visibility_for`]): cross-class
/// `klass.send(:define_method, ...)` always installs a public method,
/// regardless of any `private`/`protected` in the calling scope.
fn define_method_visibility(
    vm: &Executor,
    globals: &Globals,
    target: ClassId,
    name: IdentId,
) -> Visibility {
    if is_special_private_method_name(name) {
        return Visibility::Private;
    }
    scope_visibility_for(vm, globals, target)
}

/// Names that CRuby always installs as private even when called from a
/// public scope: the `initialize` family plus `respond_to_missing?`.
/// `def`, `define_method`, and `alias_method` all consult this list.
fn is_special_private_method_name(name: IdentId) -> bool {
    matches!(
        name,
        IdentId::INITIALIZE
            | IdentId::INITIALIZE_COPY
            | IdentId::INITIALIZE_CLONE
            | IdentId::INITIALIZE_DUP
            | IdentId::RESPOND_TO_MISSING_
    )
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
    // On a *refinement*, the list is the refined class's plus whatever
    // the refine block has defined so far: inside `refine Array do … end`
    // `instance_methods` answers as `Array.instance_methods` does
    // (CRuby).
    if let Some(refined) = globals.store[class_id].refined_class()
        && inherited_too
    {
        let mut names = globals.store.get_method_names_inherit(refined, false);
        for name in globals.store.get_method_names(class_id) {
            if !names.contains(&name) {
                names.push(name);
            }
        }
        return Ok(Value::array_from_vec(names));
    }
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_method_names(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, false)
    }))
}

///
/// ### Module#undefined_instance_methods
///
/// - undefined_instance_methods -> [Symbol]
///
/// Returns the list of method names that have been undefined on this
/// module/class itself via `undef_method`. Inherited undefs aren't
/// included.
#[monoruby_builtin]
fn undefined_instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(Value::array_from_vec(
        globals.store.get_undefined_instance_methods(class_id),
    ))
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
    // CRuby distinguishes `public_instance_methods` (public only) from
    // `instance_methods` (public + protected). The shared helpers
    // `get_method_names` / `get_method_names_inherit` cover the
    // public+protected case used by `instance_methods`; here we want
    // the strict public-only flavor.
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_public_method_names(class_id)
    } else {
        globals.store.get_public_method_names_inherit(class_id)
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
    globals.store.class_modify_check(base.id())?;
    let include_module = lfp.self_val().expect_module(globals)?;
    base.include_module(include_module)?;
    globals
        .store
        .propagate_include_to_includers(base, include_module);
    // The module may already carry a basic operation (`module M; def
    // +(o); end; end` and only then `Integer.include M`), which no
    // definition will run to mark.
    globals.store.check_mixin_basic_ops(include_module);
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
    let obj = lfp.arg(0);
    // CRuby's `rb_extend_object` is `rb_include_module(rb_singleton_class(obj), mod)`,
    // and the frozen check is the include's, on that class. So an
    // Integer, a Float, a Symbol or a pooled String fails first, with the
    // TypeError of having no singleton class; `nil`, `true` and `false`,
    // frozen as they are, take the module into NilClass / TrueClass /
    // FalseClass (#1943); and any other frozen object is refused before
    // anything is mutated.
    if !obj.is_special_singleton() {
        if matches!(obj.class(), INTEGER_CLASS | FLOAT_CLASS | SYMBOL_CLASS)
            || globals.store.is_fstring(obj)
        {
            return Err(MonorubyErr::typeerr("can't define singleton"));
        }
        obj.ensure_not_frozen(&globals.store)?;
    }
    // A plain object without a singleton class gets one made right here.
    // Nothing has resolved anything through that class yet, so mixing the
    // module into it invalidates nothing: a program that `extend`s every
    // object it builds (Loofah decorates each node of the documents it
    // scrubs, ~300 a request in shipit) would otherwise fail every
    // compiled unit's version guard once per object.
    // `ClassId`s are never reused: an id at or above this reading belongs
    // to a class created from here on.
    let classes = globals.store.classinfo_len();
    let mut class = globals.store.get_singleton(obj)?;
    // A class object's metaclass may be created here too, but classes are
    // what compiled code resolves class methods through.
    let new_singleton =
        obj.is_class_or_module().is_none() && class.id().u32() as usize >= classes;
    let include_module = lfp.self_val().expect_module(globals)?;
    if new_singleton {
        class.include_module_into_new_singleton(include_module)?;
    } else {
        class.include_module(include_module)?;
    }
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
    // error_highlight is loaded on first use rather than at boot
    // (`builtins/gem_prelude.rb`), and CRuby has its `CoreExt` underneath
    // every other module prepended to the three classes it decorates. So
    // the first prepend to one of them brings the library in first. (The
    // library's own prepends come through here too, with the autoload
    // running, which the Ruby side reads as nothing pending.)
    if let Some(class) = self_.is_class_or_module()
        && matches!(
            class.id(),
            NAME_ERROR_CLASS | TYPE_ERROR_CLASS | ARGUMENTS_ERROR_CLASS
        )
        && globals
            .store
            .get_constant(OBJECT_CLASS, IdentId::get_id("ErrorHighlight"))
            .is_some_and(|state| state.is_autoload())
    {
        let exception = globals.store[EXCEPTION_CLASS].get_module().as_val();
        vm.invoke_method_inner(
            globals,
            IdentId::get_id("__load_error_highlight"),
            exception,
            &[],
            None,
            None,
        )?;
    }
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
    // A Refinement is a Module, but it is not a mixin: CRuby rejects it
    // here rather than letting it into an ancestor chain.
    if arg.class() == REFINEMENT_CLASS {
        return Err(MonorubyErr::typeerr(format!(
            "Cannot {op} refinement"
        )));
    }
    if arg.ty() == Some(ObjTy::MODULE) {
        Ok(())
    } else {
        Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Module)",
            arg.builtin_class_name(&globals.store),
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
    globals.store.class_modify_check(base.id())?;
    let prepend_module = lfp.self_val().as_class();
    base.prepend_module(prepend_module)?;
    // CRuby propagates a `Module#prepend` to every class that has
    // already mixed in `base` — without this, an existing
    // `subclass.include(base)` silently shadows the new prepend
    // because subclass's chain still points at the un-modified
    // include-iclass. Walk the class store and splice an iclass for
    // `prepend_module` in above each iclass that wraps `base`.
    globals
        .store
        .propagate_prepend_to_subclasses(base.id(), prepend_module);
    // See `append_features`: a module that already defines a basic
    // operation replaces it the moment it is spliced in.
    globals.store.check_mixin_basic_ops(prepend_module);
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
    let method_name = existing_method_name(vm, globals, klass.id(), lfp.arg(0))?;
    let set = vm.caller_refinements(globals);
    let (func_id, _, owner) = globals
        .store
        .find_instance_method(klass.id(), method_name, set)
        .map_err(|_| {
            MonorubyErr::method_name_error(&globals.store, method_name, klass.id(), None)
        })?;
    let original_name = globals
        .store
        .original_name_by_class_id(klass.id(), method_name);
    Ok(Value::new_unbound_method_named(
        func_id,
        owner,
        method_name,
        original_name,
    ))
}

#[monoruby_builtin]
fn public_instance_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let klass = lfp.self_val().as_class();
    let method_name = existing_method_name(vm, globals, klass.id(), lfp.arg(0))?;
    let (func_id, visibility, owner) = globals
        .store
        .find_instance_method(klass.id(), method_name, RefinementSetId::EMPTY)
        .map_err(|_| {
            MonorubyErr::method_name_error(&globals.store, method_name, klass.id(), None)
        })?;
    // CRuby raises a NameError (kind, not NoMethodError) when the
    // looked-up method exists but isn't public.
    if visibility != Visibility::Public {
        return Err(MonorubyErr::method_name_error(
            &globals.store,
            method_name,
            klass.id(),
            Some(visibility),
        ));
    }
    let original_name = globals
        .store
        .original_name_by_class_id(klass.id(), method_name);
    Ok(Value::new_unbound_method_named(
        func_id,
        owner,
        method_name,
        original_name,
    ))
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
    let self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    let names = lfp.arg(0).as_array();
    // Coerce all names *before* the frozen check so that a non-name
    // argument raises TypeError (not FrozenError) on a frozen receiver,
    // matching CRuby's error precedence and the
    // "raises a TypeError when passed a not name" spec.
    let resolved: Vec<IdentId> = names
        .iter()
        .map(|n| match checked_name(vm, globals, *n)? {
            CheckedName::Id(id) => Ok(id),
            CheckedName::Unknown(s) => Err(undef_method_unknown(globals, class_id, s)),
        })
        .collect::<Result<_>>()?;
    if !resolved.is_empty() {
        globals.store.class_modify_check(class_id)?;
    }
    for name in resolved {
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
    let self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    let names = lfp.arg(0).as_array();
    let resolved: Vec<IdentId> = names
        .iter()
        .map(|n| match checked_name(vm, globals, *n)? {
            CheckedName::Id(id) => Ok(id),
            CheckedName::Unknown(s) => {
                let class_name = globals.store.nameerr_class_name(class_id);
                Err(name_error_unknown(
                    globals,
                    "method '",
                    s,
                    &format!("' not defined in {class_name}"),
                    self_val,
                ))
            }
        })
        .collect::<Result<_>>()?;
    if !resolved.is_empty() {
        globals.store.class_modify_check(class_id)?;
    }
    for name in resolved {
        globals.remove_method(class_id, name)?;
        vm.invoke_method_removed(globals, class_id, name)?;
    }
    Ok(lfp.self_val())
}

fn check_method_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Option<Visibility>> {
    let class_id = lfp.self_val().as_class_id();
    // A name no Symbol exists for names no method.
    let CheckedName::Id(func_name) = checked_name(vm, globals, lfp.arg(0))? else {
        return Ok(None);
    };
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
    let (_, names) = extract_names(globals, singleton.id(), arg)?;
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
    let (_, names) = extract_names(globals, singleton.id(), arg)?;
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
fn tos(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(module) = self_val.is_class_or_module() {
        if let Some(attached) = module.is_singleton() {
            return singleton_tos(vm, globals, module.id(), attached);
        }
        // An anonymous class/module has no name to give, so
        // `get_class_name` answers the `#<Class:0x…>` rendering
        // instead — which CRuby builds with `rb_sprintf`, not with the
        // name builder, and tags accordingly (#1494).
        if let Some((bytes, enc)) = globals.store.get_class_path_raw(module.id())
            && globals.store.get_class_name(module.id())
                == globals.store.get_class_path(module.id())
        {
            // A name that is not UTF-8 text, as `Module#name` gives it.
            return Ok(Value::string_from_inner(RStringInner::from_encoding(
                &bytes, enc,
            )));
        }
        let class_name = globals.store.get_class_name(module.id());
        Ok(Value::string_name_or_repr(class_name))
    } else {
        let class_name = globals.store.get_class_name(self_val.class());
        Ok(Value::string(format!(
            "#<{}:0x{:016x}>",
            class_name,
            self_val.id()
        )))
    }
}

/// A singleton class's `#to_s`, built as `rb_mod_to_s` builds it: a
/// US-ASCII `#<Class:` buffer, the attached object, and `>`. A class or
/// module attached is appended through `rb_inspect` — its own
/// `#inspect`, escaped where the result encoding cannot show it — so
/// the rendering stays US-ASCII unless an unescaped non-ASCII answer
/// brings its encoding in (#1518). Any other object is appended as
/// `rb_any_to_s` renders it, which is not escaped.
///
/// `Store::get_class_name` keeps the unescaped `#<Class:Foo>` form for
/// the places that name a class without a VM to call `#inspect` with.
fn singleton_tos(
    vm: &mut Executor,
    globals: &mut Globals,
    singleton: ClassId,
    attached: Value,
) -> Result<Value> {
    if attached.is_class_or_module().is_none() {
        return Ok(Value::string_usascii(globals.store.get_class_name(singleton)));
    }
    let inspected = crate::builtins::encoding::rb_inspect(vm, globals, attached)?;
    let inner = inspected.as_rstring_inner();
    let mut bytes = b"#<Class:".to_vec();
    bytes.extend_from_slice(inner.as_bytes());
    bytes.push(b'>');
    // `rb_str_append` onto the ASCII-only buffer: an ASCII answer
    // leaves it US-ASCII, anything else brings its own encoding.
    let enc = if inner.encoding().is_ascii_compatible() && inner.as_bytes().is_ascii() {
        crate::value::Encoding::UsAscii
    } else {
        inner.encoding()
    };
    Ok(Value::string_from_inner(RStringInner::from_encoding(&bytes, enc)))
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
    Ok(class_name_value(globals, class_id).unwrap_or_default())
}

///
/// What the builtin `Module#name` answers for *class_id*: the frozen,
/// cached class path, or `None` for an anonymous class or module.
///
pub(crate) fn class_name_value(globals: &mut Globals, class_id: ClassId) -> Option<Value> {
    if globals.store[class_id].get_name().is_none() || globals.store.is_name_erased(class_id) {
        return None;
    }
    if let Some(cached) = globals.store[class_id].cached_name_value() {
        return Some(cached);
    }
    let mut val = match globals.store.get_class_path_raw(class_id) {
        // A name that is not UTF-8 text is its own bytes (`Struct::\xC9`
        // in ISO-8859-1).
        Some((bytes, enc)) => Value::string_from_inner(RStringInner::from_encoding(&bytes, enc)),
        None => Value::string_usascii(globals.store.get_class_path(class_id)),
    };
    // The class path is an fstring (`rb_fstring` in `build_const_path`).
    val.set_fstring();
    globals.store[class_id].set_cached_name_value(val);
    Some(val)
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
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let class_id = self_val.as_class_id();
    let name = coerce_to_class_var_name(vm, globals, lfp.self_val(), lfp.arg(0))?;
    let val = lfp.arg(1);
    // A class variable is shared with ancestors: if a superclass already
    // defines it, update *that* definition instead of shadowing a fresh copy
    // on the receiver (CRuby's `rb_cvar_set` walks to the defining class).
    // This mirrors the `@@x = v` assignment path
    // (`Executor::set_class_variable`); shadowing would otherwise make a
    // later `class_variable_get` raise "class variable … is overtaken".
    let module = globals.store[class_id].get_module();
    let target = match globals.search_class_variables_superclass(module, name) {
        Some((m, _)) => m.id(),
        None => class_id,
    };
    globals.set_class_variable(target, name, val);
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
    let name = match class_var_name_check(vm, globals, lfp.self_val(), lfp.arg(0))? {
        CheckedName::Id(name) => name,
        CheckedName::Unknown(name) => {
            let after = format!(" in {}", globals.store.get_class_name(class_id));
            return Err(crate::builtins::kernel::name_error_unknown(
                globals,
                "uninitialized class variable ",
                name,
                &after,
                lfp.self_val(),
            ));
        }
    };
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
    // A name no Symbol exists for names no variable.
    let CheckedName::Id(name) = class_var_name_check(vm, globals, lfp.self_val(), lfp.arg(0))?
    else {
        return Ok(Value::bool(false));
    };
    let module = globals.store[class_id].get_module();
    Ok(Value::bool(globals.get_class_variable(module, name).is_ok()))
}

/// The name argument of `Module#class_variable_get` / `_set` /
/// `_defined?` (`check_setter_id` with `rb_is_class_name`): `@@` and an
/// identifier, anything else a NameError that carries the given name
/// and the receiver. `#to_str` is asked of a non-Symbol/non-String
/// argument.
fn coerce_to_class_var_name(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Value,
    name_arg: Value,
) -> Result<IdentId> {
    crate::builtins::kernel::setter_name_id(
        vm,
        globals,
        receiver,
        name_arg,
        crate::builtins::kernel::is_valid_cvar_name,
        "'",
        "' is not allowed as a class variable name",
    )
}

/// [`coerce_to_class_var_name`] for the readers, which do not intern a
/// name there is no Symbol for (`rb_check_id`).
fn class_var_name_check(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Value,
    name_arg: Value,
) -> Result<crate::builtins::kernel::CheckedName> {
    crate::builtins::kernel::setter_name_check(
        vm,
        globals,
        receiver,
        name_arg,
        crate::builtins::kernel::is_valid_cvar_name,
        "'",
        "' is not allowed as a class variable name",
    )
}

///
/// ### Module#remove_class_variable
///
/// - remove_class_variable(name) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/remove_class_variable.html]
#[monoruby_builtin]
fn remove_class_variable(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    // `rb_mod_remove_cvar` words the bad name its own way.
    let name = crate::builtins::kernel::setter_name_check(
        vm,
        globals,
        lfp.self_val(),
        lfp.arg(0),
        crate::builtins::kernel::is_valid_cvar_name,
        "wrong class variable name ",
        "",
    )?;
    let name = match name {
        crate::builtins::kernel::CheckedName::Id(name) => name,
        crate::builtins::kernel::CheckedName::Unknown(name) => {
            let after = format!(" not defined for {}", class_id.get_name(&globals.store));
            return Err(crate::builtins::kernel::name_error_unknown(
                globals,
                "class variable ",
                name,
                &after,
                lfp.self_val(),
            ));
        }
    };
    match globals.store[class_id].remove_cvar(name) {
        Some(val) => Ok(val),
        None => Err(MonorubyErr::nameerr_showing(
            format_args!("class variable "),
            name,
            format_args!(" not defined for {}", class_id.get_name(&globals.store)),
            Some(lfp.self_val()),
        )),
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
    // CRuby: a class variable defined on a singleton class is shared
    // with the attached class — `class C; class << self; @@x = …; end;
    // C.class_variables.include?(:@@x)`. Start the walk on the
    // attached non-singleton class so `class_variables` sees those.
    let start_id = singleton_attached_class_for_id(globals, class_id);
    let mut module = Some(globals.store[start_id].get_module());
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

/// Mirror of the same-named helper in `class/constants.rs`, exposed
/// to the builtin layer. Walks up the singleton chain until the
/// receiver is non-singleton (or the attached object isn't a
/// class/module).
fn singleton_attached_class_for_id(globals: &Globals, mut class_id: ClassId) -> ClassId {
    loop {
        let module = globals.store[class_id].get_module();
        match module.is_singleton() {
            Some(attached) => match attached.is_class_or_module() {
                Some(m) => class_id = m.id(),
                None => return class_id,
            },
            None => return class_id,
        }
    }
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    set_constants_visibility(vm, globals, lfp, /*private=*/ false)
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    set_constants_visibility(vm, globals, lfp, /*private=*/ true)
}

/// A name argument of `private_constant` / `public_constant` /
/// `deprecate_constant`: a constant *module* itself has.
///
/// The name is looked up, not interned (`rb_check_id`), and a String —
/// or what `to_str` answers — that names no Symbol yet is the
/// NameError's `#name` as it is; one that does, and a Symbol, is
/// reported as the Symbol.
fn own_constant_name(
    vm: &mut Executor,
    globals: &mut Globals,
    module: Value,
    arg: Value,
) -> Result<IdentId> {
    let class_id = module.as_class_id();
    let name = if let Some(sym) = arg.try_symbol() {
        sym
    } else {
        let Some(s) = arg.check_string_type(vm, globals)? else {
            return Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, arg));
        };
        match s.expect_symbol_or_existing_string(&globals.store)? {
            Some(id) => id,
            None => {
                let inner = s.as_rstring_inner();
                let (bytes, enc) = (inner.as_bytes().to_vec(), inner.encoding());
                let before = format!("constant {}::", globals.store.get_class_name(class_id));
                return Err(crate::builtins::kernel::name_error_reflection_bytes(
                    globals,
                    &before,
                    &bytes,
                    enc,
                    " not defined",
                    s,
                    module,
                ));
            }
        }
    };
    if !globals.store[class_id].has_own_constant(name) {
        return Err(MonorubyErr::constant_not_defined(
            &globals.store,
            name,
            class_id,
        ));
    }
    Ok(name)
}

/// Shared body of `Module#private_constant` and `Module#public_constant`.
fn set_constants_visibility(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    make_private: bool,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    globals.store.class_modify_check(class_id)?;
    let args = lfp.arg(0).as_array();
    // Validate every name first so a partial failure does not leave the
    // class in an inconsistent state.
    let mut names = Vec::with_capacity(args.len());
    for arg in args.iter() {
        names.push(own_constant_name(vm, globals, lfp.self_val(), *arg)?);
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

///
/// ### Module#deprecate_constant
///
/// - deprecate_constant(*name) -> self
///
/// Marks each named constant as deprecated. Subsequent reads of those
/// constants emit a `warning: constant ...::Name is deprecated` warning
/// (subject to `Warning[:deprecated]`). Each name must be a constant
/// defined directly on the receiver; otherwise `NameError` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/deprecate_constant.html]
#[monoruby_builtin]
fn deprecate_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    globals.store.class_modify_check(class_id)?;
    let args = lfp.arg(0).as_array();
    // Validate every name first so a partial failure does not leave the
    // class in an inconsistent state — matching CRuby and the behaviour
    // of `private_constant` / `public_constant`.
    let mut names = Vec::with_capacity(args.len());
    for arg in args.iter() {
        names.push(own_constant_name(vm, globals, lfp.self_val(), *arg)?);
    }
    for name in names {
        globals.store[class_id].set_constant_deprecated(name);
    }
    Ok(lfp.self_val())
}

///
/// ### Module#ruby2_keywords
///
/// - ruby2_keywords(*method_names) -> nil
///
/// monoruby does not implement the ruby2_keywords flag mechanism, so
/// the marking itself is a no-op. We still validate the arguments —
/// each must coerce to a Symbol/String (TypeError otherwise) and
/// reference a method defined directly on the receiver (NameError
/// otherwise) — so that callers relying on the validation contract
/// (e.g. specs in `core/module/ruby2_keywords_spec.rb`) behave
/// correctly.
#[monoruby_builtin]
fn ruby2_keywords(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let names = lfp.arg(0).as_array().iter().cloned().collect::<Vec<_>>();
    ruby2_keywords_mark(vm, globals, class_id, &names)
}

/// Shared body of `Module#ruby2_keywords` and `main.ruby2_keywords`:
/// validate each name, mark compatible methods (and the underlying
/// block of a define_method proc-method), warn about incompatible ones.
pub(super) fn ruby2_keywords_mark(
    vm: &mut Executor,
    globals: &mut Globals,
    class_id: ClassId,
    names: &[Value],
) -> Result<Value> {
    // `rb_mod_ruby2_keywords`: `rb_check_frozen(module)`.
    globals.store[class_id]
        .get_module()
        .as_val()
        .ensure_not_frozen(&globals.store)?;
    for arg in names.iter() {
        // expect_symbol_or_string accepts Symbol and String (and rejects
        // anything else with TypeError "<obj> is not a symbol nor a
        // string"). We don't honour `to_str` here — CRuby's
        // `ruby2_keywords` is also strict about Symbol/String only.
        let name = arg.expect_symbol_or_to_str(vm, globals)?;
        if !globals.store[class_id].has_own_method(name) {
            return Err(MonorubyErr::method_name_error(
                &globals.store,
                name,
                class_id,
                None,
            ));
        }
        // Mark the method when its signature is compatible (a `*rest`,
        // no keyword params / `**kw`, no post-rest requireds);
        // otherwise emit CRuby's "Skipping set of ruby2_keywords flag
        // for <name> (...)" compatibility warning.
        if let Ok((func_id, _, _)) = globals.store.find_method_for_class(class_id, name) {
            match ruby2_keywords_skip_reason(globals, func_id) {
                Some(reason) => {
                    let msg = format!(
                        "Skipping set of ruby2_keywords flag for {name} ({reason})",
                    );
                    crate::value::emit_verbose_warning(vm, globals, &msg)?;
                }
                None => {
                    globals.store[func_id].set_ruby2_keywords();
                    // A define_method proc-method dispatches with the
                    // *block's* FuncId in the frame (the wrapper jumps
                    // straight to the block code), so the argument
                    // marshaler consults that FuncInfo — mark it too.
                    if let crate::globals::FuncKind::Proc(proc) = &globals.store[func_id].kind {
                        let block_fid = proc.func_id();
                        globals.store[block_fid].set_ruby2_keywords();
                    }
                }
            }
        }
    }
    Ok(Value::nil())
}

/// Returns a human-readable reason string when the method signature
/// can't be marked ruby2_keywords, matching CRuby's wording.
fn ruby2_keywords_skip_reason(globals: &Globals, func_id: FuncId) -> Option<&'static str> {
    let info = &globals[func_id];
    if !info.no_keyword() {
        // Has explicit keyword params or `**kw`.
        return Some("method accepts keywords or method accepts keyword splat");
    }
    if !info.is_rest() {
        // No `*args` to absorb the trailing kwsplat.
        return Some("method does not accept argument splat");
    }
    if info.post_num() > 0 {
        // `*a, b` — post-rest required positional makes the trailing
        // kwhash undecidable.
        return Some("method accepts argument splat with post-rest required arguments");
    }
    None
}

/// True when `s` parses as `(::)?CONST(::CONST)*` where each `CONST`
/// matches `[A-Z][A-Za-z0-9_]*`. Used by `set_temporary_name` to reject
/// names that would shadow real constant lookups.
fn is_constant_path(s: &str) -> bool {
    let body = s.strip_prefix("::").unwrap_or(s);
    if body.is_empty() {
        return false;
    }
    body.split("::").all(|seg| {
        let mut chars = seg.chars();
        match chars.next() {
            Some(c) if c.is_ascii_uppercase() => {}
            _ => return false,
        }
        chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
    })
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
    // CRuby refuses to rename a module that already has a permanent
    // (constant-bound) name; the check runs before nil/string handling
    // so even `set_temporary_name(nil)` on `Object` raises.
    if globals.store[class_id].is_name_permanent() {
        return Err(MonorubyErr::runtimeerr("can't change permanent name"));
    }
    if name_val.is_nil() {
        globals.store[class_id].clear_name();
    } else {
        let name = name_val.coerce_to_string(vm, globals)?;
        if name.is_empty() {
            return Err(MonorubyErr::argumenterr("empty class/module name"));
        }
        // Reject anything that *looks* like a syntactically valid
        // constant or constant path so introspection tools that resolve
        // `A::B` aren't confused. We accept anything that wouldn't
        // round-trip as a constant ("a::B", "A::b", "A::B::", "Template['x']").
        if is_constant_path(&name) {
            return Err(MonorubyErr::argumenterr(
                "the temporary name must not be a constant path to avoid confusion",
            ));
        }
        globals.store[class_id].set_temporary_name(name);
    }
    // The receiver's leaf changed; any descendant that renders its
    // own `Module#name` through us has a stale frozen-string cache.
    // Invalidate so the next call rebuilds with the new chain.
    globals.store.invalidate_descendant_name_caches(class_id);
    Ok(lfp.self_val())
}

///
/// ### Module#using
/// - using(module) -> self
///
/// Refinements are not implemented, so a module can never carry any: this
/// validates the argument and the calling scope the way CRuby does, then
/// activates the (necessarily empty) set of refinements. The same shape as
/// `main.using`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/using.html]
#[monoruby_builtin]
fn using(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // CRuby permits `using` only where a cref is being built — a class or
    // module body, a block run in one, or the top level — never inside a
    // method body, where the activation would outlive the call.
    if let Some(caller) = vm.cfp().prev()
        && globals.store[caller.outermost_lfp().func_id()].is_method()
    {
        return Err(MonorubyErr::runtimeerr(
            "Module#using is not permitted in methods",
        ));
    }
    let arg = expect_refinement_module(globals, lfp.arg(0))?;
    activate(vm, globals, arg);
    Ok(lfp.self_val())
}

/// Collect *module*'s refinements and activate them in the calling scope.
pub(super) fn activate(vm: &mut Executor, globals: &mut Globals, module: ClassId) {
    let added = globals.store.refinements_of_module(module);
    vm.activate_refinements(globals, &added);
}

///
/// ### Module#refine
/// - refine(klass) { ... } -> Refinement
///
/// Creates (or reuses) the refinement module this module holds for
/// *klass*, evaluates the block with it as the definee, and returns it.
/// The refinement is inert until some scope activates it with `using`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/refine.html]
#[monoruby_builtin]
fn refine(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let owner = lfp.self_val().as_class_id();
    let arg = lfp.arg(0);
    let refined = match arg.is_class_or_module() {
        Some(m) => m.id(),
        None => {
            // CRuby names the class here (`rb_obj_class`), keyword or not.
            return Err(MonorubyErr::typeerr(format!(
                "wrong argument type {} (expected Class or Module)",
                arg.get_real_class_name(&globals.store)
            )));
        }
    };
    let Some(bh) = lfp.block() else {
        return Err(MonorubyErr::argumenterr("no block given"));
    };
    let data = vm.get_block_data(globals, bh)?;
    // `refine` on the same class twice extends the module made the first
    // time, so both blocks' methods land in one refinement (CRuby).
    let refinement = match globals.store[owner]
        .own_refinements()
        .iter()
        .copied()
        .find(|r| globals.store[*r].refined_class() == Some(refined))
    {
        Some(existing) => globals.store[existing].get_module(),
        None => {
            let module = globals.store.define_unnamed_module();
            let mut val = module.as_val();
            val.change_class_fresh(REFINEMENT_CLASS);
            globals.store[module.id()].set_refinement_of(refined, owner);
            globals.store[owner].add_refinement(module.id());
            module
        }
    };
    // A refinement exists now, so the resolution gate must come off even
    // if the block defines nothing.
    globals.store.refinements_mut().activate();
    let module_val = refinement.as_val();
    // CRuby activates a refinement inside its own `refine` block, so a
    // `def` there sees it (a refined method may call its siblings) and
    // so may the block's own straight-line code. Swap the scope's cell
    // for the duration; the enclosing body is unaffected once it is put
    // back.
    let saved = vm.begin_refine_block(globals, refined, refinement.id());
    vm.push_runtime_class_context(refinement.id());
    let res = vm.invoke_block_with_self(globals, &data, module_val, &[]);
    vm.pop_class_context();
    vm.end_refine_block(globals, saved);
    res?;
    // Every refinement defined in one module is visible from every other
    // one's method bodies (CRuby), but the sibling defined *after* this
    // block could not have been in the snapshot its `def`s took. Re-stamp
    // them all now that the owner's set has grown; `refine` is a
    // load-time call, and a refinement's method table is small.
    vm.restamp_refinement_methods(globals, owner);
    Ok(module_val)
}

///
/// ### Refinement#import_methods
/// - import_methods(*modules) -> self
///
/// Copies the modules' own methods into the refinement, so that calls
/// between them resolve within the refinement once it is activated.
/// Only methods written in Ruby can be imported, and only the modules'
/// *own* methods — anything they inherit is skipped (with a warning).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Refinement/i/import_methods.html]
#[monoruby_builtin]
fn import_methods(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let refinement = lfp.self_val().as_class_id();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    // Everything is validated before anything is imported: a bad
    // argument late in the list must leave the refinement untouched.
    let mut modules = vec![];
    for v in &args {
        match v.is_class_or_module() {
            Some(m) if m.is_module() && v.class() != REFINEMENT_CLASS => modules.push(m.id()),
            _ => {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong argument type {} (expected Module)",
                    v.builtin_class_name(&globals.store)
                )));
            }
        }
    }
    // Importing happens module by module: a module whose methods are
    // not all written in Ruby raises when the walk reaches it, and the
    // modules *before* it stay imported (CRuby). The argument type check
    // above is the all-or-nothing part.
    for module in &modules {
        let mut imports = vec![];
        // `ancestors` of a plain module is `[self]`; anything more means
        // it includes or prepends something, whose methods are *not*
        // imported. CRuby warns rather than failing.
        if globals.ancestors(*module).len() > 1 {
            let msg = format!(
                "warning: {} has ancestors, but Refinement#import_methods doesn't import their methods\n",
                globals.store.get_class_name(*module)
            );
            emit_stderr_warning(vm, globals, &msg);
        }
        for (name, entry) in globals.store[*module].own_method_table() {
            let Some(func_id) = entry.func_id() else {
                continue;
            };
            // Only genuinely `def`-defined bodies can be imported. CRuby
            // requires the entry itself to be VM_METHOD_TYPE_ISEQ, so an
            // `alias` entry or a visibility re-declaration of an inherited
            // method (`private :inherited`) raises even though the body it
            // leads to is written in Ruby — while `define_method(name,
            // method_obj)` re-registers the original ISEQ and imports.
            if globals.store[func_id].is_iseq().is_none()
                || entry.is_alias()
                || entry.is_visibility_shadow()
            {
                return Err(MonorubyErr::argumenterr(format!(
                    "Can't import method which is not defined with Ruby code: {}#{name}",
                    globals.store.get_class_name(*module)
                )));
            }
            imports.push((*name, func_id, entry.visibility()));
        }
        for (name, func_id, visi) in imports {
            globals.add_method(refinement, name, func_id, visi);
        }
    }
    Globals::class_version_inc();
    Ok(lfp.self_val())
}

/// Write *msg* to Ruby's `$stderr` so mspec's `complain` matcher sees it.
fn emit_stderr_warning(vm: &mut Executor, globals: &mut Globals, msg: &str) {
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or(Value::nil());
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[Value::string(msg.to_string())],
        None,
        None,
    );
}

///
/// ### Refinement#target
/// - target -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Refinement/i/target.html]
#[monoruby_builtin]
fn refinement_target(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(match globals.store[class_id].refined_class() {
        Some(refined) => globals.store[refined].get_module().as_val(),
        None => Value::nil(),
    })
}

///
/// `Refinement#to_s` / `#inspect` — `#<refinement:String@Shout>`.
/// Unlike `Module#to_s` this ignores any constant the refinement got
/// bound to; the refined class and the owner are what identify it.
#[monoruby_builtin]
fn refinement_tos(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let info = &globals.store[class_id];
    // `rb_mod_to_s` opens the refinement form's buffer with
    // `rb_usascii_str_new`; a Refinement with nothing to say falls
    // through to the anonymous-class rendering, which is
    // `rb_sprintf`'s (#1494).
    Ok(match (info.refined_class(), info.refinement_owner()) {
        (Some(refined), Some(owner)) => Value::string_usascii(format!(
            "#<refinement:{}@{}>",
            globals.store.get_class_name(refined),
            globals.store.get_class_name(owner)
        )),
        // `Refinement.new` — a Refinement that refines nothing.
        _ => Value::string_sprintf(format!("#<Refinement:0x{:016x}>", lfp.self_val().id())),
    })
}

/// `Refinement#include` / `#prepend` — always a TypeError. A refinement
/// is not a mixin target. CRuby names the method in the message, so the
/// two share an implementation but not a message.
#[monoruby_builtin]
fn refinement_include(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = vm
        .cfp()
        .lfp()
        .func_id();
    let name = globals.store[name].name().unwrap_or(IdentId::get_id("include"));
    Err(MonorubyErr::typeerr(format!(
        "Refinement#{name} has been removed"
    )))
}

///
/// ### Module#refinements
/// - refinements -> [Refinement]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/refinements.html]
#[monoruby_builtin]
fn refinements(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let v: Vec<Value> = globals.store[class_id]
        .own_refinements()
        .to_vec()
        .into_iter()
        .map(|r| globals.store[r].get_module().as_val())
        .collect();
    Ok(Value::array_from_vec(v))
}

///
/// ### Module.used_refinements
/// - used_refinements -> [Refinement]
///
/// The refinements activated in the *caller's* scope.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/s/used_refinements.html]
#[monoruby_builtin]
fn used_refinements(
    vm: &mut Executor,
    globals: &mut Globals,
    _: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let set = vm.caller_refinements(globals);
    let v: Vec<Value> = globals
        .store
        .refinements()
        .entries(set)
        .to_vec()
        .into_iter()
        .map(|(_, module)| globals.store[module].get_module().as_val())
        .collect();
    Ok(Value::array_from_vec(v))
}

///
/// ### Module.used_modules
/// - used_modules -> [Module]
///
/// The modules `using`-ed in the caller's scope — the owners of the
/// refinements [`used_refinements`] reports.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/s/used_modules.html]
#[monoruby_builtin]
fn used_modules(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = vm.caller_refinements(globals);
    let mut owners: Vec<ClassId> = vec![];
    for (_, module) in globals.store.refinements().entries(set).to_vec() {
        if let Some(owner) = globals.store[module].refinement_owner()
            && !owners.contains(&owner)
        {
            owners.push(owner);
        }
    }
    let v = owners
        .into_iter()
        .map(|o| globals.store[o].get_module().as_val())
        .collect();
    Ok(Value::array_from_vec(v))
}

/// Argument check shared by `Module#using` and `main.using`: the argument
/// must be a Module and *not* a Class (a Class can't hold refinements).
pub(super) fn expect_refinement_module(globals: &Globals, arg: Value) -> Result<ClassId> {
    match arg.is_class_or_module() {
        Some(module) if module.is_module() => Ok(module.id()),
        _ => Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Module)",
            arg.builtin_class_name(&globals.store)
        ))),
    }
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
        names.push(existing_method_name(vm, globals, class_id, *v)?);
    }
    for &name in &names {
        let func_id = globals
            .find_method_for_class(class_id, name)
            .map_err(|_| MonorubyErr::method_name_error(&globals.store, name, class_id, None))?
            .0;
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

pub(super) fn change_visi(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Array,
    visi: Visibility,
) -> Result<Value> {
    if arg.len() == 0 {
        vm.set_context_visibility(visi);
        // CRuby's `public`/`protected`/`private` (no arguments) also
        // turns off the `module_function` toggle, so subsequent `def`s
        // become regular instance methods again. Without this clear,
        // `m { module_function; def t1; end; public; def t3; end }`
        // would still install `t3` as a module function.
        vm.clear_module_function();
        return Ok(Value::nil());
    }
    // At top level `self` is the `main` object (an Object instance, not
    // a Module); CRuby's default definee there is `Object`, so e.g.
    // `private :m` makes `Object#m` private. Fall back to OBJECT_CLASS
    // for any non-module receiver.
    let class_id = if self_val.is_class().is_some() || self_val.is_module().is_some() {
        self_val.as_class_id()
    } else {
        OBJECT_CLASS
    };
    // `set_method_visibility` is `rb_check_frozen(self)` before it reads
    // a name — the class itself, also for a singleton class (unlike a
    // method definition, `Store::class_modify_check`).
    globals.store[class_id]
        .get_module()
        .as_val()
        .ensure_not_frozen(&globals.store)?;
    let (res, names) = extract_names(globals, class_id, arg)?;
    // CRuby fires `method_added` only when `private` / `protected` /
    // `public` adds a *new* entry to the receiver's method table —
    // i.e. when the method was inherited rather than defined locally.
    // Capture which names were missing locally before the call so we
    // can fire after `change_method_visibility_for_class` runs.
    let was_missing_locally: Vec<bool> = names
        .iter()
        .map(|n| !globals.store[class_id].has_own_method(*n))
        .collect();
    globals.change_method_visibility_for_class(class_id, &names, visi)?;
    for (name, was_missing) in names.iter().zip(was_missing_locally) {
        if was_missing && globals.store[class_id].has_own_method(*name) {
            vm.invoke_method_added(globals, class_id, *name, None)?;
        }
    }
    Ok(res)
}

/// The method names a visibility setter was given (`set_method_visibility`):
/// one Array of them, or the arguments themselves. A name no Symbol
/// exists for names no method of *class_id* (`rb_print_undef_str`).
fn extract_names(
    globals: &mut Globals,
    class_id: ClassId,
    arg: Array,
) -> Result<(Value, Vec<IdentId>)> {
    let (res, list) = match arg.len() {
        1 => match arg[0].try_array_ty() {
            Some(ary) => (arg[0], ary.to_vec()),
            None => (arg[0], vec![arg[0]]),
        },
        _ => (arg.as_val(), arg.to_vec()),
    };
    let mut names = Vec::with_capacity(list.len());
    for v in list {
        match v.expect_symbol_or_existing_string(&globals.store)? {
            Some(id) => names.push(id),
            None => return Err(undefined_method_unknown(globals, class_id, v)),
        }
    }
    Ok((res, names))
}

/// The name of a method of *class_id* that has to exist already
/// (`rb_check_id`, then `rb_print_undef_str`): a String no Symbol
/// exists for is the NameError with that String as `#name`, and makes
/// no Symbol.
fn existing_method_name(
    vm: &mut Executor,
    globals: &mut Globals,
    class_id: ClassId,
    arg: Value,
) -> Result<IdentId> {
    match checked_name(vm, globals, arg)? {
        CheckedName::Id(id) => Ok(id),
        CheckedName::Unknown(s) => Err(undefined_method_unknown(globals, class_id, s)),
    }
}

/// `rb_print_undef_str`: [`MonorubyErr::method_name_error`] about a
/// name that is a String.
fn undefined_method_unknown(globals: &mut Globals, class_id: ClassId, name: Value) -> MonorubyErr {
    let module = globals.store[class_id].get_module();
    let kind = if module.is_singleton().is_none() && module.as_val().ty() == Some(ObjTy::MODULE) {
        "module"
    } else {
        "class"
    };
    let class_name = globals.store.nameerr_class_name(class_id);
    name_error_unknown(
        globals,
        "undefined method '",
        name,
        &format!("' for {kind} '{class_name}'"),
        module.as_val(),
    )
}

/// `rb_method_name_error` with a String:
/// [`MonorubyErr::undef_method_name_error`] about a name no Symbol
/// exists for.
fn undef_method_unknown(globals: &mut Globals, class_id: ClassId, name: Value) -> MonorubyErr {
    let module = globals.store[class_id].get_module();
    let (kind, shown) = match module.is_singleton() {
        Some(attached) => match attached.is_class_or_module() {
            Some(m) => ("class", m),
            None => ("class", module),
        },
        None if module.as_val().ty() == Some(ObjTy::MODULE) => ("module", module),
        None => ("class", module),
    };
    let class_name = globals.store.nameerr_class_name(shown.id());
    name_error_unknown(
        globals,
        "undefined method '",
        name,
        &format!("' for {kind} '{class_name}'"),
        shown.as_val(),
    )
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn bare_visibility_no_cref_no_panic() {
        // Bare `private`/`public` at the top level — where the
        // lexical-class frame carries no Cref — previously aborted the
        // process at `set_context_visibility().unwrap()`. It must now
        // toggle the top-level default visibility (CRuby: top level
        // starts `private`).
        run_test(
            r#"
            def pa; end
            public
            def pb; end
            private
            def pc; end
            [Object.public_instance_methods(false).include?(:pb),
             Object.private_instance_methods(false).include?(:pa),
             Object.private_instance_methods(false).include?(:pc)]
            "#,
        );
        // bare modifier at top level returns nil and does not abort
        run_test(r#"def t; end; (private).inspect"#);
        run_test(r#"def t; end; (public).inspect"#);
    }

    #[test]
    fn def_forces_private_lifecycle_hooks() {
        // `initialize`, `initialize_copy/dup/clone`, and
        // `respond_to_missing?` are always private when defined via `def`.
        run_test(
            r#"
            class DefLifecycle
              def initialize; end
              def initialize_copy(o); end
              def respond_to_missing?(n, i); end
            end
            DefLifecycle.private_instance_methods(false).sort
            "#,
        );
    }

    #[test]
    fn def_on_frozen_raises_frozen_error() {
        // Defining a method on a frozen class / module / object raises
        // FrozenError; nil/true/false still accept singleton methods.
        run_test(
            r#"
            klass = Class.new; klass.freeze
            r1 = begin; klass.class_eval { def a; end }; :no; rescue FrozenError; :frozen; end
            o = Object.new; o.freeze
            r2 = begin; def o.b; end; :no; rescue FrozenError; :frozen; end
            def nil.gg; 1; end   # allowed despite nil being frozen
            [r1, r2, nil.gg]
            "#,
        );
    }

    #[test]
    fn nested_def_targets_owning_class() {
        // A nested `def` inside a method body defines on the enclosing
        // method's class and is public, whether that class comes from the
        // `class` keyword or a dynamic `Class.new`/`class_exec` block.
        run_test(
            r#"
            class NestOuter
              def build; def built; 42; end; end
            end
            NestOuter.new.build
            a = NestOuter.new.built                       # public, callable
            cls = Class.new { def build2; def built2; 7; end; end }
            cls.new.build2
            b = cls.new.built2
            c = begin; Object.new.built2; :no; rescue NoMethodError; :nme; end
            [a, b, c]
            "#,
        );
    }

    #[test]
    fn wrong_number_of_args_with_rest_uses_plus() {
        // A method with an explicit `*rest` reports the required count with
        // a `+` suffix; a private method called with an explicit receiver
        // reports "private method … called".
        run_test(
            r#"
            def rq(a, b, *c); end
            m1 = begin; rq(1); rescue ArgumentError => e; e.message; end
            class PrivRcv; private; def secret; end; end
            m2 = begin; PrivRcv.new.secret; rescue NoMethodError => e; e.message.include?("private"); end
            [m1, m2]
            "#,
        );
    }

    #[test]
    fn toplevel_private_public() {
        // Top-level `private`/`public`/`protected` target Object, and
        // Object's own private instance methods surface in
        // `Object.private_methods(true)` (CRuby semantics).
        run_test(
            r#"
            def mp1; 1; end
            def mp2; 2; end
            def mp3; 3; end
            private :mp1
            private :mp2, :mp3
            r = []
            r << Object.private_instance_methods(false).include?(:mp1)
            r << Object.private_methods(true).include?(:mp1)
            r << Object.private_methods(true).include?(:mp2)
            r << Object.private_methods(true).include?(:mp3)
            public :mp1
            r << Object.private_methods(true).include?(:mp1)
            # array argument form + return value
            r << (private([:mp2, :mp3]))
            r << (private :mp1)
            # Kernel's instance-method list must NOT bleed in Object's
            r << Kernel.public_instance_methods(true).include?(:mp1)
            r
            "#,
        );
        run_test_error(r#"private :no_such_toplevel_method_xyz"#);
    }

    #[test]
    fn toplevel_protected_and_direct() {
        // obj.protected_methods(true) includes Object's own protected
        // instance methods (the whole ancestry); the `(false)` direct
        // variants; and Kernel's instance view must not bleed Object's.
        run_test(
            r#"
            class Object
              def tp1; 1; end
              protected :tp1
              def tp2; 2; end
            end
            Object.send(:private, :tp2)
            r = []
            r << Object.protected_instance_methods(false).include?(:tp1)
            r << Object.protected_methods(true).include?(:tp1)
            r << Object.new.protected_methods(true).include?(:tp1)
            r << Object.private_methods(false).include?(:tp2)
            r << Object.protected_methods(false).include?(:tp1)
            r << Kernel.protected_instance_methods(true).include?(:tp1)
            r
            "#,
        );
    }

    #[test]
    fn test_teq() {
        run_tests(&[
            "Integer === 4",
            "Integer === 4.5",
            "Integer === 'Ruby'",
            "Float === 4.5",
            "Float === 'Ruby'",
        ]);
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
    fn attr_result_survives_gc_in_method_added_hook() {
        // The Symbol-array result of attr_reader/attr_writer/attr_accessor
        // is a bare heap object owned by the native frame while the
        // method_added hook runs Ruby per attribute; without temp-rooting,
        // a collection reached inside the hook sweeps it mid-loop and the
        // next push aborts on the freed cell (issue #985's darwin
        // SIGABRT). `GC.start` requests a collection and the loop's
        // safepoint runs it *inside the hook* — this deterministically
        // aborted (ARRAY != INVALID in Array::push, non-unwinding panic)
        // before the fix, on x86-64 too.
        run_test(
            r#"
            class C
              def self.method_added(name)
                GC.start
                10.times { |i| i }  # pass a safepoint so the GC runs here
              end
              $r = attr_accessor :a, :b, :c
              $w = attr_writer :d, :e
              $rd = attr_reader :f, :g
            end
            [$r, $w, $rd]
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
        run_test_with_prelude(
            r##"
        Recorder::RECORDS
        "##,
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
        // `run_test_once` (not a warmup loop): each `const_set` re-defines
        // `Foo::FOO`/`Foo::BAR`, so re-running would emit "already
        // initialized constant" warnings on every iteration.
        run_test_once(
            r#"
            module Foo; end
            res = []
            res << Foo.const_set(:FOO, 123)
            res << Foo::FOO
            res << Foo.const_set('BAR', 'abc')
            res << Foo::BAR
            res << Foo.const_set('BAR', '123')
            res
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
    fn prepend_chain_order() {
        // Multiple prepends layer in CRuby's expected order: the
        // *most recently prepended* iclass sits closest to the class.
        // ruby/spec `core/module/prepend_spec.rb` "prepends multiple
        // modules in the right order".
        run_test(
            r##"
            m1 = Module.new { def chain; super << :m1; end }
            m2 = Module.new { def chain; super << :m2; end; prepend(m1) }
            c  = Class.new  { def chain; [:c]; end; prepend(m2) }
            c.new.chain
            "##,
        );
    }

    #[test]
    fn prepend_propagates_to_subclass_chain() {
        // A class that already includes a module observes a later
        // prepend on that module — `b.include(a); a.prepend(m)` must
        // splice `m` into `b`'s chain. ruby/spec
        // `core/module/prepend_spec.rb` "adds the module in the
        // subclass chains" + "modifies the ancestor chain".
        run_test(
            r##"
            m = Module.new { def chain; super << :m; end }
            a = Module.new
            b = Class.new   { def chain; [:b]; end }
            b.include(a)
            a.prepend(m)
            [b.ancestors.take(4).map { |x| x.equal?(m) ? :m : x.equal?(a) ? :a : x.equal?(b) ? :b : x.name },
             b.new.chain]
            "##,
        );
    }

    #[test]
    fn prepend_dup_class() {
        // Duping a class that has prepended modules must keep the
        // prepended module reachable via the dup, and the dup must
        // still be a Class (so `c.dup.new` works). ruby/spec
        // `core/module/prepend_spec.rb` "keeps the module in the chain
        // when dupping the class".
        run_test(
            r##"
            m = Module.new { def hello; :from_m; end }
            c = Class.new { prepend m }
            d = c.dup
            [d.is_a?(Class), d.new.kind_of?(m), d.new.hello]
            "##,
        );
    }

    #[test]
    fn prepend_define_method_super() {
        // `define_method` inside a class with a prepended module:
        // `super` from the proc body must resolve through the
        // surrounding class's super, not the enclosing toplevel
        // block. ruby/spec `core/module/prepend_spec.rb` "does not
        // interfere with a define_method super in the original class".
        run_test(
            r##"
            base = Class.new { def foo(ary); ary << 1; end }
            child = Class.new(base) {
              define_method(:foo) { |ary| ary << 2; super(ary) }
            }
            mod = Module.new { def foo(ary); ary << 3; super(ary); end }
            child.prepend(mod)
            ary = []
            child.new.foo(ary)
            ary
            "##,
        );
    }

    #[test]
    fn include_reaches_existing_includers() {
        // A module included into one that is already mixed in reaches
        // every includer (CRuby 3.0+): classes, modules, and a module's
        // own singleton class after `extend self`.
        run_test_once(
            r#"
            m1 = Module.new
            m2 = Module.new { def m2; :m2; end }
            m3 = Module.new { def m3; :m3; end }
            m4 = Module.new { def m4; :m4; end }
            c = Class.new
            c.include(m1); m1.include(m2); m2.include(m3); m3.include(m4)
            r = [c.new.m2, c.new.m3, c.new.m4, c.ancestors.size]
            a = Module.new { extend self }
            b = Module.new { def foo; :foo; end }
            a.include b
            r << a.foo
            n1 = Module.new; n2 = Module.new; n3 = Module.new { include n2 }
            n2.include n1
            r << (n3.ancestors == [n3, n2, n1])
            # Already an ancestor of the includer: no second copy.
            d = Class.new { include n1; include n3 }
            n2.include n1
            r << d.ancestors.count(n1)
            r
            "#,
        );
    }

    #[test]
    fn const_added_reports_the_definition_line() {
        run_test_once(
            r#"
            lines = []
            mod = Module.new do
              define_singleton_method(:const_added) do |name|
                lines << [name, caller_locations(1, 1)[0].lineno]
              end
            end
            mod.module_eval("TEST = 1\n\nmodule Sub\nend\n\nclass SubC\nend\n", "x.rb", 10)
            mod.const_set(:SET, 1)
            lines.map(&:first) + lines.take(3).map(&:last)
            "#,
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
        run_test_with_prelude(
            r##"
            $res
            "##,
            r##"
            $res = []
            class C
              def self.const_added(name)
                $res << name
              end
              FOO = 1
              BAR = 2
            end
            "##,
        );
    }

    #[test]
    fn const_added_const_set() {
        run_test_with_prelude(
            r##"
            $res
            "##,
            r##"
            $res = []
            class C
              def self.const_added(name)
                $res << name
              end
            end
            C.const_set(:FOO, 1)
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
        run_test_with_prelude(
            r##"
        baz Foo::BAR do end
        "##,
            r##"
        module Foo
          BAR = 42
        end
        def baz(x)
          [x, block_given?]
        end
        "##,
        );
        run_test_with_prelude(
            r##"
        baz Foo::BAR do 10 end
        "##,
            r##"
        module Foo
          BAR = 42
        end
        def baz(x)
          x + yield
        end
        "##,
        );
        run_test_with_prelude(
            r##"
        f A::B::C do end
        "##,
            r##"
        module A
          module B
            C = 99
          end
        end
        def f(x)
          [x, block_given?]
        end
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
        run_tests(&[
            "Integer.to_s",
            "String.to_s",
            "Module.to_s",
            "Class.to_s",
        ]);
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

    /// A singleton class appends a class or module attached to it
    /// through `rb_inspect`, so the attached one's own `#inspect` is
    /// what it shows — `rb_obj_as_string`'d when it answers no String,
    /// its exception propagating — and anything else attached through
    /// `rb_any_to_s`, whatever its `#inspect` says (#1518).
    #[test]
    fn singleton_class_to_s_inspects_what_it_is_attached_to() {
        run_test_once(
            r##"
        t = ->(&b) { begin; s = b.call; [s.sub(/0x\h+/, "X"), s.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        c = Class.new; def c.inspect = "Cあ"
        res << t.() { c.singleton_class.inspect }
        res << t.() { c.singleton_class.to_s }
        res << t.() { c.singleton_class.singleton_class.inspect }
        c = Module.new; def c.inspect = 42
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = :sym
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = nil
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = "plain".encode("UTF-16LE")
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = "Eé".encode("ISO-8859-1")
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = "\xff".dup.force_encoding("UTF-8")
        res << t.() { c.singleton_class.inspect.bytes }
        c = Class.new; def c.inspect = "é".encode("CESU-8")
        res << t.() { c.singleton_class.inspect }
        c = Class.new; def c.inspect = raise("boom")
        res << t.() { c.singleton_class.inspect }
        o = Object.new; def o.inspect = "never"
        res << t.() { o.singleton_class.inspect }
        res << t.() { Comparable.singleton_class.inspect }
        res << t.() { Class.new.singleton_class.inspect }
        res
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
    fn class_variable_set_updates_ancestor() {
        // `class_variable_set` on a subclass updates the class variable
        // defined on an ancestor rather than shadowing a fresh copy — a
        // shadow would make a later `class_variable_get` raise "class
        // variable … is overtaken". The variable stays undefined on the
        // superclass above the definer.
        run_test(
            r#"
            a = Class.new
            b = Class.new(a)
            c = Class.new(b)
            b.class_variable_set(:@@cv, :value)
            c.class_variable_set(:@@cv, :next)
            [
              b.class_variable_get(:@@cv),
              c.class_variable_get(:@@cv),
              (a.class_variable_get(:@@cv) rescue :nameerror),
            ]
            "#,
        );
        // A fresh receiver with no ancestor definition defines it on itself.
        run_test(
            r#"
            c = Class.new
            c.class_variable_set(:@@x, 1)
            c.class_variable_set(:@@x, 2)
            c.class_variable_get(:@@x)
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
    fn alias_undef_keyword_on_immediate_raises() {
        // The `alias` / `undef` keywords inside `instance_eval` target the
        // receiver's singleton class. An immediate (Integer / Symbol) cannot
        // own a singleton class, so it must raise `TypeError` — matching
        // `def` and CRuby.
        run_test(
            r#"begin; 1.instance_eval { alias :foo :to_s }; :no; rescue TypeError; :te; end"#,
        );
        run_test(
            r#"begin; :sym.instance_eval { alias :foo :to_s }; :no; rescue TypeError; :te; end"#,
        );
        run_test(r#"begin; 1.instance_eval { undef to_s }; :no; rescue TypeError; :te; end"#);
        // A String instance *can* own a singleton class, so `alias` there
        // succeeds and defines the alias only on that object.
        run_test(
            r#"
            s = "abc"
            s.instance_eval { alias my_len length }
            [s.my_len, "other".respond_to?(:my_len)]
            "#,
        );
        // The `alias` keyword in a `class_eval` block and a class body still
        // targets the class itself.
        run_test(
            r#"
            c = Class.new { def orig; 5; end }
            c.class_eval { alias al orig }
            c.new.al
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
        run_test_with_prelude(
            r#"
            [
              M.const_defined?("N::CONST"),
              Object.const_defined?("M::N::CONST"),
              M.const_defined?("::M"),
            ]
            "#,
            r#"
            module M; class N; CONST = 1; end; end
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
        let prelude = r#"
            module M; class N; CONST = 99; end; end
            "#;
        run_test_with_prelude(r#"M.const_get("N::CONST")"#, prelude);
        run_test_with_prelude(r#"Object.const_get("M::N::CONST")"#, prelude);
        run_test_with_prelude(r#"M.const_get("::M::N::CONST")"#, prelude);
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

    #[test]
    fn module_autoload_on_frozen_module_raises_frozen_error() {
        // `Module#autoload` must raise `FrozenError` *before* recording
        // any state when the receiver is frozen. The constant must NOT
        // appear in `Module#constants` afterwards.
        run_test(
            r#"
            m = Module.new
            m.freeze
            ok = false
            begin
              m.autoload :Foo, "/dev/null"
            rescue FrozenError
              ok = true
            end
            [ok, m.constants.include?(:Foo)]
            "#,
        );
    }

    #[test]
    fn module_autoload_on_currently_loading_file_is_noop() {
        // Registering `autoload :Foo, __FILE__` from inside the file
        // currently being executed by `require` is a no-op (the file
        // is treated as already loaded). `autoload?` returns nil.
        run_test_once(
            r#"
            body = "module SelfRegister; end\nSelfRegister.autoload(:Foo, __FILE__)\n$autoload_self_register_result = SelfRegister.autoload?(:Foo)\n"
            File.write('/tmp/monoruby_autoload_self_register.rb', body)
            require '/tmp/monoruby_autoload_self_register.rb'
            $autoload_self_register_result
            "#,
        );
    }

    #[test]
    fn module_autoload_state_consumed_after_direct_require() {
        // When the file registered for an autoload is loaded directly
        // via `require`, the autoload entry is "consumed": its name
        // remains in `Module#constants(false)` but `const_defined?`
        // returns `false` and `autoload?` returns `nil`.
        run_test_once(
            r##"
            File.write('/tmp/monoruby_autoload_no_const.rb', "# defines nothing")
            module ConsumedHost
              autoload :NoSuchConst, '/tmp/monoruby_autoload_no_const.rb'
            end
            require '/tmp/monoruby_autoload_no_const.rb'
            [
              ConsumedHost.constants(false).include?(:NoSuchConst),
              ConsumedHost.const_defined?(:NoSuchConst),
              ConsumedHost.autoload?(:NoSuchConst),
            ]
            "##,
        );
    }

    #[test]
    fn module_loaded_features_replace_resyncs_require() {
        // Mutating `$LOADED_FEATURES` from Ruby (here via `replace`)
        // propagates to the require-tracking layer, so a subsequent
        // `require` of the same file actually re-executes the body.
        run_test_once(
            r#"
            File.write('/tmp/monoruby_resync.rb', "$resync_counter = ($resync_counter || 0) + 1")
            $resync_counter = 0
            require '/tmp/monoruby_resync.rb'
            first = $resync_counter
            $LOADED_FEATURES.replace($LOADED_FEATURES - ['/tmp/monoruby_resync.rb'])
            require '/tmp/monoruby_resync.rb'
            [first, $resync_counter]
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
        run_test_with_prelude(
            r#"
            loc = CSrcLocBasic.const_source_location(:MY_CONST)
            [loc.is_a?(Array), loc.size, loc[1].is_a?(Integer)]
            "#,
            r#"
            class CSrcLocBasic
              MY_CONST = 1
            end
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
        run_test_with_prelude(
            r#"
            loc = CSrcLocScopeOuter.const_source_location("Inner::INNER_CONST")
            [loc.is_a?(Array), loc.size == 2]
            "#,
            r#"
            class CSrcLocScopeOuter
              class Inner
                INNER_CONST = 42
              end
            end
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

    // ----- refinements -----

    #[test]
    fn refine_builds_a_refinement() {
        // `refine` returns a Refinement that knows its target, and the
        // refined class itself is untouched.
        run_test(
            r#"
            m = Module.new do
              refine String do
                def shout; upcase; end
              end
            end
            r = m.refinements[0]
            [r.class.to_s,
             r.target.to_s,
             r.instance_methods(false),
             m.refinements.size,
             String.instance_methods(false).include?(:shout)]
            "#,
        );
    }

    #[test]
    fn refine_is_idempotent_per_refined_class() {
        // Two `refine` blocks for the same class extend one refinement.
        run_test(
            r#"
            m = Module.new do
              refine(String) { def a; end }
              refine(String) { def b; end }
              refine(Array)  { def c; end }
            end
            [m.refinements.size,
             m.refinements.map { |r| r.target.to_s }.sort,
             m.refinements.find { |r| r.target == String }.instance_methods(false).sort]
            "#,
        );
    }

    #[test]
    fn refinement_is_not_a_mixin() {
        // A Refinement is a Module, but it cannot be included, prepended
        // or extended with, and it defines none of the hooks that would
        // let it be.
        run_test(
            r#"
            r = Module.new { refine(String) { } }.refinements[0]
            out = []
            out << Refinement.private_instance_methods(true).include?(:append_features)
            out << Refinement.private_instance_methods(true).include?(:prepend_features)
            out << Refinement.private_instance_methods(true).include?(:extend_object)
            [[:include, -> { Class.new.include(r) }],
             [:extend,  -> { Object.new.extend(r) }],
             [:r_include, -> { r.include(Module.new) }],
             [:r_prepend, -> { r.prepend(Module.new) }]].each do |name, blk|
              begin
                blk.call
                out << [name, :no_raise]
              rescue TypeError
                out << [name, :TypeError]
              end
            end
            out
            "#,
        );
    }

    #[test]
    fn used_refinements_follows_the_scope() {
        // `Module.used_refinements` / `used_modules` report the *caller's*
        // scope, so they are empty outside the `using`. Single-shot: the
        // block runs `using` on a fresh module, which the per-iseq cell
        // would accumulate across `run_test`'s 25 repeats (see
        // `ISeqInfo::refinements`).
        run_test_once(
            r#"
            m = Module.new { refine(String) { def z; end } }
            inner = nil
            Module.new do
              using m
              inner = [Module.used_refinements.size, Module.used_modules.size]
            end
            [inner, Module.used_refinements, Module.used_modules]
            "#,
        );
    }

    #[test]
    fn using_activates_from_that_point_in_the_scope() {
        // Activation is a runtime event at a lexical position: the same
        // call resolves differently before and after it.
        run_test_once(
            r#"
            module UsingA
              refine(String) { def tag; "A"; end }
            end
            out = []
            out << ("x".tag rescue "-")
            using UsingA
            out << ("x".tag rescue "-")
            out
            "#,
        );
    }

    #[test]
    fn a_def_snapshots_but_a_block_reads_through() {
        // A method defined before the `using` never sees it; a block
        // created before it does, because the block resolves through its
        // lexical parent at call time.
        run_test_once(
            r#"
            module SnapA
              refine(String) { def tag;  "A"; end }
            end
            module SnapB
              refine(String) { def tag2; "B"; end }
            end
            chk = -> { [ ("x".tag rescue "-"), ("x".tag2 rescue "-") ] }
            def snap0; [ ("x".tag rescue "-"), ("x".tag2 rescue "-") ]; end
            using SnapA
            def snap1; [ ("x".tag rescue "-"), ("x".tag2 rescue "-") ]; end
            using SnapB
            def snap2; [ ("x".tag rescue "-"), ("x".tag2 rescue "-") ]; end
            [chk.call, snap0, snap1, snap2]
            "#,
        );
    }

    #[test]
    fn refinement_does_not_escape_its_scope() {
        // The refined method is invisible outside the activating scope,
        // and the refined class itself never gains it.
        run_test_once(
            r#"
            module EscA
              refine(String) { def only_here; :yes; end }
            end
            inner = Module.new do
              using EscA
              def self.run; "x".only_here; end
            end
            [inner.run,
             ("x".only_here rescue $!.class.to_s),
             String.instance_methods(false).include?(:only_here)]
            "#,
        );
    }

    /// A refinement of a basic operation has to reach both tiers. Both the
    /// VM's assembly and the JIT answer `Integer#+` without a lookup, so
    /// `refine` has to invalidate that fast path the same way a redefinition
    /// does; the dispatch it falls back to is already refinement-aware.
    /// #1066.
    #[test]
    fn refinement_of_a_basic_operation_is_honored() {
        run_test_once(
            r#"
            module BopPlus
              refine(Integer) { def +(o); 42; end }
            end
            inner = Module.new do
              using BopPlus
              def self.direct; 1 + 1; end
              def self.viajit(a, b); a + b; end
            end
            i = 0
            while i < 200      # past the JIT thresholds
              inner.viajit(1, 1)
              i = i.succ
            end
            [inner.direct, inner.viajit(1, 1), 1 + 1]
            "#,
        );
    }

    /// A refinement of a basic operation binds lexically like any other, so
    /// a scope that never activated it keeps the builtin. That is also what
    /// lets the JIT keep its inline path there: `assume_basic_op` asks the
    /// compiling scope's set rather than a global flag, so `refine`-ing an
    /// operator costs nothing outside the scopes that `using` it.
    #[test]
    fn refinement_of_a_basic_operation_does_not_escape_its_scope() {
        run_test_once(
            r#"
            module BopScoped
              refine(Integer) { def +(o); 42; end }
            end
            inner = Module.new do
              using BopScoped
              def self.run; 1 + 1; end
            end
            def outside(a, b); a + b; end
            i = 0
            while i < 200      # past the JIT thresholds, unrefined scope
              outside(1, 1)
              i = i.succ
            end
            [inner.run, outside(1, 1), 1 + 1]
            "#,
        );
    }

    /// The library's own arithmetic is not the user's scope. monoruby writes
    /// `Array#map` and friends in Ruby, where CRuby uses C — and a C frame
    /// carries no cref, so a refinement of `Integer#+` must not reach the
    /// `i += 1` those bodies loop on. Before the fix `map` ended after one
    /// iteration.
    #[test]
    fn refinement_of_a_basic_operation_does_not_leak_into_ruby_builtins() {
        run_test_once(
            r#"
            module BopLeak
              refine(Integer) { def +(o); 42; end }
            end
            inner = Module.new do
              using BopLeak
              def self.run
                [[1, 2, 3].map { |x| x * 2 }, (1..3).sum, [1, 2, 3].sum, 1 + 1]
              end
            end
            inner.run
            "#,
        );
    }

    /// The same boundary for a protocol dispatch: interpolation runs in the
    /// user's own iseq and sees the refinement, while `Array#join` reaching
    /// for `to_s` from inside the library does not. CRuby draws the line in
    /// exactly this place because `join` is C.
    #[test]
    fn refinement_stops_at_the_ruby_implemented_builtin_boundary() {
        run_test_once(
            r##"
            module BopToS
              refine(Integer) { def to_s; "REFINED"; end }
            end
            inner = Module.new do
              using BopToS
              def self.run; ["#{1}", [1].join, [1].to_s, 1.to_s]; end
            end
            inner.run
            "##,
        );
    }

    #[test]
    fn using_gathers_refinements_of_ancestors() {
        // `using M` activates what M's *included* modules refine too.
        run_test_once(
            r#"
            module AncInner
              refine(Integer) { def wrap; "<#{to_s}>"; end }
            end
            module AncOuter
              include AncInner
              refine(String) { def wrap; "'" + self + "'"; end }
            end
            Module.new do
              using AncOuter
              def self.run; [1.wrap, "x".wrap]; end
            end.run
            "#,
        );
    }

    #[test]
    fn sibling_refinements_see_each_other() {
        // All the refinements defined in *one* module are visible from
        // each other's method bodies, including a sibling defined after
        // the body that calls into it.
        run_test_once(
            r#"
            module Json
              refine(Integer) { def fmt; to_s; end }
              refine(Array)   { def fmt; "[" + map { |i| i.fmt }.join(",") + "]"; end }
              refine(Hash)    { def fmt; "{" + map { |k, v| k.fmt + ":" + v.fmt }.join(",") + "}"; end }
            end
            Module.new do
              using Json
              def self.run; [{1 => 2}, {3 => 4}].fmt; end
            end.run
            "#,
        );
    }

    #[test]
    fn using_is_rejected_where_it_is_illegal() {
        // `Module#using` is out in a method body, `main.using` anywhere
        // but the top level.
        run_test_once(
            r#"
            OUT = []
            MAIN = self
            m = Module.new do
              def self.f; using Module.new; end
            end
            begin; m.f; rescue RuntimeError => e; OUT << e.message; end
            module UsingPos
              begin
                MAIN.send(:using, Module.new)
                OUT << :no_raise
              rescue RuntimeError => e
                OUT << e.message
              end
            end
            OUT
            "#,
        );
    }

    #[test]
    fn refined_call_sites_survive_jit_compilation() {
        // `run_test` repeats enough to compile: the refined call has to
        // resolve the same way once it is machine code, and two scopes
        // with different sets must not share an answer. The third class
        // activates nothing and must still see the unrefined world.
        run_test(
            r#"
            module JitR1
              refine(String) { def kind; "R1"; end }
            end
            module JitR2
              refine(String) { def kind; "R2"; end }
            end
            class JitA
              using JitR1
              def run(n); r = nil; n.times { r = "x".kind }; r; end
            end
            class JitB
              using JitR2
              def run(n); r = nil; n.times { r = "x".kind }; r; end
            end
            class JitC
              def run(n); r = nil; n.times { r = ("x".kind rescue "none") }; r; end
            end
            [JitA.new.run(30), JitB.new.run(30), JitC.new.run(30)]
            "#,
        );
    }

    #[test]
    fn redefining_the_refined_class_reaches_compiled_code() {
        // Reopening the refined class after the body is compiled must
        // not disturb the refinement, and must be visible outside it.
        run_test_once(
            r#"
            module JitR3
              refine(String) { def shout; upcase + "!"; end }
            end
            class JitHot
              using JitR3
              def run(n); r = nil; n.times { r = "hi".shout }; r; end
            end
            before = JitHot.new.run(300)
            class String
              def shout; "GLOBAL"; end
            end
            [before, JitHot.new.run(300), "hi".shout]
            "#,
        );
    }

    #[test]
    fn reflection_honours_the_callers_refinements() {
        // Ruby 4.0 has `send`, `respond_to?`, `Object#method`,
        // `instance_method`, blocks and `eval` all resolve through the
        // activated refinement.
        run_test_once(
            r#"
            module RefR
              refine String do
                def foo; "refined"; end
              end
            end
            class RefC
              using RefR
              def direct      = "x".foo
              def via_send    = "x".send(:foo)
              def via_respond = "x".respond_to?(:foo)
              def via_method  = "x".method(:foo).call
              def via_unbound = String.instance_method(:foo).bind("x").call
              def in_block    = [1].map { "x".foo }.first
              def in_eval     = eval('"x".foo')
            end
            c = RefC.new
            [c.direct, c.via_send, c.via_respond, c.via_method,
             c.via_unbound, c.in_block, c.in_eval,
             ("x".foo rescue $!.class.to_s),
             "x".respond_to?(:foo)]
            "#,
        );
    }

    #[test]
    fn implicit_protocol_dispatch_honours_refinements() {
        // String interpolation's `#to_s`, `Symbol#to_proc` and the
        // `&obj` coercion's `#to_proc` all dispatch from inside
        // monoruby's own Ruby-level core library; the scope that counts
        // is still the user's.
        run_test_once(
            r##"
            module ProtoR
              refine Integer do
                def to_s; "<" + (self + 0).abs.inspect + ">"; end
              end
              refine String do
                def to_proc; ->(*) { "coerced" }; end
              end
            end
            Module.new do
              using ProtoR
              def self.run
                ["#{7}", [1, 2].map(&:to_s), ["a"].map(&"anything")]
              end
            end.run
            "##,
        );
    }

    #[test]
    fn alias_inside_a_refinement_names_the_refined_class() {
        // `alias new old` in a refine block resolves `old` on the class
        // being refined; the alias exists only under the refinement.
        run_test_once(
            r#"
            r = Module.new do
              refine Array do
                alias_method :orig_count, :count
              end
            end
            inner = Module.new do
              using r
              def self.run; [1, 2].orig_count; end
            end
            [inner.run, ([1, 2].orig_count rescue $!.class.to_s)]
            "#,
        );
    }

    #[test]
    fn refinement_import_methods_argument_checks() {
        // Nothing is imported unless every argument is a Ruby module.
        run_test(
            r#"
            src = Module.new { def imported; :imported; end }
            out = []
            Module.new do
              refine String do
                begin
                  import_methods src, Integer
                rescue TypeError
                  out << :TypeError
                end
                out << instance_methods(false)
                begin
                  import_methods src, Kernel
                rescue ArgumentError
                  out << :ArgumentError
                end
                import_methods src
                out << instance_methods(false)
              end
            end
            out
            "#,
        );
    }

    #[test]
    fn refinement_import_methods_requires_def_defined_bodies() {
        // CRuby's check is on the method-entry TYPE, not on where the body
        // ultimately came from: an `alias` entry (either form) and a
        // visibility re-declaration of an inherited method raise even
        // though the body they lead to is plain Ruby, while
        // `define_method(name, method_obj)` re-registers the original
        // ISEQ and imports. `define_method` with a block, accessors and
        // native methods raise. Each rejecting module carries ONLY the
        // offending entry — a raise mid-module leaves earlier entries of
        // that module imported in CRuby, and this must not depend on
        // method-table iteration order.
        run_test(
            r#"
            probe = lambda do |src|
              begin
                Module.new { refine(String) { import_methods src } }
                :imported
              rescue ArgumentError => e
                # The message names the module by its inspect (an address
                # for anonymous modules); keep only the stable tail.
                "ArgumentError: ##{e.message.split('#').last}"
              end
            end
            base = Module.new { def rm; 1; end }
            [
              probe.call(Module.new { def ok; 1; end }),
              probe.call(Module.new { include base; private :rm }),
              probe.call(Module.new { define_method(:dm) { 1 } }),
              probe.call(Module.new { define_method(:dm2, base.instance_method(:rm)) }),
              probe.call(Module.new { attr_reader :foo }),
              probe.call(Module.new { def s; 1; end; alias_method :am, :s }.tap { |m| m.send(:remove_method, :s) }),
              probe.call(Module.new { def s; 1; end; alias kw s }.tap { |m| m.send(:remove_method, :s) }),
            ]
            "#,
        );
        // Math's methods are native in both implementations.
        run_test_error(
            r#"
            Module.new { refine(String) { import_methods Math } }
            "#,
        );
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
        // (given 4, expected 1..3)").
        run_test_error(
            r#"
            Class.new.class_eval("nil", "f", 1, :extra)
            "#,
        );
    }

    #[test]
    fn define_method_strict_arity_too_few() {
        // A method defined from a Proc/block enforces method-style
        // arity, so calling with fewer args than the block declares
        // raises ArgumentError instead of filling missing params with
        // nil (the proc-style behaviour).
        run_test_error(
            r#"
            class C
              define_method(:m) { |a, b| [a, b] }
            end
            C.new.m(1)
            "#,
        );
    }

    #[test]
    fn define_method_strict_arity_too_many() {
        // Likewise, extra arguments are rejected.
        run_test_error(
            r#"
            class C
              define_method(:m) { |a| a }
            end
            C.new.m(1, 2)
            "#,
        );
    }

    #[test]
    fn define_method_no_destructure_single_array() {
        // Block-style auto-destructuring of a single Array argument
        // must not happen for define_method bodies — the array is
        // passed through as one value.
        run_test(
            r#"
            class C
              define_method(:m) { |a| a }
            end
            C.new.m([1, 2])
            "#,
        );
    }

    #[test]
    fn define_method_rest_arity() {
        // Rest params on a method-style body still accept any extra
        // positional args without ArgumentError.
        run_test(
            r#"
            class C
              define_method(:m) { |a, b, *c| [a, b, c] }
            end
            C.new.m(1, 2, 3, 4)
            "#,
        );
    }

    #[test]
    fn define_method_proc_unaffected() {
        // The original Proc passed to define_method keeps its
        // block-style behaviour when invoked directly — only the
        // wrapper installed as a method becomes strict. Calling the
        // original proc with one arg fills `b` with nil; the method
        // would raise ArgumentError instead (covered by
        // `define_method_strict_arity_too_few`).
        run_test(
            r#"
            p = Proc.new { |a, b| [a, b] }
            Class.new do
              define_method(:m, p)
            end
            p.call(1)
            "#,
        );
    }

    #[test]
    fn undefined_instance_methods_returns_undef_names() {
        // `undef_method` marks the slot Undefined; the new builtin
        // surfaces those names.
        run_test(
            r#"
            class P
              def foo; end
              def bar; end
              undef_method :foo
            end
            P.undefined_instance_methods.sort
            "#,
        );
    }

    #[test]
    fn undefined_instance_methods_empty_when_none() {
        run_test(
            r#"
            class C
              def foo; end
            end
            C.undefined_instance_methods
            "#,
        );
    }

    #[test]
    fn undefined_instance_methods_only_self() {
        // Inherited undefs are NOT included — only entries on the
        // class's own method table count.
        run_test(
            r#"
            class P
              def foo; end
              undef_method :foo
            end
            class C < P
            end
            C.undefined_instance_methods
            "#,
        );
    }

    #[test]
    fn set_temporary_name_empty_string_raises() {
        run_test_error(
            r#"
            Module.new.set_temporary_name("")
            "#,
        );
    }

    #[test]
    fn set_temporary_name_constant_name_raises() {
        run_test_error(
            r#"
            Module.new.set_temporary_name("Object")
            "#,
        );
    }

    #[test]
    fn set_temporary_name_constant_path_raises() {
        run_test_error(
            r#"
            Module.new.set_temporary_name("A::B")
            "#,
        );
    }

    #[test]
    fn set_temporary_name_leading_colons_raises() {
        run_test_error(
            r#"
            Module.new.set_temporary_name("::A")
            "#,
        );
    }

    #[test]
    fn set_temporary_name_permanent_module_raises() {
        // Built-in / constant-bound classes are permanent and refuse
        // a temporary rename.
        run_test_error(
            r#"
            Object.set_temporary_name("fake")
            "#,
        );
    }

    #[test]
    fn set_temporary_name_accepts_non_constant_strings() {
        // Strings that don't parse as a constant or constant path are
        // accepted: lowercase initial, embedded non-identifier
        // characters, lowercase second segment, trailing `::`, etc.
        run_test(
            r#"
            m = Module.new
            results = []
            results << m.set_temporary_name("name").name
            results << m.set_temporary_name("Template['foo.rb']").name
            results << m.set_temporary_name("a::B").name
            results << m.set_temporary_name("A::b").name
            results << m.set_temporary_name("A::B::").name
            results << m.set_temporary_name("A=").name
            results
            "#,
        );
    }

    #[test]
    fn set_temporary_name_bare_leaf_for_anonymous_parent() {
        // When the parent chain contains an anonymous module, an
        // explicit `set_temporary_name` call on a leaf still returns
        // the bare name (no `#<Module:0x..>::leaf` rendering).
        run_test(
            r#"
            m = Module.new
            module m::N; end
            m::N.set_temporary_name("fake_name")
            m::N.name
            "#,
        );
    }

    #[test]
    fn set_temporary_name_anonymous_nested_module_is_not_permanent() {
        // A module assigned through an anonymous parent isn't itself
        // permanent: `set_temporary_name` succeeds on it.
        run_test(
            r#"
            m = Module.new
            module m::N; end
            m::N.set_temporary_name("ok")
            m::N.name
            "#,
        );
    }

    #[test]
    fn set_temporary_name_nil_clears_name() {
        run_test(
            r#"
            m = Module.new
            m.set_temporary_name("temp")
            m.set_temporary_name(nil)
            m.name
            "#,
        );
    }

    #[test]
    fn deprecate_constant_returns_self() {
        run_test(
            r#"
            m = Module.new
            m.const_set(:X, 1)
            m.deprecate_constant(:X).equal?(m)
            "#,
        );
    }

    #[test]
    fn deprecate_constant_undefined_name_raises() {
        run_test_error(
            r#"
            Module.new.deprecate_constant(:UNDEFINED)
            "#,
        );
    }

    #[test]
    fn deprecate_constant_emits_warning_when_warning_deprecated_true() {
        // The warning should reach $stderr.write. We capture stderr by
        // pointing $stderr at a String-with-write stub and asserting on
        // the captured message.
        run_test(
            r##"
            old_dep = Warning[:deprecated]
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              Warning[:deprecated] = true
              m = Module.new
              m.const_set(:X, 1)
              m.deprecate_constant(:X)
              m::X
            ensure
              Warning[:deprecated] = old_dep
              $stderr = old_stderr
            end
            captured =~ /warning: constant .+::X is deprecated/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn deprecate_constant_silent_when_warning_deprecated_false() {
        run_test(
            r##"
            old_dep = Warning[:deprecated]
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              Warning[:deprecated] = false
              m = Module.new
              m.const_set(:X, 1)
              m.deprecate_constant(:X)
              m::X
            ensure
              Warning[:deprecated] = old_dep
              $stderr = old_stderr
            end
            captured.empty?
            "##,
        );
    }

    #[test]
    fn deprecate_constant_accepts_multiple_names_as_strings_and_symbols() {
        run_test(
            r#"
            m = Module.new
            m.const_set(:X, 1)
            m.const_set(:Y, 2)
            m.deprecate_constant("X", :Y)
            [m::X, m::Y]
            "#,
        );
    }

    #[test]
    fn deprecate_constant_warns_when_removed() {
        // CRuby ≥3.4: `remove_const` on a deprecated constant fires
        // the deprecation warning before the slot is deleted.
        run_test(
            r##"
            old_dep = Warning[:deprecated]
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              Warning[:deprecated] = true
              m = Module.new
              m.const_set(:X, 1)
              m.deprecate_constant(:X)
              m.module_eval { remove_const :X }
            ensure
              Warning[:deprecated] = old_dep
              $stderr = old_stderr
            end
            captured =~ /warning: constant .+::X is deprecated/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn ruby2_keywords_undefined_method_raises_nameerror() {
        run_test_error(
            r#"
            Class.new { ruby2_keywords :missing_method }
            "#,
        );
    }

    #[test]
    fn ruby2_keywords_non_symbol_string_raises_typeerror() {
        run_test_error(
            r#"
            Class.new { ruby2_keywords Object.new }
            "#,
        );
    }

    #[test]
    fn ruby2_keywords_accepts_symbol_for_existing_method() {
        // No-op call returning nil; should not raise.
        run_test(
            r#"
            c = Class.new { def foo(*a); a; end; ruby2_keywords :foo }
            c.new.foo(1, 2)
            "#,
        );
    }

    #[test]
    fn ruby2_keywords_accepts_string_for_existing_method() {
        run_test(
            r#"
            c = Class.new { def foo(*a); a; end; ruby2_keywords "foo" }
            c.new.foo(1, 2)
            "#,
        );
    }

    #[test]
    fn define_method_visibility_inherits_from_class_body() {
        // Inside the class body the current visibility wins.
        run_test(
            r#"
            klass = Class.new do
              define_method(:bar) { :bar }
              private
              define_method(:baz) { :baz }
            end
            [klass.public_method_defined?(:bar), klass.private_method_defined?(:baz)]
            "#,
        );
    }

    #[test]
    fn define_method_visibility_default_public_when_cross_class() {
        // Calling via `send` from a different class always installs
        // public, even if the *caller's* lexical visibility is private.
        run_test(
            r#"
            klass = Class.new
            Class.new do
              klass.send(:define_method, :bar) { :bar }
              private
              klass.send(:define_method, :baz) { :baz }
            end
            [
              klass.public_method_defined?(:bar),
              klass.public_method_defined?(:baz),
              klass.private_method_defined?(:baz),
            ]
            "#,
        );
    }

    #[test]
    fn define_method_initialize_is_private() {
        // Even called from a public scope, `:initialize` is private.
        run_test(
            r#"
            klass = Class.new do
              define_method(:initialize) { }
            end
            [
              klass.private_method_defined?(:initialize),
              klass.public_method_defined?(:initialize),
            ]
            "#,
        );
    }

    #[test]
    fn define_method_on_frozen_module_raises_frozenerror() {
        run_test_error(
            r#"
            Class.new { freeze; define_method(:foo) {} }
            "#,
        );
    }

    #[test]
    fn attr_boolean_argument_returns_method_names() {
        // The legacy `attr name, true|false` form returns the list of
        // installed reader/writer names. Stronger CRuby-vs-monoruby
        // comparison of the emitted-warning text is covered by the
        // `core/module/attr_spec.rb` ruby/spec suite (which uses
        // mspec's `complain` matcher).
        run_test(
            r#"
            c = Class.new
            r1 = c.send(:attr, :foo, true)
            r2 = c.send(:attr, :bar, false)
            r1 + r2
            "#,
        );
    }

    #[test]
    fn class_eval_too_many_args_message_uses_1_to_3_range() {
        // CRuby's `Module#class_eval` reports `expected 1..3` (not
        // `0..3`) for the string form's arity error, since the no-arg
        // block dispatch is a different path.
        run_test_error(
            r#"
            Class.new.class_eval("nil", "f", 1, :extra)
            "#,
        );
    }

    #[test]
    fn module_eval_too_many_args_uses_1_to_3_range() {
        run_test_error(
            r#"
            Class.new.module_eval("nil", "f", 1, :extra)
            "#,
        );
    }

    #[test]
    fn method_defined_module_does_not_search_object_or_kernel() {
        // CRuby: `Module#method_defined?` on a plain Module receiver
        // does NOT walk into `Object`/`Kernel`, even though monoruby's
        // class store wires every Module's superclass through `Object`
        // for dispatch reasons. So `:hash` (a Kernel method) shouldn't
        // surface here.
        run_test(
            r#"
            Module.new.method_defined?(:hash)
            "#,
        );
    }

    #[test]
    fn method_defined_class_still_finds_inherited() {
        // Class receivers continue to walk into Object/Kernel — verify
        // we didn't break the common case.
        run_test(
            r#"
            Class.new.method_defined?(:hash)
            "#,
        );
    }

    #[test]
    fn module_eval_redef_warning_uses_qualified_name() {
        // The "warning: already initialized constant ..." message must
        // be qualified with the receiver's name even when the
        // assignment happens through `module_eval` against an
        // anonymous module — the regex
        // `/already initialized constant .+::TEST/` from
        // `core/module/const_added_spec.rb` requires a parent
        // qualifier, even if it's the anonymous-module render.
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              mod = Module.new
              mod.module_eval("TEST = 1; TEST = 2")
            ensure
              $stderr = old_stderr
            end
            captured =~ /already initialized constant .+::TEST/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn const_set_redef_warning_uses_qualified_name() {
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              m = Module.new
              m.const_set(:X, 1)
              m.const_set(:X, 2)
            ensure
              $stderr = old_stderr
            end
            captured =~ /already initialized constant .+::X/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn ruby2_keywords_warns_when_method_has_no_splat() {
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              c = Class.new { def foo(a, b, c); end }
              c.send(:ruby2_keywords, :foo)
            ensure
              $stderr = old_stderr
            end
            captured =~ /Skipping set of ruby2_keywords flag for foo/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn ruby2_keywords_warns_when_method_has_explicit_keyword() {
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              c = Class.new { def foo(*a, b:); end }
              c.send(:ruby2_keywords, :foo)
            ensure
              $stderr = old_stderr
            end
            captured =~ /Skipping set of ruby2_keywords flag for foo/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn ruby2_keywords_warns_when_method_has_post_argument() {
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              c = Class.new { def foo(*a, b); end }
              c.send(:ruby2_keywords, :foo)
            ensure
              $stderr = old_stderr
            end
            captured =~ /Skipping set of ruby2_keywords flag for foo/ ? :ok : captured
            "##,
        );
    }

    #[test]
    fn ruby2_keywords_silent_for_compatible_signature() {
        // `def foo(*args)` is the canonical compatible shape — must
        // not warn.
        run_test(
            r##"
            old_stderr = $stderr
            captured = String.new
            $stderr = Class.new {
              define_method(:write) { |s| captured << s; s.size }
            }.new
            begin
              c = Class.new { def foo(*args); end }
              c.send(:ruby2_keywords, :foo)
            ensure
              $stderr = old_stderr
            end
            captured.empty?
            "##,
        );
    }

    #[test]
    fn name_constant_rebind_promotes_existing_named_module() {
        // `m::N` was created with `module m::N; end`, so it already has
        // a leaf name "N" but with an anonymous parent. Binding it to
        // a permanent constant must replace the anon parent with the
        // new owner, so `Module#name` renders the full path.
        run_test_with_prelude(
            r#"
            m::N.name
            "#,
            r#"
            module ModuleSpecs2; module Anonymous2; end; end
            m = Module.new
            module m::N; end
            ModuleSpecs2::Anonymous2::WasAnnon2 = m::N
            "#,
        );
    }

    #[test]
    fn name_anonymous_parent_promotion_propagates_to_descendant() {
        // Promoting `m` to a permanent constant should also flip its
        // descendant `m::N` to permanent — the qualified rendering
        // walks through `m`'s new permanent name.
        run_test_with_prelude(
            r#"
            m::N.name
            "#,
            r#"
            module ModuleSpecs3; end
            m = Module.new
            module m::N; end
            ModuleSpecs3::Mod3 = m
            "#,
        );
    }

    #[test]
    fn name_promotion_discards_temporary_on_descendant() {
        // A descendant that explicitly chose a temporary name (via
        // `set_temporary_name`) gets the constant-bound leaf restored
        // when an ancestor is promoted to permanent.
        run_test_with_prelude(
            r#"
            m::N.name
            "#,
            r#"
            module ModuleSpecs4; end
            m = Module.new
            module m::N; end
            m::N.set_temporary_name("fake")
            ModuleSpecs4::Mod4 = m
            "#,
        );
    }

    #[test]
    fn set_temporary_name_propagates_to_nested_module_render() {
        // When the receiver's leaf name changes via
        // `set_temporary_name`, descendants' cached qualified names
        // must be invalidated so the next call rebuilds with the new
        // leaf.
        run_test(
            r#"
            m = Module.new
            m::N = Module.new
            m::N.name # warm the cache with the anon-parent rendering
            m.set_temporary_name "m"
            m::N.name
            "#,
        );
    }

    #[test]
    fn name_promotion_does_not_replace_already_permanent() {
        // Once a module has a permanent constant binding, a *second*
        // binding under a different parent must not silently move it.
        run_test_with_prelude(
            r#"
            ModuleSpecsP1::Inner.name
            "#,
            r#"
            module ModuleSpecsP1; end
            module ModuleSpecsP2; end
            module ModuleSpecsP1::Inner; end
            ModuleSpecsP2::Alias = ModuleSpecsP1::Inner
            "#,
        );
    }

    #[test]
    fn set_temporary_name_on_descendant_after_promotion_is_rejected() {
        // After `M = m` promotes m::N, calling `set_temporary_name`
        // on m::N must raise `RuntimeError("can't change permanent
        // name")` — the promotion really did make it permanent.
        run_test_error(
            r#"
            module ModuleSpecs5; end
            m = Module.new
            module m::N; end
            ModuleSpecs5::Mod5 = m
            m::N.set_temporary_name("nope")
            "#,
        );
    }

    #[test]
    fn class_exec_module_def_uses_lexical_scope() {
        // CRuby: `module Inner` inside `A.class_exec { ... }` creates
        // toplevel `::Inner`, NOT `A::Inner`, because `class_exec`
        // changes only the receiver (for `def`), not the lexical
        // scope.
        run_test(
            r#"
            class ClassExecLexA; end
            ClassExecLexA.class_exec {
              module ClassExecLexInner; end
            }
            [
              Object.constants.include?(:ClassExecLexInner),
              ClassExecLexA.constants.include?(:ClassExecLexInner),
            ]
            "#,
        );
    }

    #[test]
    fn module_eval_block_module_def_uses_lexical_scope() {
        // Same rule for `module_eval(&block)` — the block-form's body
        // doesn't change lexical scope for `module Foo; end`.
        run_test(
            r#"
            class ModEvalBlockA; end
            ModEvalBlockA.module_eval {
              module ModEvalBlockInner; end
            }
            [
              Object.constants.include?(:ModEvalBlockInner),
              ModEvalBlockA.constants.include?(:ModEvalBlockInner),
            ]
            "#,
        );
    }

    #[test]
    fn instance_exec_module_def_uses_lexical_scope() {
        run_test(
            r#"
            class InstanceExecA; end
            InstanceExecA.new.instance_exec {
              module InstanceExecInner; end
            }
            [
              Object.constants.include?(:InstanceExecInner),
              InstanceExecA.constants.include?(:InstanceExecInner),
            ]
            "#,
        );
    }

    #[test]
    fn class_keyword_still_pushes_lexical_scope() {
        // Negative control: `class Outer; class Inner; end; end`
        // must still nest Inner under Outer (the keyword form is a
        // real lexical push).
        run_test(
            r#"
            class ClassKwOuter
              class ClassKwInner; end
            end
            ClassKwOuter::ClassKwInner.name
            "#,
        );
    }

    #[test]
    fn class_exec_def_still_lands_on_receiver() {
        // The receiver-as-default-definee semantics for `def` must
        // be preserved — `def` inside `class_exec` still adds to the
        // receiver class.
        run_test(
            r#"
            klass = Class.new
            klass.class_exec do
              def hi; :hi; end
            end
            klass.new.hi
            "#,
        );
    }

    #[test]
    fn define_method_trailing_comma_arity_is_one() {
        // CRuby's `|a,|` reports `arity = 1` (not 2) for both proc
        // and method/lambda forms — the trailing comma is a hint to
        // the runtime, not a "second required arg".
        run_test(
            r#"
            class C; define_method(:m) { |a,| a }; end
            C.new.method(:m).arity
            "#,
        );
    }

    #[test]
    fn define_method_trailing_comma_parameters() {
        // `Method#parameters` should report the trailing-comma rest
        // as absent — only the explicit positionals appear.
        run_test(
            r#"
            class C; define_method(:m) { |a,| a }; end
            C.new.method(:m).parameters
            "#,
        );
    }

    #[test]
    fn define_method_trailing_comma_strict_arity_rejects_extras() {
        // The method form is strict: passing extras raises
        // ArgumentError even though the block-form proc would absorb
        // them.
        run_test_error(
            r#"
            class C; define_method(:m) { |a,| a }; end
            C.new.m(1, 2)
            "#,
        );
    }

    #[test]
    fn define_method_trailing_comma_no_destructure() {
        // `m([1, 2])` returns `[1, 2]` whole — trailing-comma in
        // method form does NOT auto-splat a single Array argument.
        run_test(
            r#"
            class C; define_method(:m) { |a,| a }; end
            C.new.m([1, 2])
            "#,
        );
    }

    #[test]
    fn proc_trailing_comma_block_form_auto_splats() {
        // The block form: `proc { |a,| a }.call([1, 2])` returns 1
        // because the trailing comma's "block-style implicit rest"
        // makes the caller treat a single Array as auto-splat.
        run_test(
            r#"
            proc { |a,| a }.call([1, 2])
            "#,
        );
    }

    #[test]
    fn proc_trailing_comma_block_form_absorbs_extras() {
        // Without strict-arity (block form), extras are absorbed.
        run_test(
            r#"
            proc { |a,| a }.call(1, 2, 3)
            "#,
        );
    }

    #[test]
    fn lambda_trailing_comma_arity_is_one() {
        // The `lambda` form is method-style strict but reports the
        // same arity = 1 as proc.
        run_test(
            r#"
            lambda { |a,| a }.arity
            "#,
        );
    }

    #[test]
    fn explicit_anonymous_rest_arity_negative() {
        // Sanity: an *explicit* anonymous rest still flips arity to
        // the negative form (`-2`).
        run_test(
            r#"
            class C; define_method(:m) { |a, *| a }; end
            C.new.method(:m).arity
            "#,
        );
    }

    // -- define_method Method/UnboundMethod bind validation ----------------

    #[test]
    fn define_method_unbound_subclass_ok() {
        // Binding a parent's UnboundMethod onto a subclass is OK
        // because the subclass's ancestors include the parent.
        run_test(
            r#"
            parent = Class.new { def foo; :p; end }
            child  = Class.new(parent) { define_method(:bar, parent.instance_method(:foo)) }
            child.new.bar
            "#,
        );
    }

    #[test]
    fn define_method_unbound_unrelated_class_typeerror() {
        // Binding `M::C.instance_method(:foo)` onto an unrelated
        // class raises TypeError mentioning the qualified owner.
        run_test_error(
            r#"
            module Mod1; class Owner1; def foo; end; end; end
            Class.new { define_method(:bar, Mod1::Owner1.instance_method(:foo)) }
            "#,
        );
    }

    #[test]
    fn define_method_unbound_singleton_to_other_typeerror() {
        // A singleton-class method bound onto an unrelated regular
        // class raises with the "can't bind singleton method to a
        // different class" wording.
        run_test_error(
            r#"
            c = Class.new { class << self; def foo; end; end }
            um = c.method(:foo).unbind
            Class.new { define_method(:bar, um) }
            "#,
        );
    }

    #[test]
    fn define_method_module_owner_binds_freely() {
        // A method whose owner is a Module (not Class) can be
        // re-bound to any receiver — CRuby skips the subclass check
        // because the module could be included anywhere.
        run_test(
            r#"
            mod = Module.new { def foo; :ok; end }
            obj = Object.new
            obj.singleton_class.define_method(:bar, mod.instance_method(:foo))
            obj.bar
            "#,
        );
    }

    // -- module_function toggle state machine ------------------------------

    #[test]
    fn module_function_toggle_off_by_public_no_args() {
        // `module_function` then `public` (no args) resets the
        // module-function toggle, so subsequent `def`s become
        // regular instance methods (not module functions).
        run_test(
            r#"
            m = Module.new do
              module_function
              def t1; end
              public
              def t2; end
            end
            [m.respond_to?(:t1), m.respond_to?(:t2)]
            "#,
        );
    }

    #[test]
    fn module_function_with_define_method_block() {
        // Methods installed via `define_method` after a
        // `module_function` toggle become module functions too.
        run_test(
            r#"
            m = Module.new do
              module_function
              define_method(:t1) { }
            end
            m.respond_to?(:t1)
            "#,
        );
    }

    #[test]
    fn module_function_with_define_method_unbound() {
        run_test(
            r#"
            src = Module.new { def t1; end }
            m = Module.new do
              module_function
              define_method(:t2, src.instance_method(:t1))
            end
            m.respond_to?(:t2)
            "#,
        );
    }

    // -- caller format ----------------------------------------------------

    #[test]
    fn caller_locations_returns_a_location() {
        // `caller` formats labels with single quotes (Ruby 3.4+),
        // so `caller_locations`'s regex split (`['`](.+)'`) actually
        // matches and the result is a `CallerLocation` rather than
        // the unmatched-string fallback. We assert on a property
        // that's stable across CRuby versions: lineno is non-nil.
        run_test(
            r#"
            def f; caller_locations(1, 1)[0].lineno.is_a?(Integer); end
            f
            "#,
        );
    }

    // -- constants helpers ------------------------------------------------

    #[test]
    fn module_constants_class_method_returns_toplevel() {
        // No-arg `Module.constants` includes a freshly-defined
        // toplevel constant.
        run_test(
            r#"
            count = Module.constants.size
            module ModConstSpec1; end
            after = Module.constants.size
            Object.send(:remove_const, :ModConstSpec1)
            after - count
            "#,
        );
    }

    #[test]
    fn module_constants_inherits_dedups() {
        // Inheritance through multiple includes is deduped.
        run_test_with_prelude(
            r#"
            c.constants.count(:CONST_DEDUP)
            "#,
            r#"
            m = Module.new { CONST_DEDUP = 1 }
            c = Class.new { include m }
            "#,
        );
    }

    #[test]
    fn module_constants_filters_private() {
        // `private_constant`-marked entries don't appear in
        // `Module#constants`.
        run_test(
            r#"
            c = Class.new
            c.const_set(:PUB_CONST, 1)
            c.const_set(:PRIV_CONST, 2)
            c.send(:private_constant, :PRIV_CONST)
            c.constants.sort
            "#,
        );
    }

    #[test]
    fn qualified_constant_no_toplevel_fallback() {
        // `A::X` does not fall through to Object's own top-level constants
        // (`String::Hash` raises NameError), but the walk still continues
        // into modules mixed into Object and normal ancestors resolve.
        run_tests(&[
            r#"begin; String::Hash; :no; rescue NameError; :nme; end"#,
            r#"begin; Enumerable::Hash; :no; rescue NameError; :nme; end"#,
        ]);
        run_test(
            r#"
            module QTopA; X = 1; module Inner; Y = 2; end; end
            [QTopA::X, QTopA::Inner::Y, defined?(QTopA::X)]
            "#,
        );
    }

    #[test]
    fn const_missing_message_uses_name_then_inspect() {
        // A qualified miss crafts its prefix from the module's `#name`,
        // falling back to `#inspect` for an anonymous module.
        run_test(
            r#"
            m = Module.new { def self.name; "MdName"; end }
            begin; m::NOPE; rescue NameError => e; e.message; end
            "#,
        );
    }

    #[test]
    fn private_constant_qualified_access() {
        // A `private_constant` is not `defined?` through the `A::B` form and
        // cannot be reopened with a qualified `class`/`module` keyword, but
        // stays reachable lexically.
        run_test(
            r#"
            module PrivC
              class Sec; end
              private_constant :Sec
              REACH = (Sec.equal?(const_get(:Sec)))
            end
            [
              defined?(PrivC::Sec),
              (begin; class PrivC::Sec; end; :no; rescue NameError; :nme; end),
              PrivC::REACH,
            ]
            "#,
        );
    }

    #[test]
    fn toplevel_class_binds_on_object_inside_module() {
        // `class ::Foo` / `module ::Foo` bind at the top level (Object)
        // regardless of the enclosing lexical scope, so reopening a nested
        // class through `class ::Object; module ...` reaches the same class.
        run_test(
            r#"
            module TlWrap
              class Cont; class Child; end; end
              class ::Object
                TL_TOP = :tl_top
                module TlWrap
                  class Cont
                    class Child
                      def self.m; TL_TOP; end
                    end
                  end
                end
              end
            end
            [TlWrap::Cont::Child.m, Object.const_defined?(:TL_TOP)]
            "#,
        );
    }

    #[test]
    fn private_constant_routes_through_const_missing() {
        // A private constant access invokes `const_missing`, so a custom
        // hook can intercept it; the default hook surfaces the "private
        // constant" NameError.
        run_test(
            r#"
            module PcMiss
              X = 1
              private_constant :X
              def self.const_missing(n); n == :X ? :intercepted : super; end
            end
            r1 = PcMiss::X
            module PcMiss2
              Y = 1
              private_constant :Y
            end
            r2 = (begin; PcMiss2::Y; rescue NameError => e; e.message.include?("private"); end)
            [r1, r2]
            "#,
        );
    }

    #[test]
    fn const_set_allows_non_ascii_uppercase_name() {
        // Unicode uppercase (e.g. Greek `Ἅ`) is a valid constant-name start.
        run_test(r#"m = Module.new; m.const_set("ἍBB", 5); m.const_get("ἍBB")"#);
    }

    // -- module_function eval-boundary isolation --------------------------

    #[test]
    fn module_function_inside_eval_does_not_leak_outside() {
        // CRuby: `eval "module_function"` mutates the eval's *own*
        // cref copy, so `def test1` outside the eval is a regular
        // instance method (not a module function). Without the
        // cref-dup before `Kernel#eval`'s block invocation, the
        // toggle would leak through to the surrounding module body.
        run_test(
            r#"
            m = Module.new do
              eval "module_function"
              def test1; end
            end
            m.respond_to?(:test1)
            "#,
        );
    }

    #[test]
    fn module_function_inside_eval_works_for_local_defs() {
        // Counterpart to the boundary test: when `module_function`
        // *and* the `def`s live inside the same eval, the toggle
        // does apply (as expected).
        run_test(
            r##"
            m = Module.new do
              eval <<-CODE
                module_function
                def test1; end
                def test2; end
              CODE
            end
            [m.respond_to?(:test1), m.respond_to?(:test2)]
            "##,
        );
    }

    #[test]
    fn private_inside_eval_does_not_leak_outside() {
        // Same isolation rule applies to `private` (no args). A
        // `private` toggled inside an eval must not turn the outer
        // module body's subsequent `def` into a private method.
        run_test(
            r#"
            c = Class.new do
              eval "private"
              def public_after_eval; :ok; end
            end
            c.public_method_defined?(:public_after_eval)
            "#,
        );
    }

    #[test]
    fn define_method_with_method_via_block_amp() {
        // `&Method` as a block argument needs `Method#to_proc` to fire
        // before `define_method` can install it. The bytecode for
        // `&expr` (where `expr` isn't a Lambda / LocalVar) doesn't
        // emit a `to_proc` call, so the runtime fallback in
        // `Executor::generate_proc_inner` covers it. Surfaced by
        // `core/time/fixtures/classes.rb:7`'s
        // `define_method(:now, &Time.method(:now))` — without the
        // fallback it raised `RuntimeError: not yet implemented:
        // block handler unknown handler ...`.
        run_test(
            r#"
            class Holder
              class << self
                define_method(:now, &Time.method(:now))
              end
            end
            Holder.now.is_a?(Time)
            "#,
        );
    }

    #[test]
    fn define_method_with_user_to_proc_via_block_amp() {
        // Same fallback also accepts user-defined `to_proc`. The
        // returned Proc determines the installed method body.
        run_test(
            r#"
            class Coercible
              def to_proc
                proc { |x| x * 10 }
              end
            end
            class Box
              define_method(:scale, &Coercible.new)
            end
            Box.new.scale(7)
            "#,
        );
    }

    #[test]
    fn module_using_argchecks() {
        // Refinements are unimplemented, so `using` only has to agree
        // with CRuby on what it accepts and where it may appear.
        run_test(r#"Module.new { using Module.new }.class.to_s"#);
        run_test(r#"m = Module.new; r = nil; mod = Module.new { r = using m }; r.equal?(mod)"#);
        // A Class is not a refinement container.
        run_test_error(r#"Module.new { using Class.new }"#);
        run_test_error(r#"Module.new { using "foo" }"#);
        // ... and it is not permitted in a method body.
        run_test(
            r#"
            mod = Module.new do
              def self.foo
                using Module.new
              end
            end
            begin
              mod.foo
            rescue RuntimeError => e
              e.message
            end
            "#,
        );
    }

    #[test]
    fn visibility_shadow_follows_redefinition() {
        // `public :inherited_method` is a visibility modifier, not a
        // copy of the body: redefining the ancestor's method afterwards
        // must be visible through the subclass (CRuby's ZSUPER entry).
        run_test(
            r#"
            class VSParent
              private
              def m; :before; end
            end
            class VSChild < VSParent
              public :m
            end
            class VSParent
              def m; :after; end
            end
            [VSChild.new.m,
             VSChild.instance_method(:m).owner.to_s,
             VSChild.public_instance_methods(false).include?(:m)]
            "#,
        );
    }

    #[test]
    fn non_utf8_constant_name() {
        // A constant named by a String in a non-UTF-8 encoding is one
        // name, not a `::`-separated path: it must round-trip through
        // const_set / const_get / const_defined?.
        run_test(
            r#"
            m = Module.new
            name = "CS_CONST\u{3bb}".encode("euc-jp")
            m.const_set(name, 2)
            [m.const_get(name), m.const_defined?(name), m.constants.size]
            "#,
        );
    }

    #[test]
    fn set_temporary_name_erases_nested_names() {
        // A never-named module still qualifies what is nested under it,
        // but one whose name was explicitly erased does not — and
        // re-naming it brings the subtree's names back.
        run_test(
            r#"
            m = Module.new
            m::N = Module.new
            a = m::N.name.sub(/0x\h+/, "0xADDR")
            m.set_temporary_name "m"
            b = m::N.name
            m.set_temporary_name nil
            c = m::N.name
            m.set_temporary_name "y"
            [a, b, c, m::N.name]
            "#,
        );
    }

    #[test]
    fn module_nesting_inside_method_body() {
        // `Module.nesting` reports the nesting where the running code
        // was *written*. Calling a method from an unrelated class body
        // must not leak that body's scope into the answer.
        run_test(
            r#"
            module NestA
              module NestB
                def self.f; Module.nesting.map(&:to_s); end
                def self.g; [1].map { Module.nesting.map(&:to_s) }.first; end
              end
            end
            module NestC
              $r = [NestA::NestB.f, NestA::NestB.g]
            end
            $r << Module.nesting.map(&:to_s)
            "#,
        );
    }

    #[test]
    fn eval_resolves_constants_in_the_caller_scope() {
        // The eval body is anchored to the Ruby scope that wrote the
        // call, even when reached through a builtin frame (`send`), and
        // the receiver of a `module_eval` / `instance_eval` is pushed
        // *onto* that scope rather than replacing it.
        run_test(
            r#"
            module EvA
              Lookup = :found
              module EvB
                def self.direct;      module_eval("Lookup"); end
                def self.via_send(m); send(m, "Lookup"); end
                def self.nesting;     module_eval("Module.nesting.map(&:to_s)"); end
                def self.plain;       eval("Lookup"); end
                def self.plain_send(m); send(m, "Lookup"); end
              end
            end
            [EvA::EvB.direct,
             EvA::EvB.via_send(:module_eval),
             EvA::EvB.nesting,
             EvA::EvB.plain,
             EvA::EvB.plain_send(:eval)]
            "#,
        );
    }

    #[test]
    fn instance_eval_constant_lookup_order() {
        // `instance_eval "..."` looks in the receiver's singleton class,
        // then the receiver's class, and only then in the caller's
        // lexical scope.
        run_test(
            r#"
            module IEv
              module ReceiverScope
                class Receiver
                  FOO = :Receiver
                end
              end
              module CallerScope
                FOO = :CallerScope
                class Caller
                  FOO = :Caller
                  def get(receiver) = receiver.instance_eval("FOO")
                end
              end
            end
            r = IEv::ReceiverScope::Receiver.new
            a = IEv::CallerScope::Caller.new.get(r)
            r.singleton_class.const_set(:FOO, :singleton)
            [a, IEv::CallerScope::Caller.new.get(r)]
            "#,
        );
    }

    #[test]
    fn const_source_location_of_the_class_keyword() {
        // The location recorded for a constant created by `class Foo` /
        // `module Foo` is that keyword's own line, not the line of the
        // body it appears in.
        run_test_once(
            r#"
            module CslOuter
              class CslInner
              end
              module CslMod
              end
            end
            base = CslOuter.const_source_location(:CslInner)[1]
            [base, CslOuter.const_source_location(:CslMod)[1] - base]
            "#,
        );
    }

    #[test]
    fn method_added_sees_the_def_line() {
        // `caller_locations` inside `method_added` reports the line of
        // the `def` that triggered it. The hook is dispatched through
        // the invoker, which writes no caller pc, so the location has to
        // travel out-of-band.
        run_test_once(
            r#"
            $lines = []
            Class.new do
              def self.method_added(name)
                $lines << caller_locations(1, 1)[0].lineno
              end

              def first
              end

              def second
              end
            end
            $lines.map { |l| l - $lines[0] }
            "#,
        );
    }
    #[test]
    fn define_method_argument_errors() {
        // `rb_mod_define_method` names the argument's *class*, so `nil`
        // reports `NilClass`; with no body at all it is
        // `rb_block_lambda`'s missing-block ArgumentError, not an arity
        // mismatch.
        run_tests(&[
            r#"c = Class.new; (c.send(:define_method, :m, "s") rescue [$!.class, $!.message])"#,
            r#"c = Class.new; (c.send(:define_method, :m, 5) rescue [$!.class, $!.message])"#,
            r#"c = Class.new; (c.send(:define_method, :m, nil) rescue [$!.class, $!.message])"#,
            r#"c = Class.new; (c.send(:define_method, :m, true) rescue [$!.class, $!.message])"#,
            r#"c = Class.new; (c.send(:define_method, :m) rescue [$!.class, $!.message])"#,
            // The working forms are untouched.
            r#"c = Class.new; c.send(:define_method, :m) { 7 }; c.new.m"#,
            r#"c = Class.new; c.send(:define_method, :m, proc { 8 }); c.new.m"#,
            r#"c = Class.new { def orig; 9; end }; c.send(:define_method, :m, c.instance_method(:orig)); c.new.m"#,
        ]);
    }
    /// Modules nested in a cycle (`m::N::Back = m`): naming walks stop
    /// where the chain comes round, and a temporary name is the whole
    /// prefix of what nests under it.
    #[test]
    fn module_names_through_a_nesting_cycle() {
        run_test_once(
            r#"
            # (Anonymous prefixes carry addresses, and CRuby before 4.0
            # renders them where 4.0 passes a temporary name down: only
            # the leaf is compared.)
            leaf = ->(mod) { mod.name&.sub(/\A.*::/, "") }
            m = Module.new
            m::N = Module.new
            m::N::Back = m
            r = [leaf.(m::N), leaf.(m::N::Back)]
            m.set_temporary_name "m"
            r << leaf.(m::N) << m::N::Back.name
            a = Module.new
            b = Module.new
            a::B = b
            b::A = a
            r << leaf.(a) << leaf.(b) << leaf.(a::B) << leaf.(b::A)
            Outer = a
            r << a.name << b.name << a::B.name << b::A.name
            r
            "#,
        );
    }

    #[test]
    fn alias_method_invalid_symbol_name() {
        run_test_once(
            r#"
            c = Class.new { def m = 1 }
            bad = (+"\xFF").force_encoding(Encoding::UTF_8)
            o = Object.new
            o.define_singleton_method(:to_str) { bad }
            [(c.send(:alias_method, bad, :m) rescue [$!.class, $!.message]),
             (c.send(:alias_method, o, :m) rescue [$!.class, $!.message])]
            "#,
        );
    }

    /// `const_get` / `const_defined?` with a String path while the Ruby
    /// the walk runs for a segment — an autoload, a `const_missing`, each
    /// running `GC.start` here — collects. A segment's Symbol is looked
    /// for, and its String or "wrong constant name" NameError made, only
    /// when the walk reaches it, so nothing made before is freed under
    /// it, and a name the autoloaded file defined is found; the String a
    /// `#to_str` answered stays reachable for the messages.
    #[test]
    fn const_path_walk_survives_collections() {
        run_test_once(
            r##"
            r = []
            paths = [1, 2].map { "/tmp/monoruby_const_path_gc_#{_1}_#{Process.pid}.rb" }
            File.write(paths[0], <<~RUBY)
              module CgGcFoo
                GC.start
                $cg_junk = Array.new(3000) { "y" * 40 }
                class Qx7Widget; end
              end
            RUBY
            File.write(paths[1], <<~RUBY)
              module CgGcBar
                GC.start
                $cg_junk = Array.new(3000) { "y" * 40 }
                class Qx8Gadget; end
              end
            RUBY
            autoload :CgGcFoo, paths[0]
            autoload :CgGcBar, paths[1]
            class CgGcHost
              def self.const_missing(n)
                GC.start
                $cg_junk = Array.new(3000) { "y" * 40 }
                case n.to_s
                when /\ACgGcBaz/ then const_set(n, Module.new)
                when "CgGcNum" then 1
                else super
                end
              end
            end
            class CgGcPath
              def initialize(*parts) = @parts = parts
              def to_str = @parts.join
            end
            r << Object.const_get("CgGcFoo::" + "Qx7" + "Widget").name
            r << Object.const_defined?("CgGcBar::" + "Qx8" + "Gadget")
            ["CgGcBaz1::bad_name", "CgGcBaz2::" + "Zq9" + "Thing",
             CgGcPath.new("CgGc", "Baz3::"), CgGcPath.new("CgGc", "Num::X")].each do |name|
              r << CgGcHost.const_get(name)
            rescue NameError, TypeError => e
              r << [e.class, e.message]
            end
            paths.each { File.delete(_1) }
            r
            "##,
        );
    }
}
