use super::*;

//
// main object
//

pub(super) fn init(globals: &mut Globals) {
    let main = globals.main_object;
    globals.define_builtin_singleton_func_with(main, "include", include, 0, 0, true);
    // CRuby exposes `public` / `private` (and `protected`) as
    // singleton methods of `main` that delegate to the Object class
    // — the receiver of top-level `def`. Names are coerced via
    // Symbol/String, an Array argument is accepted, and the result is
    // the original argument value (per spec parity with `Module#public`).
    globals.define_builtin_singleton_func_with(main, "public", main_public, 0, 0, true);
    globals.define_builtin_singleton_func_with(main, "private", main_private, 0, 0, true);
    // Refinements aren't supported by monoruby; `using` is provided
    // as a stub that argument-checks (ArgumentError on no-args,
    // TypeError on non-Module) and otherwise no-ops.
    globals.define_builtin_singleton_func(main, "using", main_using, 1);
    // CRuby's main object responds to a private `ruby2_keywords`
    // (no-op). monoruby doesn't propagate keyword splat metadata, so
    // we register a private no-op method that swallows its args.
    let main_singleton_id = globals.store.get_singleton(main).unwrap().id();
    let func_id = globals.define_builtin_func_with(
        main_singleton_id,
        "ruby2_keywords",
        main_ruby2_keywords,
        0,
        0,
        true,
    );
    globals
        .change_method_visibility_for_class(
            main_singleton_id,
            &[IdentId::get_id("ruby2_keywords")],
            Visibility::Private,
        )
        .unwrap();
    let _ = func_id;
    let singleton_class_id = globals.store.get_singleton(main).unwrap().id();
    globals.define_builtin_funcs_with_effect(
        singleton_class_id,
        "define_method",
        &[],
        main_define_method,
        1,
        2,
        false,
        Effect::CAPTURE,
    );
}

///
/// ### main.include
///
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let mut class = globals.store.object_class();
    for v in args.iter().cloned().rev() {
        class.include_module(v.expect_module(globals)?)?;
    }
    Ok(class.as_val())
}

///
/// ### main#public
///
/// - public(*name) -> *name
/// - public(names) -> names
///
/// Set the visibility of named methods to public on Object (the
/// implicit receiver of top-level `def`). Mirrors CRuby's main-object
/// `public` so `public :foo` at the top level reopens Object.
#[monoruby_builtin]
fn main_public(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0).as_array();
    let object = globals.store.object_class().as_val();
    super::module::change_visi(vm, globals, object, arg, Visibility::Public)
}

///
/// ### main#private
///
/// - private(*name) -> *name
/// - private(names) -> names
///
/// Counterpart of `main#public` — sets named methods on Object to
/// private. With no argument, raises (top-level visibility-default
/// switching is handled by the parser, not this method).
#[monoruby_builtin]
fn main_private(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0).as_array();
    let object = globals.store.object_class().as_val();
    super::module::change_visi(vm, globals, object, arg, Visibility::Private)
}

///
/// ### main.using
///
/// - using(mod) -> self
///
/// Stub for refinement support. monoruby does not implement
/// refinements, so this validates the argument (must be a Module)
/// and otherwise no-ops. `using` with no argument raises ArgumentError
/// via the standard arity check.
#[monoruby_builtin]
fn main_using(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_class_or_module().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Module)",
            arg.get_real_class_name(globals)
        )));
    }
    Ok(lfp.self_val())
}

///
/// ### main.ruby2_keywords
///
/// - ruby2_keywords(*method_names) -> nil
///
/// CRuby uses this to flag a method as a ruby2_keywords-style
/// keyword-passthrough method. monoruby doesn't propagate keyword
/// splat metadata, so this is a private no-op — the spec only asserts
/// that `main` responds to a private `ruby2_keywords` method.
#[monoruby_builtin]
fn main_ruby2_keywords(
    _: &mut Executor,
    _: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### main.define_method
///
/// - define_method(name, method) -> Symbol
/// - define_method(name) { ... } -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/main/s/define_method.html]
#[monoruby_builtin]
fn main_define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let class_id = OBJECT_CLASS;
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
    // Top-level `define_method` adds a *public* method on Object,
    // unlike top-level `def` which is private. Matches CRuby.
    vm.add_public_method(globals, class_id, name, func_id)?;
    Ok(Value::symbol(name))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn include() {
        run_test(
            r#"
            include Math
        "#,
        );
    }

    #[test]
    fn define_method_toplevel() {
        run_test(
            r#"
            define_method(:foo) { 42 }
            foo
        "#,
        );
        run_test(
            r#"
            define_method(:bar) { |x| x * 2 }
            bar(21)
        "#,
        );
    }

    #[test]
    fn define_method_with_proc() {
        run_test(
            r#"
            p = Proc.new { 100 }
            define_method(:foo, p)
            foo
        "#,
        );
        run_test(
            r#"
            p = Proc.new { |x, y| x + y }
            define_method(:bar, p)
            bar(3, 4)
        "#,
        );
        run_test(
            r#"
            p = lambda { |x| x * 3 }
            define_method(:baz, p)
            baz(5)
        "#,
        );
    }

    #[test]
    fn define_method_with_method() {
        run_test(
            r#"
            def greet; "hello"; end
            m = method(:greet)
            define_method(:greet2, m)
            greet2
        "#,
        );
        run_test(
            r#"
            def add(x, y); x + y; end
            m = method(:add)
            define_method(:add2, m)
            add2(10, 20)
        "#,
        );
    }

    #[test]
    fn define_method_with_unbound_method() {
        run_test(
            r#"
            def calc(x); x * 10; end
            um = Object.instance_method(:calc)
            define_method(:calc2, um)
            calc2(7)
        "#,
        );
    }

    #[test]
    fn define_method_creates_public_method() {
        // Top-level `define_method` adds a *public* method on Object
        // (top-level `def` would be private). Matches CRuby.
        run_test(
            r#"
            define_method(:boom) { :bam }
            Object.method_defined?(:boom)
            "#,
        );
        run_test(
            r#"
            define_method(:boom2) { :bam2 }
            Object.private_method_defined?(:boom2)
            "#,
        );
    }

    #[test]
    fn main_public() {
        // Single Symbol arg flips a top-level def to public.
        run_test(
            r#"
            def foo; end
            public :foo
            Object.private_method_defined?(:foo)
            "#,
        );
        // Multiple args.
        run_test(
            r#"
            def foo; end
            def bar; end
            public :foo, :bar
            [Object.private_method_defined?(:foo),
             Object.private_method_defined?(:bar)]
            "#,
        );
        // Single Array arg.
        run_test(
            r#"
            def foo; end
            def bar; end
            public [:foo, :bar]
            [Object.private_method_defined?(:foo),
             Object.private_method_defined?(:bar)]
            "#,
        );
    }

    #[test]
    fn main_private() {
        // public method -> private.
        run_test(
            r#"
            def foo; end
            public :foo
            private :foo
            Object.private_method_defined?(:foo)
            "#,
        );
        // Multiple args + Array form.
        run_test(
            r#"
            def foo; end
            def bar; end
            public :foo, :bar
            private [:foo, :bar]
            [Object.private_method_defined?(:foo),
             Object.private_method_defined?(:bar)]
            "#,
        );
        // Unknown name -> NameError.
        run_test_error(r#"private :main_undefined_method_xyz"#);
    }

    #[test]
    fn main_using_argchecks() {
        // No-arg form raises (arity).
        run_test_error(r#"using"#);
        // Non-Module arg -> TypeError.
        run_test_error(r#"using "foo""#);
        // Module arg accepted (no-op).
        run_test(r#"using Math; :ok"#);
    }

    #[test]
    fn binding_receiver() {
        // Binding#receiver returns self at the binding's frame.
        run_test(r#"binding.receiver == self"#);
        run_test(r#"TOPLEVEL_BINDING.receiver.equal?(self)"#);
        run_test(
            r#"
            class Foo
              def bind; binding; end
            end
            Foo.new.bind.receiver.is_a?(Foo)
            "#,
        );
    }
}
