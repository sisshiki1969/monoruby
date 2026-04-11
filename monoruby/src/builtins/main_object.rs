use super::*;

//
// main object
//

pub(super) fn init(globals: &mut Globals) {
    let main = globals.main_object;
    globals.define_builtin_singleton_func_with(main, "include", include, 0, 0, true);
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
        let proc = vm.generate_proc(bh, pc)?;
        globals.define_proc_method(proc)
    } else {
        return Err(MonorubyErr::wrong_number_of_arg(2, 1));
    };
    vm.add_private_method(globals, class_id, name, func_id)?;
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
}
