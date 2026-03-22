use super::*;
use jitgen::JitContext;

//
// BasicObject class and Object class
//

pub(super) fn init(globals: &mut Globals) {
    // BasicObject methods

    globals.define_private_builtin_func(BASIC_OBJECT_CLASS, "initialize", bo_initialize, 0);
    globals.define_builtin_inline_func(
        BASIC_OBJECT_CLASS,
        "__id__",
        object_id,
        Box::new(object_object_id),
        0,
    );
    globals.define_builtin_func(BASIC_OBJECT_CLASS, "==", eq, 1);
    globals.define_builtin_func(BASIC_OBJECT_CLASS, "equal?", eq, 1);
    globals.define_builtin_inline_func(BASIC_OBJECT_CLASS, "!", not_, Box::new(object_not), 0);
    globals.define_builtin_func(BASIC_OBJECT_CLASS, "!=", ne, 1);
    globals.define_builtin_funcs_with_effect(
        BASIC_OBJECT_CLASS,
        "instance_eval",
        &[],
        instance_eval,
        0,
        0,
        true,
        Effect::EVAL,
    );
    globals.define_builtin_funcs_with_effect(
        BASIC_OBJECT_CLASS,
        "instance_exec",
        &[],
        instance_exec,
        0,
        0,
        true,
        Effect::EVAL,
    );

    globals.define_builtin_inline_funcs_with_kw(
        BASIC_OBJECT_CLASS,
        "__send__",
        &[],
        crate::builtins::send,
        Box::new(crate::builtins::object_send),
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_private_builtin_func_rest(
        BASIC_OBJECT_CLASS,
        "method_missing",
        bo_method_missing,
    );
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_added",
        bo_noop_hook,
        1,
    );
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_removed",
        bo_noop_hook,
        1,
    );
    globals.define_private_builtin_func(
        BASIC_OBJECT_CLASS,
        "singleton_method_undefined",
        bo_noop_hook,
        1,
    );

}

///
/// ### BasicObject#initialize
///
#[monoruby_builtin]
fn bo_initialize(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

/// No-op hook for singleton_method_added/removed/undefined on BasicObject.
#[monoruby_builtin]
fn bo_noop_hook(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### BasicObject#method_missing
///
#[monoruby_builtin]
fn bo_method_missing(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let name = args[0].expect_symbol_or_string(globals)?;
    Err(MonorubyErr::method_not_found(
        &globals.store,
        name,
        lfp.self_val(),
    ))
}

///
/// ### BasicObject#!
///
/// - !obj -> bool
///
/// Returns the boolean negation of the object.
///
#[monoruby_builtin]
fn not_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(!lfp.self_val().as_bool()))
}

fn object_not(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_truthy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(false));
        }
    } else if state.is_falsy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(true));
        }
    } else {
        state.load(ir, recv, GP::Rdi);
        ir.inline(|r#gen, _, _| {
            monoasm! { &mut r#gen.jit,
                orq  rdi, (0x10);
                movq rax, (TRUE_VALUE);
                movq rsi, (FALSE_VALUE);
                cmpq rdi, (FALSE_VALUE);
                cmovneq rax, rsi;
            }
        });
        state.def_rax2acc(ir, dst);
    }
    true
}

#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() == lfp.arg(0).id()))
}

#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    let res = vm.invoke_method_inner(globals, IdentId::_EQ, self_val, &[other], None, None)?;
    Ok(Value::bool(!res.as_bool()))
}

///
/// ### BasicObject#__id__
///
/// - object_id -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/object_id.html]
#[monoruby_builtin]
pub(super) fn object_id(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().id() as i64))
}

pub(super) fn object_object_id(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst: ret, .. } = *callsite;
    state.load(ir, recv, GP::Rdi);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm! {&mut r#gen.jit,
            movq rax, (crate::executor::op::i64_to_value);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    state.def_rax2acc(ir, ret);
    true
}

///
/// Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
#[monoruby_builtin]
fn object_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::object(class);
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
    )?;
    Ok(obj)
}

///
/// ### BasicObject#instance_exec
///
/// - instance_exec(*args) {|*args| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicObject/i/instance_exec.html]
#[monoruby_builtin]
fn instance_exec(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
    let args = lfp.arg(0).as_array();
    vm.push_instance_eval_context(self_val);
    let res = vm.invoke_block_with_self(globals, &data, self_val, &args);
    vm.pop_class_context();
    res
}

/// ### BasicObject#instance_eval
///
/// - instance_eval(expr, filename = "(eval)", lineno = 1) -> object
/// - instance_eval {|obj| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicObject/i/instance_eval.html]
#[monoruby_builtin]
fn instance_eval(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    vm.push_instance_eval_context(self_val);
    let res = instance_eval_inner(vm, globals, lfp, self_val);
    vm.pop_class_context();
    res
}

fn instance_eval_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    self_val: Value,
) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let argc = args.len();
    if let Some(bh) = lfp.block() {
        if argc > 0 {
            return Err(MonorubyErr::wrong_number_of_arg(0, argc));
        }
        let data = vm.get_block_data(globals, bh)?;
        vm.invoke_block_with_self(globals, &data, self_val, &[self_val])
    } else if argc >= 1 && argc <= 3 {
        let expr = args[0].coerce_to_str(vm, globals)?;
        let cfp = vm.cfp();
        let caller_cfp = cfp.prev().unwrap();
        let path = if argc >= 2 {
            args[1].coerce_to_str(vm, globals)?
        } else {
            "(eval)".into()
        };
        let fid = globals.compile_script_eval(expr, path, caller_cfp, Some(self_val.class()))?;
        let proc = ProcData::new(caller_cfp.lfp(), fid);
        vm.invoke_block_with_self(globals, &proc, self_val, &[])
    } else {
        Err(MonorubyErr::wrong_number_of_arg_range(argc, 1..=3))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn cmp() {
        let v = &["true", "false", "nil", "10", ":true"];
        run_binop_tests2(v, &["==", "!="], v);
    }

    #[test]
    fn equal() {
        run_test(r##"100.equal?(100)"##);
        run_test(r##"100.equal?(100.0)"##);
        run_test(r##""a".equal?("a")"##);
        run_test(
            r##"
        a = "a"
        b = a
        a.equal?(b)"##,
        );
    }

    #[test]
    fn instance_eval() {
        run_test_with_prelude(
            r#"
        some = Foo.new 'XXX'
        res = []
        res << some.instance_eval { @key} #=> "XXX"
        res << some.instance_eval { do_fuga } #=> "secret" # private メソッドも呼び出せる
        res
        "#,
            r#"
        class Foo
          def initialize data
            @key = data
          end
          private
          def do_fuga
            'secret'
          end
        end
        "#,
        );
        // constant lookup in instance_eval with string
        run_test_with_prelude(
            r#"
        some = Foo.new
        some.instance_eval("BAR")
        "#,
            r#"
        class Foo
          BAR = 42
        end
        "#,
        );
        // constant lookup in instance_eval with string (caller's lexical scope)
        run_test(
            r#"
        class A
          X = 100
          def test
            instance_eval("X")
          end
        end
        A.new.test
        "#,
        );
    }

    #[test]
    fn instance_exec() {
        run_test_with_prelude(
            r#"
        some = Foo.new('hello')
        res = []
        res << some.instance_exec { @key }
        res << some.instance_exec(10) { |x| @key * x }
        res
        "#,
            r#"
        class Foo
          def initialize(data)
            @key = data
          end
        end
        "#,
        );
        run_test(
            r#"
        class Foo
          def initialize
            @x = 42
          end
        end
        Foo.new.instance_exec { @x }
        "#,
        );
        run_test(
            r#"
        class Foo
          private
          def secret; "secret"; end
        end
        Foo.new.instance_exec { secret }
        "#,
        );
        run_test(
            r#"
        1.instance_exec(2, 3) { |a, b| self + a + b }
        "#,
        );
    }

    #[test]
    fn initialize() {
        // default initialize returns the new object
        run_test("Object.new.class");
        run_test("Object.new.is_a?(Object)");
        // custom initialize sets instance variables
        run_test(
            r#"
            class Foo
              def initialize(x)
                @x = x
              end
              def x; @x; end
            end
            Foo.new(42).x
            "#,
        );
        // super in initialize
        run_test(
            r#"
            class A
              def initialize
                @a = 1
              end
            end
            class B < A
              def initialize
                super
                @b = 2
              end
              def vals; [@a, @b]; end
            end
            B.new.vals
            "#,
        );
        // initialize with block
        run_test(
            r#"
            class C
              def initialize(&blk)
                @v = blk.call
              end
              def v; @v; end
            end
            C.new { 99 }.v
            "#,
        );
    }

    #[test]
    fn method_missing_private() {
        // private method_missing should still be dispatched
        run_test(
            r#"
            class Foo
              private
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.hello
            "#,
        );
        // arguments should be forwarded in correct order
        run_test(
            r#"
            class Foo
              private
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.bar(1, 2)
            "#,
        );
        // public method_missing should also work
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                "mm:#{name}:#{args}"
              end
            end
            Foo.new.baz(10, 20, 30)
            "#,
        );
    }

    #[test]
    fn method_missing_splat() {
        // splat arguments should be expanded and forwarded
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = [1, 2, 3]
            Foo.new.bar(*a)
            "#,
        );
        // empty splat
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = []
            Foo.new.bar(*a)
            "#,
        );
        // splat mixed with normal args
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            a = [2, 3]
            Foo.new.bar(1, *a, 4)
            "#,
        );
    }

    #[test]
    fn method_missing_hash_splat() {
        // hash splat should be merged into keyword hash
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            h = {a: 1, b: 2}
            Foo.new.bar(**h)
            "#,
        );
        // keyword args combined with hash splat
        run_test(
            r#"
            class Foo
              def method_missing(name, *args)
                [name, args]
              end
            end
            h = {b: 2}
            Foo.new.bar(a: 1, **h)
            "#,
        );
    }

    #[test]
    fn method_missing_with_block() {
        // &blk parameter
        run_test_once(
            r#"
            class Foo
              def method_missing(name, *args, &blk)
                [name, args, blk ? blk.call : nil]
              end
            end
            Foo.new.bar(1, 2) { 42 }
            "#,
        );
        // block with yield
        run_test_once(
            r#"
            class Foo
              def method_missing(name, *args)
                yield(*args) if block_given?
              end
            end
            Foo.new.bar(3, 4) { |a, b| a + b }
            "#,
        );
        // no block passed
        run_test(
            r#"
            class Foo
              def method_missing(name, *args, &blk)
                [name, blk.nil?]
              end
            end
            Foo.new.baz
            "#,
        );
    }
}
