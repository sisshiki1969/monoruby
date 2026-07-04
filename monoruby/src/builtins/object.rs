use super::*;
#[cfg(target_arch = "x86_64")]
use jitgen::JitContext;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};

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
        inline_gen2!(object_object_id),
        0,
    );
    // `equal?` is an alias of `==` on BasicObject (they share one method
    // entry/FuncId), so `BasicObject.instance_method(:equal?) ==
    // BasicObject.instance_method(:==)` holds — see ruby/spec
    // core/basicobject/equal_spec.rb.
    globals.define_builtin_funcs(BASIC_OBJECT_CLASS, "==", &["equal?"], eq, 1);
    // CRuby defines `===` on Kernel/Object, NOT on BasicObject. Keeping
    // it on BasicObject would trap `===` for BasicObject subclasses (like
    // mspec's `SpecPositiveOperatorMatcher`) instead of routing through
    // `method_missing`, breaking the `actual.should === expected` idiom.
    globals.define_builtin_func(OBJECT_CLASS, "===", case_eq, 1);
    globals.define_builtin_inline_func(BASIC_OBJECT_CLASS, "!", not_, inline_gen2!(object_not), 0);
    let neq_fid = globals.define_builtin_func(BASIC_OBJECT_CLASS, "!=", ne, 1);
    globals.store.set_default_neq(neq_fid);
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
        inline_gen!(crate::builtins::object_send),
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(OBJECT_CLASS, "freeze", freeze, 0);
    globals.define_builtin_func(OBJECT_CLASS, "frozen?", frozen, 0);

    globals.define_private_builtin_func_rest(
        BASIC_OBJECT_CLASS,
        "method_missing",
        bo_method_missing,
    );
    // hook methods (no-op by default, overridden by startup.rb)
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

/// No-op hook for singleton_method_added/removed/undefined (overridden by startup.rb).
#[monoruby_builtin]
fn bo_noop_hook(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### Object#freeze
///
/// - freeze -> self
///
/// Freezes the object, preventing further modification.
///
#[monoruby_builtin]
fn freeze(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    if !self_val.is_packed_value() {
        self_val.set_frozen();
        // Freezing an object also freezes its singleton class, if one has
        // already been created (CRuby "Frozen properties").
        if let Some(singleton) = globals.store.has_singleton(self_val) {
            singleton.as_val().set_frozen();
        }
    }
    Ok(self_val)
}

///
/// ### Object#frozen?
///
/// - frozen? -> bool
///
/// Returns true if the object is frozen.
///
#[monoruby_builtin]
fn frozen(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().is_frozen()))
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
    let recv = lfp.self_val();
    // Reaching the default `method_missing` while the method actually
    // exists means the call violated visibility (a private method called
    // with an explicit receiver, or an inaccessible protected method).
    // CRuby reports these as "private/protected method … called" rather
    // than "undefined method".
    use crate::executor::Visibility;
    let err = match globals.store.check_method_for_class(recv.class(), name) {
        Some(entry) if entry.func_id().is_some() => match entry.visibility() {
            Visibility::Private => MonorubyErr::private_method_called(&globals.store, name, recv),
            Visibility::Protected => {
                MonorubyErr::protected_method_called(&globals.store, name, recv)
            }
            _ => MonorubyErr::method_not_found(&globals.store, name, recv),
        },
        _ => MonorubyErr::method_not_found(&globals.store, name, recv),
    };
    Err(err)
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
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_truthy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(false));
        }
    } else if state.is_falsy(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Immediate::bool(true));
        }
    } else {
        state.load(ir, recv, GP::Rdi);
        // Pure-LIR predicate (no arch-specific closure): Rax = !Rdi.
        ir.not_to_bool(GP::Rax, GP::Rdi);
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
    // `eq_values_bool` is the optimized `==` dispatch (builtin fast paths
    // first, full `invoke_eq` dispatch otherwise) — the same policy
    // `ne_values_bool` applies when `!=` resolves to this default. Going
    // through it avoids a global method-cache probe of `==` per call.
    let res = vm.eq_values_bool(globals, self_val, other)?;
    Ok(Value::bool(!res))
}

/// Default `Object#===` — delegates to `==` so that subclasses which
/// override `==` get consistent `case` behaviour.
#[monoruby_builtin]
fn case_eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    vm.invoke_method_inner(globals, IdentId::_EQ, self_val, &[other], None, None)
}

///
/// ### BasicObject#__id__
///
/// - object_id -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/object_id.html]
#[monoruby_builtin]
pub(super) fn object_id(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().id() as i64))
}

pub(super) fn object_object_id(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst: ret, .. } = *callsite;
    state.load(ir, recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_object_id());
    ir.fpr_restore(using_fpr);
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
    pc: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    vm.push_instance_eval_context(self_val);
    let res = instance_eval_inner(vm, globals, lfp, self_val, pc);
    vm.pop_class_context();
    res
}

fn instance_eval_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    self_val: Value,
    pc: BytecodePtr,
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
            let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
            format!("(eval at {})", caller_loc)
        };
        let lineno: i64 = if argc >= 3 {
            args[2].coerce_to_int_i64(vm, globals)?
        } else {
            1
        };
        let fid =
            globals.compile_script_eval(expr, path, caller_cfp, Some(self_val.class()), lineno)?;
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
    fn cmp_dispatch_preserves_return_value() {
        // A custom `==` / `!=` may return an arbitrary value; the binop
        // must evaluate to it untouched (CRuby semantics). Reverse
        // coercion dispatches (`1 == obj`) stay boolean-coerced, and the
        // default `!=` is the boolean negation of `==`.
        run_test_once(
            r##"
        class CustomEq; def ==(o); "EQ-RAW"; end; end
        class CustomNe; def ==(o); true; end; def !=(o); "NE-RAW"; end; end
        e = CustomEq.new
        n = CustomNe.new
        res = [e == 1, e != 1, n != 1, n == 1, 1 == e, "a" == e]
        30.times { res << (e == 1) << (n != 1) }
        res
        "##,
        );
    }

    #[test]
    fn cmp_dispatch_raw_in_bop_redefined_mode() {
        // Overwriting a basic op (Integer#!=) trips set_bop_redefine and
        // flips the VM to the *_no_opt cmp handlers; those must also
        // dispatch ==/!= raw (and honor the redefined basic op itself).
        run_test_once(
            r##"
        class Integer; def !=(o); "INT-NE"; end; end
        class CustomEqB; def ==(o); "RAW-B"; end; end
        e = CustomEqB.new
        [1 != 2, e == 1, e != 1, "a" == "a", "a" != "b", nil == nil, nil != 1]
        "##,
        );
    }

    #[test]
    fn neq_custom_redefinition() {
        // Exercises the version-stamped `custom_neq` memo: `!=` on a class
        // computes `!(a == b)` while it resolves to the default, and a
        // later `!=` redefinition must be picked up.
        run_test_once(
            r##"
        res = []
        class FooNeq
          def ==(o); true; end
        end
        f = FooNeq.new
        30.times { res << (f != 1) << (f != nil) }
        class FooNeq
          def !=(o); o == 99; end
        end
        res << (f != 99) << (f != 1)
        res
        "##,
        );
    }

    #[test]
    fn cmp() {
        let v = &["true", "false", "nil", "10", ":true"];
        run_binop_tests2(v, &["==", "!="], v);
    }

    #[test]
    fn numeric_frozen() {
        run_tests(&[
            r##"[(2**70).frozen?, (1+2i).frozen?, Rational(1,3).frozen?, 0.1.frozen?, 5.frozen?]"##,
            r##"x = 2**80; x.freeze; x.frozen?"##,
        ]);
    }

    #[test]
    fn object_spaceship() {
        run_test(
            r##"
        o = Object.new
        [o <=> o, (o <=> Object.new).inspect, (o <=> 3.14).inspect]
        "##,
        );
        run_test_once(
            r##"
        m = Object.new
        def m.==(x); true; end
        m <=> Object.new
        "##,
        );
    }

    #[test]
    fn equal() {
        run_tests(&[
            r##"100.equal?(100)"##,
            r##"100.equal?(100.0)"##,
            r##""a".equal?("a")"##,
            r##"
        a = "a"
        b = a
        a.equal?(b)"##,
        ]);
    }

    #[test]
    fn basicobject_equal_is_alias_of_eq() {
        // ruby/spec core/basicobject/equal_spec.rb: `equal?` must be the same
        // method entry as `==` on BasicObject.
        run_test("BasicObject.instance_method(:equal?) == BasicObject.instance_method(:==)");
        run_test("BasicObject.public_instance_methods(false).include?(:equal?)");
    }

    #[test]
    fn case_eq() {
        run_test(
            r#"
            class Foo
              def ==(other)
                other == 42
              end
            end
            Foo.new === 42
            "#,
        );
        run_test("Object.new === Object.new");
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
        run_test_with_prelude(
            r#"
        A.new.test
        "#,
            r#"
        class A
          X = 100
          def test
            instance_eval("X")
          end
        end
        "#,
        );
    }

    #[test]
    fn instance_eval_lineno() {
        // instance_eval with filename and lineno
        run_tests(&[
            r#"instance_eval("__LINE__", "test.rb", 42)"#,
            r#"instance_eval("__FILE__", "test.rb", 42)"#,
            r#"instance_eval("__LINE__\n__LINE__", "test.rb", 10)"#,
        ]);
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
        run_tests(&["Object.new.class", "Object.new.is_a?(Object)"]);
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
