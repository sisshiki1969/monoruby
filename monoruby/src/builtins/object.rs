use super::*;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    //globals.define_builtin_class_func(OBJECT_CLASS, "new", object_new, -1);

    globals.define_builtin_func(OBJECT_CLASS, "==", eq, 1);
    globals.define_builtin_func(OBJECT_CLASS, "!=", ne, 1);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "dup", dup, 0);
    globals.define_builtin_funcs_rest(OBJECT_CLASS, "enum_for", &["to_enum"], to_enum);
    globals.define_builtin_func(OBJECT_CLASS, "equal?", equal_, 1);
    globals.define_builtin_func_rest(OBJECT_CLASS, "extend", extend);
    globals.define_builtin_func(OBJECT_CLASS, "kind_of?", is_a, 1);
    globals.define_builtin_inline_func(
        OBJECT_CLASS,
        "object_id",
        object_id,
        Box::new(object_object_id),
        0,
    );
    globals.define_builtin_inline_func_with(
        OBJECT_CLASS,
        "respond_to?",
        respond_to,
        Box::new(object_respond_to),
        1,
        2,
        false,
    );
    globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "instance_of?", instance_of, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_defined?", iv_defined, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_set", iv_set, 2);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variable_get", iv_get, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variables", iv, 0);
    globals.define_builtin_func(OBJECT_CLASS, "is_a?", is_a, 1);
    globals.define_builtin_inline_funcs_with(
        OBJECT_CLASS,
        "send",
        &["__send__"],
        crate::builtins::send,
        Box::new(crate::builtins::object_send),
        0,
        0,
        true,
    );
    globals.define_builtin_funcs_eval_with(
        OBJECT_CLASS,
        "instance_eval",
        &[],
        instance_eval,
        0,
        3,
        false,
    );
    globals.define_builtin_func(OBJECT_CLASS, "method", method, 1);
    globals.define_builtin_func_with(OBJECT_CLASS, "methods", methods, 0, 1, false);
    globals.define_builtin_func_with(
        OBJECT_CLASS,
        "singleton_methods",
        singleton_methods,
        0,
        1,
        false,
    );
}

#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() == lfp.arg(0).id()))
}

#[monoruby_builtin]
fn ne(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() != lfp.arg(0).id()))
}

///
/// ### Object#object_id
///
/// - object_id -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/object_id.html]
#[monoruby_builtin]
fn object_id(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().id() as i64))
}

fn object_object_id(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst: ret, .. } = *callsite;
    bb.fetch(ir, recv, GP::Rdi);
    let using_xmm = bb.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |gen, _, _| {
        monoasm! {&mut gen.jit,
            movq rax, (crate::executor::op::i64_to_value);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    bb.rax2acc(ir, ret);
    true
}

///
/// Object#send
///
/// - send(name, *args) -> object
/// - send(name, *args) { .... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/send.html]
#[monoruby_builtin]
pub(crate) fn send(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.arg(0).as_array();
    if ary.len() < 1 {
        return Err(MonorubyErr::wrong_number_of_arg_min(ary.len(), 1));
    }
    let method = ary[0].expect_symbol_or_string(globals)?;
    vm.invoke_method_inner(globals, method, lfp.self_val(), &ary[1..], lfp.block())
}

pub fn object_send(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    let no_splat = !callsite.object_send_single_splat();
    if !callsite.is_simple() && no_splat {
        return false;
    }

    bb.write_back_callargs_and_dst(ir, callsite);
    bb.writeback_acc(ir);
    let using_xmm = bb.get_using_xmm();
    let error = ir.new_error(bb);
    let callid = callsite.id;
    ir.inline(move |gen, store, labels| {
        let error = &labels[error];
        gen.object_send_inline(callid, store, using_xmm, &error, no_splat);
    });
    bb.reg2acc(ir, GP::Rax, callsite.dst);
    true
}

///
/// ### Object#extend
/// - extend(*modules) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/extend.html]
#[monoruby_builtin]
fn extend(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let class = globals.store.get_singleton(lfp.self_val());
    for v in args.iter().cloned().rev() {
        globals.include_module(class, v.expect_module(globals)?)?;
    }
    Ok(lfp.self_val())
}

///
/// ### Object.new
///
/// - new -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/s/new.html]
#[monoruby_builtin]
fn object_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::object(class);
    vm.invoke_method_if_exists(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
    )?;
    Ok(obj)
}

///
/// ### Object#is_a?
///
/// - is_a?(mod) -> bool
/// - kind_of?(mod) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/is_a=3f.html]
#[monoruby_builtin]
fn is_a(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.arg(0).expect_class_or_module(&globals.store)?.id();
    Ok(Value::bool(
        lfp.self_val().is_kind_of(&globals.store, class),
    ))
}

///
/// ### Object#enum_for
///
/// - to_enum(method = :each, *args) -> Enumerator
/// - enum_for(method = :each, *args) -> Enumerator
/// - [NOT SUPPORTED] to_enum(method = :each,  *args) {|*args| ... } -> Enumerator
/// - [NOT SUPPORTED] enum_for(method = :each, *args) {|*args| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/enum_for.html]
#[monoruby_builtin]
fn to_enum(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let args = lfp.arg(0).as_array();
    let (method, args) = if args.is_empty() {
        (IdentId::EACH, vec![])
    } else {
        (
            args[0].expect_symbol_or_string(globals)?,
            args[1..].to_vec(),
        )
    };
    vm.generate_enumerator(method, lfp.self_val(), args)
}

///
/// ### Object#equal
///
/// - equal?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/equal=3f.html]
#[monoruby_builtin]
fn equal_(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().id() == lfp.arg(0).id()))
}

///
/// ### Object#dup
///
/// - dup -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/clone.html]
#[monoruby_builtin]
fn dup(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().dup())
}

///
/// ### Object#to_s
///
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn to_s(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let s = lfp.self_val().to_s(&globals.store);
    Ok(Value::string(s))
}

///
/// ### Object#respond_to?
///
/// - respond_to?(name, include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let include_all = if let Some(arg1) = lfp.try_arg(1) {
        arg1.as_bool()
    } else {
        false
    };
    Ok(Value::bool(if include_all {
        globals.check_method(lfp.self_val(), name).is_some()
    } else {
        globals.check_public_method(lfp.self_val(), name).is_some()
    }))
}

fn object_respond_to(
    bb: &mut BBContext,
    _: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callsite: &CallSiteInfo,
    recv_class: ClassId,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        dst, args, pos_num, ..
    } = *callsite;
    let dst = if let Some(dst) = dst {
        dst
    } else {
        return false;
    };
    let include_all = if pos_num != 1 {
        if bb.is_truthy(args + 1usize) {
            true
        } else if bb.is_falsy(args + 1usize) {
            false
        } else {
            return false;
        }
    } else {
        false
    };
    let method_name = if let Some(name) = bb.is_symbol_literal(args) {
        name
    } else {
        return false;
    };
    let b = if let Some(entry) =
        store.check_method_for_class(recv_class, method_name, ctx.class_version())
    {
        include_all || entry.is_public()
    } else {
        false
    };
    bb.def_concrete_value(dst, Value::bool(b));
    true
}

///
/// ### Object#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let s = lfp.self_val().inspect(&globals.store);
    Ok(Value::string(s))
}

///
/// ### Object#class
///
/// - class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/class.html]
#[monoruby_builtin]
fn class(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().real_class(&globals.store).as_val())
}

///
/// ### Object#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
#[monoruby_builtin]
fn instance_of(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = lfp.self_val().real_class(&globals.store).id()
        == lfp.arg(0).expect_class_or_module(&globals.store)?.id();
    Ok(Value::bool(b))
}

///
/// ### Object#method
///
/// - method(name) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/method.html]
#[monoruby_builtin]
fn method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let receiver = lfp.self_val();
    let method_name = lfp.arg(0).expect_symbol_or_string(globals)?;
    let (func_id, _, owner) = globals.find_method_for_object(receiver, method_name)?;
    Ok(Value::new_method(receiver, func_id, owner))
}

///
/// ### Object#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
#[monoruby_builtin]
fn singleton_class(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().get_singleton(&mut globals.store))
}

///
/// ### BasicObject#instance_eval
///
/// - instance_eval(expr, filename = "(eval)", lineno = 1) -> object
/// - instance_eval {|obj| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicObject/i/instance_eval.html]
#[monoruby_builtin]
fn instance_eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();

    if let Some(bh) = lfp.block() {
        if lfp.try_arg(0).is_some() {
            return Err(MonorubyErr::wrong_number_of_arg(0, lfp.args_count(3)));
        }
        let data = vm.get_block_data(globals, bh)?;
        vm.invoke_block_with_self(globals, &data, self_val, &[self_val])
    } else if let Some(arg0) = lfp.try_arg(0) {
        let expr = arg0.expect_string(globals)?;
        let cfp = vm.cfp();
        let caller_cfp = cfp.prev().unwrap();
        let path = if let Some(arg1) = lfp.try_arg(1) {
            arg1.expect_string(globals)?
        } else {
            "(eval)".into()
        };

        let fid = globals.compile_script_eval(expr, path, caller_cfp)?;
        let proc = ProcData::new(caller_cfp.lfp(), fid);
        vm.invoke_block_with_self(globals, &proc, self_val, &[])
    } else {
        Err(MonorubyErr::wrong_number_of_arg_range(0, 1..=3))
    }
}

///
/// ### Object#methods
///
/// - methods(include_inherited = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/methods.html]
#[monoruby_builtin]
fn methods(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        let class_id = match globals.store.has_singleton(lfp.self_val()) {
            Some(module) => module.id(),
            None => return Ok(Value::array_empty()),
        };
        globals.store.get_method_names(class_id)
    } else {
        let class_id = lfp.self_val().class();
        globals.store.get_method_names_inherit(class_id, true)
    }))
}

///
/// ### Object#singleton_methods
///
/// - singleton_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_methods.html]
#[monoruby_builtin]
fn singleton_methods(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = match globals.store.has_singleton(lfp.self_val()) {
        Some(module) => module.id(),
        None => return Ok(Value::array_empty()),
    };
    let inherited_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    Ok(Value::array_from_vec(if !inherited_too {
        globals.store.get_method_names(class_id)
    } else {
        globals.store.get_method_names_inherit(class_id, true)
    }))
}

///
/// ### Object#instance_variable_defined?
///
/// - instance_variable_defined?(var) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_defined=3f.html]
#[monoruby_builtin]
fn iv_defined(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let id = match lfp.arg(0).unpack() {
        RV::Symbol(sym) => sym,
        RV::String(s) => IdentId::get_id(s.check_utf8()?),
        _ => return Err(MonorubyErr::is_not_symbol_nor_string(globals, lfp.arg(0))),
    };
    let b = globals.store.get_ivar(lfp.self_val(), id).is_some();
    Ok(Value::bool(b))
}

///
/// ### Object#instance_variable_set
///
/// - instance_variable_set(var, value) -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_set.html]
#[monoruby_builtin]
fn iv_set(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let id = lfp.arg(0).expect_symbol_or_string(globals)?;
    let val = lfp.arg(1);
    globals.store.set_ivar(lfp.self_val(), id, val)?;
    Ok(val)
}

///
/// ### Object#instance_variable_get
///
/// - instance_variable_get(var) -> Object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variable_get.html]
#[monoruby_builtin]
fn iv_get(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let id = lfp.arg(0).expect_symbol_or_string(globals)?;
    let v = globals
        .store
        .get_ivar(lfp.self_val(), id)
        .unwrap_or_default();
    Ok(v)
}

///
/// ### Object#instance_variables
///
/// - instance_variables -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_variables.html]
#[monoruby_builtin]
fn iv(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let iter = globals
        .store
        .get_ivars(lfp.self_val())
        .into_iter()
        .map(|(id, _)| Value::symbol(id));
    Ok(Value::array_from_iter(iter))
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
    fn test_builtin() {
        run_test2(":sym.class.to_s");
        run_test2("5.class.to_s");
        run_test2("5.7.class.to_s");
        run_test2("'windows'.class.to_s");
        run_test2("puts 100");
        run_test2("print '100'");
        run_test2("p");
        run_test2("p 1");
        run_test2("p 1,2");
        run_test2("p 1,2,3");
        run_test2("nil.respond_to?(:foo)");
        run_test2("nil.inspect");
        run_test2("Time.singleton_class.to_s");
        run_test2(r#"File.write("/tmp/foo", "woo")"#);
    }

    #[test]
    fn object_id() {
        run_test_with_prelude(
            r##"
            id == a.object_id
            "##,
            r##"
            a = [1,2,3]
            id = a.object_id
            "##,
        );
        run_test_with_prelude(
            r##"
            id == a.object_id
            "##,
            r##"
            a = 1356
            id = a.object_id
            "##,
        );
        run_test_with_prelude(
            r##"
            id == a.object_id
            "##,
            r##"
            a = -49.52
            id = a.object_id
            "##,
        );
    }

    #[test]
    fn extend() {
        run_test_with_prelude(
            r#"
        res = []
        obj = Object.new
        obj.extend Foo, Bar
        res << obj.a #=> "ok Foo"
        res << obj.b #=> "ok Bar"
        res << Klass.new.a #=> "ok Foo"
        res << Klass.b     #=> "ok Bar"
        res
        "#,
            r#"
        module Foo
          def a
            'ok Foo'
          end
        end

        module Bar
          def b
            'ok Bar'
          end
        end

        class Klass
          include Foo
          extend Bar
        end
        "#,
        );
    }

    #[test]
    fn dup() {
        run_test("1.dup");
        run_test("1.5.dup");
        run_test("'Ruby'.dup");
        run_test(":Ruby.dup");
        run_test("[1,2,3].dup");
        run_test("{a:1,b:2}.dup");
        run_test("(1..3).dup");
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
    fn block_given() {
        run_test_with_prelude(
            r##"
            [check, check {}]
        "##,
            r##"
        def check
            if block_given?
                "Block is given."
            else
                "Block isn't given."
            end
        end
        "##,
        );
    }

    #[test]
    fn test_object() {
        run_test2(r#"a=Object.new; a.instance_variable_set("@i", 42)"#);
        run_test2(r#"a=Object.new; a.instance_variable_get(:@i)"#);
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@i)"#,
        );
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@j)"#,
        );
        run_test2(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_get(:@i)"#,
        );
        // TDDO: Object#instace_variables must return Array of Symbols,
        // ordered by the time of definition of the instance variables.
        run_test2(
            r#"
            a=Object.new;
            a.instance_variable_set("@b", 1);
            a.instance_variable_set("@e", 4);
            a.instance_variable_set("@c", 2);
            a.instance_variable_set("@j", 9);
            a.instance_variable_set("@d", 3);
            a.instance_variable_set("@a", 0);
            a.instance_variable_set("@g", 6);
            a.instance_variable_set("@l", 11);
            a.instance_variable_set("@h", 7);
            a.instance_variable_set("@i", 8);
            a.instance_variable_set("@f", 5);
            a.instance_variable_set("@k", 10);
            a.instance_variables.sort
            "#,
        );
    }

    #[test]
    fn respond_to() {
        run_test_with_prelude(
            r#"
        list = [F.new, D.new]
        res = []
        list.each{|it| res << it.hello if it.respond_to?(:hello)}
        list.each{|it| res << it.hello if it.respond_to?(:hello, false)}
        list.each{|it| it.instance_eval("res << hello if it.respond_to?(:hello, true)")}
        res
        "#,
            r#"
        class F
          def hello
            "Bonjour"
          end
        end

        class D
          private
          def hello
            "Guten Tag"
          end
        end
        "#,
        );
    }

    #[test]
    fn kernel_integer() {
        run_test2(r#"Integer(-2435)"#);
        run_test2(r#"Integer("2435")"#);
        run_test_error(r#"Integer("2435.78")"#);
        run_test_error(r#"Integer([4])"#);
        run_test2(r#"Integer(-2435766756886769978978435)"#);
        run_test2(r#"Integer(2435.4556787)"#);
    }

    #[test]
    fn object_instance_of() {
        run_test2(r#"5.instance_of?(Integer)"#);
        run_test2(r#"5.instance_of?(Float)"#);
        run_test2(r#":ruby.instance_of?(Symbol)"#);
        run_test2(r#":ruby.instance_of?(Object)"#);
        run_test2(
            r#"
        class C < Object
        end
        class S < C
        end

        obj = S.new
        [obj.instance_of?(S), obj.instance_of?(C)]
        "#,
        );
        run_test_error(r#"5.instance_of?(7)"#);
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
    }

    #[test]
    fn object_isa() {
        run_test("4.is_a? Integer");
        run_test("4.5.is_a? Integer");
        run_test("'Ruby'.is_a? Integer");
        run_test("4.5.is_a? Float");
        run_test("'Ruby'.is_a? Float");
        run_test(
            r#"
        class C
        end
        C.new.is_a? C"#,
        );
        run_test(
            r#"
        class S
        end
        class C < S
        end
        c = C.new
        [c.is_a?(S), c.is_a?(C)]"#,
        );
    }

    #[test]
    fn object_nil() {
        run_test("4.nil?");
        run_test("4.5.nil?");
        run_test("nil.nil?");
        run_test("true.nil?");
        run_test("false.nil?");
        run_test("[].nil?");
    }

    #[test]
    #[ignore]
    fn kernel_system() {
        run_test(r#"system "ls""#);
        run_test(r#"system "jkjkjk""#);
        run_test(r#"system "*""#);
        run_test(r#"`pwd`"#);
        run_test(r#"`*`"#);
        run_test_error(r#"``"#);
    }

    #[test]
    fn to_enum() {
        run_test(
            r#"
        e = [1,2,3,4,5].to_enum
        e.next
        "#,
        );
    }

    #[test]
    fn send() {
        run_test(
            r#"
        "ruby".send(:sub, /./, "R")
        "#,
        );
        run_test(
            r#"
        class Foo
            def foo() "foo" end
            def bar() "bar" end
            def baz() "baz" end
        end
        methods = {1 => :foo, 2 => :bar, 3 => :baz}
        f = Foo.new
        [f.send(methods[1]), f.send(methods[2]), f.send(methods[3])]
        "#,
        );
        run_test_with_prelude(
            r#"
            o.send(:f){ 3 }
            "#,
            r#"
            class C
                def f
                    yield
                end
            end
            o = C.new
            "#,
        );
    }

    #[test]
    fn object_send() {
        run_test_with_prelude(
            r##"
        o = C.new
        [o.send(:foo), o.send("foo"), o.send(:bar, 20), o.send(*[:foo]), o.send(*["bar", 100])]
        "##,
            r##"
        class C
            def foo
                1
            end
            def bar(x)
                x
            end
        end
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(200, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(:foo, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(*[])
        "##,
        );
        run_test_error(
            r##"
          class C
            def b(x, y, z)
              puts "x=#{[x, y, z]}"
            end
          end
          
          o = C.new
          30.times do |x|
            if x == 28 then
              eval('class C; def b(x); puts "x=#{x}" ;end; end')
            end
            o.send(:b, x, 2, 3)
          end 
            "##,
        );
    }

    #[test]
    fn methods() {
        run_test_with_prelude(
            r##"
        [
          obj.singleton_methods(false).sort,
          (obj.singleton_methods(true) - obj2.singleton_methods(true)).sort,
          obj.methods(false).sort,
          (obj.methods(true) - obj2.methods(true)).sort,
          Foo.singleton_methods(false).sort,
          (Foo.singleton_methods(true) - Foo2.singleton_methods(true)).sort
        ]
        "##,
            r##"
        Parent = Class.new

        class <<Parent
          private;   def private_class_parent() end
          protected; def protected_class_parent() end
          public;    def public_class_parent() end
        end

        Foo = Class.new(Parent)
        Foo2 = Class.new()

        class <<Foo
          private;   def private_class_foo() end
          protected; def protected_class_foo() end
          public;    def public_class_foo() end
        end

        module Bar
          private;   def private_bar()   end
          protected; def protected_bar() end
          public;    def public_bar()    end
        end

        obj = Foo.new
        obj2 = Foo.new

        class << obj
          include Bar
          private;   def private_self()   end
          protected; def protected_self() end
          public;    def public_self()    end
        end
        "##,
        );
    }
}
