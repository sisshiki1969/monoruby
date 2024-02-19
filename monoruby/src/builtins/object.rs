use super::*;
use crate::jitgen::conv;

//
// Object class
//

pub(super) fn init(globals: &mut Globals) {
    //globals.define_builtin_class_func(OBJECT_CLASS, "new", object_new, -1);
    globals.define_builtin_inline_func(
        OBJECT_CLASS,
        "object_id",
        object_id,
        object_object_id,
        analysis::v_v,
        0,
    );
    globals.define_builtin_func(OBJECT_CLASS, "is_a?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "kind_of?", is_a, 1);
    globals.define_builtin_func(OBJECT_CLASS, "to_enum", to_enum, 0);
    globals.define_builtin_func(OBJECT_CLASS, "enum_for", to_enum, 0);
    globals.define_builtin_func(OBJECT_CLASS, "equal?", equal_, 1);
    globals.define_builtin_func(OBJECT_CLASS, "dup", dup, 0);
    globals.define_builtin_func(OBJECT_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(OBJECT_CLASS, "respond_to?", respond_to, 1);
    globals.define_builtin_func(OBJECT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(OBJECT_CLASS, "class", class, 0);
    globals.define_builtin_func(OBJECT_CLASS, "instance_of?", instance_of, 1);
    //globals.define_builtin_func(OBJECT_CLASS, "method", method,1);
    //globals.define_builtin_func(OBJECT_CLASS, "singleton_class", singleton_class, 0);
    //globals.define_builtin_func(OBJECT_CLASS, "instance_variable_defined?", iv_defined, 1);
    //globals.define_builtin_func(OBJECT_CLASS, "instance_variable_set", iv_set, 2);
    //globals.define_builtin_func(OBJECT_CLASS, "instance_variable_get", iv_get, 1);
    globals.define_builtin_func(OBJECT_CLASS, "instance_variables", iv, 0);
    //globals.define_builtin_func(OBJECT_CLASS, "system", system);
    //globals.define_builtin_func(OBJECT_CLASS, "`", command);
    globals.define_builtin_inline_func_with(
        OBJECT_CLASS,
        "send",
        send,
        object_send,
        analysis::v_v_vv,
        1,
        1,
        true,
    );
    globals.define_builtin_func(OBJECT_CLASS, "method", method, 1);
    globals.define_builtin_func_with(OBJECT_CLASS, "__send__", send, 1, 1, true);
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
    ir: &mut AsmIr,
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    _pc: BcPc,
) {
    let CallSiteInfo { recv, dst: ret, .. } = *callsite;
    ir.fetch_to_reg(bb, recv, GP::Rdi);
    let using = bb.get_using_xmm();
    ir.inline(move |gen, _| {
        gen.xmm_save(using);
        monoasm! {&mut gen.jit,
            movq rax, (crate::executor::op::i64_to_value);
            call rax;
        }
        gen.xmm_restore(using);
    });
    ir.rax2acc(bb, ret);
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
    let class = lfp.arg(0).expect_class_or_module(globals)?;
    Ok(Value::bool(lfp.self_val().is_kind_of(globals, class)))
}

///
/// ### Object#enum_for
///
/// - to_enum([NOT SUPPORTED] method = :each, *args) -> Enumerator
/// - enum_for([NOT SUPPORTED] method = :each, *args) -> Enumerator
/// - to_enum([NOT SUPPORTED] method = :each, *args) {|*args| ... } -> Enumerator
/// - enum_for([NOT SUPPORTED] method = :each, *args) {|*args| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/enum_for.html]
#[monoruby_builtin]
fn to_enum(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    vm.generate_enumerator(IdentId::EACH, lfp.self_val(), vec![])
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
    let s = globals.to_s(lfp.self_val());
    Ok(Value::string(s))
}

///
/// ### Object#respond_to?
///
/// - respond_to?(name, [NOT SUPPORTED] include_all = false) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/respond_to=3f.html]
#[monoruby_builtin]
fn respond_to(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = match lfp.arg(0).unpack() {
        RV::Symbol(id) => id,
        RV::String(b) => IdentId::get_id(String::from_utf8_lossy(b).as_ref()),
        _ => unimplemented!(),
    };
    Ok(Value::bool(
        globals.check_method(lfp.self_val(), name).is_some(),
    ))
}

///
/// ### Object#inspect
///
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let s = globals.inspect(lfp.self_val());
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
    Ok(lfp.self_val().real_class(globals).as_val())
}

///
/// ### Object#instance_of?
///
/// - instance_of?(klass) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/instance_of=3f.html]
#[monoruby_builtin]
fn instance_of(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b =
        lfp.self_val().real_class(globals).id() == lfp.arg(0).expect_class_or_module(globals)?;
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
    let method_name = lfp.arg(0).expect_symbol_or_string()?;
    let func_id = globals.find_method(receiver, method_name, false)?;
    Ok(Value::new_method(receiver, func_id))
}

///
/// ### Object#singleton_class
///
/// - singleton_class -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/singleton_class.html]
#[monoruby_builtin]
fn singleton_class(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().get_singleton(globals))
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
        RV::String(s) => IdentId::get_id(String::from_utf8_lossy(s).as_ref()),
        _ => return Err(MonorubyErr::is_not_symbol_nor_string(lfp.arg(0))),
    };
    let b = globals.get_ivar(lfp.self_val(), id).is_some();
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
    let id = lfp.arg(0).expect_symbol_or_string()?;
    let val = lfp.arg(1);
    globals.set_ivar(lfp.self_val(), id, val)?;
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
    let id = lfp.arg(0).expect_symbol_or_string()?;
    let v = globals.get_ivar(lfp.self_val(), id).unwrap_or_default();
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
        .get_ivars(lfp.self_val())
        .into_iter()
        .map(|(id, _)| Value::symbol(id));
    Ok(Value::array_from_iter(iter))
}

///
/// Object#send
///
/// - send(name, *args) -> object
/// - send(name, *args) { .... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/send.html]
#[monoruby_builtin]
fn send(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let method = lfp.arg(0).expect_symbol_or_string()?;
    let args = lfp.arg(1).as_array().to_vec();
    vm.invoke_method_inner(globals, method, lfp.self_val(), &args, lfp.block())
}

const CACHE_SIZE: usize = 8;

fn object_send(
    ir: &mut AsmIr,
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    _pc: BcPc,
) {
    let CallSiteInfo {
        recv,
        dst,
        args,
        pos_num,
        block_fid,
        ..
    } = *callsite;
    ir.write_back_callargs(bb, callsite);
    ir.unlink(bb, dst);
    let using = bb.get_using_xmm();
    let bh = match block_fid {
        None => 0,
        Some(func_id) => BlockHandler::from(func_id).0.id(),
    };
    ir.inline(move |gen, _| {
        let cache = gen.jit.bytes(std::mem::size_of::<Cache>() * CACHE_SIZE);
        gen.xmm_save(using);
        monoasm! {&mut gen.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (conv(recv))];
            lea  rcx, [r14 - (conv(args))];
            movq r8, (pos_num);
            movq r9, (bh);
            subq rsp, 8;
            lea  rax, [rip + cache];
            pushq rax;
            movq rax, (call_send_wrapper);
            call rax;
            addq rsp, 16;
        }
        gen.xmm_restore(using);
    });
    ir.reg2acc(bb, GP::Rax, dst);
}

#[repr(C)]
struct Cache {
    method: Option<IdentId>,
    version: u32,
    fid: FuncId,
    counter: u32,
}

extern "C" fn call_send_wrapper(
    vm: &mut Executor,           // rdi
    globals: &mut Globals,       // rsi
    recv: Value,                 // rdx
    args: Arg,                   // rcx
    len: usize,                  // r8
    block: Option<BlockHandler>, // r9
    cache: &mut [Cache; CACHE_SIZE],
) -> Option<Value> {
    fn call_send(
        globals: &mut Globals,
        recv: Value,
        args: Arg,
        len: usize,
        cache: &mut [Cache; CACHE_SIZE],
    ) -> Result<FuncId> {
        if len < 1 {
            return Err(MonorubyErr::wrong_number_of_arg_min(len, 1));
        }
        let method = args[0].unwrap().expect_symbol_or_string()?;
        let mut min_i = usize::MAX;
        let mut min_count = u32::MAX;
        for (i, entry) in cache.iter_mut().enumerate().take(CACHE_SIZE) {
            if entry.method.is_none() || entry.version != globals.class_version() {
                if min_count != 0 {
                    min_count = 0;
                    min_i = i;
                }
                continue;
            }
            if entry.method == Some(method) {
                entry.counter += 1;
                return Ok(entry.fid);
            }
            if entry.counter < min_count {
                min_count = entry.counter;
                min_i = i;
            }
        }
        let fid = globals.find_method(recv, method, false)?;
        //eprintln!("cache miss:{:?} {:?}", cache as *mut _, method);
        if cache[min_i].method.is_none() {
            cache[min_i].method = Some(method);
            cache[min_i].version = globals.class_version();
            cache[min_i].fid = fid;
            cache[min_i].counter = 1;
        }

        Ok(fid)
    }

    let fid = match call_send(globals, recv, args, len, cache) {
        Ok(res) => res,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    (globals.codegen.method_invoker2)(vm, globals, fid, recv, args + 1, len - 1, block)
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
    }
}
