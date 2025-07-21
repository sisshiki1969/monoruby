use super::*;

//
// Method / UnboundMethod class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Method", METHOD_CLASS, ObjTy::METHOD);
    globals.define_builtin_funcs_rest(METHOD_CLASS, "call", &["[]", "==="], call);
    globals.define_builtin_func(METHOD_CLASS, "to_proc", to_proc, 0);

    globals.define_builtin_class_under_obj("UnboundMethod", UMETHOD_CLASS, ObjTy::METHOD);
    globals.define_builtin_func(UMETHOD_CLASS, "bind", bind, 1);
}

///
/// ### Method#call
///
/// - self[*args] -> object
/// - call(*args) -> object
/// - call(*args) { ... } -> object
/// - self === *args -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    let func_id = method.func_id();
    let receiver = method.receiver();

    vm.invoke_func(
        globals,
        func_id,
        receiver,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
    )
    .ok_or_else(|| vm.take_error())
}

///
/// ### Method#to_proc
///
/// - to_proc -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/to_proc.html]
/// TODO: support keyword arguments
#[monoruby_builtin]
fn to_proc(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let method = self_.as_method();
    let self_val = method.receiver();
    let func_id = method.func_id();
    let proc = Proc::from_parts(Lfp::heap_frame(self_val, globals[func_id].meta()), func_id);
    Ok(proc.into())
}

///
/// ### UnboundMethod#bind
///
/// - bind(obj) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/bind.html]
/// TODO: we must reject invalid objects for *obj*
#[monoruby_builtin]
fn bind(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    Ok(Value::new_method(
        lfp.arg(0),
        method.func_id(),
        method.owner(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    #[test]
    fn call1() {
        run_test_with_prelude(
            r##"
        m = Foo.new.method(:foo) # => #<Method: Foo#foo>
        [
            m[1],       # => 1
            m.call(2),  # => 2
            (m === 3),  # => 3
        ]
        "##,
            r##"
        class Foo
            def foo(arg)
                arg
            end
        end
            "##,
        );
    }

    #[test]
    fn call2() {
        run_test_with_prelude(
            r##"
        m = Foo.new.method(:foo)
        m.call(42) do |x|
            x ** 2
        end
            "##,
            r##"
        class Foo
          def foo(arg)
            yield arg
          end
        end
        "##,
        );
    }

    #[test]
    fn call3() {
        run_test_with_prelude(
            r##"
        $res = []
        class C
        	def f
        		100
        	end
        end
        c = C.new
        m = c.method(:f)
        $res <<  m.call
        class C
        	def f
        		42
        	end
        end
        $res << m.call
        $res << c.method(:f).call
        $res
            "##,
            r##"
        "##,
        );
    }

    #[test]
    fn to_proc() {
        run_test_with_prelude(
            r##"
            m = Foo.new.method(:foo) # => #<Method: Foo#foo>
            pr = m.to_proc # => #<Proc:0x007f874d026008 (lambda)>
            pr.call(1, 2) # => "foo"
            "##,
            r##"
            class Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
        "##,
        );
    }

    #[test]
    fn bind() {
        run_test_with_prelude(
            r##"
            # UnboundMethod `m' を生成
            m = Foo.instance_method(:foo) # => #<UnboundMethod: Foo#foo>
            # Foo のインスタンスをレシーバとする Method オブジェクトを生成
            m = m.bind(Foo.new)               # => #<Method: Foo#foo>
            # Method オブジェクトを呼び出す
            m.call(1, 2)                     # => "foo12"
            "##,
            r##"
            class Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
        "##,
        );
    }

    #[test]
    fn bind2() {
        run_test_with_prelude(
            r##"
            # UnboundMethod `m' を生成
            m = Foo.instance_method(:foo) # => #<UnboundMethod: Foo#foo>
            # Foo のインスタンスをレシーバとする Method オブジェクトを生成
            m = m.bind(Bar.new)               # => #<Method: Bar(Foo)#foo>
            m.call(1, 2)                     # => "foo12"
            "##,
            r##"
            module Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
            class Bar
              include Foo
            end
        "##,
        );
    }
}
