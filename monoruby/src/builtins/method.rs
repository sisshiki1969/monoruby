use super::*;

//
// Method / UnboundMethod class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Method", METHOD_CLASS);
    globals.define_builtin_func_rest(METHOD_CLASS, "call", call);
    globals.define_builtin_func_rest(METHOD_CLASS, "[]", call);
    globals.define_builtin_func_rest(METHOD_CLASS, "===", call);

    globals.define_builtin_class_under_obj("UnboundMethod", UMETHOD_CLASS);
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
    )
    .ok_or_else(|| vm.take_error())
}

#[cfg(test)]
mod test {
    use crate::tests::*;
    #[test]
    fn call() {
        run_test_with_prelude(
            r##"
        m = Foo.new.method(:foo) # => #<Method: Foo#foo>
        res = []
        res << m[1]       # => "foo called with arg 1"
        res << m.call(2)  # => "foo called with arg 2"
        res << (m === 3)  # => "foo called with arg 3"
        res
            "##,
            r##"
        class Foo
          def foo(arg)
            "foo called with arg #{arg}"
          end
        end
        "##,
        );
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
}
