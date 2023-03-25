use crate::*;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(CLASS_CLASS, "new", class_new, -1);
    globals.define_builtin_func(CLASS_CLASS, "new", new, -1);
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
    globals.define_builtin_func(CLASS_CLASS, "allocate", allocate, 0);
}

/// ### Class.new
/// - new(superclass = Object) -> Class
/// - [NOT SUPPORTED] new(superclass = Object) {|klass| ... } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/s/new.html]
extern "C" fn class_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    let superclass = if len == 0 {
        None
    } else {
        arg[0].expect_class(globals)?;
        Some(arg[0].as_class())
    };
    let obj = globals.new_unnamed_class(superclass);
    Some(obj)
}

/// ### Class#new
/// - new(*args, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
///
/// !! We must call Object#initialize.
extern "C" fn new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let obj = allocate(vm, globals, lfp, arg, 0)?;
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, dbg!(len))?;
    Some(obj)
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/superclass.html]
extern "C" fn superclass(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class = lfp.self_val().as_class();
    Some(class.superclass_value().unwrap_or_default())
}

/// ### Class#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
extern "C" fn allocate(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = lfp.self_val().as_class().class_id();
    let obj = Value::new_object(class_id);
    Some(obj)
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_class() {
        run_test("Time.superclass.to_s");
        run_test(
            r#"
        class A
          def f
            42
          end
        end
        A.new.f"#,
        );
        run_test(
            r#"
        class A
          class B
            def f
              42
            end
          end
        end
        A::B.new.f"#,
        );
    }

    #[test]
    fn initializer() {
        run_test(
            r#"
        class A
          attr_accessor :w, :x, :y, :z
          def initialize(x,y,z)
            @w = 42
            @x = x
            @y = y
            @z = z
          end
        end
        a = A.new(7, 11, 17)
        [a.w, a.x, a.y, a.z]
        "#,
        );
    }
}
