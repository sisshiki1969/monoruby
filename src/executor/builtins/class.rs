use crate::*;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(CLASS_CLASS, "new", new, -1);
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
    globals.define_builtin_func(CLASS_CLASS, "allocate", allocate, 0);
    globals.define_builtin_func(CLASS_CLASS, "to_s", tos, 0);
    globals.define_builtin_func(CLASS_CLASS, "constants", constants, 0);
    globals.define_builtin_func(CLASS_CLASS, "instance_methods", instance_methods, 0);
    globals.define_builtin_func(CLASS_CLASS, "attr_reader", attr_reader, -1);
    globals.define_builtin_func(CLASS_CLASS, "attr_writer", attr_writer, -1);
    globals.define_builtin_func(CLASS_CLASS, "attr_accessor", attr_accessor, -1);
}

/// ### Class#new
/// - new(*args, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
///
/// !! We must call Object#initialize.
extern "C" fn new(
    vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let obj = allocate(vm, globals, self_val, arg, 0)?;
    if let Some(func_id) = globals.find_method(obj, IdentId::INITIALIZE) {
        globals.check_arg(func_id, len)?;
        vm.invoke_func2(globals, func_id, obj, arg, len)?;
    };
    Some(obj)
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/superclass.html]
extern "C" fn superclass(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = self_val.as_class();
    let res = match class_id.super_class(globals) {
        None => Value::nil(),
        Some(super_id) => super_id.get_obj(globals),
    };
    Some(res)
}

/// ### Class#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
extern "C" fn allocate(
    _vm: &mut Interp,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = self_val.as_class();
    let obj = Value::new_object(class_id);
    Some(obj)
}

/// ### Class#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
extern "C" fn tos(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_name = self_val.as_class().get_name(globals);
    let res = Value::new_string(class_name);
    Some(res)
}

/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
extern "C" fn constants(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = self_val.as_class();
    let v = globals
        .get_constant_names(class_id)
        .into_iter()
        .map(|name| Value::new_symbol(name))
        .collect();
    Some(Value::new_array(v))
}

/// ### Module#instance_methods
/// - instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_methods.html]
///
/// !! Currently, this method returns only the methods that is defined in *self*.
///
/// TODO: support inherited_too.
extern "C" fn instance_methods(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = self_val.as_class();
    let v = globals
        .get_method_names(class_id)
        .into_iter()
        .map(|name| Value::new_symbol(name))
        .collect();
    Some(Value::new_array(v))
}

/// ### Module#attr_reader
/// - attr_reader(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_reader.html]
extern "C" fn attr_reader(
    vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let mut res = vec![];
    let class_id = self_val.as_class();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(vm, class_id, arg_name);
        res.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(res))
}

/// ### Module#attr_writer
/// - attr_writer(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_writer.html]
extern "C" fn attr_writer(
    vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let mut res = vec![];
    let class_id = self_val.as_class();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_writer(vm, class_id, arg_name);
        res.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(res))
}

/// ### Module#attr_accessor
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
extern "C" fn attr_accessor(
    vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let mut res = vec![];
    let class_id = self_val.as_class();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(vm, class_id, arg_name.clone());
        res.push(Value::new_symbol(method_name));
        let method_name = globals.define_attr_writer(vm, class_id, arg_name);
        res.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(res))
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
