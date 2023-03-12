use crate::*;

//
// Module class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(MODULE_CLASS, "==", eq, 1);
    globals.define_builtin_func(MODULE_CLASS, "===", teq, 1);
    globals.define_builtin_func(MODULE_CLASS, "to_s", tos, 0);
    globals.define_builtin_func(MODULE_CLASS, "constants", constants, 0);
    globals.define_builtin_func(MODULE_CLASS, "instance_methods", instance_methods, 0);
    globals.define_builtin_func(MODULE_CLASS, "attr_reader", attr_reader, -1);
    globals.define_builtin_func(MODULE_CLASS, "attr_writer", attr_writer, -1);
    globals.define_builtin_func(MODULE_CLASS, "attr_accessor", attr_accessor, -1);
    globals.define_builtin_func(MODULE_CLASS, "include", include, -1);
    globals.define_private_builtin_func(MODULE_CLASS, "module_function", module_function, -1);
    globals.define_private_builtin_func(MODULE_CLASS, "private", private, -1);
    globals.define_private_builtin_func(MODULE_CLASS, "protected", protected, -1);
    globals.define_private_builtin_func(MODULE_CLASS, "public", public, -1);
}

/// ### Module#==
/// - self == obj -> bool
///
/// []
extern "C" fn eq(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let rhs = match arg[0].is_class_or_module() {
        Some(class) => class,
        None => return Some(Value::bool(false)),
    };
    let lhs = self_val.as_class().class_id();
    Some(Value::bool(lhs == rhs))
}

/// ### Module#===
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3d=3d=3d.html]
extern "C" fn teq(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class = self_val.as_class().class_id();
    Some(Value::bool(arg[0].is_kind_of(globals, class)))
}

/// ### Module#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
extern "C" fn tos(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class_name = self_val.as_class().class_id().get_name(globals);
    let res = Value::new_string(class_name);
    Some(res)
}

/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
extern "C" fn constants(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class_id = self_val.as_class().class_id();
    let iter = globals
        .get_constant_names(class_id)
        .into_iter()
        .map(Value::new_symbol);
    Some(Value::new_array_from_iter(iter))
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
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class_id = self_val.as_class().class_id();
    let iter = globals
        .get_method_names(class_id)
        .into_iter()
        .map(Value::new_symbol);
    Some(Value::new_array_from_iter(iter))
}

/// ### Module#attr_reader
/// - attr_reader(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_reader.html]
extern "C" fn attr_reader(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let mut ary = ArrayInner::new();
    let class_id = self_val.as_class().class_id();
    let visi = vm.context_visibility();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(ary))
}

/// ### Module#attr_writer
/// - attr_writer(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_writer.html]
extern "C" fn attr_writer(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let mut ary = ArrayInner::new();
    let class_id = self_val.as_class().class_id();
    let visi = vm.context_visibility();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(ary))
}

/// ### Module#attr_accessor
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
extern "C" fn attr_accessor(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let mut ary = ArrayInner::new();
    let class_id = self_val.as_class().class_id();
    let visi = vm.context_visibility();
    for i in 0..len {
        let arg_name = arg[i].expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::new_symbol(method_name));
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::new_symbol(method_name));
    }
    Some(Value::new_array(ary))
}

/// ### Module#module_function
/// - module_function(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/module_function.html]
extern "C" fn module_function(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    if len == 0 {
        vm.set_module_function();
        Some(Value::nil())
    } else {
        let class_id = self_val.as_class().class_id();
        let visi = vm.context_visibility();
        for i in 0..len {
            let name = arg[i].expect_symbol_or_string(globals)?;
            let func_id = globals.find_method_for_class(class_id, name)?;
            globals.add_singleton_method(class_id, name, func_id, visi);
        }
        let res = Value::new_array_from_vec(arg.to_vec(len));
        Some(res)
    }
}

/// ### Module#include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
extern "C" fn include(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    globals.check_min_number_of_arguments(len, 1)?;
    let class = self_val.as_class();
    for i in 0..len {
        arg[i].expect_module(globals)?;
        globals.include_module(class, arg[len - i - 1].as_class());
    }
    Some(self_val)
}

/// ### Module#private
/// - private(*name) -> self
/// - private(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private.html]
extern "C" fn private(
    executor: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    change_visi(executor, globals, self_val, arg, len, Visibility::Private)
}

/// ### Module#protected
/// - protected(*name) -> self
/// - protected(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected.html]
extern "C" fn protected(
    executor: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    change_visi(executor, globals, self_val, arg, len, Visibility::Protected)
}

/// ### Module#public
/// - public(*name) -> self
/// - public(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public.html]
extern "C" fn public(
    executor: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    change_visi(executor, globals, self_val, arg, len, Visibility::Public)
}

fn change_visi(
    executor: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    visi: Visibility,
) -> Option<Value> {
    if len == 0 {
        executor.set_context_visibility(visi);
        return Some(Value::nil());
    }
    let class_id = self_val.as_class().class_id();
    let mut names = vec![];
    if let Some(ary) = arg[0].is_array() {
        if len == 1 {
            for v in ary.iter() {
                names.push(v.expect_symbol_or_string(globals)?);
            }
            globals.change_method_visibility_for_class(class_id, &names, visi);
            return Some(arg[0]);
        }
    }
    for i in 0..len {
        names.push(arg[i].expect_symbol_or_string(globals)?);
    }
    globals.change_method_visibility_for_class(class_id, &names, visi);
    let res = Value::new_array_from_vec(arg.to_vec(len));
    Some(res)
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_teq() {
        run_test("Integer === 4");
        run_test("Integer === 4.5");
        run_test("Integer === 'Ruby'");
        run_test("Float === 4.5");
        run_test("Float === 'Ruby'");
        run_test(
            r#"
        class C
        end
        C === C.new"#,
        );
        run_test(
            r#"
        class S
        end
        class C < S
        end
        S === C.new"#,
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
    fn module_function() {
        run_test_with_prelude(
            "[M.c]",
            r#"
            module M
              def a; 1; end
              def b; 2; end
              module_function
              def c; 3; end
            end
            "#,
        );
        run_test_with_prelude(
            "[M.a, M.b]",
            r#"
            module M
              def a; 1; end
              def b; 2; end
              def c; 3; end
              module_function :a, "b"
            end
            "#,
        );
        run_test(
            r#"
            module M
              def a; 1; end
              def b; 2; end
              module_function :a, "b"
            end
            "#,
        );
        run_test_error(
            r#"
            module M
              module_function 100
            end
            "#,
        );
    }

    #[test]
    fn include() {
        run_test_with_prelude(
            "C.new.f",
            r#"
            module M1
              def f; "M1"; end
            end
            module M2
              def f; "M2"; end
            end
            class C
              include M1, M2 
            end
            "#,
        );
        run_test_error(
            r#"
            class M; end
            class C
              include M 
            end
            "#,
        );
        run_test_error(
            r#"
            class C
              include
            end
            "#,
        );
    }

    #[test]
    fn private() {
        run_test_with_prelude(
            r#"
            C.new.g
            "#,
            r#"
            class C
              private def f; 5; end
              def g; f; end
            end
        "#,
        );
        run_test_error(
            r#"
            class C
              private def f; 5; end
              def g; f; end
            end
            C.new.f
        "#,
        );
    }

    #[test]
    fn public() {
        run_test_with_prelude(
            r#"
            D.new.f
            "#,
            r#"
            class C
              private def f; 5; end
            end

            class D < C
              public :f
            end
        "#,
        );
    }
}
