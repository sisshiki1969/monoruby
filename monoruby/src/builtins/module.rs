use super::*;

//
// Module class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Module", MODULE_CLASS);
    globals.define_builtin_func(MODULE_CLASS, "==", eq);
    globals.define_builtin_func(MODULE_CLASS, "===", teq);
    globals.define_builtin_func(MODULE_CLASS, "to_s", tos);
    globals.define_builtin_func(MODULE_CLASS, "constants", constants);
    globals.define_builtin_func(MODULE_CLASS, "const_get", const_get);
    globals.define_builtin_func(MODULE_CLASS, "instance_methods", instance_methods);
    globals.define_builtin_func(MODULE_CLASS, "attr_reader", attr_reader);
    globals.define_builtin_func(MODULE_CLASS, "attr_writer", attr_writer);
    globals.define_builtin_func(MODULE_CLASS, "attr_accessor", attr_accessor);
    globals.define_builtin_func(MODULE_CLASS, "include", include);
    globals.define_builtin_func(MODULE_CLASS, "method_defined?", method_defined);
    globals.define_private_builtin_func(MODULE_CLASS, "module_function", module_function);
    globals.define_private_builtin_func(MODULE_CLASS, "private", private);
    globals.define_private_builtin_func(MODULE_CLASS, "protected", protected);
    globals.define_private_builtin_func(MODULE_CLASS, "public", public);
    globals.define_private_builtin_func(MODULE_CLASS, "alias_method", alias_method);
}

/// ### Module#==
/// - self == obj -> bool
///
/// []
#[monoruby_builtin]
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let rhs = match arg[0].is_class_or_module() {
        Some(class) => class,
        None => return Ok(Value::bool(false)),
    };
    let lhs = lfp.self_val().as_class_id();
    Ok(Value::bool(lhs == rhs))
}

/// ### Module#===
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    Ok(Value::bool(arg[0].is_kind_of(globals, class)))
}

/// ### Module#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn tos(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let class_name = lfp.self_val().as_class_id().get_name(globals);
    let res = Value::string(class_name);
    Ok(res)
}

/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
#[monoruby_builtin]
fn constants(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let class_id = lfp.self_val().as_class_id();
    let v = if len == 0 || arg[0].as_bool() {
        globals.get_constant_names_inherit(class_id)
    } else {
        globals.get_constant_names(class_id)
    };
    let iter = v.into_iter().map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

/// ### Module#const_get
/// - const_get(name, inherit = true) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_get.html]
#[monoruby_builtin]
fn const_get(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let name = arg[0].expect_symbol_or_string(globals)?;
    let module = lfp.self_val().as_class();
    let v = if len == 1 || arg[1].as_bool() {
        globals.search_constant_superclass(module, name)
    } else {
        globals.get_constant(module.id(), name)
    };
    match v {
        Some(v) => Ok(v),
        None => Err(MonorubyErr::uninitialized_constant(name)),
    }
}

/// ### Module#instance_methods
/// - instance_methods(inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_methods.html]
///
/// !! Currently, this method returns only the methods that is defined in *self*.
///
/// TODO: support inherited_too.
#[monoruby_builtin]
fn instance_methods(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let iter = globals
        .get_method_names(class_id)
        .into_iter()
        .map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

/// ### Module#attr_reader
/// - attr_reader(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_reader.html]
#[monoruby_builtin]
fn attr_reader(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let mut ary = Array::new();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.iter() {
        let arg_name = v.expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

/// ### Module#attr_writer
/// - attr_writer(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_writer.html]
#[monoruby_builtin]
fn attr_writer(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let mut ary = Array::new();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.iter() {
        let arg_name = v.expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

/// ### Module#attr_accessor
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
#[monoruby_builtin]
fn attr_accessor(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let mut ary = Array::new();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.iter() {
        let arg_name = v.expect_symbol_or_string(globals)?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

/// ### Module#module_function
/// - module_function(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/module_function.html]
#[monoruby_builtin]
fn module_function(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    if len == 0 {
        vm.set_module_function();
        Ok(Value::nil())
    } else {
        let class_id = lfp.self_val().as_class_id();
        let visi = vm.context_visibility();
        for v in lfp.iter() {
            let name = v.expect_symbol_or_string(globals)?;
            let func_id = globals
                .find_method_entry_for_class(class_id, name)?
                .func_id();
            globals.add_singleton_method(class_id, name, func_id, visi);
        }
        let res = Value::array_from_iter(lfp.iter());
        Ok(res)
    }
}

/// ### Module#include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let self_ = lfp.self_val();
    MonorubyErr::check_min_number_of_arguments(len, 1)?;
    let class = self_.as_class();
    for v in lfp.rev() {
        v.expect_module(globals)?;
        globals.include_module(class, v.as_class());
    }
    Ok(self_)
}

/// ### Module#private
/// - private(*name) -> self
/// - private(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private.html]
#[monoruby_builtin]
fn private(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    change_visi(vm, globals, lfp.self_val(), lfp, Visibility::Private)
}

/// ### Module#protected
/// - protected(*name) -> self
/// - protected(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected.html]
#[monoruby_builtin]
fn protected(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    change_visi(vm, globals, lfp.self_val(), lfp, Visibility::Protected)
}

/// ### Module#public
/// - public(*name) -> self
/// - public(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public.html]
#[monoruby_builtin]
fn public(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    change_visi(vm, globals, lfp.self_val(), lfp, Visibility::Public)
}

fn change_visi(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    lfp: LFP,
    visi: Visibility,
) -> Result<Value> {
    let len = lfp.arg_len();
    if len == 0 {
        vm.set_context_visibility(visi);
        return Ok(Value::nil());
    }
    let class_id = self_val.as_class_id();
    let mut names = vec![];
    if let Some(ary) = lfp.arg(0).is_array() {
        if len == 1 {
            for v in ary.iter() {
                names.push(v.expect_symbol_or_string(globals)?);
            }
            globals.change_method_visibility_for_class(class_id, &names, visi);
            return Ok(lfp.arg(0));
        }
    }
    for v in lfp.iter() {
        names.push(v.expect_symbol_or_string(globals)?);
    }
    globals.change_method_visibility_for_class(class_id, &names, visi);
    let res = Value::array_from_iter(lfp.iter());
    Ok(res)
}

/// ### Module#method_defined?
/// - method_defined?(name, inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/method_defined=3f.html]
#[monoruby_builtin]
fn method_defined(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let class_id = lfp.self_val().as_class_id();
    let func_name = arg[0].expect_symbol_or_string(globals)?;
    Ok(Value::bool(globals.method_defined(class_id, func_name)))
}

/// ### Module#alias_method
/// - alias_method(new, original) -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/alias_method.html]
#[monoruby_builtin]
fn alias_method(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let new_name = arg[0].expect_symbol_or_string(globals)?;
    let old_name = arg[1].expect_symbol_or_string(globals)?;
    globals.alias_method_for_class(class_id, new_name, old_name)?;
    Ok(Value::symbol(new_name))
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
    fn constants() {
        run_test_with_prelude(
            r#"
            [C.constants.sort!, C.constants(false).sort!]
            "#,
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
        "#,
        );
    }

    #[test]
    fn const_get() {
        run_test_with_prelude(
            r#"
            C.const_get(:S1)
            "#,
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
        "#,
        );
        run_test_error(
            r#"
            class S
              S1 = 100
            end
            class C < S
              C1 = 1
              C2 = 2
            end
            C.const_get(:S1, false)
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

    #[test]
    fn alias_method() {
        run_test_once(
            r#"
            class String
              alias_method :foo, :upcase
            end
            "abdG&0[]nim".foo
            "#,
        );
    }

    #[test]
    fn method_defined() {
        run_test_once(
            r#"
        module A
          def method1()  end
          def protected_method1()  end
          protected :protected_method1
        end
        class B
          def method2()  end
          def private_method2()  end
          private :private_method2
        end
        class C < B
          include A
          def method3()  end
        end
        x = []
        x << A.method_defined?(:method1)              #=> true
        x << C.method_defined?("method1")             #=> true
        x << C.method_defined?("method2")             #=> true
        # x << C.method_defined? "method2", true      #=> true
        # x << C.method_defined? "method2", false     #=> false
        x << C.method_defined?("method3")             #=> true
        x << C.method_defined?("protected_method1")   #=> true
        x << C.method_defined?("method4")             #=> false
        x << C.method_defined?("private_method2")     #=> false
        x
        "#,
        )
    }
}
