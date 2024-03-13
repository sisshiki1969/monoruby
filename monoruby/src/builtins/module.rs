use super::*;

//
// Module class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Module", MODULE_CLASS);
    // instance methods
    globals.define_builtin_func(MODULE_CLASS, "==", eq, 1);
    globals.define_builtin_func(MODULE_CLASS, "===", teq, 1);
    globals.define_builtin_func(MODULE_CLASS, "alias_method", alias_method, 2);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_accessor", attr_accessor);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_reader", attr_reader);
    globals.define_builtin_func_rest(MODULE_CLASS, "attr_writer", attr_writer);
    globals.define_builtin_func(MODULE_CLASS, "autoload", autoload, 2);
    globals.define_builtin_funcs_eval_with(
        MODULE_CLASS,
        "class_eval",
        &["module_eval"],
        class_eval,
        0,
        2,
        false,
    );
    globals.define_builtin_func_with(MODULE_CLASS, "const_get", const_get, 1, 2, false);
    globals.define_builtin_func_with(MODULE_CLASS, "constants", constants, 0, 1, false);
    globals.define_builtin_func_rest(MODULE_CLASS, "deprecate_constant", deprecate_constant);
    globals.define_builtin_func_with(
        MODULE_CLASS,
        "instance_methods",
        instance_methods,
        0,
        1,
        false,
    );
    globals.define_builtin_func_rest(MODULE_CLASS, "include", include);
    globals.define_builtin_func(MODULE_CLASS, "method_defined?", method_defined, 1);
    globals.define_builtin_func_rest(MODULE_CLASS, "private_class_method", private_class_method);
    globals.define_builtin_func(MODULE_CLASS, "to_s", tos, 0);
    // private methos
    globals.define_private_builtin_func_rest(MODULE_CLASS, "module_function", module_function);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "private", private);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "protected", protected);
    globals.define_private_builtin_func_rest(MODULE_CLASS, "public", public);
}

///
/// ### Module#==
///
/// - self == obj -> bool
///
/// []
#[monoruby_builtin]
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let rhs = match lfp.arg(0).is_class_or_module() {
        Some(class) => class,
        None => return Ok(Value::bool(false)),
    };
    let lhs = lfp.self_val().as_class_id();
    Ok(Value::bool(lhs == rhs))
}

///
/// ### Module#===
///
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    Ok(Value::bool(lfp.arg(0).is_kind_of(globals, class)))
}

///
/// ### Module#alias_method
///
/// - alias_method(new, original) -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/alias_method.html]
#[monoruby_builtin]
fn alias_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let new_name = lfp.arg(0).expect_symbol_or_string()?;
    let old_name = lfp.arg(1).expect_symbol_or_string()?;
    globals.alias_method_for_class(class_id, new_name, old_name)?;
    Ok(Value::symbol(new_name))
}

///
/// ### Module#attr_accessor
///
/// - attr_accessor(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_accessor.html]
#[monoruby_builtin]
fn attr_accessor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#attr_reader
///
/// - attr_reader(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_reader.html]
#[monoruby_builtin]
fn attr_reader(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_reader(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#attr_writer
///
/// - attr_writer(*name) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/attr_writer.html]
#[monoruby_builtin]
fn attr_writer(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new_empty();
    let class_id = lfp.self_val().as_class_id();
    let visi = vm.context_visibility();
    for v in lfp.arg(0).as_array().iter() {
        let arg_name = v.expect_symbol_or_string()?;
        let method_name = globals.define_attr_writer(class_id, arg_name, visi);
        ary.push(Value::symbol(method_name));
    }
    Ok(ary.into())
}

///
/// ### Module#autoload
///
/// - autoload(const_name, feature) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/autoload.html]
#[monoruby_builtin]
fn autoload(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let const_name = lfp.arg(0).expect_symbol_or_string()?;
    let feature = lfp.arg(1).expect_string()?;
    globals.set_constant_autoload(lfp.self_val().as_class_id(), const_name, feature);
    Ok(Value::nil())
}

///
/// ### Module#class_eval
///
/// - module_eval(expr, fname = "(eval)", [NOT SUPPORTED] lineno = 1) -> object
/// - module_eval {|mod| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/class_eval.html]
#[monoruby_builtin]
fn class_eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let module = lfp.self_val().as_class();

    if let Some(bh) = lfp.block() {
        if lfp.try_arg(0).is_some() {
            return Err(MonorubyErr::wrong_number_of_arg(0, lfp.args_count(3)));
        }
        let data = vm.get_block_data(globals, bh)?;
        vm.push_class_context(module.id());
        let res = vm.invoke_block_with_self(globals, &data, module.get(), &[module.get()]);
        vm.pop_class_context();
        res
    } else {
        if let Some(arg0) = lfp.try_arg(0) {
            let expr = arg0.expect_string()?;
            let cfp = vm.cfp();
            let caller_cfp = cfp.prev().unwrap();
            let path = if let Some(arg1) = lfp.try_arg(1) {
                arg1.expect_string()?
            } else {
                "(eval)".into()
            };

            let fid = globals.compile_script_eval(expr, path, caller_cfp)?;
            let proc = ProcInner::from(caller_cfp.lfp(), fid);
            vm.push_class_context(module.id());
            let res = vm.invoke_block_with_self(globals, &proc, module.get(), &[]);
            vm.pop_class_context();
            res
        } else {
            Err(MonorubyErr::wrong_number_of_arg_range(0, 1..=3))
        }
    }
}

///
/// ### Module#const_get
///
/// - const_get(name, inherit = true) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/const_get.html]
#[monoruby_builtin]
fn const_get(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let name = lfp.arg(0).expect_symbol_or_string()?;
    let module = lfp.self_val().as_class();
    let inherit = lfp.try_arg(1).is_none() || lfp.arg(1).as_bool();
    vm.const_get(globals, module, name, inherit)
}

///
/// ### Module#constants
/// - constants(inherit = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/constants.html]
#[monoruby_builtin]
fn constants(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let v = if lfp.try_arg(0).is_none() || lfp.arg(0).as_bool() {
        globals.get_constant_names_inherit(class_id)
    } else {
        globals.get_constant_names(class_id)
    };
    let iter = v.into_iter().map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

///
/// Module#deprecate_constant
///
/// - deprecate_constant(*name) -> self
///
/// TODO: implement
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/deprecate_constant.html]
#[monoruby_builtin]
fn deprecate_constant(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Module#instance_methods
/// - instance_methods([NOT SUPPRTED] inherited_too = true) -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/instance_methods.html]
///
/// !! Currently, this method returns only the methods that is defined in *self*.
///
/// TODO: support inherited_too.
#[monoruby_builtin]
fn instance_methods(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let inhereted_too = lfp.try_arg(0).is_none() || lfp.arg(0).as_bool();
    if !inhereted_too {
        return Err(MonorubyErr::argumenterr(
            "Currently, inherited_too is not supported.",
        ));
    }
    let iter = globals
        .get_method_names(class_id)
        .into_iter()
        .map(Value::symbol);
    Ok(Value::array_from_iter(iter))
}

///
/// ### Module#include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = Array::new(lfp.arg(0));
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let mut class = lfp.self_val().as_class();
    for v in args.iter().cloned().rev() {
        v.expect_module(globals)?;
        class.include_module(v.as_class());
    }
    Ok(lfp.self_val())
}

///
/// ### Module#method_defined?
/// - method_defined?(name, [NOT SUPPORTED] inherit=true) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/method_defined=3f.html]
#[monoruby_builtin]
fn method_defined(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let func_name = lfp.arg(0).expect_symbol_or_string()?;
    Ok(Value::bool(globals.method_defined(class_id, func_name)))
}

///
/// ### Module#private_class_method
/// - private_class_method(*name) -> self
/// - private_class_method(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private_class_method.html]
#[monoruby_builtin]
fn private_class_method(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let singleton = globals.get_singleton(lfp.self_val());
    let arg = Array::new(lfp.arg(0));
    let (_, names) = extract_names(arg)?;
    globals.change_method_visibility_for_class(singleton.id(), &names, Visibility::Private)?;
    Ok(lfp.self_val())
}

///
/// ### Module#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_s.html]
#[monoruby_builtin]
fn tos(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_name = globals.get_class_name(lfp.self_val().as_class_id());
    let res = Value::string(class_name);
    Ok(res)
}

///
/// ### Module#module_function
/// - module_function(*name) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/module_function.html]
#[monoruby_builtin]
fn module_function(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let len = arg0.as_array().len();
    if len == 0 {
        vm.set_module_function();
        Ok(Value::nil())
    } else {
        let class_id = lfp.self_val().as_class_id();
        let visi = vm.context_visibility();
        for v in arg0.as_array().iter() {
            let name = v.expect_symbol_or_string()?;
            let func_id = globals
                .find_method_entry_for_class(class_id, name)?
                .func_id();
            globals.add_singleton_method(class_id, name, func_id, visi);
        }
        Ok(arg0)
    }
}

///
/// ### Module#private
/// - private(*name) -> self
/// - private(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/private.html]
#[monoruby_builtin]
fn private(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = Array::new(lfp.arg(0));
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Private)
}

///
/// ### Module#protected
/// - protected(*name) -> self
/// - protected(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/protected.html]
#[monoruby_builtin]
fn protected(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = Array::new(lfp.arg(0));
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Protected)
}

///
/// ### Module#public
/// - public(*name) -> self
/// - public(names) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/public.html]
#[monoruby_builtin]
fn public(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg = Array::new(lfp.arg(0));
    change_visi(vm, globals, lfp.self_val(), arg, Visibility::Public)
}

fn change_visi(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Array,
    visi: Visibility,
) -> Result<Value> {
    if arg.len() == 0 {
        vm.set_context_visibility(visi);
        return Ok(Value::nil());
    }
    let (res, names) = extract_names(arg)?;
    let class_id = self_val.as_class_id();
    globals.change_method_visibility_for_class(class_id, &names, visi)?;
    Ok(res)
}

fn extract_names(arg: Array) -> Result<(Value, Vec<IdentId>)> {
    if arg.len() == 1 {
        let mut names = vec![];
        if let Some(ary) = arg[0].try_array_ty() {
            for v in ary.iter() {
                names.push(v.expect_symbol_or_string()?);
            }
        } else {
            names.push(arg[0].expect_symbol_or_string()?);
        }
        Ok((arg[0], names))
    } else {
        let mut names = vec![];
        for v in arg.iter() {
            names.push(v.expect_symbol_or_string()?);
        }
        Ok((arg.as_val(), names))
    }
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
    fn module_eval() {
        run_test_with_prelude(
            r#"
        a = 1
        C.class_eval %Q{
          def m                   # メソッドを動的に定義できる。
            return :m, #{a}, self.class
          end
        }
        C.new.m
        "#,
            r#"
        class C
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        a = 1
        C.class_eval do |m|
            def f
                self.class
            end
        end
        C.new.f
        "#,
            r#"
        class C
        end
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
    fn private_class_method() {
        run_test_error(
            r#"
            module Foo
                def self.foo; end
            end
            Foo.private_class_method(:foo)
            Foo.foo
            "#,
        )
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
        run_test_with_prelude(
            r#"
            $res = []
            class C
                $res << public(:f)
                $res << public("f")
                $res << public
                $res << public(["f"])
                $res << public(:f, :g, :h)
            end
            $res
            "#,
            r#"
            class C
              def f; end
              def g; end
              def h; end
            end
        "#,
        );
        run_test_error(r#"class C; public(1); end"#);
        run_test_error(r#"class C; public(:k); end"#);
        run_test_error(r#"class C; public(1,2); end"#);
        run_test_error(r#"class C; public([1,2]); end"#);
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
