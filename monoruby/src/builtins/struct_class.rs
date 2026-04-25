use super::*;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Struct", STRUCT_CLASS, ObjTy::CLASS);
    // Struct itself is not allocatable (`Struct.allocate` and `Struct.new(1)`
    // both raise). Subclasses created via `Struct.new(:a, :b, ...)` get the
    // default allocator installed in `define_struct_class`.
    globals.store[STRUCT_CLASS].clear_alloc_func();
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "new", struct_new);
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "initialize", struct_initialize);

    globals.define_builtin_func(STRUCT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "to_s", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "members", members, 0);
    globals.define_builtin_funcs(STRUCT_CLASS, "==", &["eql?"], eq, 1);
    globals.define_builtin_func(STRUCT_CLASS, "!=", ne, 1);
}

///
/// Struct.[]
/// - new(*args, keyword_init: nil) -> Class
/// - new(*args, keyword_init: nil) {|subclass| block } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/=5b=5d.html]
#[monoruby_builtin]
fn struct_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let (args, name) = if let Some(arg0) = args.first()
        && let Some(s) = arg0.is_str()
    {
        if s.starts_with(|c: char| c.is_ascii_uppercase()) {
            (args[1..].to_vec(), Some(IdentId::get_id(s)))
        } else {
            return Err(MonorubyErr::identifier_must_be_constant(s));
        }
    } else {
        (args.to_vec(), None)
    };

    let new_struct = globals
        .store
        .define_struct_class(name, lfp.self_val().as_class())
        .as_val();

    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        new_struct,
        &args,
        lfp.block(),
        None,
    )?;

    Ok(new_struct)
}

///
/// Struct.[]
/// - new(*args, keyword_init: nil) -> Class
/// - new(*args, keyword_init: nil) {|subclass| block } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/=5b=5d.html]
#[monoruby_builtin]
fn struct_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut new_struct = lfp.self_val();
    let new_module = new_struct.as_class();
    let class_id = new_module.id();
    let args = lfp.arg(0).as_array();

    globals.define_builtin_class_inline_funcs_catch_all(
        class_id,
        "new",
        &["[]"],
        new,
        Box::new(super::class::gen_class_new_object()),
    );
    globals.define_builtin_class_func(class_id, "members", struct_members, 0);
    globals.define_builtin_func_rest(class_id, "initialize", initialize);
    globals.define_builtin_funcs(class_id, "==", &["eql?"], eq, 1);
    globals.define_builtin_func(class_id, "!=", ne, 1);

    let members = ArrayInner::from_iter(args.iter().cloned());

    for arg in members.iter() {
        let name = arg.expect_symbol_or_string(globals)?;
        vm.define_attr_reader(globals, class_id, name, Visibility::Public)?;
        vm.define_attr_writer(globals, class_id, name, Visibility::Public)?;
    }

    new_struct.set_instance_var(&mut globals.store, "/members", Value::array(members))?;

    if let Some(bh) = lfp.block() {
        vm.module_eval(globals, new_module, bh)?;
    };
    Ok(Value::nil())
}

#[monoruby_builtin]
fn struct_members(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let members = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    super::class::__new(vm, globals, lfp, pc)
}

fn get_members(globals: &mut Globals, mut class: Module) -> Result<Array> {
    let mut members = None;
    loop {
        if let Some(m) = globals
            .store
            .get_ivar(class.as_val(), IdentId::get_id("/members"))
        {
            members = Some(m);
            break;
        } else if let Some(s) = class.superclass()
            && s.id() != STRUCT_CLASS
        {
            class = s;
        } else {
            break;
        }
    }
    Ok(members
        .ok_or_else(|| MonorubyErr::runtimeerr("no ivar /members."))?
        .as_array())
}

#[monoruby_builtin]
fn initialize(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let len = lfp.arg(0).as_array().len();
    let self_val = lfp.self_val();
    let members = get_members(globals, self_val.get_class_obj(globals))?;
    if members.len() < len {
        return Err(MonorubyErr::argumenterr("Struct size differs."));
    };
    for (i, val) in lfp.arg(0).as_array().iter().enumerate() {
        let id = members[i].try_symbol().unwrap();
        let ivar_name = IdentId::add_ivar_prefix(id);
        globals.store.set_ivar(self_val, ivar_name, *val)?;
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut inspect = "#<struct ".to_string();
    let self_val = lfp.self_val();
    let class_id = self_val.class();
    let struct_class = globals.store[class_id].get_module();
    if let Some(name) = globals.store[class_id].get_name() {
        inspect += &format!("{name}");
    };
    let name = get_members(globals, struct_class)?;

    if name.len() != 0 {
        for x in name.iter() {
            let name = x.try_symbol().unwrap();
            let ivar_name = IdentId::add_ivar_prefix(name);
            let val = match globals.store.get_ivar(self_val, ivar_name) {
                Some(v) => v.inspect(&globals.store),
                None => "nil".to_string(),
            };
            inspect += &format!(" {:?}={},", name, val);
        }
        inspect.pop();
    }
    inspect += ">";

    Ok(Value::string(inspect))
}

///
/// Struct#==
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    // Must be the same class
    if self_val.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let members = get_members(globals, self_val.get_class_obj(globals))?;
    for member in members.iter() {
        let name = member.try_symbol().unwrap();
        let ivar_name = IdentId::add_ivar_prefix(name);
        let lhs = globals
            .store
            .get_ivar(self_val, ivar_name)
            .unwrap_or_default();
        let rhs = globals.store.get_ivar(other, ivar_name).unwrap_or_default();
        if vm.ne_values_bool(globals, lhs, rhs)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// Struct#!=
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(true));
    }
    let members = get_members(globals, self_val.get_class_obj(globals))?;
    for member in members.iter() {
        let name = member.try_symbol().unwrap();
        let ivar_name = IdentId::add_ivar_prefix(name);
        let lhs = globals
            .store
            .get_ivar(self_val, ivar_name)
            .unwrap_or_default();
        let rhs = globals.store.get_ivar(other, ivar_name).unwrap_or_default();
        if vm.ne_values_bool(globals, lhs, rhs)? {
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn members(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_obj = lfp.self_val().get_class_obj(globals).as_val();
    let members = globals
        .store
        .get_ivar(class_obj, IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn struct_test() {
        let prelude = r#"
        Customer = Struct.new(:name, :address) do
            def greeting
                "Hello #{name} at #{address}!"
            end
        end
        "#;
        let code = r#"
        [Customer.new("Dave", "New York").greeting, Customer["Gave", "Hawaii"].greeting]
        "#;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_subclass() {
        let prelude = r##"
        class S < Struct.new(:name, :address)
        end
        "##;
        let code = r##"
        s = S.new("Dave", "New York")
        "#{s.name} #{s.address} #{s} #{s.inspect}"
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_new() {
        run_test(
            r###"
            Struct.new("Foo", :a, :b).to_s
        "###,
        );
        run_test(
            r###"
            Struct.new(:a, :b, :c).new.members
        "###,
        );
        run_test(
            r###"
            Struct.new(:a, :b, :c).members
        "###,
        );
    }

    #[test]
    fn struct_inspect() {
        let code = r###"
        S = Struct.new(:a,:b)
        s = S.new(100,200)
        [s.a, s.b, s.inspect]
        "###;
        run_test(code);
    }

    #[test]
    fn struct_eq() {
        let prelude = r##"
        S = Struct.new(:a, :b)
        T = Struct.new(:a, :b)
        "##;
        let code = r##"
        res = []
        # equal structs
        res << (S.new(1, 2) == S.new(1, 2))
        # different values
        res << (S.new(1, 2) == S.new(1, 3))
        # different struct classes with same members and values
        res << (S.new(1, 2) == T.new(1, 2))
        # comparison with non-struct
        res << (S.new(1, 2) == [1, 2])
        # eql? behaves the same as ==
        res << (S.new(1, 2).eql?(S.new(1, 2)))
        res << (S.new(1, 2).eql?(S.new(1, 3)))
        # nested structs
        res << (S.new(S.new(1, 2), 3) == S.new(S.new(1, 2), 3))
        res << (S.new(S.new(1, 2), 3) == S.new(S.new(1, 9), 3))
        # default (nil) members
        res << (S.new == S.new)
        res << (S.new(1) == S.new)
        res
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_ne() {
        let prelude = r##"
        S = Struct.new(:a, :b)
        T = Struct.new(:a, :b)
        "##;
        let code = r##"
        res = []
        # equal structs
        res << (S.new(1, 2) != S.new(1, 2))
        # different values
        res << (S.new(1, 2) != S.new(1, 3))
        # different struct classes
        res << (S.new(1, 2) != T.new(1, 2))
        # comparison with non-struct
        res << (S.new(1, 2) != [1, 2])
        # nested structs
        res << (S.new(S.new(1, 2), 3) != S.new(S.new(1, 2), 3))
        res << (S.new(S.new(1, 2), 3) != S.new(S.new(1, 9), 3))
        # default (nil) members
        res << (S.new != S.new)
        res << (S.new(1) != S.new)
        res
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_method_added() {
        run_test(
            r##"
        $added = []
        S = Struct.new(:x, :y) do
          def self.method_added(name)
            $added << name
          end
        end
        $added
        "##,
        );
        run_test_once(
            r##"
        $added = []
        class Module
          alias_method :orig_method_added, :method_added
          def method_added(name)
            $added << name if self.ancestors.include?(Struct)
            orig_method_added(name)
          end
        end
        S = Struct.new(:x, :y)
        $added
        "##,
        );
    }

    #[test]
    fn struct_block_constant_lexical_scope() {
        // CRuby semantics: a constant assigned inside the `Struct.new ... do
        // ... end` block goes to the *lexical* scope (top-level here), not to
        // the new Struct subclass. Methods defined in the same block resolve
        // the constant via the same lexical chain.
        run_test_with_prelude(
            r##"
            [S.new(0).flag, S.constants, Object.constants.include?(:FLAGS)]
            "##,
            r##"
            S = Struct.new(:x) do
              FLAGS = { foo: 1 }
              def flag
                FLAGS[:foo]
              end
            end
            "##,
        );
    }
}
