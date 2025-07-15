use super::*;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Struct", STRUCT_CLASS, ObjTy::CLASS);
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "new", struct_new);
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "initialize", struct_initialize);

    globals.define_builtin_func(STRUCT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "to_s", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "members", members, 0);
}

///
/// Struct.[]
/// - new(*args, keyword_init: nil) -> Class
/// - new(*args, keyword_init: nil) {|subclass| block } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/=5b=5d.html]
#[monoruby_builtin]
fn struct_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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

    vm.invoke_method_if_exists(
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
fn struct_initialize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut new_struct = lfp.self_val();
    let class_id = new_struct.as_class().id();
    let args = lfp.arg(0).as_array();

    globals.define_builtin_class_inline_funcs_rest(
        class_id,
        "new",
        &["[]"],
        new,
        Box::new(super::class::gen_class_new_object()),
    );
    globals.define_builtin_class_func(class_id, "members", struct_members, 0);
    globals.define_builtin_func_rest(class_id, "initialize", initialize);

    let members = ArrayInner::from_iter(args.iter().cloned());

    for arg in members.iter() {
        let name = arg.expect_symbol_or_string(globals)?;
        globals.define_attr_reader(class_id, name, Visibility::Public);
        globals.define_attr_writer(class_id, name, Visibility::Public);
    }

    new_struct.set_instance_var(&mut globals.store, "/members", Value::array(members))?;

    if let Some(bh) = lfp.block() {
        vm.push_class_context(class_id);
        let data = vm.get_block_data(globals, bh)?;
        let res = vm.invoke_block_with_self(globals, &data, new_struct, &[new_struct]);
        vm.pop_class_context();
        res?;
    };
    Ok(Value::nil())
}

#[monoruby_builtin]
fn struct_members(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let members = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    super::class::__new(vm, globals, lfp)
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
fn initialize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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

#[monoruby_builtin]
fn members(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
}
