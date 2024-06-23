use super::*;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Struct", STRUCT_CLASS);
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "new", struct_new);
    globals.define_builtin_func_rest(STRUCT_CLASS, "initialize", initialize);
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
    let self_val = lfp.self_val();
    let args = lfp.arg(0).as_array();

    let mut new_struct = globals.new_unnamed_class(Some(self_val.as_class()));
    let class_id = new_struct.as_class_id();
    let start_idx = if let Some(arg0) = args.get(0)
        && let Some(s) = arg0.is_str()
    {
        if s.starts_with(|c: char| c.is_ascii_uppercase()) {
            globals.store[class_id].set_name(&format!("Struct::{s}"));
            1
        } else {
            return Err(MonorubyErr::identifier_must_be_constant(&s));
        }
    } else {
        0
    };
    globals.define_builtin_class_inline_funcs_rest(
        class_id,
        "new",
        &["[]"],
        new,
        Box::new(super::class::gen_class_new_object()),
        analysis::v_v_vv,
    );
    globals.define_builtin_class_func(class_id, "members", struct_members, 0);

    let members = ArrayInner::from_iter(args.iter().skip(start_idx).cloned());

    for arg in members.iter() {
        let name = arg.expect_symbol_or_string()?;
        globals.define_attr_reader(class_id, name, Visibility::Public);
        globals.define_attr_writer(class_id, name, Visibility::Public);
    }
    new_struct.set_instance_var(globals, "/members", Value::array(members))?;

    if let Some(bh) = lfp.block() {
        vm.push_class_context(class_id);
        let data = vm.get_block_data(globals, bh)?;
        vm.invoke_block_with_self(globals, &data, new_struct, &[new_struct])?;
        vm.pop_class_context();
    };
    Ok(new_struct)
}

#[monoruby_builtin]
fn struct_members(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let members = globals
        .get_ivar(lfp.self_val(), IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    super::class::__new(vm, globals, lfp)
}

#[monoruby_builtin]
fn initialize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let len = lfp.arg(0).as_array().len();
    let self_val = lfp.self_val();
    let struct_class = self_val.class().get_obj(globals);
    let members = globals
        .get_ivar(struct_class, IdentId::get_id("/members"))
        .unwrap()
        .as_array();
    if members.len() < len {
        return Err(MonorubyErr::argumenterr("Struct size differs."));
    };
    for (i, val) in lfp.arg(0).as_array().iter().enumerate() {
        let id = members[i].try_symbol().unwrap();
        let ivar_name = IdentId::add_ivar_prefix(id);
        globals.set_ivar(self_val, ivar_name, *val)?;
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut inspect = format!("#<struct ");
    let self_val = lfp.self_val();
    let class_id = self_val.class();
    let struct_class = class_id.get_obj(globals);
    if let Some(name) = globals.store[class_id].get_name_id() {
        inspect += &format!("{name}");
    };
    let name = globals
        .get_ivar(struct_class, IdentId::get_id("/members"))
        .unwrap()
        .as_array();

    if name.len() != 0 {
        for x in name.iter() {
            let name = x.try_symbol().unwrap();
            let ivar_name = IdentId::add_ivar_prefix(name);
            let val = match globals.get_ivar(self_val, ivar_name) {
                Some(v) => v.inspect(globals),
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
    let class_obj = lfp.self_val().class().get_obj(globals);
    let members = globals
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
