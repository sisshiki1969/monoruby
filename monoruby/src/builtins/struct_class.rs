use super::*;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Struct", STRUCT_CLASS);
    globals.define_builtin_class_func(STRUCT_CLASS, "new", struct_new);
}

#[monoruby_builtin]
fn struct_new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let self_val = lfp.self_val();
    lfp.check_min_number_of_arguments(1)?;
    let mut arg_vec = lfp.to_vec();

    let mut class = globals.new_unnamed_class(Some(self_val.as_class()));
    let class_id = class.as_class_id();
    match lfp.arg(0).is_str() {
        None => {}
        Some(s) => {
            match s.chars().nth(0) {
                Some(c) if c.is_ascii_uppercase() => {}
                _ => return Err(MonorubyErr::identifier_must_be_constant(&s)),
            };
            arg_vec.remove(0);
            globals.store[class_id].set_name(&format!("Struct::{s}"));
        }
    };
    globals.define_builtin_func(class_id, "initialize", initialize);
    globals.define_builtin_func(class_id, "inspect", inspect);
    globals.define_builtin_func(class_id, "to_s", inspect);
    globals.define_builtin_class_inline_func(
        class_id,
        "[]",
        new,
        super::class::inline_class_new,
        analysis::v_v_vv,
    );
    globals.define_builtin_class_inline_func(
        class_id,
        "new",
        new,
        super::class::inline_class_new,
        analysis::v_v_vv,
    );

    for arg in &arg_vec {
        let name = arg.expect_symbol_or_string(globals)?;
        globals.define_attr_reader(class_id, name, Visibility::Public);
        globals.define_attr_writer(class_id, name, Visibility::Public);
    }
    class.set_instance_var(globals, "/members", Value::array_from_vec(arg_vec))?;

    if let Some(bh) = lfp.block() {
        vm.push_class_context(class_id);
        let data = globals.get_block_data(vm.cfp(), bh);
        vm.invoke_block_with_self(globals, &data, class, &[class])?;
        vm.pop_class_context();
    };
    Ok(class)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    super::class::__new(vm, globals, lfp, arg)
}

#[monoruby_builtin]
fn initialize(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let self_val = lfp.self_val();
    let struct_class = self_val.class().get_obj(globals);
    let members_val = globals
        .get_ivar(struct_class, IdentId::get_id("/members"))
        .unwrap();
    let members: Array = members_val.into();
    if members.len() < len {
        return Err(MonorubyErr::argumenterr("Struct size differs."));
    };
    for (i, val) in lfp.iter().enumerate() {
        let id = members[i].try_symbol().unwrap();
        let ivar_name = IdentId::add_ivar_prefix(id);
        globals.set_ivar(self_val, ivar_name, val)?;
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
    let mut inspect = format!("#<struct ");
    let self_val = lfp.self_val();
    let class_id = self_val.class();
    let struct_class = class_id.get_obj(globals);
    if let Some(name) = globals.store[class_id].get_name_id() {
        inspect += &format!("{name}");
    };
    let name: Array = globals
        .get_ivar(struct_class, IdentId::get_id("/members"))
        .unwrap()
        .into();

    if name.len() != 0 {
        for x in name.iter() {
            let name = x.try_symbol().unwrap();
            let ivar_name = IdentId::add_ivar_prefix(name);
            let val = match globals.get_ivar(self_val, ivar_name) {
                Some(v) => globals.inspect(v),
                None => "nil".to_string(),
            };
            inspect += &format!(" {:?}={},", name, val);
        }
        inspect.pop();
    }
    inspect += ">";

    Ok(Value::string(inspect))
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
    fn struct_inspect() {
        let code = r###"
        S = Struct.new(:a,:b)
        s = S.new(100,200)
        [s.a, s.b, s.inspect] 
        "###;
        run_test(code);
    }
}
