use crate::*;

pub(crate) fn init(globals: &mut Globals) {
    let module = globals.define_class_by_str("Struct", OBJECT_CLASS.get_obj(globals), OBJECT_CLASS);
    globals.define_builtin_class_func(module.id(), "new", struct_new, -1);
}

#[monoruby_builtin]
fn struct_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let self_val = lfp.self_val();
    Executor::check_min_number_of_arguments(len, 1)?;
    let mut arg_vec = arg.to_vec(len);

    let mut class = globals.new_unnamed_class(Some(self_val.as_class()));
    let class_id = class.as_class_id();
    match arg[0].is_string() {
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
    globals.define_builtin_func(class_id, "initialize", initialize, -1);
    /*globals.define_builtin_func(class_id, "inspect", inspect, -1);*/
    globals.define_builtin_class_func(class_id, "[]", new, -1);
    globals.define_builtin_class_func(class_id, "new", new, -1);

    for arg in &arg_vec {
        let name = arg.expect_symbol_or_string(globals)?;
        globals.define_attr_reader(class_id, name, Visibility::Public);
        globals.define_attr_writer(class_id, name, Visibility::Public);
    }
    class.set_instance_var(globals, "/members", Value::new_array_from_vec(arg_vec))?;

    if let Some(block) = lfp.block() {
        vm.push_class_context(class_id);
        let data = vm.get_block_data(globals, block);
        vm.invoke_block_with_self(globals, data, class, &[class])?;
        vm.pop_class_context();
    };
    Ok(class)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    super::class::__new(vm, globals, lfp, arg, len)
}

#[monoruby_builtin]
fn initialize(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let struct_class = self_val.class().get_obj(globals).as_val();
    let members_val = globals
        .get_ivar(struct_class, IdentId::get_id("/members"))
        .unwrap();
    let members = members_val.as_array();
    if members.len() < len {
        return Err(MonorubyErr::argumenterr("Struct size differs.".to_string()));
    };
    for (i, val) in arg.iter(len).enumerate() {
        let id = members[i].as_symbol();
        let var = format!("@{}", id);
        globals.set_ivar(self_val, IdentId::get_id_from_string(var), val)?;
    }
    Ok(Value::nil())
}
/*
use std::borrow::Cow;
#[monoruby_builtin]
fn inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let mut inspect = format!("#<struct ");
    match vm.globals.get_class(self_val).op_name() {
        Some(name) => inspect += &name,
        None => {}
    };
    let name = match vm
        .globals
        .get_class(self_val)
        .get_var(IdentId::get_id("/members"))
    {
        Some(name) => name,
        None => return Err(RubyError::internal("No /members.")),
    };
    //eprintln!("{:?}", name);
    let members = match name.as_array() {
        Some(aref) => aref,
        None => {
            return Err(RubyError::internal(format!(
                "Illegal _members value. {:?}",
                name
            )))
        }
    };

    for x in &**members {
        let id = x.as_symbol().unwrap().add_prefix("@");
        let val = match self_val.get_var(id) {
            Some(v) => Cow::from(vm.val_inspect(v)?),
            None => Cow::from("nil"),
        };
        inspect = format!("{} {:?}={}", inspect, id, val);
    }
    inspect += ">";

    Ok(Value::string(inspect))
}
*/

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
        [s.a, s.b] # s.inspect 
        "###;
        run_test(code);
    }
}
