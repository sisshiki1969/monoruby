use super::*;

//
// Binding class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Binding", BINDING_CLASS, ObjTy::BINDING);
    globals.define_builtin_func(BINDING_CLASS, "local_variables", local_variables, 0);
}

#[monoruby_builtin]
fn local_variables(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let binding = self_val.as_binding_inner();
    let fid = if let Some(fid) = binding.func_id() {
        fid
    } else {
        binding.outer_lfp().meta().func_id()
    };
    let v = globals.store.iseq(fid).local_variables();
    Ok(Value::array_from_vec(v))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn binding_new() {
        run_test(
            r#"
        a = 1
        b = 2
        def f(x, &block)
          z = nil
          1.times do |x|
            a = 100
            b = 100
            z = binding
          end
          z
        end
        f(42){}.local_variables.sort
        "#,
        );
    }

    #[test]
    fn binding_eval() {
        run_test(
            r#"
        def get_binding(str)
            binding
        end
        str = "hello"
        p = []
        p << eval("str + ' Fred'")                      #=> "hello Fred"
        p << eval("str + ' Fred'", get_binding("bye"))  #=> "bye Fred"
        p
        "#,
        );
    }
}
