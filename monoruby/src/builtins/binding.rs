use super::*;

//
// Binding class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Binding", BINDING_CLASS);
    globals.define_builtin_func(BINDING_CLASS, "local_variables", local_variables, 0);
}

#[monoruby_builtin]
fn local_variables(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let binding_lfp = lfp.self_val().as_binding().binding();
    let v: Vec<Value> = dbg!(&globals[binding_lfp.meta().func_id()])
        .as_ruby_func()
        .locals
        .keys()
        .map(|id| Value::symbol(*id))
        .collect();
    Ok(Value::array_from_vec(v))
}

#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn binding_new() {
        run_test(
            r#"
        a = 1
        b = 2
        def f(x)
          a = 100
          b = 100
          binding
        end
        f(42).local_variables
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
