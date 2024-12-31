use super::*;

//
// main object
//

pub(super) fn init(globals: &mut Globals) {
    let main = globals.main_object;
    globals.define_builtin_singleton_func_with(main, "include", include, 0, 0, true);
}

///
/// ### main.include
/// - include(*mod) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Module/i/include.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.len() == 0 {
        return Err(MonorubyErr::wrong_number_of_arg_min(0, 1));
    }
    let class = globals.store.object_class();
    for v in args.iter().cloned().rev() {
        globals.include_module(class, v.expect_module(globals)?)?;
    }
    Ok(class.as_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn include() {
        run_test(
            r#"
            include Math
        "#,
        );
    }
}
