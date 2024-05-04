use super::*;

//
// Symbol class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Symbol", SYMBOL_CLASS);
    globals.define_builtin_func(SYMBOL_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "===", teq, 1);
}

///
/// ### Symbol#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::from_ord(lhs.compare(&rhs))),
        (_, None) => Ok(Value::nil()),
    }
}

///
/// ### Object#===
///
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::bool(lhs == rhs)),
        (_, None) => Ok(Value::bool(false)),
    }
}

#[cfg(test)]
mod test {
    use crate::tests::*;
    #[test]
    fn symbol_cmp() {
        run_binop_tests2(
            &[":aaa", ":xxx", ":妖精"],
            &["<=>"],
            &[":aaa", ":xxx", "nil", "3"],
        );
        run_binop_tests2(
            &[":aaa", ":xxx", ":妖精"],
            &["==="],
            &[":aaa", ":xxx", "nil", "3"],
        );
    }

    #[test]
    fn symbol_to_proc() {
        run_test(
            r#"
        :to_i.to_proc.call("42")
        "#,
        );
        run_test(
            r#"
        :to_i.to_proc["ff", 16]
        "#,
        );
        run_test(
            r#"
        (1..3).collect(&:to_s)
        "#,
        );
    }
}
