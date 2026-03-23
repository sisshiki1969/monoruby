use super::*;

//
// Symbol class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Symbol", SYMBOL_CLASS, None);
    globals.define_builtin_class_func(SYMBOL_CLASS, "allocate", super::class::undef_allocate, 0);
    globals.define_builtin_class_func(SYMBOL_CLASS, "all_symbols", all_symbols, 0);
    globals.define_builtin_func(SYMBOL_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "===", eq, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "==", eq, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "!=", ne, 1);
}

///
/// ### Symbol.all_symbols
///
/// - Symbol.all_symbols -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/s/all_symbols.html]
#[monoruby_builtin]
fn all_symbols(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let symbols: Vec<Value> = IdentId::all_symbols()
        .into_iter()
        .map(Value::symbol)
        .collect();
    Ok(Value::array_from_vec(symbols))
}

///
/// ### Symbol#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::from_ord(lhs.compare(&rhs))),
        (_, None) => Ok(Value::nil()),
    }
}

///
/// ### Symbol#==
///
/// - self == other -> true | false
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::bool(lhs == rhs)),
        (_, None) => Ok(Value::bool(false)),
    }
}

#[monoruby_builtin]
fn ne(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::bool(lhs != rhs)),
        (_, None) => Ok(Value::bool(true)),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    #[test]
    fn symbol_cmp() {
        run_binop_tests2(
            &[":aaa", ":xxx", ":妖精"],
            &["<=>", "===", "==", "!="],
            &[":aaa", ":xxx", "nil", "3"],
        );
    }

    #[test]
    fn symbol_match() {
        run_test(r#":hello.match?(/ell/)"#);
        run_test(r#":hello.match?(/xyz/)"#);
        run_test(r#":hello.match?(/\A[a-z]+\z/)"#);
    }

    #[test]
    fn symbol_methods() {
        run_test(r#":hello.upcase"#);
        run_test(r#":hello.downcase"#);
        run_test(r#":hello.capitalize"#);
        run_test(r#":hElLo.swapcase"#);
        run_test(r#":hello.size"#);
        run_test(r#":hello.length"#);
        run_test(r#":hello.empty?"#);
        run_test(r#":"".empty?"#);
        run_test(r#":hello.start_with?("he")"#);
        run_test(r#":hello.end_with?("lo")"#);
        run_test(r#":hello.succ"#);
        run_test(r#":hello.next"#);
        run_test(r#":hello.id2name"#);
        run_test(r#":hello.name"#);
        run_test(r#":hello.name.frozen?"#);
        run_test(r#":hello.intern"#);
        run_test(r#":hello.to_sym"#);
        run_test(r#":hello =~ /ell/"#);
        run_test(r#":hello[1]"#);
        run_test(r#":hello[1,3]"#);
        run_test(r#":abc.casecmp(:ABC)"#);
        run_test(r#":abc.casecmp(:abd)"#);
        run_test(r#":abc.casecmp?(:ABC)"#);
        run_test(r#":abc.casecmp("abc")"#);
    }

    #[test]
    fn symbol_all_symbols() {
        run_test(r#"Symbol.all_symbols.is_a?(Array)"#);
        run_test(r#"Symbol.all_symbols.all? { |s| s.is_a?(Symbol) }"#);
        run_test(r#"Symbol.all_symbols.include?(:to_s)"#);
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
