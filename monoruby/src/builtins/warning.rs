use super::*;

//
// Warning module
//
// The category switches live in `Globals` as a bit set
// (`WarningCategory`), so the runtime's own gating — chilled-string
// mutation, `deprecate_constant`, the unused-block check — reads a flag
// instead of dispatching a Ruby method. Only the accessors are defined
// here; `Warning.warn` stays in `builtins/warning.rb` because it is meant
// to be overridden from Ruby.
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Warning").id();
    globals.define_builtin_module_func(klass, "[]", index, 1);
    globals.define_builtin_module_func(klass, "[]=", index_assign, 2);
    globals.define_builtin_module_func(klass, "categories", categories, 0);
}

/// The category named by a `Warning[]` / `Warning[]=` argument, with
/// CRuby's errors: a non-Symbol is a TypeError, an unknown name an
/// ArgumentError.
fn category_arg(globals: &Globals, val: Value) -> Result<WarningCategory> {
    let Some(sym) = val.try_symbol() else {
        return Err(MonorubyErr::wrong_argument_type(&globals.store, val, "Symbol"));
    };
    let name = sym.get_name();
    WarningCategory::from_name(&name)
        .ok_or_else(|| MonorubyErr::argumenterr(format!("unknown category: {name}")))
}

///
/// ### Warning.[]
///
/// - Warning[category] -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Warning/s/=5b=5d.html]
#[monoruby_builtin]
fn index(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let category = category_arg(globals, lfp.arg(0))?;
    Ok(Value::bool(globals.warning_category_enabled(category)))
}

///
/// ### Warning.[]=
///
/// - Warning[category] = flag -> flag
///
/// [https://docs.ruby-lang.org/ja/latest/method/Warning/s/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let category = category_arg(globals, lfp.arg(0))?;
    let flag = lfp.arg(1);
    globals.set_warning_category(category, flag.as_bool());
    Ok(flag)
}

///
/// ### Warning.categories
///
/// - categories -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Warning/s/categories.html]
#[monoruby_builtin]
fn categories(_vm: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let v = WarningCategory::ALL
        .into_iter()
        .map(|c| Value::symbol_from_str(c.name()))
        .collect();
    Ok(Value::array_from_vec(v))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn warning_categories() {
        run_test_once(
            r#"
            res = []
            res << Warning.categories
            res << Warning[:deprecated] << Warning[:experimental]
            res << Warning[:performance] << Warning[:strict_unused_block]
            res << (Warning[:deprecated] = true)
            res << Warning[:deprecated]
            res << (Warning[:deprecated] = nil)
            res << Warning[:deprecated]
            res << (Warning[:experimental] = false)
            res << Warning[:experimental]
            Warning[:experimental] = 1
            res << Warning[:experimental]
            res
        "#,
        );
        run_test_error("Warning[42]");
        run_test_error(r#"Warning["deprecated"]"#);
        run_test_error("Warning[:unknown]");
        run_test_error("Warning[:unknown] = true");
        run_test_error("Warning[nil] = true");
    }

    #[test]
    fn warning_category_gates_runtime_warnings() {
        // The runtime reads the flag itself, so a Ruby-level override of
        // `Warning.[]` must not change what gets printed (CRuby reads the
        // C-level bit the same way).
        run_test_once(
            r#"
            require "stringio"
            out = []
            [true, false].each do |on|
              Warning[:deprecated] = on
              old = $stderr
              $stderr = StringIO.new
              begin
                s = :sym.to_s
                s << "y"
                out << $stderr.string.include?("will be frozen")
              ensure
                $stderr = old
              end
            end
            out
        "#,
        );
    }
}
