use super::*;

//
// Binding class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Binding", BINDING_CLASS, ObjTy::BINDING);
    globals.store[BINDING_CLASS].clear_alloc_func();
    globals.define_builtin_func(BINDING_CLASS, "local_variables", local_variables, 0);
    globals.define_builtin_func(BINDING_CLASS, "source_location", source_location, 0);
    globals.define_builtin_func(BINDING_CLASS, "receiver", receiver, 0);
    globals.define_builtin_func(
        BINDING_CLASS,
        "local_variable_defined?",
        local_variable_defined,
        1,
    );
    globals.define_builtin_func(BINDING_CLASS, "local_variable_get", local_variable_get, 1);
    globals.define_builtin_funcs_with_effect(
        BINDING_CLASS,
        "local_variable_set",
        &[],
        local_variable_set,
        2,
        2,
        false,
        Effect::EVAL,
    );
    globals.define_builtin_funcs_with_effect(
        BINDING_CLASS,
        "eval",
        &[],
        eval,
        1,
        3,
        false,
        Effect::EVAL,
    );
}

///
/// ### Binding#receiver
///
/// - receiver -> Object
///
/// Returns the receiver (`self`) of the frame the binding captured.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/receiver.html]
#[monoruby_builtin]
fn receiver(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_binding_inner().self_val())
}

#[monoruby_builtin]
fn source_location(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let binding = self_val.as_binding_inner();
    let fid = binding.outer_fid();
    if let Some(pc) = binding.pc {
        if let Some(iseq) = globals.store[fid].is_iseq() {
            let iseq_info = &globals.store[iseq];
            let top_pc = iseq_info.get_top_pc();
            if pc.as_ptr() as usize >= top_pc.as_ptr() as usize {
                let bc_index = pc - top_pc;
                if bc_index.to_usize() < iseq_info.sourcemap.len() {
                    let loc = iseq_info.sourcemap[bc_index.to_usize()];
                    let file_name =
                        Value::string(iseq_info.sourceinfo.file_name().into_owned());
                    let line = Value::integer(iseq_info.sourceinfo.get_line(&loc) as i64);
                    return Ok(Value::array2(file_name, line));
                }
            }
        }
    }
    // pc is not within outer_fid's ISeq (e.g. block captured as &param).
    // Fall back to the outer_lfp's ISeq start position.
    let fid = if let Some(fid) = binding.func_id() {
        fid
    } else {
        binding.outer_fid()
    };
    let iseq = globals.store.iseq(fid);
    let file_name = Value::string(iseq.sourceinfo.file_name().into_owned());
    let line = Value::integer(iseq.sourceinfo.get_line(&iseq.loc) as i64);
    Ok(Value::array2(file_name, line))
}

///
/// ### Binding#eval
///
/// - eval(expr, fname = "(eval)", lineno = 1) -> object
///
/// Evaluates `expr` in the binding's lexical context. Equivalent to
/// `Kernel#eval(expr, self)` but without the binding-typecheck (and
/// with the call-site location as the default `fname`, matching
/// CRuby).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/eval.html]
#[monoruby_builtin]
fn eval(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let expr = lfp.arg(0).coerce_to_string(vm, globals)?;
    let cfp = vm.cfp();
    let caller_cfp = cfp.prev().unwrap();
    let fname = if let Some(f) = lfp.try_arg(1) {
        f.coerce_to_str(vm, globals)?
    } else {
        let caller_loc = globals.store.get_caller_loc(caller_cfp, Some(pc));
        format!("(eval at {})", caller_loc)
    };
    let lineno: i64 = if let Some(l) = lfp.try_arg(2) {
        l.coerce_to_int_i64(vm, globals)?
    } else {
        1
    };
    // The receiver IS the binding — wrap it through `Binding::try_new`
    // so the rest of the path is identical to `Kernel#eval`'s
    // binding-form. The cast can't fail; `Module#define_builtin_func`
    // guarantees `lfp.self_val()` matches `BINDING_CLASS`.
    let binding = Binding::try_new(lfp.self_val()).expect("self is Binding");
    globals.compile_script_binding(expr, fname, binding, lineno)?;
    vm.invoke_binding(globals, binding.binding().unwrap())
}

#[monoruby_builtin]
fn local_variables(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let binding = self_val.as_binding_inner();
    let fid = if let Some(fid) = binding.func_id() {
        fid
    } else {
        binding.outer_fid()
    };
    let v = globals.store.local_variables(globals.store[fid].as_iseq());
    Ok(Value::array_from_vec(v))
}

///
/// ### Binding#local_variable_defined?
///
/// - local_variable_defined?(symbol) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/local_variable_defined=3f.html]
#[monoruby_builtin]
fn local_variable_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let name = arg_to_local_name(arg, vm, globals)?
        .ok_or_else(|| name_not_local_err(arg, &globals.store))?;
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    Ok(Value::bool(
        lookup_local_in_binding(globals, inner, name).is_some(),
    ))
}

///
/// ### Binding#local_variable_get
///
/// - local_variable_get(symbol) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/local_variable_get.html]
#[monoruby_builtin]
fn local_variable_get(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let name = arg_to_local_name(arg, vm, globals)?
        .ok_or_else(|| name_not_local_err(arg, &globals.store))?;
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    match lookup_local_in_binding(globals, inner, name) {
        Some((host, slot)) => Ok(host.register(slot).unwrap_or_default()),
        None => Err(MonorubyErr::nameerr_with_name(
            format!(
                "local variable `{}' is not defined for {}",
                name,
                self_val.to_s(&globals.store)
            ),
            name,
        )),
    }
}

///
/// ### Binding#local_variable_set
///
/// - local_variable_set(symbol, obj) -> obj
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/local_variable_set.html]
#[monoruby_builtin]
fn local_variable_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let name = arg_to_local_name(arg, vm, globals)?
        .ok_or_else(|| name_not_local_err(arg, &globals.store))?;
    let val = lfp.arg(1);
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    if let Some((mut host, slot)) = lookup_local_in_binding(globals, inner, name) {
        // SAFETY: `slot` came from this iseq's `locals` table, so it is
        // a valid register slot for `host`.
        unsafe { host.set_register(slot, Some(val)) };
        return Ok(val);
    }
    // Not yet bound — introduce the local in the binding's eval scope by
    // compiling a stub `<name> = nil`, then write the actual value.
    let binding = Binding::try_new(self_val).expect("self is Binding");
    let stub = format!("{} = nil", name);
    globals.compile_script_binding(stub, "(local_variable_set)", binding, 1)?;
    vm.invoke_binding(globals, binding.binding().unwrap())?;
    let inner = self_val.as_binding_inner();
    let (mut host, slot) = lookup_local_in_binding(globals, inner, name)
        .expect("stub eval registered the new local");
    // SAFETY: see above.
    unsafe { host.set_register(slot, Some(val)) };
    Ok(val)
}

/// Coerce a `local_variable_*` argument to an interned name. Returns
/// `Ok(None)` when the value parses to something that cannot be a local
/// variable (e.g. `:$0`, `:@x`, `:Foo`); callers decide whether that
/// becomes `false` (defined?) or a NameError (get/set).
fn arg_to_local_name(
    arg: Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<IdentId>> {
    let name = if let Some(sym) = arg.try_symbol() {
        sym
    } else {
        let s = arg.coerce_to_string(vm, globals)?;
        IdentId::get_id(&s)
    };
    let s = name.get_name();
    if is_valid_local_name(&s) {
        Ok(Some(name))
    } else {
        Ok(None)
    }
}

fn name_not_local_err(arg: Value, store: &Store) -> MonorubyErr {
    MonorubyErr::nameerr(format!(
        "wrong local variable name `{}' for {}",
        arg.to_s(store),
        "Binding"
    ))
}

fn is_valid_local_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    let valid_first = first.is_ascii_lowercase() || first == '_' || !first.is_ascii();
    if !valid_first {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || !c.is_ascii())
}

/// Walk the lfp chain captured by the binding looking for `name` in
/// each iseq's `locals` table. Returns the lfp that hosts the slot and
/// the slot index, or `None` if the name is not bound anywhere along
/// the chain.
fn lookup_local_in_binding(
    globals: &Globals,
    inner: &BindingInner,
    name: IdentId,
) -> Option<(Lfp, SlotId)> {
    let mut lfp = match inner.binding() {
        Some(l) => l,
        None => inner.outer_lfp(),
    };
    loop {
        let fid = lfp.func_id();
        if let Some(iseq_id) = globals.store[fid].is_iseq() {
            let iseq = &globals.store[iseq_id];
            if let Some(bc_local) = iseq.locals.get(&name) {
                return Some((lfp, SlotId(1 + bc_local.0)));
            }
        }
        match lfp.outer() {
            Some(outer) => lfp = outer,
            None => return None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn binding_dup_clone() {
        // Cloning a Binding RValue previously hit
        // `unreachable!("BINDING")` and aborted (ERB dups `binding`).
        run_test(
            r#"
            x = 41
            b = binding
            b2 = b.dup
            b3 = b.clone
            [b2.class.name, b2.eval("x"), b3.eval("x + 1"),
             b2.local_variable_get(:x)]
            "#,
        );
        run_test(
            r#"
            def m
              y = 7
              binding
            end
            b = m.dup
            [b.eval("y"), b.class == Binding]
            "#,
        );
    }

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
    fn binding_new2() {
        run_test(
            r#"
        a = 1
        b = 2
        def f(x, &block)
          z = nil
          1.times do
            z = 10
            1.times do
              a = 100
              b = 100
              z = binding
            end
            return z
          end
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

    #[test]
    fn binding_eval2() {
        run_test_with_prelude(
            r#"
        $b = binding
        a = nil
        foo
        a.inspect
        "#,
            r#"
        def foo 
          eval("a = 1", $b)
        end
        "#,
        )
    }

    #[test]
    fn binding_eval3() {
        run_test_with_prelude(
            r#"
        eval("$b = binding")
        a = nil
        foo
        a.inspect
        "#,
            r#"
        def foo
          eval("a = 1", $b)
        end
        "#,
        )
    }

    #[test]
    fn binding_eval4() {
        run_test_with_prelude(
            r#"
        res = []
        100.times do
          x = 1
          1.times do |b|
            $b = binding
          end
          x = 100
          bar
          res << x
        end
        res
        "#,
            r#"
        def bar
          eval("x += 1", $b)
        end
            "#,
        );
    }

    #[test]
    fn binding_source_location() {
        // source_location returns [String, Integer]
        run_test(
            r#"
        b = binding
        sl = b.source_location
        [sl.is_a?(Array), sl.size == 2, sl[0].is_a?(String), sl[1].is_a?(Integer)]
        "#,
        );
        // source_location line matches the binding call line
        run_test(
            r#"
        line = __LINE__; b = binding
        b.source_location[1] == line
        "#,
        );
        // binding inside a block returns the block's line
        run_test(
            r#"
        b = nil
        line = nil
        1.times do
          line = __LINE__; b = binding
        end
        b.source_location[1] == line
        "#,
        );
    }

    #[test]
    fn binding_eval_method_basic() {
        // `Binding#eval(expr)` returns the expression's value, like
        // `Kernel#eval(expr, self)` but with the binding-typecheck
        // already satisfied.
        run_test(
            r#"
            b = binding
            b.eval("1 + 1")
            "#,
        );
    }

    #[test]
    fn binding_eval_method_captures_locals() {
        // Locals captured at the binding-creation site are visible.
        run_test(
            r#"
            def get_binding
              x = 100
              binding
            end
            get_binding.eval("x + 1")
            "#,
        );
    }

    #[test]
    fn binding_eval_method_mutates_outer_local() {
        // Assignments inside the eval write through to the outer
        // local that the binding captured.
        run_test(
            r#"
            x = 1
            binding.eval("x = 42")
            x
            "#,
        );
    }

    #[test]
    fn binding_eval_method_introduces_new_local() {
        // A `=` in the eval that targets a name not present in the
        // captured locals introduces it for subsequent evals on the
        // same binding (eval-only scope, doesn't leak to the
        // surrounding method).
        run_test(
            r#"
            b = binding
            b.eval("y = 7")
            b.eval("y * 2")
            "#,
        );
    }

    #[test]
    fn binding_eval_method_uses_explicit_filename_and_lineno() {
        // The optional `fname` / `lineno` args drive the backtrace
        // location so external tooling can attribute the error to
        // the source string's origin. We compare only the
        // `file:line:` prefix because monoruby and CRuby format the
        // trailing label differently (`/main` vs `<main>`) — a
        // pre-existing labelling quirk unrelated to Binding#eval.
        run_test(
            r#"
            b = binding
            begin
              b.eval("missing_method", "myfile.rb", 42)
            rescue NameError => e
              e.backtrace.first[/\Amyfile\.rb:42:/]
            end
            "#,
        );
    }

    #[test]
    fn binding_eval_method_arity_error() {
        // Calling without args matches CRuby's arity range message.
        run_test_error(
            r#"
            binding.eval
            "#,
        );
    }

    #[test]
    fn binding_local_variable_defined() {
        run_test(
            r#"
            x = 1
            b = binding
            [
              b.local_variable_defined?(:x),
              b.local_variable_defined?(:y),
              b.local_variable_defined?("x"),
              b.local_variable_defined?("y"),
            ]
            "#,
        );
    }

    #[test]
    fn binding_local_variable_defined_invalid_name() {
        // Names that can't be locals (`:$0`, `:@x`, `:Foo`, …) raise
        // NameError, matching CRuby's `Binding#local_variable_defined?`.
        run_test_error(
            r#"
            binding.local_variable_defined?(:$0)
            "#,
        );
        run_test_error(
            r#"
            binding.local_variable_defined?(:@x)
            "#,
        );
        run_test_error(
            r#"
            binding.local_variable_defined?(:Foo)
            "#,
        );
    }

    #[test]
    fn binding_local_variable_defined_after_eval() {
        run_test(
            r#"
            b = binding
            b.eval("z = 1")
            b.local_variable_defined?(:z)
            "#,
        );
    }

    #[test]
    fn binding_local_variable_get() {
        run_test(
            r#"
            x = 42
            binding.local_variable_get(:x)
            "#,
        );
        run_test(
            r#"
            x = 42
            binding.local_variable_get("x")
            "#,
        );
    }

    #[test]
    fn binding_local_variable_get_missing() {
        // Asking for an unbound name raises NameError.
        run_test_error(
            r#"
            binding.local_variable_get(:no_such)
            "#,
        );
        // Reserved-prefix names are rejected as not-a-local.
        run_test_error(
            r#"
            binding.local_variable_get(:$0)
            "#,
        );
    }

    #[test]
    fn binding_local_variable_set_existing() {
        // Writes through to the outer local the binding captured.
        run_test(
            r#"
            x = 1
            binding.local_variable_set(:x, 42)
            x
            "#,
        );
    }

    #[test]
    fn binding_local_variable_set_introduces_new_local() {
        // Setting a name not already bound creates it in the binding's
        // eval scope (does not leak to the surrounding method).
        run_test(
            r#"
            b = binding
            b.local_variable_set(:foo, 7)
            [b.local_variable_get(:foo), b.eval("foo"), b.local_variables.include?(:foo)]
            "#,
        );
    }

    #[test]
    fn binding_local_variable_set_string_name() {
        run_test(
            r#"
            b = binding
            b.local_variable_set("bar", 99)
            b.local_variable_get(:bar)
            "#,
        );
    }

    #[test]
    fn binding_local_variable_set_invalid_name() {
        // `:$0` is not a local-variable name → NameError on set.
        run_test_error(
            r#"
            binding.local_variable_set(:$0, "x")
            "#,
        );
    }
}
