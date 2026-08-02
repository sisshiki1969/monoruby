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
    globals.define_builtin_func(BINDING_CLASS, "implicit_parameters", implicit_parameters, 0);
    globals.define_builtin_func(
        BINDING_CLASS,
        "implicit_parameter_defined?",
        implicit_parameter_defined,
        1,
    );
    globals.define_builtin_func(
        BINDING_CLASS,
        "implicit_parameter_get",
        implicit_parameter_get,
        1,
    );
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
    let src_encoding = crate::builtins::eval_src_encoding(lfp.arg(0));
    let expr = crate::builtins::eval_source_bytes(vm, globals, lfp.arg(0))?;
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
    globals.compile_script_binding(expr, fname, binding, lineno, src_encoding)?;
    vm.flush_compile_warnings(globals);
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
    if let Some(err) = numbered_param_error(name) {
        return Err(err);
    }
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    if name.get_name() == "it" && it_is_implicit_param(globals, inner) {
        return Ok(Value::bool(false));
    }
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
    if let Some(err) = numbered_param_error(name) {
        return Err(err);
    }
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    // The implicit `it` parameter is not a local variable — report it as
    // undefined rather than returning the parameter's value.
    let it_implicit = name.get_name() == "it" && it_is_implicit_param(globals, inner);
    if !it_implicit
        && let Some((host, slot)) = lookup_local_in_binding(globals, inner, name)
    {
        return Ok(host.register(slot).unwrap_or_default());
    }
    Err(MonorubyErr::nameerr_with_name(
        format!(
            "local variable '{}' is not defined for {}",
            name,
            self_val.to_s(&globals.store)
        ),
        name,
    ))
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
    if let Some(err) = numbered_param_error(name) {
        return Err(err);
    }
    let val = lfp.arg(1);
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    // Assigning `:it` when `it` is the implicit block parameter does not
    // touch the parameter (it is not a real local); match CRuby by leaving
    // it unchanged and returning the value.
    if name.get_name() == "it" && it_is_implicit_param(globals, inner) {
        return Ok(val);
    }
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
    globals.compile_script_binding(stub.into_bytes(), "(local_variable_set)", binding, 1, None)?;
    vm.invoke_binding(globals, binding.binding().unwrap())?;
    let inner = self_val.as_binding_inner();
    let (mut host, slot) = lookup_local_in_binding(globals, inner, name)
        .expect("stub eval registered the new local");
    // SAFETY: see above.
    unsafe { host.set_register(slot, Some(val)) };
    Ok(val)
}

///
/// ### Binding#implicit_parameters
///
/// - implicit_parameters -> [Symbol]
///
/// The implicit block parameters of the scope this binding captured:
/// `[:it]`, `[:_1, .., :_N]` up to the highest numbered parameter that
/// scope references, or `[]`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/implicit_parameters.html]
#[monoruby_builtin]
fn implicit_parameters(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let names = implicit_params_of(globals, self_val.as_binding_inner());
    Ok(Value::array_from_iter(names.into_iter().map(Value::symbol)))
}

///
/// ### Binding#implicit_parameter_defined?
///
/// - implicit_parameter_defined?(symbol) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/implicit_parameter_defined=3f.html]
#[monoruby_builtin]
fn implicit_parameter_defined(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let name = arg_to_implicit_name(arg, vm, globals)?;
    if !is_implicit_param_name(name) {
        return Err(MonorubyErr::nameerr_with_name(
            format!("'{name}' is not an implicit parameter"),
            name,
        ));
    }
    let self_val = lfp.self_val();
    let defined = implicit_params_of(globals, self_val.as_binding_inner()).contains(&name);
    Ok(Value::bool(defined))
}

///
/// ### Binding#implicit_parameter_get
///
/// - implicit_parameter_get(symbol) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Binding/i/implicit_parameter_get.html]
#[monoruby_builtin]
fn implicit_parameter_get(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let name = arg_to_implicit_name(arg, vm, globals)?;
    if !is_implicit_param_name(name) {
        return Err(MonorubyErr::nameerr_with_name(
            format!("'{name}' is not an implicit parameter"),
            name,
        ));
    }
    let self_val = lfp.self_val();
    let inner = self_val.as_binding_inner();
    if implicit_params_of(globals, inner).contains(&name) {
        // An implicit parameter occupies an ordinary local slot of the
        // scope that declares it, so the plain lookup finds it — and the
        // membership test above is what keeps a parent scope's parameter
        // (which the same lookup would walk out to) from answering here.
        if let Some((host, slot)) = lookup_local_in_binding(globals, inner, name) {
            return Ok(host.register(slot).unwrap_or_default());
        }
    }
    Err(MonorubyErr::nameerr_with_name(
        format!(
            "implicit parameter '{}' is not defined for {}",
            name,
            self_val.to_s(&globals.store)
        ),
        name,
    ))
}

/// The implicit block parameters of the scope `inner` was captured in.
///
/// Nothing is walked: a parent scope's implicit parameters are not this
/// scope's, and neither are a nested block's. The answer is a static
/// property of the block — the parameters exist for the whole scope even
/// if the reference that created them comes later in it.
fn implicit_params_of(globals: &Globals, inner: &BindingInner) -> Vec<IdentId> {
    let Some(iseq_id) = globals.store[inner.outer_fid()].is_iseq() else {
        return vec![];
    };
    let params = &globals.store[iseq_id].args;
    if params.it_param() {
        return vec![IdentId::get_id("it")];
    }
    // Numbered parameters are synthesized as ordinary leading parameters
    // named `_1`.., and `_1`.. is reserved syntax, so a parameter can
    // only carry such a name by being one.
    let mut out = vec![];
    for i in 1..=9u32 {
        let name = IdentId::get_id(&format!("_{i}"));
        if params.args_names.get(i as usize - 1) != Some(&Some(name)) {
            break;
        }
        out.push(name);
    }
    out
}

/// `it` and `_1`..`_9` are the only names an implicit parameter can have;
/// anything else is a NameError rather than a plain `false`.
fn is_implicit_param_name(name: IdentId) -> bool {
    let s = name.get_name();
    if s == "it" {
        return true;
    }
    let b = s.as_bytes();
    b.len() == 2 && b[0] == b'_' && (b'1'..=b'9').contains(&b[1])
}

/// Coerce an `implicit_parameter_defined?` argument to an interned name.
/// A Symbol, a String, or anything with `#to_str`; everything else is a
/// TypeError naming the offending object.
fn arg_to_implicit_name(arg: Value, vm: &mut Executor, globals: &mut Globals) -> Result<IdentId> {
    if let Some(sym) = arg.try_symbol() {
        return Ok(sym);
    }
    if arg.is_str().is_some() || globals.check_method(arg, IdentId::get_id("to_str")).is_some() {
        let s = arg.coerce_to_str(vm, globals)?;
        return Ok(IdentId::get_id(&s));
    }
    Err(MonorubyErr::typeerr(format!(
        "{} is not a symbol nor a string",
        arg.inspect(&globals.store)
    )))
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
    // Same coercion — and the same TypeError — as the implicit-parameter
    // accessors: CRuby names the offending object rather than reporting a
    // generic conversion failure.
    let name = arg_to_implicit_name(arg, vm, globals)?;
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

/// Whether `it` resolves to the implicit block parameter (Ruby 3.4) in this
/// binding's scope — as opposed to a real `it = ...` local. The implicit
/// parameter is not a reportable/settable local variable, so
/// `Binding#local_variable_*` treat it as absent. Walks to the iseq that
/// hosts `it` and reports whether that iseq takes the implicit parameter.
fn it_is_implicit_param(globals: &Globals, inner: &BindingInner) -> bool {
    let it = IdentId::get_id("it");
    let mut lfp = match inner.binding() {
        Some(l) => l,
        None => inner.outer_lfp(),
    };
    loop {
        let fid = lfp.func_id();
        if let Some(iseq_id) = globals.store[fid].is_iseq() {
            let iseq = &globals.store[iseq_id];
            if iseq.locals.get(&it).is_some() {
                return iseq.args.it_param();
            }
        }
        match lfp.outer() {
            Some(outer) => lfp = outer,
            None => return false,
        }
    }
}

/// `_1`..`_9` are reserved numbered block parameters, never reportable as
/// local variables. `Binding#local_variable_get` / `_set` / `_defined?`
/// raise a specific NameError for them regardless of whether the block
/// actually uses numbered parameters (matching CRuby). `_0` and `_10`+ are
/// ordinary names.
fn numbered_param_error(name: IdentId) -> Option<MonorubyErr> {
    let s = name.get_name();
    let b = s.as_bytes();
    if b.len() == 2 && b[0] == b'_' && (b'1'..=b'9').contains(&b[1]) {
        Some(MonorubyErr::nameerr_with_name(
            format!("numbered parameter '{s}' is not a local variable"),
            name,
        ))
    } else {
        None
    }
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

    #[test]
    fn binding_it_and_numbered_params_are_not_locals() {
        // The implicit `it` parameter is not a reportable/settable local:
        // `defined?` is false, `get` raises, and `set` leaves it untouched.
        run_test(r#"-> { a = it; binding.local_variable_defined?(:it) }.call("x")"#);
        run_test(r#"-> { a = it; binding.local_variable_set(:it, :b); [a, it] }.call(:a)"#);
        run_test(
            r#"-> { it; (binding.local_variable_get(:it) rescue [$!.class.name, $!.message.start_with?("local variable 'it' is not defined")]) }.call("x")"#,
        );
        // A real `it = ...` local is an ordinary variable.
        run_test(r#"it = 5; binding.local_variable_set(:it, 9); [binding.local_variable_get(:it), binding.local_variable_defined?(:it), it]"#);
        // `_1`..`_9` are reserved numbered parameters — all three operations
        // raise, regardless of whether numbered parameters are used.
        run_test(r#"-> { _1; (binding.local_variable_get(:_1) rescue $!.message) }.call("x")"#);
        run_test(r#"-> { _1; (binding.local_variable_set(:_1, 1) rescue $!.message) }.call("x")"#);
        run_test(r#"-> { _1; (binding.local_variable_defined?(:_1) rescue $!.message) }.call("x")"#);
        run_test(r#"(binding.local_variable_defined?(:_9) rescue $!.message)"#);
        // `_0` and `_10` are ordinary (not numbered) names.
        run_test(r#"binding.local_variable_defined?(:_0)"#);
        run_test(r#"binding.local_variable_defined?(:_10)"#);
    }

    #[test]
    fn binding_implicit_parameters() {
        // The implicit parameters belong to the scope the binding was
        // captured in — not a parent's, not a nested block's — and they
        // exist for the whole scope, however late the reference that
        // created them appears.
        run_test_once(
            r##"
            [
              binding.implicit_parameters,
              proc { it; binding.implicit_parameters }.call(:a),
              proc { _3; binding.implicit_parameters }.call(:a, :b, :c, :d),
              proc { r = binding.implicit_parameters; a = it; r }.call(:a),
              proc { foo = it; proc { binding.implicit_parameters }.call }.call(:a),
              proc { foo = -> { _1 }; binding.implicit_parameters }.call,
              proc { |x| binding.implicit_parameters }.call(:a),
            ]
            "##,
        );
    }

    #[test]
    fn binding_implicit_parameter_defined() {
        run_test_once(
            r##"
            def try; yield; rescue => e; [e.class.to_s, e.message]; end
            [
              binding.implicit_parameter_defined?(:it),
              binding.implicit_parameter_defined?(:_1),
              proc { it; binding.implicit_parameter_defined?(:it) }.call(:a),
              proc { _3;
                [binding.implicit_parameter_defined?(:_1),
                 binding.implicit_parameter_defined?(:_3),
                 binding.implicit_parameter_defined?(:_4)]
              }.call(:a, :b, :c, :d),
              # a String or anything with #to_str names one too
              proc { _1; binding.implicit_parameter_defined?("_1") }.call(:a),
              # a parent's / a nested block's parameters are not this scope's
              proc { foo = _1; -> { binding.implicit_parameter_defined?(:_1) }.call }.call(:a),
              proc { foo = -> { it }; binding.implicit_parameter_defined?(:it) }.call,
              try { binding.implicit_parameter_defined?(:a) },
              try { binding.implicit_parameter_defined?(1) },
            ]
            "##,
        );
    }

    #[test]
    fn binding_implicit_parameter_get() {
        run_test_once(
            r##"
            def try; yield; rescue => e; [e.class.to_s, e.message.split(" for ")[0]]; end
            [
              proc { _1; binding.implicit_parameter_get(:_1) }.call(:a),
              proc { r = binding.implicit_parameter_get(:_1); _1; r }.call(:a),
              proc { _3;
                [binding.implicit_parameter_get(:_1),
                 binding.implicit_parameter_get(:_2),
                 binding.implicit_parameter_get(:_3)]
              }.call(:a, :b, :c, :d),
              proc { it; binding.implicit_parameter_get("it") }.call(:a),
              try { proc { binding.implicit_parameter_get(:_1) }.call },
              # defined only in a parent scope: still not defined here
              try { proc { foo = _1; proc { binding.implicit_parameter_get(:_1) }.call }.call(:a) },
              try { binding.implicit_parameter_get(:a) },
              try { binding.implicit_parameter_get(1) },
            ]
            "##,
        );
    }

    #[test]
    fn binding_name_argument_type_error() {
        // A non-Symbol / non-String name names the offending object.
        run_test_once(
            r##"
            def try; yield; rescue => e; [e.class.to_s, e.message]; end
            [
              try { binding.local_variable_defined?(1) },
              try { binding.local_variable_get(1) },
              try { binding.local_variable_set(1, 2) },
            ]
            "##,
        );
    }
}
