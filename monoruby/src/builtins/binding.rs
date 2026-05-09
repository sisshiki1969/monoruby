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
                        Value::string(iseq_info.sourceinfo.short_file_name().to_string());
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
    let file_name = Value::string(iseq.sourceinfo.short_file_name().to_string());
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
}
