use super::*;

//
// Binding class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Binding", BINDING_CLASS, ObjTy::BINDING);
    globals.define_builtin_class_func(BINDING_CLASS, "allocate", super::class::undef_allocate, 0);
    globals.define_builtin_func(BINDING_CLASS, "local_variables", local_variables, 0);
    globals.define_builtin_func(BINDING_CLASS, "source_location", source_location, 0);
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
    let fid = binding.outer_lfp().func_id();
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
        binding.outer_lfp().func_id()
    };
    let iseq = globals.store.iseq(fid);
    let file_name = Value::string(iseq.sourceinfo.short_file_name().to_string());
    let line = Value::integer(iseq.sourceinfo.get_line(&iseq.loc) as i64);
    Ok(Value::array2(file_name, line))
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
        binding.outer_lfp().func_id()
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
}
