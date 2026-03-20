use super::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Proc", PROC_CLASS, ObjTy::PROC);
    globals.define_builtin_class_func_with_effect(PROC_CLASS, "new", new, 0, 0, Effect::CAPTURE);
    globals.define_builtin_class_func(PROC_CLASS, "allocate", super::class::undef_allocate, 0);
    globals.define_builtin_funcs_with_kw(
        PROC_CLASS,
        "call",
        &["[]", "yield", "==="],
        call,
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(PROC_CLASS, "binding", binding_, 0);
    globals.define_builtin_func(PROC_CLASS, "source_location", source_location, 0);
}

///
/// ### Proc.new
///
/// - new { ... } -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, _: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(bh, pc)?;
        Ok(p.into())
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

///
/// ### Proc#call
///
/// - self[*arg] -> ()
/// - call(*arg) -> ()
/// - self === *arg -> ()
/// - yield(*arg) -> ()
///
/// TODO: we must support [] with >2 args.
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    vm.invoke_proc(globals, &proc, &lfp.arg(0).as_array())
}

///
/// ### Proc#source_location
///
/// - source_location -> [String, Integer] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/source_location.html]
#[monoruby_builtin]
fn source_location(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    // fallback: use the proc's own ISeq location
    let func_id = proc.func_id();
    if let Some(iseq) = globals.store[func_id].is_iseq() {
        let iseq_info = &globals.store[iseq];
        let file_name = Value::string(iseq_info.sourceinfo.short_file_name().to_string());
        let line = Value::integer(iseq_info.sourceinfo.get_line(&iseq_info.loc) as i64);
        Ok(Value::array2(file_name, line))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Proc#binding
///
/// - binding -> Binding
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/binding.html]
#[monoruby_builtin]
fn binding_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    let outer_lfp = proc.outer_lfp();
    let pc = proc.source();
    Ok(Binding::from_outer(outer_lfp, Some(pc)).as_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn proc_new() {
        run_test_no_result_check("Proc.new {}");
        run_test_error("Proc.new");
        run_test(
            "
            a = 100
            p = Proc.new {|x, y|
                a += x / y
            }
            p.call(42, 7)
        ",
        );
        run_test(
            "
        a = 100
        p = nil
        1.times {
          p = Proc.new {
            3.times {
              a+=1
            }
          }
        }
        p.call
        a
        ",
        )
    }

    #[test]
    fn proc1() {
        run_test_with_prelude(
            r#"
        [p.call(3,4), p.yield(3,4), p[3,4], p.===(3,4)]
        "#,
            r#"
        p = Proc.new {|x,y| x * y} 
        "#,
        );
    }

    #[test]
    fn proc2() {
        run_test(
            "
            a = 100
            p = nil
            q = nil
            3.times {
              p = Proc.new {
                3.times {
                  a+=1
                }
              }
              q = Proc.new {
                5.times {
                  a+=10
                }
              }
            }
            p.call
            q.call
            a
        ",
        );
    }

    #[test]
    fn proc_param() {
        run_test(
            "
            a = []
            p = Proc.new {|x|
                a << x
            }
            5.times(&p)
            a
        ",
        );
    }

    #[test]
    fn block_param() {
        run_test_with_prelude(
            r#"
            $ans = []
            [1, 2, 3].each(&Foo.new)
        "#,
            r#"
            class Foo
              def to_proc
                Proc.new {|v| $ans << v * v}
              end
            end
        "#,
        );
        run_test_error(
            r#"
        class Foo
            def to_proc
                :xxx
            end
        end

        [1,2,3].each(&Foo.new)
        "#,
        );
        run_test_error(
            r#"
        class Foo
        end

        [1,2,3].each(&Foo.new)
        "#,
        );
    }

    #[test]
    fn duplicate_underscore_param() {
        // `_` can appear multiple times in block parameters (Ruby spec).
        // The name `_` resolves to the first occurrence.
        run_test(
            r#"
        [[1,2,3,4]].map { |x, _, y, _| [x, _, y] }
        "#,
        );
        // Destructured block parameters with duplicate `_`.
        run_test(
            r#"
        [[1,2,3,4]].sort_by { |(_, b, _, d)| d }
        "#,
        );
        // `_` used alone multiple times.
        run_test(
            r#"
        res = []
        {a: 1, b: 2}.each { |_, _| res << _ }
        res
        "#,
        );
    }

    #[test]
    fn proc() {
        run_test_with_prelude(
            r#"
        a = :a
        $b = proc { a = 1 }
        a = nil
        foo
        a.inspect
"#,
            r#"
        def foo
          $b.call
        end
        "#,
        )
    }

    #[test]
    fn proc_binding() {
        run_test(
            r#"
        x = 42
        p = Proc.new { x }
        b = p.binding
        b.is_a?(Binding)
        "#,
        );
        run_test(
            r#"
        x = 42
        p = Proc.new { x }
        b = p.binding
        b.local_variables.include?(:x)
        "#,
        );
        run_test(
            r#"
        x = 10
        p = proc { x + 1 }
        eval("x", p.binding)
        "#,
        );
    }

    #[test]
    fn proc_2() {
        run_test_once(
            r#"
        a = 5
        p = proc {
          a = 10
        }
        a = 1
        p.call
        a
        "#,
        );

        run_test_once(
            r#"
        p = proc {
          a = 10
        }
        a = 1
        p.call
        a
        "#,
        );
    }

    #[test]
    fn proc_source_location() {
        // source_location returns [String, Integer]
        run_test_once(
            r#"
        p = proc {}
        sl = p.source_location
        [sl.is_a?(Array), sl.size == 2, sl[0].is_a?(String), sl[1].is_a?(Integer)]
        "#,
        );
        // source_location line matches the proc creation line
        run_test_once(
            r#"
        line = __LINE__; p = proc {}
        p.source_location[1] == line
        "#,
        );
        // lambda source_location line matches creation line
        run_test_once(
            r#"
        line = __LINE__; l = lambda {}
        l.source_location[1] == line
        "#,
        );
        // two procs on consecutive lines have consecutive line numbers
        run_test_once(
            r#"
        p1 = proc {}
        p2 = proc {}
        p2.source_location[1] == p1.source_location[1] + 1
        "#,
        );
        // Proc#binding.source_location returns the same line as the proc
        run_test_once(
            r#"
        p = proc {}
        b = p.binding
        p.source_location[1] == b.source_location[1]
        "#,
        );
    }
}
