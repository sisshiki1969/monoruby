use super::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Proc", PROC_CLASS);
    globals.define_builtin_class_func(PROC_CLASS, "new", new, 0);
    globals.define_builtin_funcs_rest(PROC_CLASS, "call", &["[]", "yield", "==="], call);
}

///
/// ### Proc.new
///
/// - new { ... } -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(globals, bh)?;
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
fn call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let p = Proc::new(lfp.self_val());
    vm.invoke_proc(globals, p, &lfp.arg(0).as_array())
}

mod test {
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
}
