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
    globals.define_builtin_func(PROC_CLASS, "lambda?", lambda_, 0);
    globals.define_builtin_func(PROC_CLASS, "arity", proc_arity, 0);
    globals.define_builtin_func(PROC_CLASS, "parameters", parameters, 0);
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
    // Fast path for Symbol#to_proc procs: dispatch directly so that the
    // block passed to Proc#call is forwarded to the invoked method. The
    // regular invoke_proc/block_invoker path currently drops block handlers.
    if proc.func_id() == SYMBOL_TO_PROC_BODY_FUNCID {
        let symbol = proc.outer_lfp().self_val();
        let symbol_id = symbol
            .try_symbol()
            .expect("symbol-to-proc outer self is not a Symbol");
        let args_val = lfp.arg(0);
        let args = args_val.as_array();
        if args.is_empty() {
            return Err(MonorubyErr::argumenterr("no receiver given"));
        }
        let recv = args[0];
        let rest: Vec<Value> = args[1..].to_vec();
        let class_id = recv.class();
        if let Some(entry) = globals.check_method_for_class(class_id, symbol_id) {
            match entry.visibility() {
                Visibility::Private => {
                    return Err(MonorubyErr::private_method_called(
                        globals, symbol_id, recv,
                    ));
                }
                Visibility::Protected => {
                    return Err(MonorubyErr::protected_method_called(
                        globals, symbol_id, recv,
                    ));
                }
                _ => {}
            }
        }
        let bh = lfp.block();
        return vm.invoke_method_inner(globals, symbol_id, recv, &rest, bh, None);
    }
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

/// ### Proc#lambda?
#[monoruby_builtin]
fn lambda_(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    let func_id = proc.func_id();
    Ok(Value::bool(!globals[func_id].is_block_style()))
}

/// ### Proc#parameters
///
/// - parameters -> [[Symbol, Symbol], ...]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/parameters.html]
#[monoruby_builtin]
fn parameters(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    let func_id = proc.func_id();
    let func_info = &globals[func_id];
    let is_lambda = !func_info.is_block_style();
    let params = func_info.params();
    let req_tag = if is_lambda {
        Value::symbol(IdentId::get_id("req"))
    } else {
        Value::symbol(IdentId::get_id("opt"))
    };
    let opt_tag = Value::symbol(IdentId::get_id("opt"));
    let rest_tag = Value::symbol(IdentId::get_id("rest"));
    let key_tag = Value::symbol(IdentId::get_id("key"));
    let keyrest_tag = Value::symbol(IdentId::get_id("keyrest"));
    let block_tag = Value::symbol(IdentId::get_id("block"));
    let mut result = vec![];
    let args_names = &params.args_names;
    let mut name_idx = 0;
    // required params
    for _ in 0..params.req_num() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(req_tag, Value::symbol(*name))
        } else {
            Value::array1(req_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // optional params
    for _ in 0..params.opt_num() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(opt_tag, Value::symbol(*name))
        } else {
            Value::array1(opt_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // rest param
    if params.is_rest().is_some() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(rest_tag, Value::symbol(*name))
        } else {
            Value::array1(rest_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // post params (required after rest)
    for _ in 0..params.post_num() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(req_tag, Value::symbol(*name))
        } else {
            Value::array1(req_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // keyword params
    for kw_name in &params.kw_names {
        result.push(Value::array2(key_tag, Value::symbol(*kw_name)));
    }
    // keyword rest
    if params.kw_rest.is_some() {
        result.push(Value::array1(keyrest_tag));
    }
    // block param
    if let Some(block_name) = params.block_param {
        result.push(Value::array2(block_tag, Value::symbol(block_name)));
    }
    Ok(Value::array_from_vec(result))
}

/// ### Proc#arity
#[monoruby_builtin]
fn proc_arity(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    let func_id = proc.func_id();
    Ok(Value::integer(globals[func_id].arity()))
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
        run_test(
            r#"
        p = proc {}
        sl = p.source_location
        [sl.is_a?(Array), sl.size == 2, sl[0].is_a?(String), sl[1].is_a?(Integer)]
        "#,
        );
        // source_location line matches the proc creation line
        run_test(
            r#"
        line = __LINE__; p = proc {}
        p.source_location[1] == line
        "#,
        );
        // lambda source_location line matches creation line
        run_test(
            r#"
        line = __LINE__; l = lambda {}
        l.source_location[1] == line
        "#,
        );
        // two procs on consecutive lines have consecutive line numbers
        run_test(
            r#"
        p1 = proc {}
        p2 = proc {}
        p2.source_location[1] == p1.source_location[1] + 1
        "#,
        );
        // Proc#binding.source_location returns the same line as the proc
        run_test(
            r#"
        p = proc {}
        b = p.binding
        p.source_location[1] == b.source_location[1]
        "#,
        );
    }

    #[test]
    fn proc_yield_detached_context() {
        // yield in Proc whose enclosing method was called with a block but has
        // already returned should raise LocalJumpError, not panic.
        run_test_error(
            r#"
        def make_proc
          Proc.new { yield }
        end
        def get_proc_with_block
          make_proc { 99 }
        end
        get_proc_with_block.call
        "#,
        );
        // yield in Proc whose enclosing method was called without a block
        run_test_error(
            r#"
        def make_proc
          Proc.new { yield }
        end
        make_proc.call
        "#,
        );
    }

    #[test]
    fn proc_yield_same_context() {
        // yield in Proc called within the same method context should work
        run_test(
            r#"
        def call_with_block
          p = Proc.new { yield }
          p.call
        end
        call_with_block { 42 }
        "#,
        );
    }

    #[test]
    fn lambda_q() {
        run_test("Proc.new {}.lambda?");
        run_test("lambda {}.lambda?");
        run_test("->(x) { x }.lambda?");
    }

    #[test]
    fn proc_arity() {
        run_test("Proc.new {}.arity");
        run_test("Proc.new {|x| x}.arity");
        run_test("Proc.new {|x, y| x}.arity");
        run_test("lambda {}.arity");
        run_test("lambda {|x| x}.arity");
        run_test("->(x, y) { x }.arity");
    }

    #[test]
    fn proc_parameters() {
        // lambda: required params are :req
        run_test("->(x, y) {}.parameters");
        // lambda: optional params are :opt
        run_test("->(x, y=1) {}.parameters");
        // lambda: rest params
        run_test("->(x, *rest) {}.parameters");
        // lambda: block param
        run_test("->(x, &blk) {}.parameters");
        // lambda: mixed
        run_test("->(x, y=1, *rest, &blk) {}.parameters");
        // proc: all positional params are :opt
        run_test("Proc.new {|x, y| }.parameters");
        // proc: rest
        run_test("Proc.new {|x, *rest| }.parameters");
        // no params
        run_test("lambda {}.parameters");
        run_test("Proc.new {}.parameters");
    }

    #[test]
    fn proc_curry() {
        run_test("proc {|a,b,c| a+b+c}.curry[1][2][3]");
        run_test("proc {|a,b,c| a+b+c}.curry[1,2][3]");
        run_test("proc {|a,b,c| a+b+c}.curry[1,2,3]");
        run_test("->(a,b) { a + b }.curry[1][2]");
        run_test("proc {|a,b,c| [a,b,c]}.curry(3)[1][2][3]");
    }
}
