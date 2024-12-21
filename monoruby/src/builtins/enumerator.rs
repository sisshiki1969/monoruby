use super::*;

use std::sync::Once;

pub static mut YIELDER: Option<Module> = None;
static YIELDER_INIT: Once = Once::new();

//
// Enumerator class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj_with_instance_ty(
        "Enumerator",
        ENUMERATOR_CLASS,
        ObjKind::ENUMERATOR,
    );
    globals.define_builtin_class_func(ENUMERATOR_CLASS, "new", enumerator_new, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next", next, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next_values", next_values, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "each", each, 0);
    globals.define_builtin_func_with(ENUMERATOR_CLASS, "with_index", with_index, 0, 1, false);
    globals.define_builtin_func(ENUMERATOR_CLASS, "peek", peek, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "rewind", rewind, 0);

    let yielder = globals.define_class_by_str(
        "Yielder",
        globals.store.classes[ARRAY_CLASS].get_module(),
        ENUMERATOR_CLASS,
    );
    unsafe { YIELDER_INIT.call_once(|| YIELDER = Some(yielder)) }
    globals.define_builtin_func(yielder.id(), "<<", yielder_push, 1);
    globals.define_builtin_func_rest(yielder.id(), "yield", yielder_yield);

    globals.define_builtin_class_with_instance_ty(
        "Generator",
        GENERATOR_CLASS,
        globals.store.classes.object_class(),
        ENUMERATOR_CLASS,
        ObjKind::GENERATOR,
    );
    globals.define_builtin_class_func(GENERATOR_CLASS, "new", generator_new, 0);
    globals.define_builtin_func(GENERATOR_CLASS, "each", generator_each, 0);
}

///
/// ### Enumerator.new
///
/// - new([NOT SUPPORTED] size=nil) {|y| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/s/new.html]
#[monoruby_builtin]
fn enumerator_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh)?;
    let obj = Value::new_generator(proc);
    vm.generate_enumerator(IdentId::EACH, obj, vec![])
}

///
/// ### Enumerator#next
///
/// - next -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/next.html]
#[monoruby_builtin]
fn next(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    e.next(vm, globals)
}

///
/// ### Enumerator#next_values
///
/// - next_values -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/next_values.html]
#[monoruby_builtin]
fn next_values(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    Ok(e.next_values(vm, globals)?.into())
}

///
/// ### Enumerator#each
///
/// - each {...} -> object
/// - each -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn each_inner(
        vm: &mut Executor,
        globals: &mut Globals,
        mut internal: Fiber,
        block_data: &ProcInner,
        self_val: Enumerator,
    ) -> Result<Value> {
        loop {
            let v = internal.enum_yield_values(vm, globals, self_val, Value::nil())?;
            if internal.is_terminated() {
                return Ok(v);
            }
            let a = v.as_array();
            vm.invoke_block(globals, block_data, &[a.peel()])?;
        }
    }
    let self_val: Enumerator = Enumerator::new(lfp.self_val());
    let data = if let Some(bh) = lfp.block() {
        vm.get_block_data(globals, bh)?
    } else {
        return Ok(self_val.into());
    };

    let internal = Fiber::from(self_val.proc);
    vm.temp_push(internal.into());
    let res = each_inner(vm, globals, internal, &data, self_val);
    vm.temp_pop();
    res
}

///
/// ### Enumerator#with_index
///
/// - with_index(offset = 0) {|(*args), idx| ... } -> object
/// - with_index(offset = 0) -> Enumeratorf
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/with_index.html]
#[monoruby_builtin]
fn with_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn with_index_inner(
        vm: &mut Executor,
        globals: &mut Globals,
        mut internal: Fiber,
        block_data: &ProcInner,
        mut count: Value,
        self_val: Enumerator,
    ) -> Result<Value> {
        let mut res = Value::nil();
        loop {
            let v = internal.enum_yield_values(vm, globals, self_val, res)?;
            if internal.is_terminated() {
                return Ok(v);
            }
            let a = v.as_array();
            res = vm.invoke_block(globals, block_data, &[a.peel(), count])?;
            match count.unpack() {
                RV::Fixnum(i) => count = Value::integer(i + 1),
                RV::BigInt(i) => count = Value::bigint(i + 1),
                _ => unreachable!(),
            }
        }
    }
    let count = if lfp.try_arg(0).is_none() {
        Value::integer(0)
    } else {
        match lfp.arg(0).unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => lfp.arg(0),
            RV::Float(f) => Value::integer(f as i64),
            _ => {
                return Err(MonorubyErr::no_implicit_conversion(
                    lfp.arg(0),
                    INTEGER_CLASS,
                ))
            }
        }
    };
    let self_val = Enumerator::new(lfp.self_val());

    let id = IdentId::get_id("with_index");
    let data = if let Some(bh) = lfp.block() {
        vm.get_block_data(globals, bh)?
    } else {
        return vm.generate_enumerator(id, lfp.self_val(), vec![]);
    };

    let internal = Fiber::from(self_val.proc);
    vm.temp_push(internal.into());
    let res = with_index_inner(vm, globals, internal, &data, count, self_val);
    vm.temp_pop();
    res
}

///
/// ### Enumerator#peek
///
/// - peek -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/peek.html]
#[monoruby_builtin]
fn peek(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    e.peek(vm, globals)
}

///
/// ### Enumerator#rewind
///
/// - rewind -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/rewind.html]
#[monoruby_builtin]
fn rewind(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    e.rewind();
    Ok(e.into())
}

///
/// ### Enumerator::Yielder#<<
///
/// - self << object -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aYielder/i/=3c=3c.html]
#[monoruby_builtin]
fn yielder_push(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    vm.yield_fiber(globals, Value::array1(lfp.arg(0)))
}

///
/// ### Enumerator::Yielder#yield
///
/// - yield(*object) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aYielder/i/yield.html]
#[monoruby_builtin]
fn yielder_yield(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    vm.yield_fiber(globals, lfp.arg(0))
}

///
/// ### Generator.new
///
/// - new() {|y| ... } -> Enumerator
///
#[monoruby_builtin]
fn generator_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh)?;
    Ok(Value::new_generator(proc))
}

///
/// ### Generator#each
///
/// - each {...} -> object
///
#[monoruby_builtin]
fn generator_each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn each_inner(
        vm: &mut Executor,
        globals: &mut Globals,
        mut internal: Fiber,
        block_data: &ProcInner,
    ) -> Result<Value> {
        let yielder = Value::yielder_object();
        loop {
            let v = internal.generator_yield_values(vm, globals, yielder)?;
            if internal.is_terminated() {
                return Ok(v);
            }
            let a: Array = v.as_array();
            vm.invoke_block(globals, block_data, &[a.peel()])?;
        }
    }
    let self_val = Generator::new(lfp.self_val());
    let data = vm.get_block_data(globals, lfp.expect_block()?)?;
    let internal = self_val.create_internal();
    vm.temp_push(internal.into());
    let res = each_inner(vm, globals, internal, &data);
    vm.temp_pop();

    res
}
#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn enumerator1() {
        run_test(
            r##"
            a = Enumerator.new do |y|
                3.times do |i|
                    y << i
                end
            end
            [a.next, a.peek, a.peek, a.next, a.peek, a.next]
            "##,
        );
    }

    #[test]
    fn enumerator2() {
        run_test_no_result_check(
            r##"
            a = Enumerator.new do |y|
                3.times do |i|
                    y << i
                end
            end
            [a.inspect, a.to_s]
        "##,
        );
    }

    #[test]
    fn enumerator3() {
        run_test(
            r#"
        p = []
        o = Object.new
        def o.each
          yield
          yield 1
          yield 1, 2
          yield nil
          yield [1, 2]
        end
        e = o.to_enum
        5.times do
            p << e.next_values
        end
        e = o.to_enum
        5.times do
            p << e.next
        end
        p
        "#,
        );
    }

    #[test]
    fn enum_free() {
        run_test(
            r##"
            20.times do
                a = Enumerator.new do |y|
                    3.times do |i|
                        y << i
                    end
                end
                a.next
                a.next
            end
        "##,
        );
    }

    #[test]
    fn fib() {
        run_test(
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do
                    y << a
                    a, b = a + b, a
                end
            end
            30.times do fib.next end
            fib.next
        "##,
        );
    }

    #[test]
    fn fib_each1() {
        run_test_with_prelude(
            r##"
            ans = []
            ans << fib.each {|x| ans << x}
        "##,
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do
                    y << a
                    a, b = a + b, a
                    if a > 30 then break end
                end
            end"##,
        );
    }

    #[test]
    fn fib_each2() {
        run_test_with_prelude(
            r##"
            ans = []
            ans << fib.with_index {|x, i| ans << x; ans << i}
        "##,
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do
                    y.<< a
                    a, b = a + b, a
                    if a > 100 then break end
                end
            end"##,
        );
    }

    #[test]
    fn fib_each3() {
        run_test_with_prelude(
            r##"
            ans = []
            ans << fib.with_index(1000) {|x, i| ans << x; ans << i}
            ans
        "##,
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do
                    y.<< a
                    a, b = a + b, a
                    if a > 100 then break end
                end
            end"##,
        );
    }

    #[test]
    fn each() {
        run_test(
            r##"
            res = []
            e = [1,2,3,4].to_enum
            e.each do |x|
                res << x.to_s
            end
            res
        "##,
        );
    }

    #[test]
    fn rewind() {
        run_test(
            r##"
            res = []
            e = [1,2,3,4].to_enum
            res << e.next
            res << e.next
            res << e.next
            e.rewind
            res << e.next
            res << e.next
            e.rewind
            res << e.next
            res
        "##,
        );
    }

    #[test]
    fn generator() {
        run_test_with_prelude(
            r##"
            res = []
            fib.each do |num|
                if num > 1000
                    break
                end
                res << num
            end
            res
        "##,
            r##"
            fib = Enumerator::Generator.new do |y|
                a = b = 1
                loop do
                    y << a
                    a, b = a + b, a
                end
            end
            "##,
        );
    }

    #[test]
    fn enum_chain() {
        run_test_with_prelude(
            r##"
        res = []
        e.with_index.with_index do |(num, idx2), idx1|
            res << num
            res << idx1
            res << idx2
            if num > 1000
                break
            end
        end
        res
        "##,
            r##"
        e = Enumerator.new do |y|
            a = b = 1
            loop do
                y << a
                a, b = a + b, a
            end
        end
        "##,
        );
    }
}
