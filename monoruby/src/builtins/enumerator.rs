use super::*;

//
// Enumerator class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Enumerator", ENUMERATOR_CLASS, ObjTy::ENUMERATOR);
    globals.define_builtin_class_func_with_effect(
        ENUMERATOR_CLASS,
        "new",
        enumerator_new,
        0,
        1,
        Effect::CAPTURE,
    );
    globals.define_builtin_func_with(ENUMERATOR_CLASS, "with_index", with_index, 0, 1, false);
    globals.define_builtin_func(ENUMERATOR_CLASS, "with_object", with_object, 1);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next", next, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next_values", next_values, 0);
    globals.define_builtin_func_rest(ENUMERATOR_CLASS, "each", each);
    globals.define_builtin_func(ENUMERATOR_CLASS, "peek", peek, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "peek_values", peek_values, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "rewind", rewind, 0);
    globals.define_builtin_funcs(ENUMERATOR_CLASS, "size", &["length"], enumerator_size, 0);

    // `Enumerator::Yielder` inherits from `Object` (as in CRuby), NOT `Array`.
    // The yielder object is a plain OBJECT-typed RValue; inheriting `Array`
    // meant inherited methods like `Array#inspect` ran `Value::as_array` on a
    // non-array and aborted the process (e.g. `p` on a value that had leaked a
    // Yielder). Its `<<` / `yield` are defined directly below.
    let object_class = globals[OBJECT_CLASS].get_module();
    globals.define_builtin_class(
        "Yielder",
        YIELDER_CLASS,
        object_class,
        ENUMERATOR_CLASS,
        None::<ObjTy>,
    );
    globals.define_builtin_func(YIELDER_CLASS, "<<", yielder_push, 1);
    globals.define_builtin_func_rest(YIELDER_CLASS, "yield", yielder_yield);

    let object_class = globals.object_class();
    globals.define_builtin_class(
        "Generator",
        GENERATOR_CLASS,
        object_class,
        ENUMERATOR_CLASS,
        ObjTy::GENERATOR,
    );
    globals.define_builtin_class_func_with_effect(
        GENERATOR_CLASS,
        "new",
        generator_new,
        0,
        0,
        Effect::CAPTURE,
    );
    globals.define_builtin_func_rest(GENERATOR_CLASS, "each", generator_each);
}

///
/// ### Enumerator#size
///
/// - size -> Integer or Float::INFINITY or nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/size.html]
///
/// Returns the size attached to the Enumerator at construction
/// (`Enumerator.new(size)`, `to_enum(...) { size }`, or one of the
/// builtin iterators that pass it via `generate_enumerator_with_size`).
/// `nil` when no size hint was provided -- there is no method-name
/// dispatch here; each method is responsible for its own size.
#[monoruby_builtin]
fn enumerator_size(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let e = lfp.self_val();
    if e.ty() != Some(ObjTy::ENUMERATOR) {
        return Ok(Value::nil());
    }
    let inner = e.as_enumerator_inner();
    let Some(stored) = inner.size() else {
        return Ok(Value::nil());
    };
    if let Some(proc) = stored.is_proc() {
        return vm.invoke_proc(globals, &proc, &[]);
    }
    Ok(stored)
}

///
/// ### Enumerator.new
///
/// - new(size=nil) {|y| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/s/new.html]
#[monoruby_builtin]
fn enumerator_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh, pc)?;
    let obj = Value::new_generator(proc);
    // Optional size argument: nil (default) / Integer / Float /
    // Float::INFINITY / Proc. Stored verbatim; Enumerator#size resolves
    // Proc values lazily.
    let size = lfp.try_arg(0).filter(|v| !v.is_nil());
    vm.generate_enumerator_with_size(IdentId::EACH, obj, vec![], pc, size)
}

///
/// ### Enumerator#next
///
/// - next -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/next.html]
#[monoruby_builtin]
fn next(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn next_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val: Enumerator = match Enumerator::try_new(lfp.self_val()) {
        Some(e) => e,
        None => {
            return Err(MonorubyErr::typeerr("not an Enumerator"));
        }
    };
    let Some(bh) = lfp.block() else {
        return Ok(self_val.into());
    };

    // Internal iteration: re-invoke the source method with the caller's
    // own block, exactly as CRuby's `enumerator_block_call` does
    // (`rb_block_call_kw(e->obj, e->meth, ...)`). Only external
    // iteration (`#next` / `#peek`) needs to suspend the producer, and
    // that is the one place a fiber is created — see `Enumerator::next`.
    // Driving one here too would run the source on a separate stack,
    // detaching it from the caller's backtrace and `ensure` unwinding.

    // Record the user block's arity so a predicate-consuming method
    // driven through its no-block enumerator (e.g. `Set#divide`, which
    // only receives the internal yielder proc as its block) can pick
    // the right mode from the real arity.
    let blk_arity = vm
        .get_block_data(globals, bh)?
        .func_id()
        .map(|fid| globals[fid].arity())
        .unwrap_or(-1);
    globals.push_enum_block_arity(blk_arity);
    let res = vm.invoke_method_inner(
        globals,
        self_val.method,
        self_val.obj,
        &self_val.args,
        Some(bh),
        self_val.kw_args,
    );
    globals.pop_enum_block_arity();
    res
}

///
/// ### Enumerator#with_index
///
/// - with_index(offset = 0) {|(*args), idx| ... } -> object
/// - with_index(offset = 0) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/with_index.html]
#[monoruby_builtin]
fn with_index(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let offset = match lfp.try_arg(0) {
        // CRuby: `NIL_P(memo) ? 0 : rb_to_int(memo)` — an explicit nil
        // means 0, a Float truncates, anything else goes through #to_int.
        None => Value::integer(0),
        Some(v) if v.is_nil() => Value::integer(0),
        Some(v) => match v.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => v,
            RV::Float(f) => Value::integer(f as i64),
            _ => Value::integer(v.coerce_to_int_i64(vm, globals)?),
        },
    };
    let self_val = Enumerator::new(lfp.self_val());
    let Some(bh) = lfp.block() else {
        // The offset is part of the enumerator's replayed arguments —
        // `e.with_index(5).to_a` must start at 5.
        return vm.generate_enumerator_with_size(
            IdentId::get_id("with_index"),
            lfp.self_val(),
            vec![offset],
            pc,
            self_val.size(),
        );
    };
    enum_adapter_call(vm, globals, self_val, bh, offset, WITH_INDEX_ADAPTER_FUNCID, pc)
}

///
/// ### Enumerator#with_object
///
/// - with_object(memo) {|(*args), memo| ... } -> memo
/// - with_object(memo) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/with_object.html]
#[monoruby_builtin]
fn with_object(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let memo = lfp.arg(0);
    let self_val = Enumerator::new(lfp.self_val());
    let Some(bh) = lfp.block() else {
        return vm.generate_enumerator_with_size(
            IdentId::get_id("with_object"),
            lfp.self_val(),
            vec![memo],
            pc,
            self_val.size(),
        );
    };
    enum_adapter_call(vm, globals, self_val, bh, memo, WITH_OBJECT_ADAPTER_FUNCID, pc)?;
    // CRuby returns the memo, not the source's return value.
    Ok(memo)
}

/// Run the enumerator's source with a *native* adapter block
/// (`adapter_fid`) that carries `state` and forwards to the user's
/// block — CRuby's `enumerator_block_call(obj, enumerator_with_index_i,
/// memo)` shape. No fiber: only external iteration needs to suspend the
/// producer.
///
/// `state` rides on the adapter proc's `outer_lfp` self as a
/// single-element Array (mutable, so `#with_index` can bump its
/// counter); that same Array keys the user block parked on the
/// executor, which keeps nesting correct.
fn enum_adapter_call(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Enumerator,
    bh: BlockHandler,
    state: Value,
    adapter_fid: FuncId,
    pc: BytecodePtr,
) -> Result<Value> {
    let data = vm.get_block_data(globals, bh)?;
    let state = Value::array1(state);
    vm.temp_push(state);
    let outer = Lfp::dummy_heap_frame_with_self(state);
    let adapter: Value = Proc::from_outer(outer, adapter_fid, pc).into();
    vm.temp_push(adapter);
    vm.push_adapter_block(state, data);
    let res = vm.invoke_method_inner(
        globals,
        self_val.method,
        self_val.obj,
        &self_val.args,
        Some(BlockHandler::new(adapter)),
        self_val.kw_args,
    );
    vm.pop_adapter_block();
    vm.temp_pop();
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
fn peek(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    e.peek(vm, globals)
}

///
/// ### Enumerator#peek_values
///
/// - peek_values -> array
///
/// Like `peek`, but always returns the next yield as an Array (a
/// single-arg yield becomes `[v]`, a multi-arg yield stays
/// `[a, b, ...]`) and does not advance the position.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/peek_values.html]
#[monoruby_builtin]
fn peek_values(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut e = Enumerator::new(lfp.self_val());
    Ok(e.peek_values(vm, globals)?.into())
}

///
/// ### Enumerator#rewind
///
/// - rewind -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/rewind.html]
#[monoruby_builtin]
fn rewind(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn yielder_push(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `<<` takes exactly one value, so it always yields exactly one.
    let self_val = lfp.self_val();
    yielder_call(vm, globals, self_val, &[lfp.arg(0)])?;
    // CRuby's `yielder_yield_push` returns the yielder so `y << 1 << 2`
    // chains; the consumer block's value is only surfaced by `#yield`.
    Ok(self_val)
}

///
/// ### Enumerator::Yielder#yield
///
/// - yield(*object) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aYielder/i/yield.html]
#[monoruby_builtin]
fn yielder_yield(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `yield` is rest-args and forwards them verbatim: `Enumerator#each`
    // reproduces the source yield exactly, it does not pack.
    //   `y.yield`      -> block called with no args
    //   `y.yield 1, 2` -> block called with two args
    // Packing into a single value (`rb_enum_values_pack`) is the job of
    // the `Enumerable` methods that consume the yield, not of the
    // yielder — see monoruby/builtins/enumerable.rb.
    let args: Array = lfp.arg(0).as_array();
    let args: Vec<Value> = args.iter().copied().collect();
    yielder_call(vm, globals, lfp.self_val(), &args)
}

/// Hand `args` to the block of the `Generator#each` call this yielder
/// belongs to. The index parked in the yielder's ivar selects the
/// right block when generators are nested.
fn yielder_call(
    vm: &mut Executor,
    globals: &mut Globals,
    yielder: Value,
    args: &[Value],
) -> Result<Value> {
    let data = vm
        .adapter_block(yielder)
        .ok_or_else(|| MonorubyErr::runtimeerr("yielder used outside its Generator#each"))?;
    vm.invoke_block(globals, &data, args)
}

///
///
/// ### Generator.new
///
/// - new() {|y| ... } -> Enumerator
///
#[monoruby_builtin]
fn generator_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh, pc)?;
    Ok(Value::new_generator(proc))
}

///
/// ### Generator#each
///
/// - each {...} -> object
///
#[monoruby_builtin]
fn generator_each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = Generator::new(lfp.self_val());
    // Run the generator body *directly* on the caller's stack, handing
    // it a Yielder bound to this call's block — the same shape as
    // CRuby's `generator_each` (`rb_proc_call` + `yielder_new`).
    // Suspending the body is only needed for external iteration
    // (`#next` / `#peek`), and that fiber is created by the enumerator,
    // one level up; driving one here as well would nest a second fiber
    // under every `#each`, cut the generator body off from the caller's
    // backtrace and `ensure` handling, and cost two extra context
    // switches per element.
    let data = vm.get_block_data(globals, lfp.expect_block()?)?;
    let yielder = Value::yielder_object();
    vm.temp_push(yielder);
    vm.push_adapter_block(yielder, data);
    let body = ProcData::from_proc(&self_val.body());
    let res = vm.invoke_block(globals, &body, &[yielder]);
    vm.temp_pop();
    vm.pop_adapter_block();
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
    fn with_index_and_with_object_run_without_a_fiber() {
        // Return values: `#with_index` gives back the source's, whereas
        // `#with_object` gives back the memo.
        run_test(r#"[1, 2].each.with_index { |v, i| }"#);
        run_test(r#"[1, 2].each.with_object(:m) { |v, m| }"#);
        run_test(r#"[1, 2, 3].each.with_object([]) { |v, m| m << v * 2 }"#);
        // Offset handling: default, explicit, nil (== 0), Float
        // (truncated), and a #to_int-able object.
        run_test(r#"r = []; [1, 2].each.with_index { |v, i| r << [v, i] }; r"#);
        run_test(r#"r = []; [1, 2].each.with_index(5) { |v, i| r << [v, i] }; r"#);
        run_test(r#"r = []; [1, 2].each.with_index(nil) { |v, i| r << [v, i] }; r"#);
        run_test(r#"r = []; [1, 2].each.with_index(2.9) { |v, i| r << [v, i] }; r"#);
        run_test(
            r#"o = Object.new
               def o.to_int; 3; end
               r = []; [1, 2].each.with_index(o) { |v, i| r << [v, i] }; r"#,
        );
        // The blockless form keeps the offset / memo and the source size.
        run_test(r#"[1, 2].each.with_index(5).to_a"#);
        run_test(r#"[1, 2].each.with_object(:m).to_a"#);
        run_test(r#"[[1, 2].each.with_index.size, [1, 2].each.with_object(:m).size]"#);
        // Source yields are packed the way rb_enum_values_pack does.
        run_test(
            r#"[Enumerator.new { |y| y.yield },
                Enumerator.new { |y| y.yield :v },
                Enumerator.new { |y| y.yield 1, 2 }].map { |e|
                  r = []; e.with_index { |v, i| r << [v, i] }; r
                }"#,
        );
        run_test(r#"r = []; ({a: 1, b: 2}).each.with_index { |v, i| r << [v, i] }; r"#);
        // `break` leaves the enumerator method with the block's value.
        run_test(r#"[1, 2, 3].each.with_index { |v, i| break [v, i] if i == 1 }"#);
        run_test(r#"[1, 2, 3].each.with_object(:m) { |v, m| break v if v == 2 }"#);
        // Running the source on the caller's stack (no fiber) means its
        // `ensure` fires and its frames are on the backtrace.
        run_test(
            r#"
            log = []
            begin
              Enumerator.new { |y| begin; y << 1; ensure; log << :ensured; end }
                        .with_index { |v, i| raise "boom" }
            rescue => e
              log << e.message
            end
            log
            "#,
        );
        run_test(
            r#"r = []; Enumerator.new { |y| y << caller.empty? }.with_index { |v, i| r << v }; r"#,
        );
        // Nested enumerator calls keep their own state.
        run_test(
            r#"out = []
               [10, 20].each.with_index do |a, i|
                 [1, 2].each.with_index(100) { |b, j| out << [a, i, b, j] }
               end
               out"#,
        );
    }

    #[test]
    fn generator_each_runs_on_the_callers_stack() {
        // `#each` must not push the generator body onto a fiber: it has
        // to see the caller's frames and unwind through them.
        // `ensure` in the body runs when the consumer block raises.
        run_test(
            r#"
            log = []
            begin
              Enumerator.new { |y| begin; y << 1; ensure; log << :ensured; end }
                        .each { |v| raise "boom" }
            rescue => e
              log << e.message
            end
            log
            "#,
        );
        // The body is part of the caller's backtrace.
        run_test(r#"Enumerator.new { |y| y << caller.empty? }.first"#);
        // A body that yields the current fiber sees the caller's, so
        // `Fiber.yield` is an error exactly as in CRuby (it used to hit
        // the enumerator's private fiber and abort the process).
        run_test(
            r#"
            begin
              Enumerator.new { |y| Fiber.yield; y << 1 }.each { }
            rescue FiberError => e
              e.class.to_s
            end
            "#,
        );
        // `break` / exception propagation out of the consumer block.
        run_test(r#"Enumerator.new { |y| y << 1; y << 2 }.each { |v| break v * 10 }"#);
        run_test(
            r#"(Enumerator.new { |y| raise "gen" }.each { }) rescue $!.message"#,
        );
    }

    #[test]
    fn generator_value_packing_and_yielder_returns() {
        // `Enumerator#each` reproduces the source yield verbatim; the
        // packing into one value is Enumerable's job.
        run_test(r#"r = []; Enumerator.new { |y| y.yield }.each { |*a| r << a }; r"#);
        run_test(r#"r = []; Enumerator.new { |y| y.yield :v }.each { |*a| r << a }; r"#);
        run_test(r#"r = []; Enumerator.new { |y| y.yield 1, 2 }.each { |*a| r << a }; r"#);
        run_test(r#"Enumerator.new { |y| y.yield }.each { |a| a }.nil?"#);
        // rb_enum_values_pack, as seen through Enumerable consumers.
        run_test(
            r#"[Enumerator.new { |y| y.yield },
                Enumerator.new { |y| y.yield :v },
                Enumerator.new { |y| y.yield 1, 2 }].map { |e|
                  a = nil; e.reject { false }.each { |*x| a = x }; a
                }"#,
        );
        run_test(
            r#"[Enumerator.new { |y| y.yield },
                Enumerator.new { |y| y.yield 1, 2 }].map { |e| e.take_while { true } }"#,
        );
        // `#yield` returns the consumer block's value; `<<` returns the
        // yielder so it chains.
        run_test(r#"r = []; Enumerator.new { |y| r << y.yield(1) }.each { |v| :from_block }; r"#);
        run_test(r#"Enumerator.new { |y| y << 1 << 2 }.to_a"#);
        // Nested generators resolve to their own yielder.
        run_test(
            r#"Enumerator.new { |y|
                 Enumerator.new { |z| z << [:inner, 9] }.each { |v| y << v }
                 y << :outer
               }.to_a"#,
        );
        // Struct#each_pair picks its yield shape from the block arity,
        // which is what makes the enumerator form pack correctly.
        run_test(
            r#"S = Struct.new(:a, :b)
               s = S.new(1, 2)
               r = []
               s.each_pair { |k, v| r << [k, v] }
               s.each_pair { |x| r << x }
               [r, s.each_pair.map { |v| v }, s.each_pair.to_a]"#,
        );
    }

    #[test]
    fn yielder_yield_returns_nil() {
        // Issue #905: `Yielder#yield` must return nil (the value fed back on
        // resume), not the Yielder itself. Previously the generator driver
        // resumed the fiber with the yielder, so `y.yield(x)` returned the
        // Yielder; capturing it (`r << y.yield(1)`) leaked a Yielder into user
        // data, and inspecting it aborted the process (Array#inspect ->
        // Value::as_array on a non-array Yielder).
        run_test(
            r##"
            r = []
            Enumerator.new { |y| r << y.yield(1) }.to_a
            r
            "##,
        );
        run_test("Enumerator.new { |y| y.yield(1) }.first; :ok");
        // Yielder is an Object subclass (as in CRuby), and inspecting one no
        // longer crashes.
        run_test("Enumerator::Yielder.superclass.to_s");
        run_test(
            r##"
            y = nil
            Enumerator.new { |yy| y = yy; yy.yield 1 }.first
            [y.class.to_s, y.is_a?(Enumerator::Yielder)]
            "##,
        );
    }

    #[test]
    fn enumerator_with_object() {
        run_tests(&[
            // block form returns the memo
            r##"
            r = [:a, :b, :c].each.with_object("") { |x, m| m << x.to_s }
            "##,
            // memo is the same identity across yields
            r##"
            seen = []
            memo = Object.new
            [1, 2, 3].each.with_object(memo) { |_, m| seen << m.equal?(memo) }
            seen
            "##,
            // block-less returns an Enumerator that yields [elem, memo]
            r##"
            [:a, :b].each.with_object("hi").to_a
            "##,
            // each_with_object on Array (Enumerable delegation)
            r##"
            [1, 2, 3].each_with_object([]) { |x, m| m << x * 10 }
            "##,
        ]);
    }

    #[test]
    fn enumerator_peek_values() {
        run_tests(&[
            // Single-arg yield ⇒ [v]; multi-arg yield ⇒ [a, b, ...].
            r##"
            o = Object.new
            def o.each
              yield :a
              yield :b1, :b2
              yield
            end
            e = o.to_enum
            [e.peek_values, e.next, e.peek_values, e.next, e.peek_values, e.next]
            "##,
            // Repeated peek_values without advancing is stable.
            r##"
            e = [10, 20].each
            [e.peek_values, e.peek_values, e.next, e.peek_values]
            "##,
        ]);
    }

    #[test]
    fn enumerable_first_arg_validation() {
        run_tests(&[
            // no arg / explicit count
            r#"(1..5).first"#,
            r#"(1..5).first(0)"#,
            r#"(1..5).first(3)"#,
            r#"(1..5).first(100)"#,
            // #to_int coercion
            r#"o = Object.new; def o.to_int; 2; end; (10..20).first(o)"#,
            // negative -> ArgumentError; explicit nil / non-numeric -> TypeError
            r#"begin; (1..3).first(-1); rescue ArgumentError; :ae; end"#,
            r#"begin; (1..3).first(nil); rescue TypeError; :te; end"#,
            r#"begin; (1..3).first("a"); rescue TypeError; :te; end"#,
            r#"begin; [].first(2 ** 70); rescue RangeError; :re; end"#,
            r#"begin; (1..3).first(1, 2); rescue ArgumentError; :ae; end"#,
            // laziness: consumes only what is needed
            r#"
            e = Enumerator.new { |y| y << 1; y << 2; raise "boom"; y << 3 }
            e.first(2)
            "#,
        ]);
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
        run_tests(&[
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
        ]);
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
        run_tests(&[
            r##"
            res = []
            e = [1,2,3,4].to_enum
            e.each do |x|
                res << x.to_s
            end
            res
        "##,
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
        ]);
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

    // Note: Enumerator::Lazy is defined in Ruby (enumerable.rb) but
    // monoruby has a block variable capture limitation that prevents
    // nested block forwarding from working correctly. Tests are
    // disabled until the underlying issue is fixed.

    #[test]
    fn one_() {
        run_tests(&[
            r##"
            res = []
            res << ['ant', 'bear', 'cat'].one? {|word| word.length == 4}  # => true
            res << ['ant', 'bear', 'cat'].one? {|word| word.length == 3}  # => false
            res << ['ant', 'bear', 'cat'].one? {|word| word.length > 4}   # => false
            res << ['ant', 'bear', 'cat'].one?(/t/)                       # => false
            res << [nil, true, 99].one?                                   # => false
            res << [nil, true, false].one?                                # => true
            res << [nil, true, nil].one?                                  # => false
            res << [nil, true, 99].one?(Integer)                          # => true
            res << [nil, true, "99"].one?(Integer)                        # => false
            res << [nil, 7, 99].one?(Integer)                             # => false
            res << [].one?                                                # => true
            res
        "##,
            r##"
            f = [nil, true, false]
            t = [nil, false, nil]
            a = ["ant", "bear", "cat"]
            res = []
            res << a.none? {|word| word.length == 4}  # => false
            res << a.none? {|word| word.length > 4}   # => true
            res << t.none?                            # => true
            res << f.none?                            # => false
            res << a.none?(Integer)                   # => true
            res << [].none?                           # => true
            res
        "##,
            r##"
            res = []
            a = %w(albatross dog horse)
            # res << a.min_by                    # => #<Enumerator: ["albatross", "dog", "horse"]:min_by>
            res << a.min_by { |x| x.length }   # => "dog"
            # res << a.min_by(2)                 # => #<Enumerator: ["albatross", "dog", "horse"]:min_by(2)>
            # res << a.min_by(2) {|x| x.length } # => ["dog", "horse"]
            res
        "##,
            r##"
            res = []
            res << Enumerator.new { |y| y << 1 }.size
            res
        "##,
            "1.upto(5).size",
            "5.downto(1).size",
            "3.times.size",
        ]);
    }
}
