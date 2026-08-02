use super::*;

//
// GC module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("GC").id();
    globals.define_builtin_module_func_with(klass, "stat", stat, 0, 1, false);
    globals.define_builtin_module_func(klass, "enable", enable, 0);
    globals.define_builtin_module_func(klass, "disable", disable, 0);
    globals.define_builtin_module_func(klass, "count", count, 0);
    globals.define_builtin_module_func(klass, "total_time", total_time, 0);
    globals.define_builtin_module_func(klass, "measure_total_time", measure_total_time, 0);
    globals.define_builtin_module_func(klass, "measure_total_time=", set_measure_total_time, 1);
    globals.define_builtin_module_func(klass, "stress", stress, 0);
    globals.define_builtin_module_func(klass, "stress=", set_stress, 1);
    // Internals the Ruby-level `GC.start` / `GC.config` in
    // `builtins/gc.rb` drive; see there for why they are split.
    globals.define_builtin_module_func(klass, "__request_gc", request_gc, 1);
    globals.define_builtin_module_func(klass, "__allow_full_mark", allow_full_mark, 0);
    globals.define_builtin_module_func(klass, "__allow_full_mark=", set_allow_full_mark, 1);

    let profiler = globals
        .define_module_with_identid(IdentId::get_id("Profiler"), klass)
        .id();
    globals.define_builtin_module_func(profiler, "enabled?", profiler_enabled, 0);
    globals.define_builtin_module_func(profiler, "enable", profiler_enable, 0);
    globals.define_builtin_module_func(profiler, "disable", profiler_disable, 0);
    globals.define_builtin_module_func(profiler, "clear", profiler_clear, 0);
    globals.define_builtin_module_func(profiler, "result", profiler_result, 0);
    globals.define_builtin_module_func(profiler, "total_time", profiler_total_time, 0);
    globals.define_builtin_module_func(profiler, "raw_data", profiler_raw_data, 0);
}

/// The `GC.stat` keys, in CRuby 4.0 order.
///
/// Everything monoruby's collector actually has an answer for is a real
/// allocator counter (see [`stat_value`]). The rest — compaction and
/// finalizer bookkeeping, and CRuby's separate old-generation malloc
/// accounting — is reported as 0 because monoruby has no equivalent
/// concept, not because the number is unavailable.
const STAT_KEYS: &[&str] = &[
    "count",
    "time",
    "marking_time",
    "sweeping_time",
    "heap_allocated_pages",
    "heap_empty_pages",
    "heap_allocatable_slots",
    "heap_available_slots",
    "heap_live_slots",
    "heap_free_slots",
    "heap_final_slots",
    "heap_marked_slots",
    "heap_eden_pages",
    "total_allocated_pages",
    "total_freed_pages",
    "total_allocated_objects",
    "total_freed_objects",
    "malloc_increase_bytes",
    "malloc_increase_bytes_limit",
    "minor_gc_count",
    "major_gc_count",
    "compact_count",
    "read_barrier_faults",
    "total_moved_objects",
    "remembered_wb_unprotected_objects",
    "remembered_wb_unprotected_objects_limit",
    "old_objects",
    "old_objects_limit",
    "oldmalloc_increase_bytes",
    "oldmalloc_increase_bytes_limit",
];

/// Value of a `GC.stat` key. `None` for keys we don't recognise, so
/// `GC.stat(:bogus)` can raise.
fn stat_value(name: &str) -> Option<i64> {
    crate::alloc::ALLOC.with(|alloc| {
        let alloc = alloc.borrow();
        Some(match name {
            "count" => alloc.total_gc_counter() as i64,
            // CRuby reports the three timers in milliseconds.
            "time" => (alloc.gc_time_ns() / 1_000_000) as i64,
            "marking_time" => (alloc.mark_time_ns() / 1_000_000) as i64,
            "sweeping_time" => (alloc.sweep_time_ns() / 1_000_000) as i64,
            "heap_allocated_pages" | "heap_eden_pages" => alloc.page_count() as i64,
            "heap_empty_pages" => alloc.empty_page_count() as i64,
            "heap_allocatable_slots" => alloc.allocatable_slots() as i64,
            "heap_available_slots" => alloc.available_slots() as i64,
            "heap_live_slots" | "heap_marked_slots" => alloc.live_count() as i64,
            "heap_free_slots" => alloc.free_count() as i64,
            "total_allocated_pages" => alloc.total_allocated_pages() as i64,
            "total_freed_pages" => alloc.total_freed_pages() as i64,
            "total_allocated_objects" => alloc.total_allocated() as i64,
            "total_freed_objects" => alloc.total_freed() as i64,
            "malloc_increase_bytes" => crate::alloc::malloc_amount() as i64,
            "malloc_increase_bytes_limit" => crate::alloc::malloc_gc_threshold() as i64,
            "minor_gc_count" => alloc.minor_gc_count() as i64,
            "major_gc_count" => alloc.major_gc_count() as i64,
            "remembered_wb_unprotected_objects" => alloc.remembered_count() as i64,
            "old_objects" => alloc.old_count() as i64,
            "old_objects_limit" => alloc.old_objects_limit() as i64,
            // No finalizers, no compaction, no separate old-gen malloc
            // accounting.
            "heap_final_slots"
            | "compact_count"
            | "read_barrier_faults"
            | "total_moved_objects"
            | "remembered_wb_unprotected_objects_limit"
            | "oldmalloc_increase_bytes"
            | "oldmalloc_increase_bytes_limit" => 0,
            _ => return None,
        })
    })
}

///
/// ### GC.stat
///
/// - stat -> Hash
/// - stat(key) -> Integer
/// - stat(hash) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/stat.html]
#[monoruby_builtin]
fn stat(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.try_arg(0) {
        // `GC.stat(nil)` behaves like the no-argument form.
        Some(arg) if arg.is_nil() => stat_full_hash(vm, globals),
        // `GC.stat(hash)` fills the given hash (preserving keys we don't
        // set) and returns the *same* hash object.
        Some(arg) if arg.try_hash_ty().is_some() => {
            let mut hash = arg.try_hash_ty().unwrap();
            for key in STAT_KEYS {
                let v = Value::integer(stat_value(key).unwrap_or(0));
                hash.insert(Value::symbol_from_str(key), v, vm, globals)?;
            }
            Ok(arg)
        }
        // `GC.stat(:key)` / `GC.stat("key")` returns a single Integer.
        Some(arg) if arg.try_symbol().is_some() || arg.is_str().is_some() => {
            let key_name = arg.expect_symbol_or_string(globals)?;
            let name = key_name.to_string();
            match stat_value(&name) {
                Some(v) => Ok(Value::integer(v)),
                None => Err(MonorubyErr::argumenterr(format!("unknown key: {}", name))),
            }
        }
        // Anything else (Integer, Array, …) is a TypeError in CRuby.
        Some(_) => Err(MonorubyErr::typeerr("non-hash or symbol given")),
        None => stat_full_hash(vm, globals),
    }
}

/// Build the full `GC.stat` Hash (all keys → Integer values).
fn stat_full_hash(vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
    let mut inner = HashmapInner::default();
    for key in STAT_KEYS {
        let v = Value::integer(stat_value(key).unwrap_or(0));
        inner.insert(Value::symbol_from_str(key), v, vm, globals)?;
    }
    Ok(Value::hash_from_inner(inner))
}

///
/// ### GC.count
///
/// - count -> Integer
///
/// The number of times GC has run so far.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/count.html]
#[monoruby_builtin]
fn count(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let n = crate::alloc::ALLOC.with(|alloc| alloc.borrow().total_gc_counter());
    Ok(Value::integer(n as i64))
}

///
/// Internal: ask for a collection at the next VM safepoint.
///
/// Running one inline here would be unsafe — only at a safepoint are the
/// JIT caller's live registers spilled where the root scan can see them
/// — so `GC.start` (Ruby side, `builtins/gc.rb`) requests here and then
/// crosses a safepoint before returning. A truthy `full_mark` forces the
/// pending collection to be a major one; otherwise the collector picks
/// minor or major as usual.
#[monoruby_builtin]
fn request_gc(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::alloc::request_gc(lfp.arg(0).as_bool());
    Ok(Value::nil())
}

///
/// ### GC.total_time
///
/// - total_time -> Integer
///
/// Nanoseconds spent collecting so far. Frozen while
/// `GC.measure_total_time` is false.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/total_time.html]
#[monoruby_builtin]
fn total_time(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = crate::alloc::ALLOC.with(|alloc| alloc.borrow().gc_time_ns());
    Ok(Value::integer(ns as i64))
}

///
/// ### GC.measure_total_time
///
/// - measure_total_time -> bool
/// - measure_total_time=(flag) -> flag
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/measure_total_time.html]
#[monoruby_builtin]
fn measure_total_time(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let flag = crate::alloc::ALLOC.with(|alloc| alloc.borrow().measure_time());
    Ok(Value::bool(flag))
}

#[monoruby_builtin]
fn set_measure_total_time(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let flag = lfp.arg(0).as_bool();
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().set_measure_time(flag));
    Ok(lfp.arg(0))
}

///
/// ### GC.stress
///
/// - stress -> bool
/// - stress=(flag) -> flag
///
/// With stress on, every VM safepoint collects: the poll flag is put
/// back into its trigger band at the end of each collection. CRuby
/// collects once per *allocation*; monoruby cannot, because the JIT
/// inlines the allocation fast path, but collecting at every safepoint
/// shakes out missing roots the same way.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/stress.html]
#[monoruby_builtin]
fn stress(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let flag = crate::alloc::ALLOC.with(|alloc| alloc.borrow().stress());
    Ok(Value::bool(flag))
}

#[monoruby_builtin]
fn set_stress(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let flag = lfp.arg(0).as_bool();
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().set_stress(flag));
    Ok(lfp.arg(0))
}

///
/// Internal: `GC.config[:rgengc_allow_full_mark]` accessors. Off means
/// `decide_gc_kind` never picks a major collection on its own.
#[monoruby_builtin]
fn allow_full_mark(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let flag = crate::alloc::ALLOC.with(|alloc| alloc.borrow().allow_full_mark());
    Ok(Value::bool(flag))
}

#[monoruby_builtin]
fn set_allow_full_mark(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let flag = lfp.arg(0).as_bool();
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().set_allow_full_mark(flag));
    Ok(lfp.arg(0))
}

//
// GC::Profiler
//

#[monoruby_builtin]
fn profiler_enabled(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let flag = crate::alloc::ALLOC.with(|alloc| alloc.borrow().profile_enabled());
    Ok(Value::bool(flag))
}

#[monoruby_builtin]
fn profiler_enable(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().set_profile_enabled(true));
    Ok(Value::nil())
}

#[monoruby_builtin]
fn profiler_disable(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().set_profile_enabled(false));
    Ok(Value::nil())
}

#[monoruby_builtin]
fn profiler_clear(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    crate::alloc::ALLOC.with(|alloc| alloc.borrow_mut().clear_profile());
    Ok(Value::nil())
}

///
/// ### GC::Profiler.result
///
/// - result -> String
///
/// The collected records in CRuby's report layout.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC=3a=3aProfiler/s/result.html]
#[monoruby_builtin]
fn profiler_result(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let s = crate::alloc::ALLOC.with(|alloc| {
        let alloc = alloc.borrow();
        let records = alloc.profile_records();
        // CRuby reports nothing at all unless the profiler is running
        // and has something to show.
        if !alloc.profile_enabled() || records.is_empty() {
            return String::new();
        }
        // The header counts every collection the process has run (CRuby
        // prints `GC.count` here); the rows are the ones recorded since
        // the profiler was enabled.
        let mut s = format!("GC {} invokes.\n", alloc.total_gc_counter());
        s += "Index    Invoke Time(sec)       Use Size(byte)     Total Size(byte)         Total Object                    GC Time(ms)\n";
        for (i, r) in records.iter().enumerate() {
            s += &format!(
                "{:5} {:19.3} {:20} {:20} {:20} {:30.20}\n",
                i + 1,
                r.invoke_time,
                r.heap_use_size,
                r.heap_total_size,
                r.heap_total_objects,
                r.gc_time_ns as f64 / 1_000_000.0,
            );
        }
        s
    });
    Ok(Value::string(s))
}

///
/// ### GC::Profiler.total_time
///
/// - total_time -> Float
///
/// Seconds spent in the collections the profiler recorded.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC=3a=3aProfiler/s/total_time.html]
#[monoruby_builtin]
fn profiler_total_time(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let secs = crate::alloc::ALLOC.with(|alloc| {
        alloc
            .borrow()
            .profile_records()
            .iter()
            .map(|r| r.gc_time_ns as f64)
            .sum::<f64>()
            / 1_000_000_000.0
    });
    Ok(Value::float(secs))
}

///
/// ### GC::Profiler.raw_data
///
/// - raw_data -> [Hash] | nil
///
/// One Hash per recorded collection; `nil` while the profiler is off.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC=3a=3aProfiler/s/raw_data.html]
#[monoruby_builtin]
fn profiler_raw_data(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let (enabled, records) = crate::alloc::ALLOC.with(|alloc| {
        let alloc = alloc.borrow();
        (alloc.profile_enabled(), alloc.profile_records().to_vec())
    });
    if !enabled {
        return Ok(Value::nil());
    }
    let mut ary = vec![];
    for r in records {
        let mut inner = HashmapInner::default();
        let mut put = |k: &str, v: Value| -> Result<()> {
            inner.insert(Value::symbol_from_str(k), v, vm, globals)?;
            Ok(())
        };
        put("GC_TIME", Value::float(r.gc_time_ns as f64 / 1e9))?;
        put("GC_INVOKE_TIME", Value::float(r.invoke_time))?;
        put("HEAP_USE_SIZE", Value::integer(r.heap_use_size as i64))?;
        put("HEAP_TOTAL_SIZE", Value::integer(r.heap_total_size as i64))?;
        put(
            "HEAP_TOTAL_OBJECTS",
            Value::integer(r.heap_total_objects as i64),
        )?;
        // monoruby never collects lazily, so every recorded cycle marked;
        // the informative bit is whether it was a full-heap one.
        put("GC_IS_MARKED", Value::bool(true))?;
        put("MAJOR_GC", Value::bool(r.major))?;
        ary.push(Value::hash_from_inner(inner));
    }
    Ok(Value::array_from_vec(ary))
}

///
/// ### GC.enable
///
/// - enable -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/enable.html]
#[monoruby_builtin]
fn enable(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let was_disabled = !Globals::gc_enable(true);
    Ok(Value::bool(was_disabled))
}

///
/// ### GC.disable
///
/// - disable -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/disable.html]
#[monoruby_builtin]
fn disable(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let was_disabled = !Globals::gc_enable(false);
    Ok(Value::bool(was_disabled))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn gc_stat() {
        run_test("GC.stat.class");
        run_test("GC.stat.is_a?(Hash)");
        run_test("GC.stat(:count).is_a?(Integer)");
    }

    #[test]
    fn gc_count() {
        run_test("GC.count.is_a?(Integer)");
    }

    #[test]
    fn gc_enable_disable() {
        run_test("GC.enable; GC.disable; GC.disable");
        run_test("GC.enable; GC.disable; GC.enable");
    }

    #[test]
    fn gc_start() {
        run_test("GC.start");
    }

    #[test]
    fn gc_stat_variants() {
        run_test("GC.stat(:count).is_a?(Integer)");
        run_test("GC.stat(nil).is_a?(Hash)");
        run_test("GC.stat.values.all? { |v| v.is_a?(Integer) }");
        run_test(
            "h = { count: \"x\", __other__: \"y\" }; r = GC.stat(h); \
             [r.equal?(h), h[:count].is_a?(Integer), h[:__other__]]",
        );
        run_test("(GC.stat(7) rescue $!.class)");
        run_test("(GC.stat(:bogus_key) rescue $!.class)");
    }

    #[test]
    fn gc_stat_is_consistent() {
        // The counters describe one heap, so they have to agree with
        // each other — that is what makes them worth reporting.
        run_test_once(
            r##"
            GC.start
            s = GC.stat
            [
              s[:heap_live_slots] <= s[:heap_available_slots],
              s[:heap_free_slots] <= s[:heap_available_slots],
              s[:heap_allocatable_slots] <= s[:heap_available_slots],
              s[:total_allocated_objects] >= s[:total_freed_objects],
              s[:total_allocated_pages] >= s[:total_freed_pages],
              s[:count] == s[:minor_gc_count] + s[:major_gc_count],
              s[:major_gc_count] > 0,
              s[:heap_allocated_pages] > 0,
              s[:heap_available_slots] > 0,
              s[:old_objects] <= s[:heap_live_slots],
              s[:malloc_increase_bytes_limit] > 0,
              s[:time] >= s[:marking_time],
              s[:time] >= s[:sweeping_time],
              s.values.all? { |v| v.is_a?(Integer) && v >= 0 },
            ]
            "##,
        );
    }

    #[test]
    fn gc_start_is_synchronous() {
        // `GC.start` must have collected by the time it returns, with no
        // loop of the caller's own to reach a safepoint.
        run_test_once(
            r##"
            before = GC.count
            GC.start
            after = GC.count
            major = GC.stat(:major_gc_count)
            GC.start
            [after > before, GC.stat(:major_gc_count) > major]
            "##,
        );
    }

    #[test]
    fn gc_start_full_mark_selects_the_kind() {
        // `full_mark: false` asks for a young-generation collection; the
        // default asks for a full one.
        run_test_once(
            r##"
            minor = GC.stat(:minor_gc_count)
            major = GC.stat(:major_gc_count)
            GC.start(full_mark: false)
            a = [GC.stat(:minor_gc_count) > minor, GC.stat(:major_gc_count) == major]
            major = GC.stat(:major_gc_count)
            GC.start
            [a, GC.stat(:major_gc_count) > major]
            "##,
        );
    }

    #[test]
    fn gc_time_measurement() {
        // `GC.total_time` is nanoseconds and only moves while
        // `measure_total_time` is on.
        run_test_once(
            r##"
            GC.measure_total_time = false
            frozen = GC.total_time
            GC.start
            a = GC.total_time == frozen
            GC.measure_total_time = true
            before = GC.total_time
            GC.start
            b = GC.total_time > before
            [GC.total_time.is_a?(Integer), a, b, GC.measure_total_time]
            "##,
        );
    }

    #[test]
    fn gc_stress_collects() {
        // Stress mode collects far more often than allocation pressure
        // alone would.
        run_test_once(
            r##"
            GC.stress = false
            before = GC.count
            GC.stress = true
            100.times { Object.new }
            grew = GC.count - before
            GC.stress = false
            [GC.stress, grew > 10]
            "##,
        );
    }

    #[test]
    fn gc_auto_compact_is_unsupported() {
        // monoruby never moves an object, so both accessors raise —
        // exactly as CRuby does where its GC cannot compact. Written so
        // the answer is the same on both.
        run_test_once(
            r##"
            r = begin; GC.auto_compact; rescue NotImplementedError; false; end
            w = begin; GC.auto_compact = false; rescue NotImplementedError; false; end
            [[true, false].include?(r), [true, false].include?(w)]
            "##,
        );
    }

    #[test]
    fn gc_config() {
        run_test("GC.config.is_a?(Hash)");
        run_test("GC.config[:implementation]");
        run_test("GC.config({}) == GC.config");
        run_test("GC.config(nil) == GC.config");
        run_test("(GC.config(implementation: \"x\") rescue $!.class)");
        // `:rgengc_allow_full_mark` round-trips, unknown keys are ignored
        // and the value is coerced to a boolean.
        run_test_once(
            r##"
            was = GC.config[:rgengc_allow_full_mark]
            begin
              a = GC.config(rgengc_allow_full_mark: nil)[:rgengc_allow_full_mark]
              b = GC.config[:rgengc_allow_full_mark]
              c = GC.config(rgengc_allow_full_mark: 1.23)[:rgengc_allow_full_mark]
              d = GC.config(foo: "bar")[:rgengc_allow_full_mark]
              [a, b, c, d]
            ensure
              GC.config(rgengc_allow_full_mark: was)
            end
            "##,
        );
    }

    #[test]
    fn gc_config_suppresses_self_chosen_major() {
        // With full marking off the collector only ever picks a minor
        // collection; an explicit `GC.start` still forces a major.
        run_test_once(
            r##"
            GC.config(rgengc_allow_full_mark: false)
            begin
              major = GC.stat(:major_gc_count)
              200000.times { Object.new }
              a = GC.stat(:major_gc_count) == major
              GC.start
              [a, GC.stat(:major_gc_count) > major]
            ensure
              GC.config(rgengc_allow_full_mark: true)
            end
            "##,
        );
    }

    #[test]
    fn gc_garbage_collect() {
        run_test("o = Object.new; o.extend(GC); o.garbage_collect");
    }

    #[test]
    fn gc_profiler() {
        run_test(
            "GC::Profiler.enable; r = GC::Profiler.enabled?; \
             GC::Profiler.disable; [r, GC::Profiler.enabled?]",
        );
        run_test("GC::Profiler.result.is_a?(String)");
        run_test("GC::Profiler.total_time.is_a?(Float)");
        run_test("GC::Profiler.clear");
    }

    #[test]
    fn gc_profiler_records_each_collection() {
        // One record per collection while enabled, none once cleared or
        // disabled, and `raw_data` carries the per-cycle numbers.
        run_test_once(
            r##"
            GC::Profiler.clear
            GC::Profiler.enable
            begin
              GC.start
              GC.start
              data = GC::Profiler.raw_data
              # The header counts every collection the process ran, so
              # only its shape is comparable across implementations.
              head = GC::Profiler.result.lines[0]
              header = head.start_with?("GC ") && head.end_with?(" invokes.\n")
              keys = data[0].keys.sort
              ok = data.all? do |r|
                r[:GC_TIME].is_a?(Float) && r[:GC_TIME] >= 0 &&
                  r[:GC_INVOKE_TIME].is_a?(Float) &&
                  r[:HEAP_USE_SIZE] <= r[:HEAP_TOTAL_SIZE] &&
                  r[:HEAP_TOTAL_OBJECTS] > 0
              end
              total = GC::Profiler.total_time
              GC::Profiler.clear
              [data.size >= 2, ok, header, keys.include?(:GC_TIME),
               total.is_a?(Float), total >= 0, GC::Profiler.result.lines[0]]
            ensure
              GC::Profiler.disable
              GC::Profiler.clear
            end
            "##,
        );
    }

    #[test]
    fn gc_profiler_raw_data_is_nil_when_disabled() {
        run_test_once("GC::Profiler.disable; GC::Profiler.raw_data");
    }
}
