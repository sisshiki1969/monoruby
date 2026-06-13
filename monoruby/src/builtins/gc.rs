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
    globals.define_builtin_module_func_rest(klass, "start", start);
}

/// The `GC.stat` keys, in CRuby order. Only a handful map to real
/// allocator counters (see [`stat_value`]); the rest are reported as 0
/// because monoruby's mark-and-sweep collector has no equivalent.
const STAT_KEYS: &[&str] = &[
    "count",
    "heap_allocated_pages",
    "heap_sorted_length",
    "heap_allocatable_pages",
    "heap_available_slots",
    "heap_live_slots",
    "heap_free_slots",
    "heap_final_slots",
    "heap_marked_slots",
    "heap_eden_pages",
    "heap_tomb_pages",
    "total_allocated_pages",
    "total_freed_pages",
    "total_allocated_objects",
    "total_freed_objects",
    "malloc_increase_bytes",
    "malloc_increase_bytes_limit",
    "minor_gc_count",
    "major_gc_count",
    "remembered_wb_unprotected_objects",
    "remembered_wb_unprotected_objects_limit",
    "old_objects",
    "old_objects_limit",
    "oldmalloc_increase_bytes",
    "oldmalloc_increase_bytes_limit",
];

/// Value of a `GC.stat` key. Returns `None` for keys we don't recognise
/// (so `GC.stat(:bogus)` can raise), real allocator counters for the
/// keys we track, and 0 for the remaining known-but-unsupported keys.
fn stat_value(name: &str) -> Option<i64> {
    crate::alloc::ALLOC.with(|alloc| {
        let alloc = alloc.borrow();
        Some(match name {
            "count" => alloc.total_gc_counter() as i64,
            "minor_gc_count" => alloc.minor_gc_count() as i64,
            "major_gc_count" => alloc.major_gc_count() as i64,
            "total_allocated_objects" => alloc.total_allocated() as i64,
            "heap_live_slots" | "heap_marked_slots" => alloc.live_count() as i64,
            "heap_free_slots" => alloc.free_count() as i64,
            _ if STAT_KEYS.contains(&name) => 0,
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
/// ### GC.start
///
/// - start(full_mark: true, immediate_mark: true, immediate_sweep: true) -> nil
///
/// Force a garbage collection. The keyword arguments are accepted for
/// compatibility but ignored — `GC.start` always runs a full collection.
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/start.html]
#[monoruby_builtin]
fn start(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Running a collection inline here is unsafe (the JIT caller's live
    // registers aren't spilled for the GC root scan), so request a full
    // collection at the next VM safepoint, where registers are saved.
    crate::alloc::request_gc(true);
    Ok(Value::nil())
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
    fn gc_mode_flags() {
        // Explicit set/read round-trips (default-independent: CRuby's
        // default for these knobs is build/platform-specific).
        run_test("GC.stress = true; r = GC.stress; GC.stress = false; [r, GC.stress]");
        run_test(
            "GC.measure_total_time = true; a = GC.measure_total_time; \
             GC.measure_total_time = false; b = GC.measure_total_time; [a, b]",
        );
        run_test("GC.total_time.is_a?(Integer)");
        // `auto_compact=` may raise NotImplementedError on some platforms.
        run_test("(GC.auto_compact = false rescue nil); [true, false].include?(GC.auto_compact)");
    }

    #[test]
    fn gc_config() {
        run_test("GC.config.is_a?(Hash)");
        run_test("GC.config[:implementation]");
        run_test("GC.config({}) == GC.config");
        run_test("GC.config(nil) == GC.config");
        run_test("(GC.config(implementation: \"x\") rescue $!.class)");
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
}
