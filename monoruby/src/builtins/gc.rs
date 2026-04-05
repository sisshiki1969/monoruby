use super::*;

//
// GC module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("GC").id();
    globals.define_builtin_module_func_with(klass, "stat", stat, 0, 1, false);
    globals.define_builtin_module_func(klass, "enable", enable, 0);
    globals.define_builtin_module_func(klass, "disable", disable, 0);
}

///
/// ### GC.stat
///
/// - stat -> Hash
/// - stat(key) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/GC/s/stat.html]
#[monoruby_builtin]
fn stat(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys: &[&str] = &[
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

    if let Some(key) = lfp.try_arg(0) {
        let key_name = key.expect_symbol_or_string(globals)?;
        let name = key_name.to_string();
        if keys.contains(&name.as_str()) {
            Ok(Value::integer(0))
        } else {
            Err(MonorubyErr::argumenterr(format!(
                "unknown key: {}",
                name
            )))
        }
    } else {
        let mut inner = HashmapInner::default();
        for key_name in keys {
            let k = Value::symbol_from_str(key_name);
            let v = Value::integer(0);
            inner.insert(k, v, vm, globals)?;
        }
        Ok(Value::hash_from_inner(inner))
    }
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
}
