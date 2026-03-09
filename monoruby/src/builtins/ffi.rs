use super::*;

// ---------------------------------------------------------------------------
// FFI module initialisation
//
// Registers low-level builtin functions directly under the FFI module so that
// startup/ffi_c.rb does not need to depend on the Fiddle module.
//
// The actual implementations are reused from fiddle.rs and kernel.rs.
// ---------------------------------------------------------------------------

pub(super) fn init(globals: &mut Globals) {
    let ffi = globals.define_toplevel_module("FFI").id();

    // Low-level primitives (reuse fiddle.rs implementations)
    globals.define_builtin_module_func(ffi, "___call", fiddle::fiddle_call, 4);
    globals.define_builtin_module_func(ffi, "___read", fiddle::fiddle_read, 2);
    globals.define_builtin_module_func(ffi, "___write", fiddle::fiddle_write, 3);
    globals.define_builtin_module_func(ffi, "___read_string", fiddle::fiddle_read_string, 1);
    globals.define_builtin_module_func(ffi, "___read_bytes", fiddle::fiddle_read_bytes, 2);
    globals.define_builtin_module_func(ffi, "___write_bytes", fiddle::fiddle_write_bytes, 2);
    globals.define_builtin_module_func(ffi, "___free", fiddle::fiddle_free, 1);

    // dlopen/dlsym/malloc (reuse kernel.rs implementations)
    globals.define_builtin_module_func_with(ffi, "___dlopen", kernel::dlopen, 1, 2, false);
    globals.define_builtin_module_func(ffi, "___dlsym", kernel::dlsym, 2);
    globals.define_builtin_module_func_with(ffi, "___malloc", kernel::malloc, 1, 2, false);

    // SIZEOF constants for x86-64 Linux
    for (name, size) in [
        ("SIZEOF_VOIDP", 8i64),
        ("SIZEOF_CHAR", 1),
        ("SIZEOF_SHORT", 2),
        ("SIZEOF_INT", 4),
        ("SIZEOF_LONG", 8),
        ("SIZEOF_LONG_LONG", 8),
        ("SIZEOF_FLOAT", 4),
        ("SIZEOF_DOUBLE", 8),
        ("SIZEOF_BOOL", 4),
        ("SIZEOF_INTPTR_T", 8),
        ("SIZEOF_UINTPTR_T", 8),
        ("SIZEOF_PTRDIFF_T", 8),
        ("SIZEOF_SIZE_T", 8),
        ("SIZEOF_SSIZE_T", 8),
    ] {
        globals.set_constant_by_str(ffi, name, Value::integer(size));
    }
}
