require "mkmf"

# The probes mkmf answers by linking a conftest against the interpreter's
# library: on monoruby that is libmonoruby_capi.a, so a function the C
# API layer has is found and one it does not have is not.
abort "rb_str_new_cstr is missing" unless have_func("rb_str_new_cstr", "ruby.h")
have_func("rb_no_such_function_xyzzy", "ruby.h")
have_header("ruby/config.h")
have_type("rb_data_type_t", "ruby.h")

$defs << "-DCAPI_GEM_FROM_EXTCONF=1"
create_makefile("capi_gem/capi_gem")
