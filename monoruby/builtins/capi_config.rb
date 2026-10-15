# RbConfig for building C extensions against monoruby (doc/c_api.md §3.10).
#
# mkmf reads RbConfig to find `ruby.h`, to pick the C compiler and its
# flags, and to name the library an extension links against. The
# vendored rbconfig.rb answers all of that for the CRuby it was
# snapshotted from, so an `extconf.rb` run on monoruby would compile
# against *that* Ruby's headers and produce a library monoruby cannot
# load. Point the header directories at the install root's `include/`
# (the C API layer's `ruby.h` and friends) and the library at
# `capi/libmonoruby_capi.a` (the layer itself, compiled by build.rs), so
# an extension built here is an extension of monoruby: `rb_*` resolve
# inside the extension, and the archive's `monoruby_capi_init` is what
# the loader looks for.
#
# `RbConfig.fire_update!` sets both CONFIG and MAKEFILE_CONFIG and
# re-expands every value that referred to the key (`sitehdrdir =
# $(rubyhdrdir)/site_ruby`, ...), which is what the generated Makefile
# reads. The values are literal paths, not `$(...)` references, so they
# survive into a Makefile that defines neither `capidir` nor
# `RUBY_SO_NAME`.
#
# This file lives at `<install root>/builtins/capi_config.rb`.
__capi_root = File.dirname(File.dirname(File.realpath(__FILE__)))
__capi_include = "#{__capi_root}/include"
__capi_lib = "#{__capi_root}/capi"
__capi_arg = "-L#{__capi_lib} -lmonoruby_capi"

RbConfig.fire_update!('rubyhdrdir', __capi_include)
# The layer has one header tree for every platform; `ruby/config.h`
# (which mkmf lists as a prerequisite of every object) is in it.
RbConfig.fire_update!('rubyarchhdrdir', __capi_include)
# `topdir` is where mkmf looks when `rubyhdrdir` has no ruby/ruby.h; keep
# it consistent anyway.
RbConfig.fire_update!('topdir', __capi_include)
RbConfig.fire_update!('RUBY_SO_NAME', 'monoruby_capi')
RbConfig.fire_update!('LIBRUBY_SO', 'libmonoruby_capi.a')
RbConfig.fire_update!('LIBRUBY_A', 'libmonoruby_capi.a')
RbConfig.fire_update!('LIBRUBY', 'libmonoruby_capi.a')
# mkmf links a conftest executable with LIBRUBYARG_STATIC and the
# extension with LIBRUBYARG (= LIBRUBYARG_SHARED); both are the archive,
# so `have_func("rb_...")` answers for the layer.
RbConfig.fire_update!('LIBRUBYARG_SHARED', __capi_arg)
RbConfig.fire_update!('LIBRUBYARG_STATIC', __capi_arg)
RbConfig.fire_update!('LIBRUBYARG', __capi_arg)
# A `.so` with no `libruby` to find: nothing to put in an rpath.
RbConfig.fire_update!('LIBRUBY_RELATIVE', 'no')

# The toolchain keys of the snapshot are GNU/Linux's (gcc, GNU ld flags).
# On macOS CRuby's own rbconfig answers with clang and a `-bundle` link;
# `DLEXT` is already `bundle` there (startup.rb). `host_os` is the
# version-stripped name startup.rb derived from RUBY_PLATFORM.
if RbConfig::CONFIG['host_os'] == 'darwin'
  {
    'CC' => 'clang',
    'CXX' => 'clang++',
    'CPP' => '$(CC) -E',
    'LDSHARED' => '$(CC) -dynamic -bundle',
    'LDSHAREDXX' => '$(CXX) -dynamic -bundle',
    'DLDFLAGS' => '-Wl,-undefined,dynamic_lookup',
    'LDFLAGS' => '-L.',
    'CCDLFLAGS' => '-fno-common',
    'EXTDLDFLAGS' => '',
    'ARCH_FLAG' => '',
    'LIBS' => '',
    'MAINLIBS' => '',
    'DLDLIBS' => '',
    'warnflags' => '-Wall -Wextra -Wno-unused-parameter -Wno-unused-value ' \
                   '-Wno-missing-field-initializers',
    'AR' => 'ar',
    'RANLIB' => 'ranlib',
    'NM' => 'nm',
    'LD' => 'ld',
    'OBJDUMP' => 'objdump',
    'OBJCOPY' => '',
    'STRIP' => 'strip -A -n',
    'GNU_LD' => 'no',
    'LIBPATHENV' => 'DYLD_LIBRARY_PATH',
    'RPATHFLAG' => '',
  }.each { |k, v| RbConfig.fire_update!(k, v) }
end
