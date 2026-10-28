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
# mkmf links its conftest executables (`have_func`, `have_library`, ...)
# and the extension with LIBRUBYARG; both are the archive, so
# `have_func("rb_...")` answers for the layer.
RbConfig.fire_update!('LIBRUBYARG_SHARED', __capi_arg)
RbConfig.fire_update!('LIBRUBYARG_STATIC', __capi_arg)
RbConfig.fire_update!('LIBRUBYARG', __capi_arg)
# `libdirname` names the CONFIG key of the directory that holds LIBRUBY
# (CRuby: 'libdir', or 'archlibdir' on some platforms). Consumers that
# locate the library through it -- ruby/spec's C API helper stats
# `CONFIG[CONFIG['libdirname']]/LIBRUBY` to decide whether to rebuild, and
# mkmf puts `-L$(<libdirname>)` on every link line -- must land in capi/,
# where libmonoruby_capi.a is, not in lib/. A key of its own keeps
# `libdir` (and everything derived from it: rubylibprefix, the gem dirs)
# where it is; mkmf writes every `*dir` key into the Makefile, so
# `$(capidir)` resolves there.
RbConfig.fire_update!('capidir', __capi_lib)
RbConfig.fire_update!('libdirname', 'capidir')
# A `.so` with no `libruby` to find: nothing to put in an rpath.
RbConfig.fire_update!('LIBRUBY_RELATIVE', 'no')
# The snapshot names its host's tools by absolute path (`/usr/bin/mkdir`,
# which is `/bin/mkdir` on macOS); `make install` runs them, so resolve
# them on PATH instead.
RbConfig.fire_update!('MKDIR_P', 'mkdir -p')
RbConfig.fire_update!('MAKEDIRS', 'mkdir -p')
RbConfig.fire_update!('INSTALL', 'install -c')
RbConfig.fire_update!('GREP', 'grep')
RbConfig.fire_update!('EGREP', 'grep -E')
RbConfig.fire_update!('SHELL', '/bin/sh')

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

# The extension's own link line -- and only that; mkmf's conftests do not
# use EXTDLDFLAGS -- also gets the stubs archive build.rs generates: one
# NotImplementedError-raising stub per CRuby API function the layer lacks
# (doc/c_api.md §3.16). An extension that references such a function
# still builds and loads, and only its call fails; `have_func` keeps
# answering "no" for it, so the extension's `#ifdef HAVE_RB_FOO`
# fallbacks stay in force.
RbConfig.fire_update!('EXTDLDFLAGS', "#{RbConfig::CONFIG['EXTDLDFLAGS']} -L#{__capi_lib} -lmonoruby_capi_stubs".strip)

# GCC 14 and clang 16 make a call to an undeclared function an error (C99
# and later); it was a warning before. An extension that calls a CRuby
# API function the layer's headers do not declare (one the stubs archive
# answers for, doc/c_api.md §3.16) must still compile, as it did with the
# older compilers, so the error goes back to being a warning.
RbConfig.fire_update!('warnflags', "#{RbConfig::CONFIG['warnflags']} -Wno-error=implicit-function-declaration")
