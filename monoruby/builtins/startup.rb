require 'rbconfig'

# The vendored `rbconfig.rb` (snapshotted from the build host's CRuby
# install) hard-codes the build host's `prefix` (e.g.
# `/opt/rbenv/versions/ruby-4.0.2-custom`), because its `TOPDIR`
# detection (`File.dirname(__FILE__).chomp!("/lib/ruby/4.0.0/x86_64-linux")`)
# does not match monoruby's actual install layout (`~/.monoruby/lib`).
# Anything that resolves a Ruby binary via RbConfig — most notably
# mspec's `resolve_ruby_exe`, which requires the path to exist
# (`File.executable?`) — would then fail on every machine that is
# not the build host, breaking ruby/spec runs.
#
# Override the host-specific keys at process startup so they point at
# the actually-running monoruby executable (resolved via
# `/proc/self/exe`; monoruby is x86-64 Linux only). User-set
# `RUBY_EXE` continues to win because consumers read it before
# falling back to RbConfig.
if File.exist?('/proc/self/exe')
  exe = (File.realpath('/proc/self/exe') rescue '/proc/self/exe')
  bindir = File.dirname(exe)
  install_name = File.basename(exe)
  RbConfig::CONFIG['bindir'] = bindir
  RbConfig::CONFIG['ruby_install_name'] = install_name
  RbConfig::CONFIG['RUBY_INSTALL_NAME'] = install_name
  # `prefix` / `exec_prefix` are the parent of bindir in a standard
  # `/bin` layout (works for both `/usr/local/bin/monoruby` and
  # `~/.cargo/bin/monoruby`).
  prefix = File.dirname(bindir)
  RbConfig::CONFIG['prefix'] = prefix
  RbConfig::CONFIG['exec_prefix'] = prefix
end


# Re-derive host-Ruby-dependent RbConfig keys from env vars populated
# by `Globals::new` (Rust side) when GEM_PATH points at a host CRuby
# install.
#
# The vendored rbconfig.rb captures the *build* host's prefix
# (e.g. `/opt/rbenv/versions/4.0.2`) but expands every `$(rubylibprefix)`
# / `$(libdir)` reference eagerly at load time. After that the only way
# to fix downstream consumers (chiefly `Gem.default_dir`, which sources
# default gems like `bundler` from
# `$(rubylibprefix)/gems/$(ruby_version)`) is to overwrite the expanded
# values directly. The Rust side derives them from GEM_PATH (avoiding
# Ruby-level String operations that would otherwise set `$~` at this
# top-level scope and leak `defined?($&)` truthiness into user code)
# and stashes the result in two env vars; we just read and apply.
host_rubylibprefix = ENV['MONORUBY_HOST_RUBYLIBPREFIX']
ruby_api_version   = ENV['MONORUBY_HOST_RUBY_API_VERSION']
host_configured = host_rubylibprefix && !host_rubylibprefix.empty? &&
                  ruby_api_version && !ruby_api_version.empty?

# Without a configured host CRuby (no GEM_PATH at startup, or a consumer
# such as `Bundler.setup` cleared it), the vendored rbconfig.rb's
# `rubylibprefix` still points at the *build* host's prefix (e.g.
# `/opt/rbenv/versions/ruby-4.0.2-custom/lib/ruby`), which exists on no
# other machine. `Gem.default_dir` (== `<rubylibprefix>/gems/<ruby_version>`)
# would then be that bogus path and `Gem::Specification` enumeration would
# silently come up empty. Fall back to a prefix derived from monoruby's own
# install root — this file lives at `<root>/builtins/startup.rb` — so the
# value is at least a real, local path rather than a snapshot-build-machine
# one. (No gems live there in the no-host case, which is fine: enumeration
# is legitimately empty instead of pointed at a nonexistent directory.)
unless host_configured
  install_root = (File.dirname(File.dirname(File.realpath(__FILE__))) rescue nil)
  if install_root && !install_root.empty?
    host_rubylibprefix = "#{install_root}/lib/ruby"
    ruby_api_version   = RbConfig::CONFIG['ruby_version']
  end
end

if host_rubylibprefix && !host_rubylibprefix.empty? && ruby_api_version && !ruby_api_version.empty?
  RbConfig::CONFIG['rubylibprefix']  = host_rubylibprefix
  RbConfig::CONFIG['rubylibdir']     = "#{host_rubylibprefix}/#{ruby_api_version}"
  RbConfig::CONFIG['sitedir']        = "#{host_rubylibprefix}/site_ruby"
  RbConfig::CONFIG['vendordir']      = "#{host_rubylibprefix}/vendor_ruby"
  # libdir is one level above rubylibprefix (rubylibprefix == libdir/ruby).
  RbConfig::CONFIG['libdir']         = File.dirname(host_rubylibprefix)
  # The vendored rbconfig.rb hard-codes ENABLE_SHARED="no" (monoruby is a
  # static binary), which makes Gem.extension_api_version append "-static".
  # That mismatches the host's gem extension layout
  # (".../extensions/<plat>/<api>/" without "-static"), so rubygems/bundler
  # judge every host C-extension gem as having unbuilt extensions and skip
  # it — a Gemfile.lock pinning json/cgi/… then fails with GemNotFound.
  # When we are resolving against a host CRuby's gems, report "yes" so the
  # api version matches and those gems resolve. monoruby still can't run
  # their native .so, but require.rs pins its own pure-Ruby stubs ahead of
  # $LOAD_PATH for the libraries it replaces, so they load the stub. Only
  # meaningful for the host case; leave the vendored value otherwise.
  RbConfig::CONFIG['ENABLE_SHARED']  = 'yes' if host_configured
end

# CRuby's default $LOAD_PATH ends with the "gem prelude" tail — the
# site_ruby / vendor_ruby / rubylib directories derived from RbConfig,
# each entry carrying @gem_prelude_index (rubygems uses it to splice
# gem paths ahead of the defaults). monoruby's load path is built from
# the runtime host probe instead, so append the RbConfig-derived tail
# here (skipping entries already present) and tag it. Non-existent
# directories are harmless — CRuby also keeps them in $LOAD_PATH.
begin
  arch = RbConfig::CONFIG['arch']
  ver  = RbConfig::CONFIG['ruby_version']
  prelude = []
  if (site = RbConfig::CONFIG['sitedir'])
    prelude << File.join(site, ver) << File.join(site, ver, arch) << site
  end
  if (vendor = RbConfig::CONFIG['vendordir'])
    prelude << File.join(vendor, ver) << File.join(vendor, ver, arch) << vendor
  end
  if (rubylib = RbConfig::CONFIG['rubylibdir'])
    prelude << rubylib << File.join(rubylib, arch)
  end
  # CRuby's order puts sitelibdir/sitearchdir (== sitedir/ver{,/arch})
  # first; make sure the exact CONFIG values are covered too.
  prelude.unshift(RbConfig::CONFIG['sitearchdir']) if RbConfig::CONFIG['sitearchdir']
  prelude.unshift(RbConfig::CONFIG['sitelibdir']) if RbConfig::CONFIG['sitelibdir']
  prelude.uniq!
  # The prelude must form a CONTIGUOUS tail (everything from sitelibdir
  # to the end carries the ivar, nothing before it does — the spec
  # checks both sides), so a prelude dir already present earlier in
  # $LOAD_PATH is MOVED to the tail. That also matches CRuby's layout:
  # gem dirs come before the default directories.
  # Enumerable is not loaded yet at this point in startup; plain loops.
  i = 0
  while i < prelude.size
    dir = prelude[i]
    entry = nil
    j = 0
    while j < $LOAD_PATH.size
      if $LOAD_PATH[j] == dir
        entry = $LOAD_PATH.delete_at(j)
        break
      end
      j += 1
    end
    entry = dir.dup unless entry
    $LOAD_PATH << entry
    entry.instance_variable_set(:@gem_prelude_index, i)
    i += 1
  end
end

# The vendored rbconfig.rb hard-codes `target_os` / `target_cpu` to
# `linux` / `x86_64` (it is a snapshot of an x86_64-linux CRuby build).
# Platform-aware gems — most notably the `ffi` gem's
# `FFI::Platform::OS` / `LIBSUFFIX` / `mac?` and its `map_library_name`
# — read these keys to pick library naming (`libfoo.so` vs
# `libfoo.dylib`) and search paths (`/opt/homebrew/lib` etc.). Without
# this override, `dlopen` on macOS tries `libfoo.so` and fails for
# every shared library. Derive the values from `RUBY_PLATFORM`, which
# `Globals::new` sets to the actually-running host. When a host CRuby
# was present at build time this is its exact RUBY_PLATFORM, so on
# macOS it carries the Darwin major version (e.g. `arm64-darwin23`);
# otherwise it is a cfg-derived default (`arm64-darwin`, `x86_64-darwin`,
# `aarch64-linux`, or `x86_64-linux`).
#
# `arch` / `sitearch` must reproduce the host's value verbatim
# (including the Darwin version): rubygems keys each gem's built
# C-extension directory on `Gem::Platform.local` — which parses this
# `arch` — so a stripped `arm64-darwin` would look in a nonexistent
# `extensions/arm64-darwin/...` and warn that every C-extension gem is
# unbuilt. host_os/target_os stay version-stripped (`darwin` / `linux`)
# for ffi's substring matching. Avoid regex here — a `$~` write at this
# top-level scope would leak `defined?($&)` truthiness into user code.
__host_cpu, __host_os_full = RUBY_PLATFORM.split('-', 2)
__host_cpu = 'x86_64' if __host_cpu.nil? || __host_cpu.empty?
__host_os_full = 'linux' if __host_os_full.nil? || __host_os_full.empty?
__host_os =
  if __host_os_full.start_with?('darwin') then 'darwin'
  elsif __host_os_full.start_with?('linux') then 'linux'
  else __host_os_full
  end
RbConfig::CONFIG['host_os']    = __host_os
RbConfig::CONFIG['host_cpu']   = __host_cpu
RbConfig::CONFIG['target_os']  = __host_os
RbConfig::CONFIG['target_cpu'] = __host_cpu
RbConfig::CONFIG['arch']       = RUBY_PLATFORM
RbConfig::CONFIG['sitearch']   = RUBY_PLATFORM
RbConfig::CONFIG['target']     = "#{__host_cpu}-pc-#{__host_os}"
RbConfig::CONFIG['host']       = "#{__host_cpu}-pc-#{__host_os}"

# ---------------------------------------------------------------------------
# Pure-Ruby builtins, one file per class/module. Load order is significant:
#  * everything up to `comparable` must not reference Comparable/Enumerable
#    at load time (e.g. ObjectSpace::WeakMap deliberately skips its
#    `include ::Enumerable` because the module is not defined yet);
#  * `io_buffer` (IO::Buffer includes Comparable) loads after `comparable`;
#  * the class files after `enumerable` may `include Enumerable`;
#  * the spec-compat alias fixups inside each class file run when that file
#    loads, after the natives and Ruby methods they re-point.
# ---------------------------------------------------------------------------

require_relative 'basic_object'
require_relative 'kernel'
require_relative 'object'
require_relative 'class'
require_relative 'regexp'
require_relative 'exception'
require_relative 'module'
require_relative 'warning'
require_relative 'process'
require_relative 'file'
require_relative 'thread'
require_relative 'boolean'
require_relative 'marshal'
require_relative 'gc'
require_relative 'object_space'
require_relative 'io'
require_relative 'encoding'

# TOPLEVEL_BINDING is defined by the runtime (Executor::init): a Binding
# over an empty, outer-less frame that the *main script* is then executed
# in (Executor::exec_main_script), so it exposes exactly the main script's
# locals — not this file's.

require_relative 'comparable'
require_relative 'io_buffer'
require_relative 'data'

require_relative 'enumerable'
IO.include(Enumerable)
require_relative 'arithmetic_sequence'
require_relative 'numeric'
require_relative 'integer'
require_relative 'range'
require_relative 'array'
require_relative 'rational'
require_relative 'complex'
require_relative 'float'
require_relative 'string'
require_relative 'symbol'
require_relative 'error'
require_relative 'set'
require_relative 'struct'
require_relative 'monitor'
require_relative 'enumerator'
require_relative 'hash'
require_relative 'file_stat'
require_relative 'proc'
require_relative 'method'
require_relative 'dir'
require_relative 'time'
require_relative 'match_data'
require_relative 'filetest'
require_relative 'env'
require_relative 'pathname_builtins'

require_relative 'argf'
