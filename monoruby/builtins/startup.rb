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

class BasicObject
  private
  def singleton_method_added(name)
  end

  def singleton_method_removed(name)
  end

  def singleton_method_undefined(name)
  end
end

module Kernel
  private

  # Internal helper: coerce value to Integer via to_int.
  # Raises TypeError with CRuby-compatible message if conversion fails.
  def __to_int(val)
    return val if val.is_a?(Integer)
    if val.respond_to?(:to_int)
      result = val.to_int
      unless result.is_a?(Integer)
        raise TypeError, "can't convert #{val.class} into Integer (#{val.class}#to_int gives #{result.class})"
      end
      result
    else
      raise TypeError, "no implicit conversion of #{val.class} into Integer"
    end
  end

  # Internal helper: coerce value to String via to_str.
  # Raises TypeError with CRuby-compatible message if conversion fails.
  def __to_str(val)
    return val if val.is_a?(String)
    if val.respond_to?(:to_str)
      result = val.to_str
      unless result.is_a?(String)
        raise TypeError, "can't convert #{val.class} into String (#{val.class}#to_str gives #{result.class})"
      end
      result
    else
      raise TypeError, "no implicit conversion of #{val.class} into String"
    end
  end
end

class Object
  def initialize(...)
  end

  def tap
    yield self
    self
  end

  def itself
    self
  end

  def then
    return to_enum(:then) { 1 } unless block_given?
    yield self
  end
  alias yield_self then

  def respond_to_missing?(method_name, include_private = false)
    false
  end

  def <=>(other)
    return 0 if equal?(other)
    # The `self == other` fallback would recurse infinitely when
    # `==` is `Comparable#==` (a Comparable class that did not
    # override `==`): that `==` calls `<=>`, which reaches here
    # again. Skip the fallback in that case (covers both a missing
    # `<=>` and a user `<=>` that calls `super`). Net behaviour
    # matches CRuby, which uses an equivalent recursion guard.
    return nil if self.class.instance_method(:==).owner == Comparable
    0 if (self == other)
  end
end

class Class
  #def new(...)
  #  o = __builtin_allocate__
  #  o.__builtin_initialize__(...)
  #  o
  #end

  private
  def inherited(subclass)
  end
end

class Regexp
  IGNORECASE = 1
  EXTENDED = 2
  MULTILINE = 4
  FIXEDENCODING = 16
  NOENCODING = 32
end

# Raised when a `case/in` exhausts its branches or an `expr => pattern`
# match fails (the pattern-matching desugar references these by name).
class NoMatchingPatternError < StandardError
end

class NoMatchingPatternKeyError < NoMatchingPatternError
  def initialize(message = nil, matchee: nil, key: nil)
    @matchee = matchee
    @key = key
    super(message)
  end

  attr_reader :matchee, :key
end

class Module
  # Partial ordering on the include / inherit graph, matching CRuby's
  # `rb_mod_cmp`:
  #
  # | relationship                                    | result |
  # | ----------------------------------------------- | ------ |
  # | `self` is `other`                               | `0`    |
  # | `self` is a subclass of, or includes, `other`   | `-1`   |
  # | `other` is a subclass of, or includes, `self`   | `+1`   |
  # | unrelated                                       | `nil`  |
  # | `other` is not a Module / Class                 | `nil`  |
  #
  # Defined on Module so `<` / `>` / `<=` / `>=` (provided by
  # `Comparable` in CRuby; we synthesise them) all reduce to this.
  def <=>(other)
    return nil unless other.is_a?(Module)
    return 0 if equal?(other)
    if ancestors.include?(other)
      -1
    elsif other.ancestors.include?(self)
      +1
    else
      nil
    end
  end

  private
  def extended(mod)
  end

  def prepended(mod)
  end

  def included(mod)
  end

  # CRuby keeps these as private instance methods of Module so subclasses
  # override them with `def method_added(name); …; end` (i.e. via the
  # default-private visibility inside a `class M`). `public` here would
  # surface them as `Module.public_instance_methods` entries, which the
  # spec explicitly disallows ("is a private instance method").
  def method_added(name)
  end

  def method_removed(name)
  end

  def method_undefined(name)
  end

  def const_added(name)
  end

  public
  def const_missing(name)
    # Drop the implicit `Object::` prefix so a top-level miss reads
    # `uninitialized constant Foo` (not `Object::Foo`), matching CRuby.
    # For a qualified miss, CRuby crafts the prefix from the module's
    # `#name`, falling back to `#inspect` when the module is anonymous.
    if self.equal?(Object)
      qual = name.to_s
    else
      prefix = self.name || self.inspect
      qual = "#{prefix}::#{name}"
    end
    raise NameError.new("uninitialized constant #{qual}", name)
  end

  def include?(mod)
    # CRuby `Module#include?` accepts only true Modules, not Classes (even
    # though `Class < Module`). And the receiver is *not* counted as one of
    # its own included modules — `M.include?(M)` is `false`.
    if !mod.is_a?(Module) || mod.is_a?(Class)
      raise TypeError, "wrong argument type #{mod.class} (expected Module)"
    end
    return false if equal?(mod)
    ancestors.include?(mod)
  end

  # `Module.used_refinements` returns the refinements active in the
  # current scope. monoruby has no refinement support, so this returns
  # an empty Array as a permissive mock — gems and code that
  # defensively read this list (RSpec, Sorbet) won't crash. Defined in
  # Ruby (not Rust) so the user can override it in specs that actually
  # exercise refinements. CRuby only exposes the class form (no
  # `Module#used_refinements` instance method), so we follow suit.
  def self.used_refinements
    []
  end
end

module Warning
  @categories = {
    deprecated: false, experimental: true, performance: false,
    strict_unused_block: false,
  }

  def self.categories
    @categories.keys
  end

  def self.[](category)
    unless category.is_a?(Symbol)
      raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
    end
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category]
  end

  def self.[]=(category, value)
    unless category.is_a?(Symbol)
      raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
    end
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category] = value ? true : false
  end

  def warn(msg, category: nil)
    $stderr.write(msg)
    nil
  end
  extend self
end

module Kernel
  # Faithful Kernel#warn: combine the (flattened) messages, gate by
  # $VERBOSE / Warning[category], then delegate to Warning.warn so a
  # user-overridden Warning.warn takes effect (matches CRuby).
  def warn(*messages, uplevel: nil, category: nil)
    messages = messages.flatten
    return nil if messages.empty?

    unless category.nil?
      if category.is_a?(Symbol)
        # already a Symbol
      elsif category.respond_to?(:to_sym)
        category = category.to_sym
        unless category.is_a?(Symbol)
          raise TypeError, "can't convert to Symbol"
        end
      else
        raise TypeError,
              "no implicit conversion of #{category.class} into Symbol"
      end
    end

    # $VERBOSE == nil silences all warnings.
    return nil if $VERBOSE.nil?

    # Category gating: outside verbose mode a known but disabled
    # category suppresses the message; an unknown category is passed
    # through.
    if category && $VERBOSE != true
      enabled =
        begin
          Warning[category]
        rescue ArgumentError
          true
        end
      return nil unless enabled
    end

    str = +""
    unless uplevel.nil?
      uplevel = __to_int(uplevel)
      raise ArgumentError, "negative level (#{uplevel})" if uplevel < 0
      loc = caller_locations(uplevel + 1, 1)
      if loc && (loc = loc.first)
        str << "#{loc.path}:#{loc.lineno}: warning: "
      end
    end
    messages.each do |m|
      s = m.to_s
      str << s
      str << "\n" unless s.end_with?("\n")
    end

    # Pass the category keyword, but fall back to a positional-only
    # call if Warning.warn was redefined without keyword support
    # (matches CRuby, and stays robust when Warning.warn is a mock).
    begin
      Warning.warn(str, category: category)
    rescue ArgumentError
      Warning.warn(str)
    end
    nil
  end
  module_function :warn

  # Internal: deprecation warnings raised from the Rust runtime (e.g.
  # assigning non-nil to $/). Routed through Kernel#warn so the
  # :deprecated category gating and Warning.warn overrides apply.
  def __warn_deprecated(msg)
    warn(msg, category: :deprecated)
  end
  module_function :__warn_deprecated

  # Internal: the prism lowerer desugars a top-level `return <arg>`
  # into a call to this helper followed by the return, so the warning
  # fires only when the return actually executes (CRuby behavior).
  # rb_warn-level: silenced by -W0 ($VERBOSE == nil), not by default.
  def __warn_toplevel_return(path)
    warn("#{path}: warning: argument of top-level return is ignored") unless $VERBOSE.nil?
  end
  module_function :__warn_toplevel_return
end

module Process
  CLOCK_REALTIME = 0
  CLOCK_MONOTONIC = 1
  CLOCK_PROCESS_CPUTIME_ID = 2
  CLOCK_THREAD_CPUTIME_ID	= 3
  CLOCK_MONOTONIC_RAW	= 4
  CLOCK_REALTIME_COARSE	= 5
  CLOCK_MONOTONIC_COARSE = 6
  CLOCK_BOOTTIME = 7
  CLOCK_REALTIME_ALARM = 8
  CLOCK_BOOTTIME_ALARM = 9

  # Reap child `pid` and return a thread whose #value waits for it and yields
  # the resulting Process::Status. monoruby's Thread is cooperative (the block
  # runs at #value/#join time), which matches Open3's "drain the pipes first,
  # then read the exit status" ordering.
  def self.detach(pid)
    Thread::Waiter.new(pid)
  end

  class Tms
    attr_accessor :utime, :stime, :cutime, :cstime
  end

  # Wraps the raw POSIX wait(2) status word (as returned by `waitpid`).
  # Callers pass the raw int and pid; all predicates decode the bit layout:
  #   bits 0..6  = termination signal (0 = normal exit)
  #   bit  7     = core dumped flag
  #   bits 8..15 = exit code (for normal exit)
  #   low byte 0x7F = stopped (SIGSTOP etc.)
  class Status
    attr_reader :pid

    def initialize(raw_status, pid)
      @status = raw_status
      @pid = pid
    end

    def exited?
      (@status & 0x7F) == 0
    end

    def exitstatus
      exited? ? (@status >> 8) & 0xFF : nil
    end

    def signaled?
      low = @status & 0x7F
      low != 0 && low != 0x7F
    end

    def termsig
      signaled? ? @status & 0x7F : nil
    end

    def stopped?
      (@status & 0xFF) == 0x7F
    end

    def stopsig
      stopped? ? (@status >> 8) & 0xFF : nil
    end

    def coredump?
      (@status & 0x80) != 0
    end

    def success?
      exited? ? exitstatus == 0 : nil
    end

    def to_i
      @status
    end

    def to_s
      if signaled?
        "pid #{@pid} SIG#{termsig}#{coredump? ? ' (core dumped)' : ''}"
      elsif stopped?
        "pid #{@pid} stopped SIG#{stopsig}"
      else
        "pid #{@pid} exit #{exitstatus}"
      end
    end

    def inspect
      "#<Process::Status: #{to_s}>"
    end

    def ==(other)
      if other.is_a?(Integer)
        to_i == other
      else
        super
      end
    end
  end
end

module File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
  # Open(2) flags and flock(2) operations (Linux values), mirrored on
  # File itself below via `include File::Constants` in CRuby; monoruby
  # historically defined them directly on File, so keep both in sync.
  RDONLY   = 0
  WRONLY   = 1
  RDWR     = 2
  APPEND   = 1024
  CREAT    = 64
  EXCL     = 128
  TRUNC    = 512
  NONBLOCK = 2048
  BINARY   = 0
  LOCK_SH  = 1
  LOCK_EX  = 2
  LOCK_UN  = 8
  LOCK_NB  = 4
  NULL = "/dev/null"
end

class File
  include File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_DOTMATCH = 4
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
  NULL = "/dev/null"
  BINARY = 0

  RDONLY   = 0
  WRONLY   = 1
  RDWR     = 2
  APPEND   = 1024
  CREAT    = 64
  EXCL     = 128
  TRUNC    = 512
  NONBLOCK = 2048
  LOCK_SH  = 1
  LOCK_EX  = 2
  LOCK_UN  = 8
  LOCK_NB  = 4

  Separator = "/"
  SEPARATOR = "/"
  ALT_SEPARATOR = nil
  PATH_SEPARATOR = ":"
end

class Fiber
  @@main_fiber = nil
  def self.current
    # Return the main fiber (monoruby is single-threaded and doesn't track
    # the current fiber across resume/yield).
    @@main_fiber ||= Fiber.new {}
  end

  # --- Fiber-local storage (CRuby 3.2+) --------------------------------
  #
  # `Fiber.current.storage` is a Hash whose keys must be Symbols. The
  # `Fiber.[key]` / `Fiber.[]=key, val` class-method sugar reads/writes
  # the *current* fiber's storage. Pure-Ruby implementation here —
  # monoruby tracks an `@storage` ivar per Fiber, lazily materialised.

  # Returns the storage Hash. CRuby returns `nil` until `[]=` /
  # `storage=` materialises one (no lazy init on bare read), so do
  # not auto-create here.
  def storage
    @storage
  end

  # Assigns the storage Hash. `nil` clears it. Non-Hash raises
  # TypeError; non-Symbol keys raise TypeError.
  def storage=(hash)
    if hash.nil?
      @storage = nil
      return nil
    end
    raise TypeError, "no implicit conversion of #{hash.class} into Hash" \
      unless hash.is_a?(Hash)
    hash.each_key do |k|
      raise TypeError, "wrong argument type #{k.class} (expected Symbol)" \
        unless k.is_a?(Symbol)
    end
    @storage = hash
  end

  # Class-method sugar over `Fiber.current` storage. Symbol-keyed.
  # `Fiber[:k]` on a fiber with no storage returns nil (CRuby).
  def self.[](key)
    raise TypeError, "wrong argument type #{key.class} (expected Symbol)" \
      unless key.is_a?(Symbol)
    s = current.storage
    s ? s[key] : nil
  end

  # `Fiber[:k] = v` materialises an empty storage on first write.
  def self.[]=(key, value)
    raise TypeError, "wrong argument type #{key.class} (expected Symbol)" \
      unless key.is_a?(Symbol)
    cur = current
    cur.storage = {} if cur.storage.nil?
    cur.storage[key] = value
  end

  # --- Scheduler -------------------------------------------------------
  #
  # monoruby has no fiber scheduler, so `set_scheduler` is effectively
  # a no-op store. CRuby allows `nil` to clear the scheduler, so accept
  # that too. `scheduler` returns whatever was set (default nil).

  @@scheduler = nil

  def self.scheduler
    @@scheduler
  end

  def self.set_scheduler(scheduler)
    @@scheduler = scheduler
  end

  def self.current_scheduler
    # CRuby: nil when the current fiber is blocking. monoruby fibers
    # are always blocking, so this is always nil.
    nil
  end

  # --- Blocking / non-blocking ----------------------------------------
  #
  # CRuby contracts (3.2+):
  #   `Fiber.blocking?`         returns `1` when the *current* fiber
  #                             is blocking, `false` otherwise.
  #   `Fiber#blocking?`         instance form: `true` / `false`.
  # monoruby has no non-blocking fibers, so the class form is `1`
  # and the instance form is `true`.

  def self.blocking?
    1
  end

  def blocking?
    true
  end
end

class Thread
  # monoruby threads are cooperative green threads multiplexed on the one
  # OS thread by the native scheduler (src/scheduler.rs). The native side
  # provides: Thread.new / .start / .fork (queue a body), .current /
  # .main / .list / .pass / .stop, and #join / #value / #status /
  # #alive? / #stop? / #wakeup / #run. Blocking APIs park the calling
  # thread on the scheduler; a body starts running the first time any
  # thread reaches a blocking point.
  #
  # This Ruby side keeps only the pure-bookkeeping surface: a name,
  # thread-/fiber-local storage, and an interrupt-mask no-op (real
  # asynchronous interruption — #raise / #kill — is not implemented yet,
  # so there is nothing to mask).

  # Interrupt masking: monoruby delivers no asynchronous interrupts yet,
  # so just run the block (used by Bundler's connection_pool and by
  # timeout).
  def self.handle_interrupt(mask)
    yield
  end

  def self.each_caller_location
    caller_locations(1).each do |loc|
      yield loc
    end
  end

  attr_accessor :name

  # Thread objects are native (ObjTy::THREAD) and no longer run a Ruby
  # initialize, so the local-storage tables are created lazily.
  def [](key)
    @fiber_locals && @fiber_locals[key]
  end

  def []=(key, value)
    (@fiber_locals ||= {})[key] = value
  end

  def thread_variable_get(key)
    @thread_variables && @thread_variables[key]
  end

  def thread_variable_set(key, value)
    (@thread_variables ||= {})[key] = value
  end

  # The object returned by `Process.detach`. Reaping one specific child via
  # `Process.wait2` is a *terminating* operation (unlike an arbitrary thread
  # body), so — unlike a general Thread, which defines no #join / #value —
  # the waiter safely runs the reaper on #join / #value. A missing child
  # (`Errno::ECHILD`) yields nil, matching CRuby. This is what keeps Open3's
  # `wait_thr.value` / `wait_thr.join` working.
  class Waiter < Thread
    # The native Thread.new requires a block (it queues a green thread);
    # a Waiter is an inert shell around one specific child pid, so build
    # it via allocate.
    def self.new(pid)
      w = allocate
      w.__send__(:__init_waiter, pid)
      w
    end

    def __init_waiter(pid)
      @pid = pid
      self[:pid] = pid
    end
    private :__init_waiter

    attr_reader :pid

    def value
      __reap
    end

    def join(limit = nil)
      __reap
      self
    end

    private

    def __reap
      return @status if @reaped
      @reaped = true
      @status = begin
        Process.wait2(@pid)[1]
      rescue SystemCallError
        nil
      end
    end
  end

  # Green threads switch only at blocking points (sleep / Thread.stop /
  # join / pass), so plain Ruby code is atomic with respect to the
  # scheduler — no preemption can occur between two non-blocking
  # statements. That makes these synchronization primitives correct as
  # pure Ruby built on Thread.stop / Kernel#sleep (park) and
  # Thread#wakeup (unpark): every "test then enqueue then park" sequence
  # below is uninterruptible up to the park itself.

  class Mutex
    def locked?
      !!@owner
    end

    def owned?
      @owner == Thread.current
    end

    def try_lock
      if @owner
        false
      else
        @owner = Thread.current
        true
      end
    end

    def lock
      raise ThreadError, "deadlock; recursive locking" if owned?
      until try_lock
        (@waiters ||= []) << Thread.current
        begin
          Thread.stop
        ensure
          @waiters.delete(Thread.current)
        end
      end
      self
    end

    def unlock
      unless @owner
        raise ThreadError, "Attempt to unlock a mutex which is not locked"
      end
      unless owned?
        raise ThreadError, "Attempt to unlock a mutex which is locked by another thread"
      end
      @owner = nil
      # Hand the next waiter a chance; it re-contends in its lock loop.
      if @waiters
        while (w = @waiters.shift)
          if w.alive?
            w.wakeup
            break
          end
        end
      end
      self
    end

    def synchronize
      raise ThreadError, "must be called with a block" unless block_given?
      lock
      begin
        yield
      ensure
        unlock
      end
    end

    # Atomically release the mutex, sleep, and re-acquire it on wake
    # (including on an exception raised into the sleeper).
    def sleep(timeout = nil)
      raise ThreadError, "Attempt to unlock a mutex which is not locked" unless owned?
      unlock
      begin
        timeout ? Kernel.sleep(timeout) : Thread.stop
      ensure
        lock
      end
    end
  end

  class Queue
    def initialize
      @items = []
      @waiters = []
      @closed = false
    end

    def push(item)
      raise ClosedQueueError, "queue closed" if @closed
      @items.push(item)
      __wake_one(@waiters)
      self
    end
    alias << push
    alias enq push

    def pop(non_block = false, timeout: nil)
      if non_block
        raise ThreadError, "queue empty" if @items.empty? && !@closed
        return @items.shift
      end
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      loop do
        return @items.shift unless @items.empty?
        return nil if @closed
        if deadline
          remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
          return nil if remaining <= 0
          @waiters << Thread.current
          begin
            Kernel.sleep(remaining)
          ensure
            @waiters.delete(Thread.current)
          end
        else
          @waiters << Thread.current
          begin
            Thread.stop
          ensure
            @waiters.delete(Thread.current)
          end
        end
      end
    end
    alias shift pop
    alias deq pop

    def empty?
      @items.empty?
    end

    def size
      @items.size
    end
    alias length size

    def clear
      @items.clear
      self
    end

    # Closing wakes every parked consumer/producer: consumers drain the
    # remaining items then get nil; producers raise ClosedQueueError.
    def close
      return self if @closed
      @closed = true
      __wake_all(@waiters)
      __wake_all(@push_waiters) if defined?(@push_waiters) && @push_waiters
      self
    end

    def closed?
      @closed
    end

    def num_waiting
      @waiters.size + (defined?(@push_waiters) && @push_waiters ? @push_waiters.size : 0)
    end

    private

    def __wake_one(waiters)
      while (w = waiters.shift)
        if w.alive?
          w.wakeup
          return
        end
      end
    end

    def __wake_all(waiters)
      until waiters.empty?
        __wake_one(waiters)
      end
    end
  end

  class SizedQueue < Queue
    def initialize(max)
      max = max.to_int if !max.is_a?(Integer) && max.respond_to?(:to_int)
      raise ArgumentError, "queue size must be positive" unless max.is_a?(Integer) && max > 0
      super()
      @max = max
      @push_waiters = []
    end

    attr_reader :max

    def max=(new_max)
      raise ArgumentError, "queue size must be positive" unless new_max.is_a?(Integer) && new_max > 0
      grew = new_max > @max
      @max = new_max
      __wake_all(@push_waiters) if grew
      new_max
    end

    def push(item, non_block = false, timeout: nil)
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      loop do
        raise ClosedQueueError, "queue closed" if @closed
        if @items.size < @max
          @items.push(item)
          __wake_one(@waiters)
          return self
        end
        raise ThreadError, "queue full" if non_block
        if deadline
          remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
          return nil if remaining <= 0
          @push_waiters << Thread.current
          begin
            Kernel.sleep(remaining)
          ensure
            @push_waiters.delete(Thread.current)
          end
        else
          @push_waiters << Thread.current
          begin
            Thread.stop
          ensure
            @push_waiters.delete(Thread.current)
          end
        end
      end
    end
    alias << push
    alias enq push

    def pop(non_block = false, timeout: nil)
      v = super
      # Popping frees a slot: let one parked producer through.
      __wake_one(@push_waiters)
      v
    end
    alias shift pop
    alias deq pop
  end

  class ConditionVariable
    def initialize
      @waiters = []
    end

    # Atomically release `mutex` and park until signaled (or the timeout
    # elapses), then re-acquire `mutex` before returning — also on an
    # exception raised into the waiter.
    def wait(mutex, timeout = nil)
      @waiters << Thread.current
      begin
        mutex.unlock
        begin
          timeout ? Kernel.sleep(timeout) : Thread.stop
        ensure
          mutex.lock
        end
      ensure
        @waiters.delete(Thread.current)
      end
      self
    end

    def signal
      while (w = @waiters.shift)
        if w.alive?
          w.wakeup
          break
        end
      end
      self
    end

    def broadcast
      until @waiters.empty?
        signal
      end
      self
    end
  end

  class Backtrace
    class Location
      def initialize(frame)
        @frame = frame.to_s
        if @frame =~ /\A(.+):(\d+):in ['`](.+)'\z/
          @path = $1
          @lineno = $2.to_i
          @label = $3
        else
          @path = @frame
          @lineno = 0
          @label = ""
        end
      end

      attr_reader :path, :lineno, :label

      def base_label
        l = @label
        l = $1 while l =~ /\Ablock (?:\(\d+ levels\) )?in (.+)\z/
        l
      end

      # CRuby returns nil for frames without a real file (eval'd code,
      # internal frames).
      def absolute_path
        return nil if @path.start_with?("(") || @path.start_with?("<")
        File.expand_path(@path)
      end

      def to_s
        @frame
      end
      alias inspect to_s
    end
  end
end

# Top-level aliases (CRuby compatibility)
class ThreadError < StandardError; end unless defined?(::ThreadError)
# Raised by push/pop on a closed queue (CRuby: subclass of StopIteration,
# so `loop { q.pop }` exits cleanly when the queue is closed).
class ClosedQueueError < StopIteration; end unless defined?(::ClosedQueueError)
Queue = Thread::Queue
SizedQueue = Thread::SizedQueue
ConditionVariable = Thread::ConditionVariable

class Exception
  def backtrace_locations
    # Locations come from the raise-time capture, independent of any
    # string backtrace installed via #set_backtrace (CRuby: after
    # `set_backtrace(strings)` on a never-raised exception,
    # #backtrace_locations stays nil). Memoized so repeated calls
    # return the same, mutable Array.
    # A prior #backtrace_locations memo, or locations installed via
    # set_backtrace(locations), win over re-deriving from the capture.
    return @backtrace_locations if defined?(@backtrace_locations) && @backtrace_locations
    frames = __raise_backtrace
    return nil if frames.nil?
    @backtrace_locations = frames.map { |f| Thread::Backtrace::Location.new(f) }
  end

  # CRuby 3.4+: `set_backtrace` (and thus `$@ =`) also accepts an Array
  # of `Thread::Backtrace::Location`, stored as their string forms. Mixed
  # or otherwise invalid arrays fall through to the native checker, which
  # raises the usual TypeError.
  def set_backtrace(bt)
    if bt.is_a?(Array) && !bt.empty? && bt.all? { |e| Thread::Backtrace::Location === e }
      # CRuby 3.4+: an Array of Locations sets both the string backtrace
      # and #backtrace_locations.
      __set_backtrace(bt.map(&:to_s))
      @backtrace_locations = bt
    else
      # Setting a string backtrace leaves #backtrace_locations as it was
      # (the raise-time capture, or nil), so don't touch the memo here.
      __set_backtrace(bt)
    end
  end

  # CRuby's Exception#inspect goes through #to_s (which user classes
  # may override): `#<ClassName: to_s>`, or just the class name when
  # #to_s returns an empty string.
  def inspect
    s = to_s
    if s.nil? || s.empty?
      self.class.name || self.class.to_s
    else
      "#<#{self.class}: #{s}>"
    end
  end

  # Whether the uncaught-exception report would be written to a tty —
  # the default for `full_message`'s :highlight option.
  def self.to_tty?
    $stderr.equal?(STDERR) && STDERR.tty?
  rescue NoMethodError
    false
  end

  def full_message(highlight: nil, order: :top, **opts)
    # CRuby's first line is `bt[0]: <detailed_message>` (i.e.
    # "message (ClassName)"), followed by `\tfrom <frame>` lines.
    # `--backtrace-limit=N` truncates the from-lines to N entries plus
    # a `\t ... K levels...` marker, exactly like the top-level
    # uncaught-error report. All keyword arguments except :order are
    # forwarded to #detailed_message, with :highlight resolved to its
    # default first.
    highlight = Exception.to_tty? if highlight.nil?
    msg = nil
    if respond_to?(:detailed_message)
      dm = detailed_message(highlight: highlight, **opts)
      dm = dm.to_str if !dm.is_a?(String) && dm.respond_to?(:to_str)
      msg = dm if dm.is_a?(String)
    end
    if msg.nil?
      # No usable #detailed_message: fall back to the class name.
      msg = highlight ? "\e[1;4m#{self.class}\e[m" : self.class.to_s
    end
    bt = backtrace
    # An exception that was never raised has no backtrace; CRuby shows
    # the caller of full_message instead.
    bt = caller if bt.nil? || bt.empty?
    if bt && !bt.empty?
      rest = bt[1..]
      limit = Kernel.__backtrace_limit
      trailer = nil
      if limit && rest.size > limit
        trailer = "\t ... #{rest.size - limit} levels...\n"
        rest = rest[0, limit]
      end
      out = if order == :bottom
        header = highlight ? "\e[1mTraceback\e[m (most recent call last):\n"
                           : "Traceback (most recent call last):\n"
        # CRuby numbers the reversed frames by their distance from the
        # error line: `\tN: from <frame>` counting down to 1.
        numbered = []
        i = rest.size
        rest.each do |l|
          numbered << "\t#{i}: from #{l}\n"
          i -= 1
        end
        numbered << trailer if trailer
        header + numbered.reverse.join + "#{bt[0]}: #{msg}\n"
      else
        lines = rest.map { |l| "\tfrom #{l}\n" }
        lines << trailer if trailer
        "#{bt[0]}: #{msg}\n" + lines.join
      end
    else
      out = msg + "\n"
    end
    # Chain the cause's report so every exception in the chain appears.
    c = cause
    out += c.full_message(highlight: highlight, order: :top) if c
    out
  end

  # CRuby `Exception#detailed_message(highlight: false, **)`:
  # decorate the message with the class name; empty-message and
  # anonymous-class cases have special forms.
  def detailed_message(highlight: false, **)
    msg = message.to_s
    cls = self.class
    if msg.empty?
      base = instance_of?(::RuntimeError) ? "unhandled exception" : (cls.name || cls.to_s)
      return highlight ? "\e[1;4m#{base}\e[m" : base
    end
    first, *rest = msg.split("\n", -1)
    # With :highlight every line of a multi-line message is bolded
    # individually (`\e[1m…\e[m` per line).
    if cls.name.nil?
      head = highlight ? "\e[1m#{first}\e[m" : first
    elsif highlight
      head = "\e[1m#{first} (\e[1;4m#{cls}\e[m\e[1m)\e[m"
    else
      head = "#{first} (#{cls})"
    end
    if highlight
      rest = rest.map { |l| l.empty? ? l : "\e[1m#{l}\e[m" }
    end
    ([head] + rest).join("\n")
  end
end

Mutex = Thread::Mutex

class TrueClass
  class << self
    undef_method :new
  end
  TRUE_TO_S = "true".freeze
  def to_s
    TRUE_TO_S
  end
end

class FalseClass
  class << self
    undef_method :new
  end
  FALSE_TO_S = "false".freeze
  def to_s
    FALSE_TO_S
  end
end

class NilClass
  class << self
    undef_method :new
  end
  NIL_TO_S = "".freeze
  def to_s
    NIL_TO_S
  end

  def to_a
    []
  end

  def to_i
    0
  end

  def to_f
    0.0
  end

  def to_h
    {}
  end

  def =~(_other)
    nil
  end

  def to_c
    Complex(0, 0)
  end

  def to_r
    Rational(0)
  end

  def rationalize(*args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 0..1)"
    end
    Rational(0)
  end
end

module Marshal
  MAJOR_VERSION = 4
  MINOR_VERSION = 8
end

module Kernel
  module_function

  def puts(*args)
    $stdout.puts(*args)
    nil
  end

  # `Kernel#print` is a native builtin (see builtins/kernel.rs): it
  # delegates to `$stdout.print` but additionally writes `$_` when
  # called with no arguments, which a pure-Ruby definition can't do
  # because `$_` is frame-local (it would read print's own nil slot).

  def p(*args)
    if args.size == 1
      $stdout.puts(args[0].inspect)
      args[0]
    elsif args.empty?
      nil
    else
      args.each { |a| $stdout.puts(a.inspect) }
      args
    end
  end

  def printf(fmt = nil, *args)
    if fmt.respond_to?(:write)
      fmt.write(format(*args))
    elsif fmt
      $stdout.write(format(fmt, *args))
    end
    nil
  end

  # Prevent CRuby's bundled_gems.rb from patching require with warning
  # logic.  monoruby provides its own implementations of formerly-bundled
  # gems (fiddle, strscan, etc.) so the warnings are not applicable.
  # bundled_gems.rb's replace_require checks for this method and returns
  # early if it already exists.
  alias no_warning_require require
  module_function :no_warning_require
end

module Kernel
  module_function

  def open(name, *args, &block)
    if name.respond_to?(:to_open)
      name.to_open(*args, &block)
    else
      name = name.to_path if name.respond_to?(:to_path)
      name = name.to_str if name.respond_to?(:to_str)
      raise TypeError, "no implicit conversion of #{name.class} into String" unless name.is_a?(String)
      File.open(name, *args, &block)
    end
  end

  def String(arg)
    return arg if arg.is_a?(::String)
    if arg.respond_to?(:to_str)
      result = arg.to_str
      return result if result.is_a?(::String)
      raise TypeError, "can't convert #{arg.class} to String (#{arg.class}#to_str gives #{result.class})"
    end
    result = arg.to_s
    unless result.is_a?(::String)
      raise TypeError, "can't convert #{arg.class} to String (#{arg.class}#to_s gives #{result.class})"
    end
    result
  end

  def Hash(arg)
    return {} if arg.nil?
    return {} if arg.is_a?(::Array) && arg.empty?
    return arg if arg.is_a?(::Hash)
    if arg.respond_to?(:to_hash)
      result = arg.to_hash
      return {} if result.nil?
      return result if result.is_a?(::Hash)
      raise TypeError, "can't convert #{arg.class} to Hash (#{arg.class}#to_hash gives #{result.class})"
    end
    raise TypeError, "can't convert #{arg.class} into Hash"
  end

  def srand(*args)
    Random.srand(*args)
  end

  def putc(ch)
    if ch.is_a?(String)
      s = ch[0]
    else
      i = ch.is_a?(Integer) ? ch : __to_int(ch)
      s = (i & 0xff).chr
    end
    $stdout.write(s)
    ch
  end

  # Kernel#test(cmd, file1[, file2]) — minimal subset of CRuby's file-test
  # operator. `cmd` accepts either a single-character string or its integer
  # code-point.
  def test(cmd, file1, file2 = nil)
    c = cmd.is_a?(Integer) ? cmd : cmd.to_s.ord
    case c
    when ?e.ord then File.exist?(file1)
    when ?f.ord then File.file?(file1)
    when ?d.ord then File.directory?(file1)
    when ?r.ord then File.readable?(file1)
    when ?R.ord then File.readable_real?(file1)
    when ?w.ord then File.writable?(file1)
    when ?W.ord then File.writable_real?(file1)
    when ?x.ord then File.executable?(file1)
    when ?X.ord then File.executable_real?(file1)
    when ?l.ord then File.symlink?(file1)
    when ?p.ord then File.pipe?(file1)
    when ?S.ord then File.socket?(file1)
    when ?b.ord then File.blockdev?(file1)
    when ?c.ord then File.chardev?(file1)
    when ?u.ord then File.setuid?(file1)
    when ?g.ord then File.setgid?(file1)
    when ?k.ord then File.sticky?(file1)
    when ?o.ord then File.owned?(file1)
    when ?G.ord then File.grpowned?(file1)
    when ?s.ord
      sz = File.size?(file1)
      sz && sz > 0 ? sz : nil
    when ?z.ord then File.zero?(file1)
    when ?M.ord then File.mtime(file1)
    when ?A.ord then File.atime(file1)
    when ?C.ord then File.ctime(file1)
    when ?-.ord then File.identical?(file1, file2)
    when ?=.ord then File.mtime(file1) == File.mtime(file2)
    when ?<.ord then File.mtime(file1) < File.mtime(file2)
    when ?>.ord then File.mtime(file1) > File.mtime(file2)
    else
      raise ArgumentError, "unknown command '#{cmd.is_a?(Integer) ? cmd.chr : cmd}'"
    end
  end
end

module GC
  # `GC.count` and `GC.start` are implemented in Rust (they read the
  # allocator's collection counter / force a full collection).

  # --- boolean mode flags -------------------------------------------------
  # monoruby has no stress mode, no auto-compaction and no time
  # measurement, but the accessors round-trip a stored value so callers
  # (and specs) can set and read them back.
  @stress = false
  @measure_total_time = false
  @auto_compact = false

  def self.stress
    @stress
  end

  def self.stress=(flag)
    @stress = flag
  end

  def self.measure_total_time
    @measure_total_time
  end

  def self.measure_total_time=(flag)
    @measure_total_time = flag
  end

  def self.auto_compact
    @auto_compact
  end

  def self.auto_compact=(flag)
    @auto_compact = flag
  end

  # Total time spent in GC, in nanoseconds. monoruby does not measure
  # this, so report 0 (an Integer, never decreasing).
  def self.total_time
    0
  end

  # --- GC.config ----------------------------------------------------------
  def self.config(*args)
    @config ||= { implementation: "default", rgengc_allow_full_mark: true }
    return @config.dup if args.empty?
    arg = args[0]
    return @config.dup if arg.nil?
    unless arg.is_a?(Hash)
      raise ArgumentError, "GC.config requires a Hash argument"
    end
    # `:implementation` is read-only.
    if arg.key?(:implementation)
      raise ArgumentError, 'Attempting to set read-only key "Implementation"'
    end
    # Update known knobs (coercing arbitrary truthy/falsey values to a
    # boolean, as MRI does); unknown keys are ignored.
    arg.each do |k, v|
      next unless @config.key?(k)
      @config[k] = v ? true : false
    end
    @config.dup
  end

  # Instance form (available via `obj.extend(GC)`); always returns nil.
  def garbage_collect(full_mark: true, immediate_mark: true, immediate_sweep: true)
    GC.start
    nil
  end

  module Profiler
    @enabled = false

    def self.enabled?
      @enabled
    end

    def self.enable
      @enabled = true
      nil
    end

    def self.disable
      @enabled = false
      nil
    end

    def self.clear
      nil
    end

    def self.result
      ""
    end

    def self.report(out = $stdout)
      out.print(result)
      nil
    end

    def self.total_time
      0.0
    end
  end
end

# Minimal ObjectSpace stub for monoruby.
#
# monoruby has no support for weak references or general object iteration.
# Provide just enough surface area for libraries that defensively reference
# ObjectSpace constants (ActiveSupport::DescendantsTracker, ConnectionPool,
# weakref) — `WeakMap` is implemented as a strong-referenced hash so keys
# are never GCed while held by the map. That is semantically weaker than
# CRuby but correct enough for class loading and most non-GC-sensitive use.
module ObjectSpace
  class WeakMap
    include ::Enumerable if defined?(::Enumerable)

    def initialize
      @map = {}
    end

    def [](key)
      @map[key.object_id]&.first
    end

    def []=(key, value)
      @map[key.object_id] = [value, key]
      value
    end

    def key?(key)
      @map.key?(key.object_id)
    end
    alias include? key?
    alias member? key?

    def delete(key)
      pair = @map.delete(key.object_id)
      pair ? pair.first : nil
    end

    def keys
      @map.values.map { |pair| pair[1] }
    end

    def values
      @map.values.map { |pair| pair[0] }
    end

    def each
      return to_enum(:each) unless block_given?
      @map.each_value { |pair| yield pair[1], pair[0] }
      self
    end
    alias each_pair each

    def each_key
      return to_enum(:each_key) unless block_given?
      @map.each_value { |pair| yield pair[1] }
      self
    end

    def each_value
      return to_enum(:each_value) unless block_given?
      @map.each_value { |pair| yield pair[0] }
      self
    end

    def size
      @map.size
    end
    alias length size

    def inspect
      "#<ObjectSpace::WeakMap:#{format('0x%016x', object_id << 1)} size=#{size}>"
    end
  end

  def self.each_object(klass = nil)
    return to_enum(:each_object, klass) unless block_given?
    0
  end

  # Register a finalizer for +obj+. The finalizer (a callable or block,
  # invoked with the object's id) is run at program termination. monoruby
  # never runs finalizers asynchronously at GC time, which the spec
  # explicitly permits. The actual registry lives in the runtime; the
  # private +__register_finalizer+ primitive records the pair.
  def self.define_finalizer(obj, *args, &block)
    callable = block || args[0]
    if callable.nil?
      raise ArgumentError, "wrong number of arguments (given 1, expected 2)"
    end
    unless callable.respond_to?(:call)
      raise ArgumentError, "no _id2ref or finalizer is given; must respond to #call"
    end
    # The primitive returns the effective callable: the one already
    # registered when an equal finalizer was given before, else +callable+.
    [0, __register_finalizer(obj, callable)]
  end

  def self.undefine_finalizer(obj)
    __unregister_finalizer(obj)
  end

  def self.garbage_collect(**opts)
    GC.start(**opts)
  end

  def self._id2ref(id)
    raise RangeError, "0x#{id.to_s(16)} is not id value"
  end

  def self.count_objects(result_hash = {})
    result_hash[:TOTAL] = 0
    result_hash[:FREE] = 0
    result_hash
  end
end

class IO
  SEEK_SET = 0
  SEEK_CUR = 1
  SEEK_END = 2

  # Mix-ins tagging the exceptions raised by `*_nonblock` when the
  # operation would block. The concrete classes subclass the matching
  # Errno (on Linux EAGAIN == EWOULDBLOCK) and include these so
  # `rescue IO::WaitReadable` / `rescue IO::WaitWritable` work.
  module WaitReadable; end
  module WaitWritable; end
  class EAGAINWaitReadable < Errno::EAGAIN
    include IO::WaitReadable
  end
  class EAGAINWaitWritable < Errno::EAGAIN
    include IO::WaitWritable
  end
  EWOULDBLOCKWaitReadable = EAGAINWaitReadable
  EWOULDBLOCKWaitWritable = EAGAINWaitWritable

  def sync
    raise IOError, "closed stream" if closed?
    false
  end

  # CRuby raises EOFError (not RuntimeError) at end of stream for the
  # `read*` family; #getbyte/#getc return nil at EOF.
  def readbyte
    raise IOError, "closed stream" if closed?
    b = getbyte
    raise EOFError, "end of file reached" if b.nil?
    b
  end

  def readchar
    raise IOError, "closed stream" if closed?
    c = getc
    raise EOFError, "end of file reached" if c.nil?
    c
  end

  def each_byte
    raise IOError, "closed stream" if closed?
    return to_enum(:each_byte) { nil } unless block_given?
    while (b = getbyte)
      yield b
    end
    self
  end

  def each_char
    raise IOError, "closed stream" if closed?
    return to_enum(:each_char) { nil } unless block_given?
    while (c = getc)
      yield c
    end
    self
  end

  def each_codepoint
    raise IOError, "closed stream" if closed?
    return to_enum(:each_codepoint) { nil } unless block_given?
    each_char { |c| yield c.ord }
    self
  end

  # Enumerator-replay helper: positional-only arguments survive the
  # to_enum round-trip (a `chomp:` keyword would come back as a
  # positional Hash and be misparsed as a separator/limit).
  def __each_line(args, chomp)
    # #gets implements the full (sep, limit, chomp:) semantics natively.
    while (line = gets(*args, chomp: chomp))
      yield line
    end
    self
  end
  private :__each_line

  def each_line(*args, chomp: false, **)
    raise IOError, "closed stream" if closed?
    # CRuby rejects a zero limit up front (a zero-limit #gets would
    # return "" forever). The limit is the second positional argument,
    # or the first when it is not a (nil/String) separator.
    lim = if args.size >= 2
            args[1]
          elsif args.size == 1 && !args[0].nil? && !args[0].is_a?(String)
            args[0]
          end
    if lim.is_a?(Integer) && lim == 0
      raise ArgumentError, "invalid limit: 0 for each_line"
    end
    unless block_given?
      return to_enum(:__each_line, args, chomp) { nil }
    end
    __each_line(args, chomp) { |line| yield line }
  end
  alias each each_line

  def self.for_fd(fd, mode = nil, **opts)
    new(fd, mode, **opts)
  end

  # CRuby's rb_io_s_open: `new(*args)` plus block handling — the IO is
  # closed when the block exits (via #close dispatch, so overrides run);
  # an IOError meaning "already closed" is swallowed, everything else
  # (StandardError or not) propagates.
  def self.open(*args, **opts)
    io = new(*args, **opts)
    return io unless block_given?
    begin
      yield io
    ensure
      begin
        io.close
      rescue IOError => e
        raise unless e.message.include?("closed stream")
      end
    end
  end
end

class FloatDomainError < RangeError; end

class Encoding
  class CompatibilityError < EncodingError; end
  class InvalidByteSequenceError < EncodingError; end
  class UndefinedConversionError < EncodingError; end
  class ConverterNotFoundError < EncodingError; end
  class Converter; end

  def self.default_internal
    $DEFAULT_INTERNAL
  end
end

# TOPLEVEL_BINDING is defined by the runtime (Executor::init): a Binding
# over an empty, outer-less frame that the *main script* is then executed
# in (Executor::exec_main_script), so it exposes exactly the main script's
# locals — not this file's.

require_relative 'comparable'

class IO
  class Buffer
    include Comparable

    # Error hierarchy (CRuby io_buffer.c). The Buffer class itself is
    # native; only the exception constants live here.
    class AllocationError < RuntimeError; end
    class AccessError < RuntimeError; end
    class LockedError < RuntimeError; end
    class InvalidatedError < RuntimeError; end
    class MaskError < ArgumentError; end
  end
end

# Data class (Ruby 3.2+): immutable value objects with a keyword-based
# initializer. Built on top of Struct for storage, attribute readers,
# equality, hashing and inspection; the keyword initializer, argument
# validation, `new`/`[]` positional-to-keyword coercion, frozen instances
# and `with` are layered on here.
class Data
  def self.define(*members, &block)
    members = members.map do |m|
      case m
      when Symbol then m
      when String then m.to_sym
      else raise TypeError, "#{m} is not a symbol"
      end
    end
    klass = ::Struct.new(*members)
    klass.class_eval do
      # CRuby renders `Data` instances as `#<data ...>` rather than
      # `#<struct ...>`. Reuse Struct's (correct) inspect — which already
      # handles qualified/anonymous class names and bypasses an overridden
      # `#name` — and only relabel the prefix.
      def inspect
        ::Struct.instance_method(:inspect).bind(self).call.sub(/\A#<struct/, '#<data')
      end
      alias_method :to_s, :inspect

      # The keyword initializer is layered in a module so a user-defined
      # `initialize` (from the `define` block) can `super` into it.
      include(::Module.new do
        def initialize(*args, **kw)
          ms = self.class.members
          kw = ::Hash[ms.zip(args)].merge(kw) unless args.empty?
          values = ::Data.__data_values(ms, kw)
          super(*values)
          freeze
        end
      end)

      # `new` / `[]` accept positional *or* keyword arguments; positional
      # ones are zipped onto the members, then `initialize` (possibly
      # user-overridden) is dispatched with keywords.
      def self.new(*args, **kw)
        ::Data.__data_alloc_init(self, args, kw)
      end
      class << self
        alias_method :[], :new
      end

      # Returns a frozen copy with the given members replaced. Allocates
      # and initializes directly rather than going through `new`, matching
      # CRuby (a redefined `new` must not affect `with`).
      def with(**kw)
        return self if kw.empty?
        norm = {}
        kw.each { |k, v| norm[k.is_a?(::String) ? k.to_sym : k] = v }
        copy = self.class.allocate
        copy.send(:initialize, **to_h.merge(norm))
        copy
      end

      # `deconstruct_keys(keys)` for pattern matching: `nil` returns all
      # members; otherwise the requested keys (Symbol / String / `#to_str`)
      # are looked up, stopping at the first non-member.
      def deconstruct_keys(keys)
        return to_h if keys.nil?
        unless keys.is_a?(::Array)
          raise TypeError, "wrong argument type #{keys.class} (expected Array or nil)"
        end
        ms = self.class.members
        return {} if keys.size > ms.size
        result = {}
        keys.each do |k|
          sym, rkey =
            case k
            when ::Symbol then [k, k]
            when ::String then [k.to_sym, k]
            else
              if k.respond_to?(:to_str)
                s = k.to_str
                unless s.is_a?(::String)
                  raise TypeError, "can't convert #{k.class} into String"
                end
                [s.to_sym, s]
              else
                raise TypeError, "#{k} is not a symbol nor a string"
              end
            end
          break unless ms.include?(sym)
          result[rkey] = send(sym)
        end
        result
      end
    end
    klass.class_eval(&block) if block
    klass
  end

  # The base initializer, reachable as `Data.instance_method(:initialize)`
  # (used by e.g. marshalling libraries to populate an allocated instance).
  def initialize(**kw)
    ms = self.class.members
    values = ::Data.__data_values(ms, kw)
    ms.each_with_index { |m, i| send("#{m}=", values[i]) }
    freeze
  end

  def self.__data_alloc_init(klass, args, kw)
    ms = klass.members
    unless args.empty?
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)" unless kw.empty?
      unless args.size == ms.size
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)"
      end
      kw = ::Hash[ms.zip(args)]
    end
    obj = klass.allocate
    obj.send(:initialize, **kw)
    obj
  end

  # Validate `kw` against `members` (converting String / `#to_str` keys to
  # Symbols) and return the member values in declaration order.
  def self.__data_values(members, kw)
    norm = {}
    unknown = []
    kw.each do |k, v|
      key, disp = __data_key(k)
      if members.include?(key)
        norm[key] = v
      else
        unknown << disp
      end
    end
    missing = members.reject { |m| norm.key?(m) }
    unless missing.empty?
      s = missing.size == 1 ? "" : "s"
      raise ArgumentError, "missing keyword#{s}: #{missing.map { |m| ":#{m}" }.join(", ")}"
    end
    unless unknown.empty?
      s = unknown.size == 1 ? "" : "s"
      raise ArgumentError, "unknown keyword#{s}: #{unknown.join(", ")}"
    end
    members.map { |m| norm[m] }
  end

  # Normalize a keyword key to `[symbol, display]`, where `display` is how
  # the key appears in an "unknown keyword" message (`:sym` / `"str"`).
  def self.__data_key(k)
    case k
    when Symbol then [k, ":#{k}"]
    when String then [k.to_sym, k.inspect]
    else
      if k.respond_to?(:to_str)
        s = k.to_str
        raise TypeError, "can't convert #{k.class} into String" unless s.is_a?(String)
        [s.to_sym, s.inspect]
      else
        raise TypeError, "#{k} is not a symbol nor a string"
      end
    end
  end
end unless defined?(::Data)

require_relative 'enumerable'
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
require_relative 'builtins'
require_relative 'pathname_builtins'

# Minimal ARGF implementation. Full semantics (transparent line-by-line
# reading across ARGV files with $_/$.) are only partially implemented;
# enough shape is provided for specs to load and for simple cases to
# proceed. Deliberately defined after enumerable is loaded so the
# `include Enumerable` on the class body resolves.
class ARGFClass
  include Enumerable

  def initialize(*argv)
    @argv = argv.empty? ? (defined?(::ARGV) ? ::ARGV : []) : argv
    @current_file = nil
    @current_name = nil
    @lineno = 0
    # Set once every input stream has been consumed. ARGF must NOT fall
    # back to $stdin after named ARGV files are exhausted (CRuby reads
    # stdin only when ARGV was empty to begin with) — doing so made
    # `ARGF.read; ARGF.readlines` block forever on stdin.
    @exhausted = false
  end

  def argv
    @argv
  end

  def filename
    @current_name || (@argv.first || '-')
  end
  alias_method :path, :filename

  def file
    __stream || @current_file || $stdin
  end

  # The current open stream, opening the next ARGV file (or binding
  # $stdin when ARGV was empty from the start) as needed. Returns nil
  # once every input has been consumed — never falling back to $stdin
  # after named files are done.
  def __stream
    return @current_file if @current_file && !@current_file.closed?
    return nil if @exhausted
    if @argv.empty?
      if @current_name
        # Named files were consumed earlier; there is no more input.
        @exhausted = true
        return nil
      end
      @current_name = '-'
      @current_file = $stdin
    else
      @current_name = @argv.shift
      @current_file = @current_name == '-' ? $stdin : File.open(@current_name)
    end
    @current_file
  end
  private :__stream

  # Close out the current stream (it hit EOF) and mark ARGF exhausted
  # when no further ARGV entries remain.
  def __finish_stream
    f = @current_file
    f.close if f && !f.closed? && !f.equal?($stdin)
    @current_file = nil
    @exhausted = true if @argv.empty?
  end
  private :__finish_stream

  def advance
    !!__stream
  end

  def lineno;     @lineno; end
  def lineno=(n); @lineno = n; end
  def pos;        0;       end
  alias_method :tell, :pos
  def pos=(n);    n;       end
  def closed?
    !@current_file || @current_file.closed?
  end
  def close
    @current_file.close if @current_file && !@current_file.closed? && @current_file != $stdin
    self
  end
  def eof?
    @argv.empty? && (@current_file.nil? || @current_file.closed?)
  end
  alias_method :eof, :eof?
  def skip
    if @current_file && !@current_file.closed? && @current_file != $stdin
      @current_file.close
    end
    @current_file = nil
    self
  end
  def rewind
    @current_file.rewind if @current_file && !@current_file.closed?
    @lineno = 0
  end
  def each
    return to_enum(:each) unless block_given?
    while (f = __stream)
      f.each_line do |line|
        @lineno += 1
        yield line
      end
      __finish_stream
    end
    self
  end
  alias_method :each_line, :each
  alias_method :lines, :each
  def readlines(*args)
    result = []
    each { |line| result << line }
    result
  end
  # CRuby-compatible ARGF.read (verified against CRuby 4.0.2):
  # - read           -> the concatenation of every remaining stream; ""
  #                     for empty input; nil once ARGF is exhausted.
  # - read(len)      -> up to len bytes, continuing across file
  #                     boundaries; stops at exactly len (this bound is
  #                     what keeps `ARGF.read(100)` on /dev/zero from
  #                     reading forever); nil at EOF; "" for len == 0.
  # - read(len, buf) -> fills and returns buf (same object).
  def read(length = nil, outbuf = nil)
    if length && length < 0
      raise ArgumentError, "negative length #{length} given"
    end
    buf = String.new
    had_stream = false
    read_any = false
    unless length == 0
      loop do
        break if length && buf.bytesize >= length
        f = __stream
        break unless f
        had_stream = true
        need = length && length - buf.bytesize
        chunk = need ? f.read(need) : f.read
        if chunk && !chunk.empty?
          read_any = true
          buf << chunk
        end
        # A short (or nil) chunk means this stream hit EOF; a full read
        # without length always consumes the stream.
        if need.nil? || chunk.nil? || chunk.bytesize < need
          __finish_stream
        end
      end
    end
    result =
      if length.nil?
        had_stream ? buf : nil
      elsif length == 0
        ""
      else
        read_any ? buf : nil
      end
    if outbuf
      outbuf.replace(result || "")
      result = outbuf if result
    end
    result
  end
  def readline(*args)
    line = nil
    each { |l| line = l; break }
    raise EOFError, "end of file reached" if line.nil?
    line
  end
  def gets(*args)
    line = nil
    each { |l| line = l; break }
    line
  end
  def getc;     nil; end
  def readchar; raise EOFError; end
  def getbyte;  nil; end
  def readbyte; raise EOFError; end
  def inspect;  'ARGF'; end
  def to_s;     'ARGF'; end
  def to_a
    readlines
  end
  def fileno
    file.fileno
  end
  alias_method :to_i, :fileno
  def to_io
    file
  end
  def binmode;  self; end
  def binmode?; false; end
  def external_encoding; Encoding.default_external; end
  def internal_encoding; Encoding.default_internal; end
  def inplace_mode;  nil; end
  def inplace_mode=(v); v; end
  def set_encoding(*); self; end
  def write(*args); args.map(&:to_s).join.bytesize; end
  def print(*args); $stdout.print(*args); end
  def puts(*args);  $stdout.puts(*args); end
end

ARGF = ARGFClass.new
