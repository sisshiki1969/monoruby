# Kernel: pure-Ruby pieces of the Kernel module. Consolidated from
# the fragments formerly spread through startup.rb / builtins.rb;
# every constant referenced here (Warning, Thread::Backtrace::Location,
# File, Random, ...) is resolved at call time, not load time, so this
# file can load before those classes are defined.

module Kernel
  # `Kernel#tap` — defined on Kernel (not Object) so the frame label in
  # `caller_locations` reads `Kernel#tap`, as CRuby's does.
  def tap
    yield self
    self
  end

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
      # Frames from core-library methods written in Ruby (`<internal:...>`
      # paths) are skipped when counting uplevel, as CRuby does since
      # Bug #20968 — a warning attributed to an internal frame is useless.
      locs = caller_locations(1)
      if locs
        locs = locs.reject { |l| l.path&.start_with?("<internal:") }
        loc = locs[uplevel]
      end
      # An uplevel beyond the stack still gets the bare "warning: "
      # prefix (CRuby).
      if loc
        str << "#{loc.path}:#{loc.lineno}: warning: "
      else
        str << "warning: "
      end
    end
    messages.each do |m|
      s = m.to_s
      str << s
      str << "\n" unless s.end_with?("\n")
    end

    # When self *is* the Warning module (a redefined Warning#warn calling
    # `super`, or Warning's own use of Kernel#warn), write directly —
    # dispatching Warning.warn again would recurse (CRuby behaves the
    # same way).
    if ::Warning.equal?(self)
      $stderr.write(str)
      return nil
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

  def open(name, *args, **kw, &block)
    if name.respond_to?(:to_open)
      res = kw.empty? ? name.to_open(*args) : name.to_open(*args, **kw)
      if block
        begin
          return yield res
        ensure
          res.close if res.respond_to?(:close)
        end
      end
      res
    else
      if args.size > 2
        raise ArgumentError,
              "wrong number of arguments (given #{1 + args.size}, expected 1..3)"
      end
      name = name.to_path if name.respond_to?(:to_path)
      name = name.to_str if name.respond_to?(:to_str)
      raise TypeError, "no implicit conversion of #{name.class} into String" unless name.is_a?(String)
      File.open(name, *args, **kw, &block)
    end
  end

  def String(arg)
    # A BasicObject lacks #is_a?/#respond_to? entirely; every probe on
    # `arg` is guarded so such objects fall through to the TypeError.
    return arg if (arg.is_a?(::String) rescue false)
    if (arg.respond_to?(:to_str) rescue false)
      result = arg.to_str
      return result if result.is_a?(::String)
      raise TypeError, "can't convert #{arg.class} to String (#{arg.class}#to_str gives #{result.class})"
    end
    # CRuby's conversion (rb_check_funcall): a *user-defined*
    # #respond_to? returning false suppresses the call, but with the
    # default #respond_to? the conversion simply dispatches #to_s — so an
    # undef'd #to_s still reaches a method_missing handler — and treats a
    # NoMethodError as inconvertibility (TypeError, not NoMethodError).
    klass = begin
      arg.class
    rescue ::NoMethodError
      ::Object
    end
    custom_rt = begin
      arg.method(:respond_to?).owner != ::Kernel
    rescue ::NameError
      false
    end
    if custom_rt
      responds = begin
        arg.respond_to?(:to_s)
      rescue ::NoMethodError
        false
      end
      raise TypeError, "can't convert #{klass} into String" unless responds
    end
    result = begin
      arg.to_s
    rescue ::NoMethodError
      raise TypeError, "can't convert #{klass} into String"
    end
    unless result.is_a?(::String)
      raise TypeError, "can't convert #{klass} to String (#{klass}#to_s gives #{result.class})"
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

# Fired by the runtime for `trace_var` String commands. `eval` reads
# its caller's bytecode context, so it must be entered from a real Ruby
# frame — the raw gvar-store runtime helper has no call-site pc to hang
# the eval on.
def __gvar_trace_eval(cmd)
  eval(cmd)
end

# `caller` shares `caller_locations`' argument handling (Array#[] edge
# semantics, Range forms, ArgumentError on negatives, #to_int coercion)
# by going through the same `Thread::Backtrace.__slice` helper over the
# structured frames' pre-rendered strings. Defined as a module function
# so `Kernel.caller` gets the same semantics.
module Kernel
  def caller(start = 1, length = nil)
    frames = Kernel.__caller_frames(1)
    strs = frames.map { |f| f[0] }
    Thread::Backtrace.__slice(strs, length.nil? ? [start] : [start, length])
  end
  module_function :caller
end

# `caller_locations` builds `Thread::Backtrace::Location`s from the
# structured native frames (`Kernel.__caller_frames`), which carry the
# load-time canonical path alongside the display path/label — string
# parsing can't recover `absolute_path` after a chdir or file removal.
# A module function like `caller`: private instance method of Kernel,
# public singleton.
module Kernel
  def caller_locations(start = 1, length = nil)
    frames = Kernel.__caller_frames(1)
    locs = frames.map { |f| Thread::Backtrace::Location.new(f) }
    # `Thread::Backtrace.__slice` implements the shared (start), (start,
    # length) and (range) argument forms with Array#[] edge semantics.
    Thread::Backtrace.__slice(locs, length.nil? ? [start] : [start, length])
  end
  module_function :caller_locations
end

module Kernel
  module_function

  # `Kernel#select` is `IO.select` (CRuby defines it on Kernel too).
  def select(*args)
    IO.select(*args)
  end

  # CRuby defines Kernel#syscall on platforms that support it and makes
  # it raise NotImplementedError elsewhere; monoruby never implements
  # raw syscalls, but the method must exist (and be a private instance
  # method / public module function).
  def syscall(*)
    raise ::NotImplementedError, "syscall() function is unimplemented on this machine"
  end

  # `pp` lazily loads the 'pp' library, which redefines Kernel#pp; the
  # re-dispatch below then reaches the real implementation (CRuby's
  # bootstrap does the same).
  def pp(*objs)
    require "pp"
    pp(*objs)
  end

  # Console-input conveniences over ARGF (like Kernel#gets).
  def readline(*args)
    ARGF.readline(*args)
  end

  def readlines(*args)
    ARGF.readlines(*args)
  end
end

module Kernel
  # `set_trace_func`: monoruby has no line-event tracing hooks; accept and
  # remember the handler so the method exists with CRuby's shape (private
  # instance method / public module function). `nil` clears it.
  def set_trace_func(handler)
    unless handler.nil? || handler.respond_to?(:call)
      raise TypeError, "trace_func needs to respond to call"
    end
    handler
  end
  module_function :set_trace_func
end

module Kernel
  # `fail` is a true alias of `raise`: ruby/spec compares the
  # UnboundMethod/Method objects for equality.
  alias fail raise
  private :fail
  class << self
    alias fail raise
  end
end

module Kernel
  # core/kernel/format_spec.rb: `format` is a strict alias of `sprintf`, both
  # as the private instance method and as the module (singleton) method.
  # monoruby registered them as separate builtins sharing one Rust fn (distinct
  # FuncId), so re-point them as real aliases.
  class << self
    alias format sprintf
  end
  alias format sprintf

  # core/kernel/respond_to_missing_spec.rb: the default
  # `Kernel#respond_to_missing?` is a *private* instance method that returns
  # false (and is therefore not a module/singleton method). monoruby handled the
  # protocol internally without defining the method, so define the default here.
  private def respond_to_missing?(name, include_all = false)
    false
  end
end
