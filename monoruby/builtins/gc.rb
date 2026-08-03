module GC
  # Most of the module is implemented in Rust (`src/builtins/gc.rs`),
  # reading the allocator's own counters: `count`, `stat`, `total_time`,
  # `measure_total_time`, `stress`, `enable` / `disable`, and the whole
  # of `GC::Profiler`. What is left here needs either Ruby-level control
  # flow (`start`) or is pure bookkeeping (`config`).

  def self.start(full_mark: true, immediate_mark: true, immediate_sweep: true)
    before = count
    __request_gc(full_mark)
    # The collection runs at the next VM safepoint, never inside the
    # builtin above: only at a safepoint are the JIT caller's live
    # registers spilled where the root scan can find them. A loop
    # back-edge *is* such a safepoint, so spin over one until the
    # collection has happened and `GC.start` returns synchronously, the
    # way CRuby's does.
    #
    # The trip count is bounded because the counter never moves while
    # collection is disabled (`GC.disable`), which is also why this is a
    # `while` and not a bare `until`.
    spins = 0
    while count == before && spins < 100
      spins += 1
    end
    nil
  end

  # Instance form (available via `obj.extend(GC)`).
  def garbage_collect(full_mark: true, immediate_mark: true, immediate_sweep: true)
    GC.start(full_mark: full_mark, immediate_mark: immediate_mark,
             immediate_sweep: immediate_sweep)
  end

  # monoruby's collector never moves an object, so compaction is always
  # off. The accessors round-trip a stored value so callers (including
  # yjit-bench's harness-common.rb which calls `GC.auto_compact = false`
  # unconditionally) get the same interface as CRuby without errors.
  @auto_compact = false

  def self.auto_compact
    @auto_compact
  end

  def self.auto_compact=(flag)
    @auto_compact = flag ? true : false
  end

  def self.compact
    raise NotImplementedError, "GC.compact is not supported on this platform"
  end

  # --- GC.config ----------------------------------------------------------
  # `:rgengc_allow_full_mark` is a real knob: with it off, the collector
  # only ever chooses a minor collection on its own (an explicit
  # `GC.start` still forces a full one).
  def self.config(*args)
    return __config if args.empty?
    arg = args[0]
    return __config if arg.nil?
    unless arg.is_a?(Hash)
      raise ArgumentError, "GC.config requires a Hash argument"
    end
    # `:implementation` is read-only.
    if arg.key?(:implementation)
      raise ArgumentError, 'Attempting to set read-only key "Implementation"'
    end
    # Known knobs take any truthy/falsey value (as MRI does); unknown
    # keys are ignored.
    if arg.key?(:rgengc_allow_full_mark)
      self.__allow_full_mark = arg[:rgengc_allow_full_mark] ? true : false
    end
    __config
  end

  # Key order matches CRuby's, so `p GC.config` reads the same.
  def self.__config
    { rgengc_allow_full_mark: __allow_full_mark, implementation: "default" }
  end
  private_class_method :__config

  module Profiler
    # `enabled?` / `enable` / `disable` / `clear` / `result` /
    # `raw_data` / `total_time` are implemented in Rust.
    def self.report(out = $stdout)
      out.print(result)
      nil
    end
  end
end
