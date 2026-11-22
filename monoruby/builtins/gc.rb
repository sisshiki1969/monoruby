# monoruby-hidden-ivars: true

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
    # For `latest_gc_info`: the collection this call caused.
    @__method_gc_count = count if count != before
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
  @__auto_compact = false
  @__method_gc_count = nil

  def self.auto_compact
    @__auto_compact
  end

  def self.auto_compact=(flag)
    @__auto_compact = flag ? true : false
  end

  # `compact` degrades to a full collection and reports that nothing
  # moved, in CRuby's return shape (empty per-type tables). It must not
  # raise: the idiomatic `GC.compact if GC.respond_to?(:compact)` guard
  # passes here, so the call has to succeed.
  def self.compact
    GC.start
    { considered: {}, moved: {}, moved_up: {}, moved_down: {} }
  end

  # What the last (non-)compaction did — same shape as `compact`.
  def self.latest_compact_info
    { considered: {}, moved: {}, moved_up: {}, moved_down: {} }
  end

  # What started the latest collection and what kind it was, in
  # CRuby's keys (`rb_gc_impl_latest_gc_info`). A collection
  # `GC.start` asked for is `gc_by: :method`, swept at once; any other
  # is `:newobj`. monoruby sweeps inside the collection, so the state
  # is always `:none`, and it has no finalizer queue or weak-reference
  # table to count.
  def self.latest_gc_info(hash_or_key = nil)
    major = __latest_gc_major
    by_method = !major.nil? && @__method_gc_count == count
    info = {
      major_by: major ? (by_method ? :force : :oldgen) : nil,
      need_major_by: nil,
      gc_by: by_method ? :method : :newobj,
      have_finalizer: false,
      immediate_sweep: by_method,
      state: :none,
      weak_references_count: 0,
      retained_weak_references_count: 0,
    }
    if hash_or_key.nil?
      info
    elsif hash_or_key.is_a?(Symbol)
      info.fetch(hash_or_key) { raise ArgumentError, "unknown key: #{hash_or_key}" }
    elsif hash_or_key.is_a?(Hash)
      info.each { |k, v| hash_or_key[k] = v }
      hash_or_key
    else
      raise TypeError, "non-hash or symbol given"
    end
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
