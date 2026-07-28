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
