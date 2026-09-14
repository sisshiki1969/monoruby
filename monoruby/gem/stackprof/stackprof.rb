# stackprof/stackprof.rb – monoruby's stand-in for stackprof.so
#
# The stackprof gem is `gem "stackprof", platforms: :mri` in many Gemfiles
# (rails, railsbench, lobsters), and monoruby *is* `RUBY_ENGINE == "ruby"`
# to Bundler, so `Bundler.require` loads it at boot. Its Ruby half
# (`lib/stackprof.rb`, `StackProf::Report`, `StackProf::Middleware`) is
# plain Ruby over the C extension's `start` / `stop` / `results` /
# `sample`, which this file provides.
#
# monoruby has no signal-driven sampling of its frames, so the profiler
# is *inert*: `start` answers false and reports that once, `results`
# answers nil, and `StackProf.run { }` runs its block and returns nil,
# the same as stackprof does when it fails to start. Every gem that
# merely depends on the library loads and runs; only the profiles are
# missing.

module StackProf
  @running = false
  @warned = false

  class << self
    def running?
      @running
    end

    def start(mode: :cpu, interval: nil, raw: false, aggregate: true, save_every: nil, out: nil, ignore_gc: false, metadata: {}, debug: false)
      unless @warned
        @warned = true
        warn "StackProf: sampling profiles are not available on monoruby; StackProf.run/start does nothing"
      end
      false
    end

    def stop
      false
    end

    def results(filename = nil)
      nil
    end

    def sample
      nil
    end

    def use_postponed_job!
      nil
    end

    def run(**opts)
      start(**opts)
      yield
      results
    end
  end
end
