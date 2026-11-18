# monoruby-hidden-ivars: true

module Process
  # CLOCK_* constants are installed from libc (platform-correct values)
  # in builtins/process.rs.

  # Reap child `pid` and return a thread whose #value waits for it and yields
  # the resulting Process::Status. monoruby's Thread is cooperative (the block
  # runs at #value/#join time), which matches Open3's "drain the pipes first,
  # then read the exit status" ordering.
  def self.detach(pid)
    pid = __num2long(pid)
    Process::Waiter.__detach(pid)
  end

  # A Struct, as in CRuby (`Process.times.to_a`, `#<struct Process::Tms
  # utime=...>`).
  Tms = Struct.new(:utime, :stime, :cutime, :cstime)

  # Wraps the raw POSIX wait(2) status word (as returned by `waitpid`).
  # The word and the pid are hidden instance variables
  # (doc/hidden_ivars.md): CRuby keeps them in a C struct.
  # Callers pass the raw int and pid; all predicates decode the bit layout:
  #   bits 0..6  = termination signal (0 = normal exit)
  #   bit  7     = core dumped flag
  #   bits 8..15 = exit code (for normal exit)
  #   low byte 0x7F = stopped (SIGSTOP etc.)
  #
  # As in CRuby, a Status is made only by the interpreter (`Status.new` is
  # undefined; the runtime calls `__new`), and one made otherwise —
  # `allocate`, or `dup` / `clone`, which do not copy the C struct — is
  # uninitialized: its fields read as 0 and it inspects as
  # "uninitialized" (#2006).
  class Status
    class << self
      alias __new new
      undef_method :new
    end

    def pid
      @__pid || 0
    end

    def initialize(raw_status, pid)
      @__status = raw_status
      @__pid = pid
      # CRuby freezes the status it stores in `$?`; nothing here writes
      # an ordinary instance variable.
      freeze
    end

    def initialize_copy(other)
      @__status = nil
      @__pid = nil
    end
    private :initialize_copy

    def exited?
      (to_i & 0x7F) == 0
    end

    def exitstatus
      exited? ? (to_i >> 8) & 0xFF : nil
    end

    def signaled?
      low = to_i & 0x7F
      low != 0 && low != 0x7F
    end

    def termsig
      signaled? ? to_i & 0x7F : nil
    end

    def stopped?
      (to_i & 0xFF) == 0x7F
    end

    def stopsig
      stopped? ? (to_i >> 8) & 0xFF : nil
    end

    def coredump?
      (to_i & 0x80) != 0
    end

    def success?
      exited? ? exitstatus == 0 : nil
    end

    def to_i
      @__status || 0
    end

    def to_s
      if signaled?
        "pid #{pid} SIG#{termsig}#{coredump? ? ' (core dumped)' : ''}"
      elsif stopped?
        "pid #{pid} stopped SIG#{stopsig}"
      else
        "pid #{pid} exit #{exitstatus}"
      end
    end

    def inspect
      return "#<Process::Status: uninitialized>" if @__status.nil?
      __sprintf_repr("#<Process::Status: #{to_s}>")
    end

    # CRuby's `pst_equal`: the same object, or `to_i == other`.
    def ==(other)
      equal?(other) || to_i == other
    end
  end
end
