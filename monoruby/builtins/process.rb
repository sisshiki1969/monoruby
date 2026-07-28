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
