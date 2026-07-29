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

  def self.each_caller_location(&block)
    Kernel.raise LocalJumpError, "no block given" unless block
    locs = caller_locations(1)
    locs.each(&block) if locs
    nil
  end

  # Whether a detected deadlock is ignored rather than aborting. monoruby
  # does not run the deadlock detector, but the flag round-trips.
  @@ignore_deadlock = false
  def self.ignore_deadlock
    @@ignore_deadlock
  end

  def self.ignore_deadlock=(flag)
    @@ignore_deadlock = flag
  end

  # NOTE: every raise inside an *instance* method of Thread must be the
  # explicit `Kernel.raise` — a bare `raise` dispatches on `self`, and
  # `self` here is the receiver Thread, so it would silently become the
  # asynchronous `Thread#raise` *into that thread* (queueing the
  # exception there and returning nil to the caller) whenever the
  # receiver is not the current thread.

  attr_reader :name

  # CRuby coerces the name with #to_str (TypeError otherwise), rejects
  # embedded NULs, and allows nil to clear the name.
  def name=(name)
    unless name.nil?
      unless name.is_a?(String)
        if name.respond_to?(:to_str)
          name = name.to_str
          unless name.is_a?(String)
            Kernel.raise TypeError,
              "can't convert #{name.class} to String (#{name.class}#to_str gives #{name.class})"
          end
        else
          Kernel.raise TypeError, "no implicit conversion of #{name.class} into String"
        end
      end
      if name.include?("\0")
        Kernel.raise ArgumentError, "string contains null byte"
      end
    end
    @name = name
  end

  # Reported when a thread dies with an unhandled exception (read by the
  # native finalizer). Falls back to the class-level default
  # (`Thread.report_on_exception`), which defaults to true, as in CRuby.
  def report_on_exception
    @report_on_exception.nil? ? Thread.report_on_exception : @report_on_exception
  end

  def report_on_exception=(flag)
    @report_on_exception = flag
  end

  # The group this thread was added to (ThreadGroup#add), defaulting to
  # ThreadGroup::Default. The timeout gem consults
  # `watcher.group.enclosed?` when it spawns its watchdog thread.
  def group
    @__thread_group || ThreadGroup::Default
  end

  # CRuby's Thread#to_s: `#<Thread:0xADDR[@name] status>` (the spawn
  # file:line is not tracked). ruby/spec interpolates this into the
  # report_on_exception output matchers, so the `#<Thread:` prefix and
  # the status word matter.
  def to_s
    st = status
    st = "dead" unless st
    s = +"#<Thread:#{format('0x%016x', object_id << 1)}"
    s << "@#{@name}" if @name
    s << " #{@__spawn_location}" if @__spawn_location
    s << " #{st}>"
    s
  end
  alias inspect to_s

  # When set (per-thread, or the Thread global default), an exception
  # that terminates this thread is re-raised in the main thread (read by
  # the native finalizer, which also suppresses the report in that case,
  # as in CRuby). Default false.
  def abort_on_exception
    @abort_on_exception.nil? ? false : @abort_on_exception
  end

  def abort_on_exception=(flag)
    @abort_on_exception = flag
  end

  class << self
    def abort_on_exception
      @abort_on_exception.nil? ? false : @abort_on_exception
    end

    def abort_on_exception=(flag)
      @abort_on_exception = flag
    end

    # Global default for Thread#report_on_exception (true, as in CRuby).
    # Threads whose instance flag is unset fall back to this; ruby/spec's
    # spec_helper requires the class-level accessor to exist (otherwise
    # it installs a shim instance method that raises).
    def report_on_exception
      @report_on_exception.nil? ? true : @report_on_exception
    end

    def report_on_exception=(flag)
      @report_on_exception = flag
    end
  end

  # Thread objects are native (ObjTy::THREAD) and no longer run a Ruby
  # initialize, so the local-storage tables are created lazily.

  # Normalize a fiber-local / thread-variable key to a Symbol, as CRuby
  # does: Symbols pass through, Strings convert, an object with #to_str
  # converts through that, anything else is a TypeError.
  def __thread_key(key)
    case key
    when Symbol
      key
    when String
      key.to_sym
    else
      if key.respond_to?(:to_str)
        key.to_str.to_sym
      else
        Kernel.raise TypeError, "#{key.inspect} is not a symbol nor a string"
      end
    end
  end
  private :__thread_key

  # Fiber-locals genuinely belong to the *fiber* (CRuby): reads/writes
  # key a per-fiber table. At a thread's root context (no explicit
  # Fiber) the table is keyed under :root; reading another thread
  # resolves its root table, which is where a plain thread body's
  # locals live.
  def __fiber_local_table(create)
    fk = Fiber.__current_fiber || :root
    all = @fiber_locals
    if all.nil?
      return nil unless create
      all = @fiber_locals = {}
    end
    t = all[fk]
    if t.nil?
      return nil unless create
      t = all[fk] = {}
    end
    t
  end
  private :__fiber_local_table

  def [](key)
    k = __thread_key(key)
    t = __fiber_local_table(false)
    t && t[k]
  end

  def []=(key, value)
    Kernel.raise FrozenError, "can't modify frozen thread locals" if frozen?
    k = __thread_key(key)
    __fiber_local_table(true)[k] = value
  end

  def key?(key)
    k = __thread_key(key)
    t = __fiber_local_table(false)
    !!(t && t.key?(k))
  end

  def keys
    t = __fiber_local_table(false)
    t ? t.keys : []
  end

  def fetch(key, *default)
    if default.size > 1
      Kernel.raise ArgumentError, "wrong number of arguments (given #{default.size + 1}, expected 1..2)"
    end
    k = __thread_key(key)
    t = __fiber_local_table(false)
    if t && t.key?(k)
      t[k]
    elsif block_given?
      warn "warning: block supersedes default value argument" unless default.empty?
      yield(key)
    elsif !default.empty?
      default[0]
    else
      Kernel.raise KeyError.new("key not found: #{key.inspect}", receiver: self, key: key)
    end
  end

  def thread_variable_get(key)
    k = __thread_key(key)
    @thread_variables && @thread_variables[k]
  end

  def thread_variable_set(key, value)
    Kernel.raise FrozenError, "can't modify frozen thread locals" if frozen?
    (@thread_variables ||= {})[__thread_key(key)] = value
  end

  def thread_variable?(key)
    # CRuby: assigning nil "removes" the variable as far as this
    # predicate is concerned (the key itself stays listed in
    # #thread_variables) — so test the value, not key presence.
    k = __thread_key(key)
    !!(@thread_variables && !@thread_variables[k].nil?)
  end

  def thread_variables
    @thread_variables ? @thread_variables.keys : []
  end

  # monoruby has no real thread-scheduler priority; store the value so it
  # round-trips (a new thread defaults to 0, matching an unset priority).
  def priority
    @priority || 0
  end

  def priority=(value)
    unless value.is_a?(Integer)
      unless value.respond_to?(:to_int) && (value = value.to_int).is_a?(Integer)
        Kernel.raise TypeError, "no implicit conversion of #{value.class} into Integer"
      end
    end
    # CRuby clamps the stored priority to -3..3.
    value = 3 if value > 3
    value = -3 if value < -3
    @priority = value
  end

  # A per-Thread identifier while the thread is alive (nil once it has
  # finished). monoruby multiplexes green threads onto one OS thread, so
  # this is a distinct-per-object token rather than a real kernel tid.
  def native_thread_id
    alive? ? object_id : nil
  end

  # Thread#backtrace / #backtrace_locations: the native `__backtrace`
  # yields the raw frame strings (nil for a dead thread); the argument
  # slicing and Location wrapping live here.
  def backtrace(*args)
    bt = __backtrace
    return bt if bt.nil?
    Thread::Backtrace.__slice(bt, args)
  end

  def backtrace_locations(*args)
    bt = __backtrace
    return nil if bt.nil?
    sliced = Thread::Backtrace.__slice(bt, args)
    sliced && sliced.map { |f| Thread::Backtrace::Location.new(f) }
  end

  # True aliases (ruby/spec checks `instance_method(:terminate) ==
  # instance_method(:kill)` and `method(:fork) == method(:start)`).
  alias terminate kill
  alias exit kill
  class << self
    alias fork start

    # Thread has no allocator: instances are only created through
    # Thread.new/start/fork (CRuby raises the same TypeError).
    def allocate
      Kernel.raise TypeError, "allocator undefined for Thread"
    end
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
      # Not `allocate` — Thread.allocate deliberately raises TypeError
      # (no user-visible allocator); the privileged spelling still works.
      w = __builtin_allocate__
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

  # With timeslice preemption, a switch can occur at any safepoint —
  # every method call and loop back-edge. These pure-Ruby primitives
  # stay correct under that model through two rules:
  #
  # 1. Test-and-set sequences contain no safepoint between the test and
  #    the set: straight-line ivar reads/writes only, with any needed
  #    method calls (e.g. `Thread.current`) hoisted before the test.
  #    See `Mutex#try_lock`.
  # 2. Every "register as waiter → park" sequence tolerates a wakeup
  #    landing in the window before the park: `Thread#wakeup` on a
  #    *running* thread arms its park permit, making the next park
  #    return immediately (scheduler.rs, `ThreadInner::park_permit`).
  #    The park sites all sit in retry loops, so a spurious early
  #    return is re-checked.

  class Mutex
    def locked?
      # A thread releases its locks when it terminates (CRuby semantics), so
      # an owner that is no longer alive means the lock has been abandoned
      # and the mutex is effectively unlocked.
      (o = @owner) ? o.alive? : false
    end

    def owned?
      # Ownership is per-Fiber (CRuby): the same Thread's other Fibers do not
      # own a mutex this Fiber locked. The Thread test also makes a cross-
      # thread check false regardless of Fiber identity.
      @owner == Thread.current && @owner_fiber == Fiber.current
    end

    def try_lock
      # Atomic under preemption: `Thread.current` is a method call (a
      # safepoint where a timeslice switch can occur), so it must be
      # evaluated BEFORE the test. The test-and-set below is straight-line
      # code — ivar read, branch, ivar write — with no calls and no loop
      # back-edges, hence no safepoint can interleave another thread
      # between the test and the set.
      cur = Thread.current
      cur_fiber = Fiber.current
      if @owner
        false
      else
        @owner = cur
        @owner_fiber = cur_fiber
        true
      end
    end

    def lock
      # Deadlock detection is per-Thread, not per-Fiber: a mutex already held
      # by THIS thread (in any fiber) can only be released by this thread, so
      # trying to acquire it again — recursively or from a sibling fiber —
      # would park the one thread that could release it. Raise rather than
      # hang. (Ownership *reported* by #owned? is still per-Fiber.)
      raise ThreadError, "deadlock; recursive locking" if @owner == Thread.current
      until try_lock
        # Reclaim a lock abandoned by a terminated owner (a dead thread
        # never unlocks or wakes waiters, so a contender must take it over
        # rather than park forever). Clearing @owner is idempotent under a
        # race — concurrent reclaimers write the same nil — and the actual
        # acquisition still goes through try_lock's atomic test-and-set, so
        # no two threads can both win.
        if (o = @owner) && !o.alive?
          @owner = nil
          next
        end
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
      # Thread-level check (not the per-Fiber #owned?): keeps the hot unlock
      # path free of the extra Fiber.current safepoint, and every unlock spec
      # exercises the cross-thread case anyway.
      unless @owner == Thread.current
        raise ThreadError, "Attempt to unlock a mutex which is locked by another thread"
      end
      @owner = nil
      # Hand the next waiter a chance; it re-contends in its lock loop.
      if @waiters
        while (w = @waiters.shift)
          if w.alive?
            # Permit-arming wake: unlike the public Thread#wakeup, a wake
            # racing the waiter's park in Mutex#lock (preempted between
            # registering and parking) is not lost — the next park returns
            # immediately and the lock loop re-contends.
            w.__wakeup_permit
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
    # (including on an exception raised into the sleeper). Returns the
    # rounded number of seconds actually slept, as in CRuby. A negative
    # duration is an ArgumentError (validated before the ownership check).
    def sleep(timeout = nil)
      if timeout && timeout < 0
        raise ArgumentError, "time interval must not be negative"
      end
      raise ThreadError, "Attempt to unlock a mutex which is not locked" unless owned?
      start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
      unlock
      begin
        timeout ? Kernel.sleep(timeout) : Thread.stop
      ensure
        lock
      end
      (Process.clock_gettime(Process::CLOCK_MONOTONIC) - start).round
    end
  end

  class Queue
    # Layered on Mutex + ConditionVariable, exactly like CRuby's
    # thread_sync.c. Mutual exclusion (not statement ordering) is what
    # makes check-and-take atomic here: with callee-entry GC/preempt
    # polls, EVERY method call is a safepoint, so an unlocked
    # `@items.empty?` / `@items.shift` pair lets a competing consumer
    # take the last item in between and the shift returns a phantom nil.
    #
    # NOTE: the wait loops are `while true`, never `loop do` —
    # `Kernel#loop` swallows StopIteration and ClosedQueueError is a
    # StopIteration subclass (so `loop { q.pop }` ends cleanly on close);
    # a `raise ClosedQueueError` inside a `loop` block would be silently
    # eaten and turned into a nil return.
    def initialize
      @items = []
      @closed = false
      @mutex = Mutex.new
      @cv_pop = ConditionVariable.new
    end

    def push(item)
      @mutex.synchronize do
        raise ClosedQueueError, "queue closed" if @closed
        @items.push(item)
        @cv_pop.signal
      end
      self
    end
    alias << push
    alias enq push

    # CRuby-compatible `timeout:` validation, shared by Queue#pop,
    # SizedQueue#pop and SizedQueue#push. `false` must NOT be treated as
    # "no timeout": without the type check it fell through to the
    # unbounded `Thread.stop` branch and blocked forever.
    def __check_timeout(timeout, non_block)
      return nil if timeout.nil?
      raise ArgumentError, "can't set a timeout if non_block is enabled" if non_block
      return timeout if Numeric === timeout
      case timeout
      when true, false, String
        desc = String === timeout ? "string" : timeout.inspect
        raise TypeError, "no implicit conversion to float from #{desc}"
      end
      unless timeout.respond_to?(:to_f)
        raise TypeError, "can't convert #{timeout.class} into Float"
      end
      f = timeout.to_f
      unless Float === f
        raise TypeError,
              "can't convert #{timeout.class} to Float (#{timeout.class}#to_f gives #{f.class})"
      end
      f
    end
    private :__check_timeout

    def pop(non_block = false, timeout: nil)
      timeout = __check_timeout(timeout, non_block)
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      @mutex.synchronize do
        while true
          return __pop_locked unless @items.empty?
          # CRuby raises even on a closed queue: only a *blocking* pop
          # returns nil for closed-and-drained.
          raise ThreadError, "queue empty" if non_block
          return nil if @closed
          if deadline
            remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
            return nil if remaining <= 0
            @cv_pop.wait(@mutex, remaining)
          else
            @cv_pop.wait(@mutex)
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
      @mutex.synchronize { @items.clear }
      self
    end

    # Closing wakes every parked consumer/producer: consumers drain the
    # remaining items then get nil; producers raise ClosedQueueError.
    def close
      @mutex.synchronize do
        return self if @closed
        @closed = true
        @cv_pop.broadcast
      end
      self
    end

    def closed?
      @closed
    end

    def num_waiting
      @cv_pop.__num_waiting
    end

    private

    # Take the head item; the caller holds @mutex and has checked
    # non-emptiness. SizedQueue overrides this to also free a producer.
    def __pop_locked
      @items.shift
    end
  end

  class SizedQueue < Queue
    def initialize(max)
      max = max.to_int if !max.is_a?(Integer) && max.respond_to?(:to_int)
      raise ArgumentError, "queue size must be positive" unless max.is_a?(Integer) && max > 0
      super()
      @max = max
      @cv_push = ConditionVariable.new
    end

    attr_reader :max

    def max=(new_max)
      raise ArgumentError, "queue size must be positive" unless new_max.is_a?(Integer) && new_max > 0
      @mutex.synchronize do
        grew = new_max > @max
        @max = new_max
        # Growing frees capacity: every parked producer re-checks.
        @cv_push.broadcast if grew
      end
      new_max
    end

    def push(item, non_block = false, timeout: nil)
      timeout = __check_timeout(timeout, non_block)
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      @mutex.synchronize do
        while true
          raise ClosedQueueError, "queue closed" if @closed
          if @items.size < @max
            @items.push(item)
            @cv_pop.signal
            return self
          end
          raise ThreadError, "queue full" if non_block
          if deadline
            remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
            return nil if remaining <= 0
            @cv_push.wait(@mutex, remaining)
          else
            @cv_push.wait(@mutex)
          end
        end
      end
    end
    alias << push
    alias enq push

    def clear
      @mutex.synchronize do
        @items.clear
        # Clearing frees capacity (CRuby wakes blocked producers here).
        @cv_push.broadcast
      end
      self
    end

    def close
      @mutex.synchronize do
        return self if @closed
        @closed = true
        @cv_pop.broadcast
        @cv_push.broadcast
      end
      self
    end

    def num_waiting
      @cv_pop.__num_waiting + @cv_push.__num_waiting
    end

    private

    def __pop_locked
      v = @items.shift
      # Popping frees a slot: let one parked producer through.
      @cv_push.signal
      v
    end
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
        # Delegate to Mutex#sleep so the mutex is atomically released,
        # the caller parks, and the mutex is re-acquired on wake — exactly
        # as CRuby does (which is why `#wait` calls `#sleep` on its arg).
        mutex.sleep(timeout)
      ensure
        @waiters.delete(Thread.current)
      end
      self
    end

    # ConditionVariable holds runtime synchronization state that cannot be
    # serialized; CRuby raises TypeError from Marshal.
    def marshal_dump
      raise TypeError, "can't dump #{self.class}"
    end

    def signal
      while (w = @waiters.shift)
        if w.alive?
          # Permit-arming wake (see Mutex#unlock): a signal racing the
          # waiter's park in CV#wait must not be lost.
          w.__wakeup_permit
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

    # Waiter-count peek for Queue#num_waiting (a waiter registers before
    # releasing the mutex and stays registered until it re-acquires it,
    # so this tracks "parked in #wait" closely and race-free).
    def __num_waiting
      @waiters.size
    end
  end

  class Backtrace
    # Maximum backtrace length set by --backtrace-limit, or -1 when the
    # option was not given (CRuby). The runtime exposes the raw option
    # (nil when unset) as Kernel.__backtrace_limit.
    def self.limit
      Kernel.__backtrace_limit || -1
    end

    # Slice a raw backtrace per the Thread#backtrace(_locations) /
    # Kernel#caller(_locations) argument forms: (), (start), (start,
    # length) or (range). Array#[] slice semantics apply (an exactly
    # consumed start yields [], beyond it nil), plus CRuby's
    # ArgumentErrors for negative values. Lives on Backtrace (not
    # Thread) so a bare `raise` cannot dispatch to Thread#raise.
    def self.__slice(bt, args)
      case args.size
      when 0
        bt
      when 1
        a = args[0]
        if a.is_a?(Range)
          bt[a]
        else
          a = __slice_int(a)
          raise ArgumentError, "negative level (#{a})" if a < 0
          bt[a..]
        end
      when 2
        s = __slice_int(args[0])
        raise ArgumentError, "negative level (#{s})" if s < 0
        if args[1].nil?
          bt[s..]
        else
          l = __slice_int(args[1])
          raise ArgumentError, "negative size (#{l})" if l < 0
          bt[s, l]
        end
      else
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..2)"
      end
    end

    def self.__slice_int(v)
      return v if v.is_a?(Integer)
      if v.respond_to?(:to_int)
        r = v.to_int
        return r if r.is_a?(Integer)
      end
      raise TypeError, "no implicit conversion of #{v.class} into Integer"
    end

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

# Thread groups. Threads carry their group in an ivar (nil = Default);
# scheduling is untouched — groups are pure bookkeeping, which matches
# their CRuby role. The timeout gem needs `Thread#group`,
# `ThreadGroup#enclosed?` and `ThreadGroup::Default.add`.
class ThreadGroup
  def initialize
    @enclosed = false
  end

  def enclose
    @enclosed = true
    self
  end

  def enclosed?
    @enclosed
  end

  def add(thread)
    if @enclosed
      raise ThreadError, "can't move to the enclosed thread group"
    end
    if thread.group.enclosed?
      raise ThreadError, "can't move from the enclosed thread group"
    end
    thread.instance_variable_set(:@__thread_group, self)
    self
  end

  def list
    Thread.list.select { |t| t.group == self }
  end

  Default = new
end

ConditionVariable = Thread::ConditionVariable
Mutex = Thread::Mutex
