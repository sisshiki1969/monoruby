# monoruby-hidden-ivars: true
#
# Everything these classes keep about themselves is, in CRuby, a field of
# a C struct: nothing a program can list, read, marshal or collide with,
# and nothing `freeze` protects. The magic comment above gives this file
# the same: an instance variable spelled `@__name` here is a hidden slot
# (doc/hidden_ivars.md). Every instance variable in this file is one.

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
    # Skip this method's own frame so the yielded locations match
    # `caller_locations` as seen by our caller.
    locs = caller_locations(2)
    locs.each(&block) if locs
    nil
  end

  # NOTE: every raise inside an *instance* method of Thread must be the
  # explicit `Kernel.raise` — a bare `raise` dispatches on `self`, and
  # `self` here is the receiver Thread, so it would silently become the
  # asynchronous `Thread#raise` *into that thread* (queueing the
  # exception there and returning nil to the caller) whenever the
  # receiver is not the current thread.

  def name = @__name

  # The mutexes this thread holds, as a list threaded through the mutexes
  # themselves (`Mutex#__held_next`), most recently locked first — CRuby's
  # `keeping_mutexes`. `Mutex#try_lock` pushes, `Mutex#unlock` pops (or,
  # unlocking out of order, unlinks with #__unhold), and whatever is left
  # when the thread ends is released by the exit hook
  # (`scheduler::thread_body_exit`), as CRuby's
  # `rb_threadptr_unlock_all_locking_mutexes` does. Accessors rather than
  # methods with bodies: the lock and unlock paths are hot, and these the
  # JIT inlines — and entering a method with a body is a point where the
  # thread may be switched or interrupted, which Mutex#try_lock and
  # #unlock rely on there being none of. (`attr_accessor :__name` in a
  # file with hidden instance variables is over the hidden one.)
  attr_accessor :__held

  # Unlink `mutex` from the held list wherever it is (an unlock in other
  # than locking order; the common case is handled inline in Mutex#unlock).
  def __unhold(mutex)
    prev = nil
    m = @__held
    while m
      nxt = m.__held_next
      if m.equal?(mutex)
        if prev
          prev.__held_next = nxt
        else
          @__held = nxt
        end
        return
      end
      prev = m
      m = nxt
    end
    nil
  end

  # The exit hook's Ruby half, run in this thread as its last act: release
  # every mutex still held, waking their waiters — a fiber scheduler's
  # through `unblock`. Last locked, first released, as CRuby. A waiter's
  # `unblock` runs in this thread and may lock and unlock mutexes of its
  # own; they come and go on this same list, hence the re-read each round.
  def __unlock_on_exit
    while (m = @__held)
      @__held = m.__held_next
      m.__unlock_abandoned
    end
    nil
  end

  # A fiber parked through the fiber scheduler, standing in a Mutex's or
  # a ConditionVariable's waiter list where a Thread otherwise stands: it
  # was parked by `scheduler.block(blocker, timeout)` (or `kernel_sleep`),
  # and the wake a Thread waiter gets as `__wakeup_permit` reaches it as
  # the scheduler's `unblock(blocker, fiber)` — the thread's scheduler at
  # that moment, as CRuby's wakeup_one; none installed any more, nothing
  # to tell.
  class SchedulerWaiter
    def fiber = @__fiber

    def initialize(blocker)
      @__fiber = Fiber.current
      @__blocker = blocker
    end

    def alive?
      @__fiber.alive?
    end

    def __wakeup_permit
      scheduler = Fiber.scheduler
      scheduler.unblock(@__blocker, @__fiber) if scheduler
    end
  end

  # CRuby coerces the name with #to_str (TypeError otherwise), rejects
  # embedded NULs, and allows nil to clear the name.
  def name=(name)
    unless name.nil?
      name = __to_str(name) unless name.is_a?(String)
      if name.include?("\0")
        Kernel.raise ArgumentError, "string contains null byte"
      end
    end
    @__name = name
  end

  # Reported when a thread dies with an unhandled exception (read by the
  # native finalizer). Falls back to the class-level default
  # (`Thread.report_on_exception`), which defaults to true, as in CRuby.
  def report_on_exception
    @__report_on_exception.nil? ? Thread.report_on_exception : @__report_on_exception
  end

  def report_on_exception=(flag)
    @__report_on_exception = flag
  end

  # The group this thread was added to (ThreadGroup#add), defaulting to
  # ThreadGroup::Default. The timeout gem consults
  # `watcher.group.enclosed?` when it spawns its watchdog thread.
  def group
    @__thread_group || ThreadGroup::Default
  end

  # ThreadGroup#add's half of the move.
  def __group=(group)
    @__thread_group = group
  end

  # CRuby's Thread#to_s: `#<Thread:0xADDR[@__name] status>` (the spawn
  # file:line is not tracked). ruby/spec interpolates this into the
  # report_on_exception output matchers, so the `#<Thread:` prefix and
  # the status word matter.
  def to_s
    st = status
    st = "dead" unless st
    s = +"#<Thread:#{format('0x%016x', object_id << 1)}"
    s << "@#{@__name}" if @__name
    s << " #{@__spawn_location}" if @__spawn_location
    s << " #{st}>"
    # CRuby renders the description in BINARY, except that a non-ASCII
    # name's encoding carries through.
    s.force_encoding(@__name && !@__name.ascii_only? ? @__name.encoding : Encoding::BINARY)
  end
  alias inspect to_s

  # When set (per-thread, or the Thread global default), an exception
  # that terminates this thread is re-raised in the main thread (read by
  # the native finalizer, which also suppresses the report in that case,
  # as in CRuby). Default false.
  def abort_on_exception
    @__abort_on_exception.nil? ? false : @__abort_on_exception
  end

  def abort_on_exception=(flag)
    @__abort_on_exception = flag
  end

  class << self
    def abort_on_exception
      @__abort_on_exception.nil? ? false : @__abort_on_exception
    end

    def abort_on_exception=(flag)
      @__abort_on_exception = flag
    end

    # Global default for Thread#report_on_exception (true, as in CRuby).
    # Threads whose instance flag is unset fall back to this; ruby/spec's
    # spec_helper requires the class-level accessor to exist (otherwise
    # it installs a shim instance method that raises).
    def report_on_exception
      @__report_on_exception.nil? ? true : @__report_on_exception
    end

    def report_on_exception=(flag)
      @__report_on_exception = flag
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
      __check_id(key).to_sym
    end
  end
  private :__thread_key

  # `rb_check_id`'s reading of a key (`#[]`, `#key?`, `#fetch`): as
  # `__thread_key`, except that a String broken in its encoding is an
  # EncodingError where `#[]=` would make it a Symbol.
  def __thread_key_check(key)
    case key
    when Symbol
      key
    when String
      __name_to_sym(key)
    else
      __name_to_sym(__check_id(key))
    end
  end
  private :__thread_key_check

  # Fiber-locals genuinely belong to the *fiber* (CRuby): reads/writes
  # key a per-fiber table. At a thread's root context (no explicit
  # Fiber) the table is keyed under :root; reading another thread
  # resolves its root table, which is where a plain thread body's
  # locals live.
  def __fiber_local_table(create)
    # The caller's fiber only keys accesses to the *current* thread's
    # locals; another thread's locals resolve through its root table
    # (its own current fiber — the root, for any parked plain thread —
    # is not observable from outside).
    fk = if equal?(Thread.current)
      Fiber.__current_fiber || :root
    else
      :root
    end
    all = @__fiber_locals
    if all.nil?
      return nil unless create
      all = @__fiber_locals = {}
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
    k = __thread_key_check(key)
    t = __fiber_local_table(false)
    t && t[k]
  end

  def []=(key, value)
    Kernel.raise FrozenError, "can't modify frozen thread locals" if frozen?
    k = __thread_key(key)
    if value.nil?
      # CRuby: assigning nil removes the fiber-local (`key?` / `keys`
      # stop listing it).
      t = __fiber_local_table(false)
      t.delete(k) if t
      return value
    end
    __fiber_local_table(true)[k] = value
  end

  def key?(key)
    k = __thread_key_check(key)
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
    k = __thread_key_check(key)
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
    @__thread_variables && @__thread_variables[k]
  end

  def thread_variable_set(key, value)
    Kernel.raise FrozenError, "can't modify frozen thread locals" if frozen?
    (@__thread_variables ||= {})[__thread_key(key)] = value
  end

  def thread_variable?(key)
    # CRuby: assigning nil "removes" the variable as far as this
    # predicate is concerned (the key itself stays listed in
    # #thread_variables) — so test the value, not key presence.
    k = __thread_key(key)
    !!(@__thread_variables && !@__thread_variables[k].nil?)
  end

  def thread_variables
    @__thread_variables ? @__thread_variables.keys : []
  end

  # monoruby has no real thread-scheduler priority; store the value so it
  # round-trips (a new thread defaults to 0, matching an unset priority).
  def priority
    @__priority || 0
  end

  def priority=(value)
    value = __num2long(value)
    # CRuby clamps the stored priority to -3..3.
    value = 3 if value > 3
    value = -3 if value < -3
    @__priority = value
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
    if equal?(Thread.current)
      # Structured frames for the live stack. CRuby's C-implemented
      # #backtrace_locations reports itself as the first location,
      # rendered at its call site — synthesize that frame, then the
      # caller's stack.
      frames = Kernel.__caller_frames(1)
      site = frames[0]
      locs = []
      if site
        locs << Thread::Backtrace::Location.new(
          ["#{site[1]}:#{site[3]}:in 'Thread#backtrace_locations'",
           site[1], site[2], site[3], "Thread#backtrace_locations"]
        )
      end
      frames.each { |f| locs << Thread::Backtrace::Location.new(f) }
      return Thread::Backtrace.__slice(locs, args)
    end
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
      Kernel.raise TypeError, "allocator undefined for #{self}"
    end
  end

  # The object returned by `Process.detach`. Reaping one specific child via
  # `Process.wait2` is a *terminating* operation (unlike an arbitrary thread
  # body), so — unlike a general Thread, which defines no #join / #value —
  # the waiter safely runs the reaper on #join / #value. A missing child
  # (`Errno::ECHILD`) yields nil, matching CRuby. This is what keeps Open3's
  # `wait_thr.value` / `wait_thr.join` working.
  class ::Process::Waiter < Thread
    # The native Thread.new requires a block (it queues a green thread);
    # a Waiter is an inert shell around one specific child pid, so build
    # it via allocate. `Process.detach` is the only way to one: CRuby
    # undefines `new` on the class.
    def self.__detach(pid)
      # Not `allocate` — Thread.allocate deliberately raises TypeError
      # (no user-visible allocator); the privileged spelling still works.
      w = __builtin_allocate__
      w.__send__(:__init_waiter, pid)
      w
    end

    class << self
      undef_method :new
    end

    def __init_waiter(pid)
      @__pid = pid
      self[:pid] = pid
      # Reap the child as soon as it exits, not only at #value/#join time:
      # a real green thread parks on a pidfd (pollable process handle)
      # and collects the status when it fires, so detached children never
      # linger as zombies that a later `Process.wait(-1)` would reap by
      # accident (CRuby's detach thread gives the same guarantee).
      waiter = self
      fd = ::Process.__pidfd_open(pid)
      if fd
        @__reaper = ::Thread.new do
          begin
            io = ::IO.for_fd(fd)
            ::IO.select([io])
            io.close
          rescue ::Exception
          end
          waiter.__send__(:__reap)
        end
      end
    end
    private :__init_waiter

    def pid = @__pid

    def value
      @__reaper.join if @__reaper
      __reap
    end

    def join(limit = nil)
      @__reaper.join(limit) if @__reaper
      __reap
      self
    end

    private

    def __reap
      return @__status if @__reaped
      @__reaped = true
      @__status = begin
        Process.wait2(@__pid)[1]
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
    # A copy is a new, unlocked mutex with no waiters: CRuby allocates it
    # fresh and its lock is not state `dup` copies. Here the lock lives
    # in ivars, which `dup` does copy, so they are reset (#1624).
    private def initialize_copy(other)
      super
      @__owner = nil
      @__owner_fiber = nil
      @__held_next = nil
      @__waiters = nil
      self
    end

    # A Mutex holds native synchronization state, so it has no wire
    # form. CRuby's T_DATA path reports the missing `_dump_data` hook.
    def marshal_dump
      raise TypeError, "no _dump_data is defined for class #{self.class}"
    end

    def locked?
      # A thread releases its locks when it terminates (CRuby semantics), so
      # an owner that is no longer alive means the lock has been abandoned
      # and the mutex is effectively unlocked.
      (o = @__owner) ? o.alive? : false
    end

    def owned?
      # Ownership is per-Fiber (CRuby): the same Thread's other Fibers do not
      # own a mutex this Fiber locked. The Thread test also makes a cross-
      # thread check false regardless of Fiber identity.
      @__owner == Thread.current && @__owner_fiber == Fiber.current
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
      if @__owner
        false
      else
        @__owner = cur
        @__owner_fiber = cur_fiber
        @__held_next = cur.__held
        cur.__held = self
        true
      end
    end

    # The next mutex in the owner thread's held list (Thread#__held).
    attr_accessor :__held_next

    def lock
      # Deadlock detection is per-Thread, not per-Fiber: a mutex already held
      # by THIS thread (in any fiber) can only be released by this thread, so
      # trying to acquire it again — recursively or from a sibling fiber —
      # would park the one thread that could release it. Raise rather than
      # hang. (Ownership *reported* by #owned? is still per-Fiber.)
      if @__owner == Thread.current
        # With a fiber scheduler, another fiber of this thread may hold it
        # and can run and release it while this fiber is parked through the
        # scheduler (CRuby's check is per fiber); without one nothing could,
        # and raising is the only way out.
        if @__owner_fiber == Fiber.current || !Fiber.current_scheduler
          raise ThreadError, "deadlock; recursive locking"
        end
      end
      until try_lock
        # Reclaim a lock abandoned by a terminated owner (a dead thread
        # never unlocks or wakes waiters, so a contender must take it over
        # rather than park forever). Clearing @__owner is idempotent under a
        # race — concurrent reclaimers write the same nil — and the actual
        # acquisition still goes through try_lock's atomic test-and-set, so
        # no two threads can both win.
        if (o = @__owner) && !o.alive?
          @__owner = nil
          next
        end
        if (scheduler = Fiber.current_scheduler)
          # Park through the scheduler (CRuby's do_mutex_lock): `block(mutex,
          # nil)`, answered by `unblock(mutex, fiber)` from #unlock.
          w = SchedulerWaiter.new(self)
          (@__waiters ||= []) << w
          begin
            scheduler.block(self, nil)
          ensure
            @__waiters.delete(w)
          end
        else
          (@__waiters ||= []) << Thread.current
          begin
            Thread.__stop
          ensure
            @__waiters.delete(Thread.current)
          end
        end
      end
      self
    end

    def unlock
      unless @__owner
        raise ThreadError, "Attempt to unlock a mutex which is not locked"
      end
      # Thread-level check (not the per-Fiber #owned?): keeps the hot unlock
      # path free of the extra Fiber.current safepoint, and every unlock spec
      # exercises the cross-thread case anyway.
      cur = Thread.current
      unless @__owner == cur
        raise ThreadError, "Attempt to unlock a mutex which is locked by another thread"
      end
      # Off the thread's list first, while this thread still owns the
      # mutex: #__unhold is a method with a body, and a thread that took
      # the mutex while this one was switched out in there would have put
      # its own link in `@__held_next`. The link is then dropped — it
      # would keep the next mutex alive for as long as this one lives.
      if cur.__held.equal?(self)
        cur.__held = @__held_next
      else
        cur.__unhold(self)
      end
      @__held_next = nil
      @__owner = nil
      __wake_waiter if @__waiters
      self
    end

    # Hand the next live waiter a chance; it re-contends in its lock loop.
    def __wake_waiter
      while (w = @__waiters.shift)
        if w.alive?
          # Permit-arming wake: unlike the public Thread#wakeup, a wake
          # racing the waiter's park in Mutex#lock (preempted between
          # registering and parking) is not lost — the next park returns
          # immediately and the lock loop re-contends. A SchedulerWaiter
          # takes it as the scheduler's `unblock`.
          w.__wakeup_permit
          break
        end
      end
    end
    private :__wake_waiter

    # The owner thread is ending with the mutex still locked
    # (Thread#__unlock_on_exit): release it and wake a waiter.
    def __unlock_abandoned
      @__held_next = nil
      @__owner = nil
      @__owner_fiber = nil
      __wake_waiter if @__waiters
      nil
    end

    def synchronize
      raise ThreadError, "must be called with a block" unless block_given?
      lock
      begin
        yield
      ensure
        # The release is uninterruptible, and it has to be: asynchronous
        # interrupts arrive in a stream (`Thread#raise` in a loop, a
        # kill following a raise), and `owned?` is a method call, so the
        # ensure is itself full of safepoints. A second interrupt
        # landing in here unwound the thread with the mutex still held
        # by a fiber that no longer exists; the thread's next `lock` on
        # it then reported "deadlock; recursive locking" against itself.
        # `Mutex#sleep`'s re-acquire is uninterruptible for the same
        # reason.
        #
        # `owned?`, not an unconditional `unlock`: the body may have
        # released the mutex itself (`Mutex#sleep` does, and CRuby lets
        # a `synchronize` block unlock), and unlocking twice would raise
        # out of the ensure over whatever exception is already in
        # flight.
        #
        # The exit delivery point is *not* a blocking one — this ensure
        # is not a blocking call, whatever the body was — so an
        # `:on_blocking` interrupt stays deferred here. That keeps
        # `Queue#pop(true)`, a `synchronize` that raises `ThreadError`
        # without ever parking, from turning into a delivery point.
        Thread.__uninterruptible { unlock if owned? }
      end
    end

    # Atomically release the mutex, sleep, and re-acquire it on wake
    # (including on an exception raised into the sleeper). Returns the
    # number of seconds slept, rounded, when something woke the sleeper,
    # and nil when `timeout` ran out, as in CRuby (rb_mut_sleep) — which
    # is how `MonitorMixin::ConditionVariable#wait` tells the two apart. A
    # negative duration is an ArgumentError (validated before the
    # ownership check).
    def sleep(timeout = nil)
      # `rb_time_interval`, before the mutex is touched: a bad interval is
      # reported whether or not the caller holds the lock.
      limit = timeout.nil? ? nil : Thread.__time_interval(timeout)
      raise ThreadError, "Attempt to unlock a mutex which is not locked" unless owned?
      start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
      unlock
      woken = true
      begin
        if (scheduler = Fiber.current_scheduler)
          # CRuby's rb_mutex_sleep: the scheduler's `kernel_sleep(timeout)`,
          # nil included; a ConditionVariable#signal wakes the fiber with
          # `unblock(mutex, fiber)`. Whatever the hook did counts as woken.
          scheduler.kernel_sleep(timeout)
        elsif limit
          woken = Thread.__sleep(limit)
        else
          Thread.__stop
        end
      ensure
        # The re-acquire must complete even when a kill / Thread#raise
        # arrives while parked waiting for the lock (CRuby re-acquires
        # with mutex_lock_uninterruptible): a ConditionVariable#wait-er
        # killed after being signaled still owns the mutex in its own
        # ensure blocks. The deferred interrupt fires right after the
        # lock is held — at a blocking delivery point, since the sleep
        # this is the ensure of is itself the blocking call.
        Thread.__uninterruptible(true) { lock }
      end
      woken ? (Process.clock_gettime(Process::CLOCK_MONOTONIC) - start).round : nil
    end
  end

  class Queue
    # A queue cannot be copied: CRuby undefines the copy hook, so `dup` /
    # `clone` answer `NoMethodError` for `initialize_copy` rather than
    # sharing (or silently re-creating) the waiters (#1624).
    # `SizedQueue < Queue` inherits it.
    undef_method :initialize_copy

    # Same as ConditionVariable: the queue's waiters and monitor cannot
    # be serialized. (`SizedQueue < Queue` inherits this, and CRuby
    # names the receiver's own class in both.)
    def marshal_dump
      raise TypeError, "can't dump #{self.class}"
    end

    # Layered on Mutex + ConditionVariable, exactly like CRuby's
    # thread_sync.c. Mutual exclusion (not statement ordering) is what
    # makes check-and-take atomic here: with callee-entry GC/preempt
    # polls, EVERY method call is a safepoint, so an unlocked
    # `@__items.empty?` / `@__items.shift` pair lets a competing consumer
    # take the last item in between and the shift returns a phantom nil.
    #
    # NOTE: the wait loops are `while true`, never `loop do` —
    # `Kernel#loop` swallows StopIteration and ClosedQueueError is a
    # StopIteration subclass (so `loop { q.pop }` ends cleanly on close);
    # a `raise ClosedQueueError` inside a `loop` block would be silently
    # eaten and turned into a nil return.
    def initialize(enum = nil)
      @__items = []
      @__closed = false
      @__mutex = Mutex.new
      @__cv_pop = ConditionVariable.new
      unless enum.nil?
        # CRuby's rb_convert_type protocol (to_a), messages included.
        ary = __convert_type(enum, Array, "Array", :to_a, false)
        @__items.concat(ary)
      end
    end

    # Queue holds runtime synchronization state: freezing is rejected
    # (CRuby raises regardless of receiver state).
    def freeze
      raise TypeError, "cannot freeze #{self}"
    end

    def push(item)
      @__mutex.synchronize do
        raise ClosedQueueError, "queue closed" if @__closed
        @__items.push(item)
        @__cv_pop.signal
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
      # `rb_num2dbl`
      __num2dbl(timeout)
    end
    private :__check_timeout

    def pop(non_block = false, timeout: nil)
      timeout = __check_timeout(timeout, non_block)
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      @__mutex.synchronize do
        while true
          return __pop_locked unless @__items.empty?
          # CRuby raises even on a closed queue: only a *blocking* pop
          # returns nil for closed-and-drained.
          raise ThreadError, "queue empty" if non_block
          return nil if @__closed
          if deadline
            remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
            return nil if remaining <= 0
            @__cv_pop.__wait(@__mutex, remaining, self)
          else
            @__cv_pop.__wait(@__mutex, nil, self)
          end
        end
      end
    end
    alias shift pop
    alias deq pop

    def empty?
      @__items.empty?
    end

    def size
      @__items.size
    end
    alias length size

    def clear
      @__mutex.synchronize { @__items.clear }
      self
    end

    # Closing wakes every parked consumer/producer: consumers drain the
    # remaining items then get nil; producers raise ClosedQueueError.
    def close
      @__mutex.synchronize do
        return self if @__closed
        @__closed = true
        @__cv_pop.broadcast
      end
      self
    end

    def closed?
      @__closed
    end

    def num_waiting
      @__cv_pop.__num_waiting
    end

    private

    # Take the head item; the caller holds @__mutex and has checked
    # non-emptiness. SizedQueue overrides this to also free a producer.
    def __pop_locked
      @__items.shift
    end
  end

  class SizedQueue < Queue
    def initialize(max)
      max = __check_capacity(max)
      super()
      @__max = max
      @__cv_push = ConditionVariable.new
    end

    def max = @__max

    # CRuby's NUM2LONG semantics for the capacity: a non-Integer converts
    # via #to_int (so 12.9 truncates to 12), anything inconvertible is a
    # TypeError, and only then is positivity checked (ArgumentError).
    def __check_capacity(v)
      v = __num2long(v)
      raise ArgumentError, "queue size must be positive" unless v > 0
      v
    end
    private :__check_capacity

    def max=(new_max)
      new_max = __check_capacity(new_max)
      @__mutex.synchronize do
        grew = new_max > @__max
        @__max = new_max
        # Growing frees capacity: every parked producer re-checks.
        @__cv_push.broadcast if grew
      end
      new_max
    end

    def push(item, non_block = false, timeout: nil)
      timeout = __check_timeout(timeout, non_block)
      deadline = timeout && Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
      @__mutex.synchronize do
        while true
          raise ClosedQueueError, "queue closed" if @__closed
          if @__items.size < @__max
            @__items.push(item)
            @__cv_pop.signal
            return self
          end
          raise ThreadError, "queue full" if non_block
          if deadline
            remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
            return nil if remaining <= 0
            @__cv_push.__wait(@__mutex, remaining, self)
          else
            @__cv_push.__wait(@__mutex, nil, self)
          end
        end
      end
    end
    alias << push
    alias enq push

    def clear
      @__mutex.synchronize do
        @__items.clear
        # Clearing frees capacity (CRuby wakes blocked producers here).
        @__cv_push.broadcast
      end
      self
    end

    def close
      @__mutex.synchronize do
        return self if @__closed
        @__closed = true
        @__cv_pop.broadcast
        @__cv_push.broadcast
      end
      self
    end

    def num_waiting
      @__cv_pop.__num_waiting + @__cv_push.__num_waiting
    end

    private

    def __pop_locked
      v = @__items.shift
      # Popping frees a slot: let one parked producer through.
      @__cv_push.signal
      v
    end
  end

  class ConditionVariable
    def initialize
      @__waiters = []
    end

    # Not copyable, as a Queue is not (see there).
    undef_method :initialize_copy

    # Atomically release `mutex` and park until signaled (or the timeout
    # elapses), then re-acquire `mutex` before returning — also on an
    # exception raised into the waiter. Returns what `mutex.sleep(timeout)`
    # returned, as CRuby does: for a Mutex, nil when the timeout elapsed.
    def wait(mutex, timeout = nil)
      __wait(mutex, timeout, nil)
    end

    # Park on this variable: `mutex` is released for the wait and held
    # again after, also on an exception raised into the waiter. A Thread
    # parks through Mutex#sleep, which does exactly that (and is why
    # CRuby's `#wait` calls `#sleep` on its argument). A non-blocking
    # fiber with a fiber scheduler parks through the scheduler instead: a
    # Queue's wait names the queue as `blocker` and goes through
    # `block(queue, timeout)` (CRuby's queue_sleep), a plain #wait through
    # Mutex#sleep's `kernel_sleep`; #signal wakes either with
    # `unblock(blocker or mutex, fiber)`.
    def __wait(mutex, timeout, blocker)
      if (scheduler = Fiber.current_scheduler)
        w = SchedulerWaiter.new(blocker || mutex)
        @__waiters << w
        begin
          if blocker
            mutex.unlock
            begin
              scheduler.block(blocker, timeout)
            ensure
              Thread.__uninterruptible(true) { mutex.lock }
            end
          else
            __sleep_on(mutex, timeout)
          end
        ensure
          @__waiters.delete(w)
        end
      else
        @__waiters << Thread.current
        begin
          __sleep_on(mutex, timeout)
        ensure
          @__waiters.delete(Thread.current)
        end
      end
    end

    # CRuby's do_sleep: a Mutex sleeps directly, anything else is sent
    # `sleep` with `rb_funcallv`, which does not look at visibility — so
    # an object whose only `sleep` is the private Kernel#sleep serves as
    # the "mutex" of a #wait.
    def __sleep_on(mutex, timeout)
      if Mutex === mutex
        mutex.sleep(timeout)
      else
        mutex.__send__(:sleep, timeout)
      end
    end
    private :__sleep_on

    # ConditionVariable holds runtime synchronization state that cannot be
    # serialized; CRuby raises TypeError from Marshal.
    def marshal_dump
      raise TypeError, "can't dump #{self.class}"
    end

    def signal
      while (w = @__waiters.shift)
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
      until @__waiters.empty?
        signal
      end
      self
    end

    # Waiter-count peek for Queue#num_waiting (a waiter registers before
    # releasing the mutex and stays registered until it re-acquires it,
    # so this tracks "parked in #wait" closely and race-free).
    def __num_waiting
      @__waiters.size
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
      r = __check_int(v)
      raise TypeError, "no implicit conversion of #{__builtin_class_name(v)} into Integer" if r.nil?
      r
    end

    class Location
      # A C struct in CRuby, with no `_dump_data`: `Marshal.dump` refuses it.
      @__no_dump_data = true

      # Structured form (from `Kernel.__caller_frames`):
      # `[to_s, path, absolute_path, lineno, label]` — carries the
      # load-time canonical absolute path. String form (from raw
      # backtrace frame strings, e.g. `Thread#backtrace_locations` /
      # `Exception#backtrace_locations`): parsed lazily, with
      # `absolute_path` reconstructed best-effort via expand_path.
      def initialize(frame)
        if frame.is_a?(Array)
          @__frame, @__path, @__absolute_path, @__lineno, @__label = frame
          @__absolute_known = true
          return
        end
        @__absolute_known = false
        @__frame = frame.to_s
        if @__frame =~ /\A(.+):(\d+):in ['`](.+)'\z/
          @__path = $1
          @__lineno = $2.to_i
          @__label = $3
        elsif @__frame =~ /\A(<.+>):in ['`](.+)'\z/
          # Native frame without a resolvable call site.
          @__path = $1
          @__lineno = 0
          @__label = $2
        else
          @__path = @__frame
          @__lineno = 0
          @__label = ""
        end
      end

      def path = @__path
      def lineno = @__lineno
      def label = @__label

      def base_label
        l = @__label
        l = $1 while l =~ /\Ablock (?:\(\d+ levels\) )?in (.+)\z/
        # The owner qualifier is stripped too: the base label of
        # "block in M::C.foo" is the bare "foo" (CRuby). Special
        # frames (<main>, <top (required)>, <module:A>, singleton
        # class) carry no qualifier and pass through.
        l = $2 if !l.start_with?("<") && l =~ /\A(.+)[#.]([^#.]+)\z/
        l
      end

      # CRuby returns nil for frames without a real file (eval'd code,
      # internal frames).
      def absolute_path
        return @__absolute_path if @__absolute_known
        return nil if @__path.start_with?("(") || @__path.start_with?("<")
        File.expand_path(@__path)
      end

      def to_s
        @__frame
      end

      # CRuby's location_inspect: the string form, inspected.
      def inspect
        @__frame.inspect
      end
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
  # A C struct in CRuby, with no `_dump_data`: `Marshal.dump` refuses it.
  @__no_dump_data = true

  def initialize
    @__enclosed = false
  end

  def enclose
    @__enclosed = true
    self
  end

  def enclosed?
    @__enclosed
  end

  def add(thread)
    if @__enclosed
      raise ThreadError, "can't move to the enclosed thread group"
    end
    if thread.group.enclosed?
      raise ThreadError, "can't move from the enclosed thread group"
    end
    thread.__group = self
    self
  end

  def list
    Thread.list.select { |t| t.group == self }
  end

  Default = new
end

ConditionVariable = Thread::ConditionVariable
Mutex = Thread::Mutex
