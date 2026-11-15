# frozen_string_literal: true
#
# Monitor, MonitorMixin and MonitorMixin::ConditionVariable.
#
# CRuby splits these between a C extension (ext/monitor/monitor.c, the
# `Monitor` class) and a Ruby file (ext/monitor/lib/monitor.rb,
# `MonitorMixin` and the rest of `Monitor`). Here the first half is that
# C file written in Ruby, and the second half is CRuby 4.0.9's monitor.rb
# as it is, less its `require 'monitor.so'`.
#
# They are part of the bootstrap (startup.rb loads this file), so
# `require "monitor"` only has to find stdlib/monitor.rb, which is empty.

#
# A re-entrant mutex: monitor.c's `struct rb_monitor`, which is a
# Thread::Mutex, the Fiber that holds it and how many times that fiber
# entered. Only the owner reads or writes `@count`, and `@owner` changes
# hands only while the mutex is held, so neither needs a lock of its own.
#
class Monitor
  # CRuby makes the mutex in the allocator, so a subclass whose
  # `initialize` does not call `super` still has one; here it is made on
  # first use. `Mutex.new` is a call, hence a point where another thread
  # may run, so it comes before the test: two threads racing to their
  # first `enter` must end up with the same mutex (the rule for these
  # primitives, see Thread::Mutex in thread.rb).
  private def __mutex
    m = Thread::Mutex.new
    @mutex ||= m
  end

  # A copy is a new monitor nobody holds: CRuby's `dup` runs the
  # allocator, and the lock is not state it copies.
  private def initialize_copy(other)
    super
    @mutex = nil
    @owner = nil
    @count = 0
    self
  end

  # As Thread::Mutex: T_DATA without a `_dump_data`.
  def marshal_dump
    raise TypeError, "no _dump_data is defined for class #{self.class}"
  end

  # CRuby keeps the mutex, the owner and the count in the object's C
  # struct, so its `inspect` shows none of them.
  private def instance_variables_to_inspect = []

  #
  # call-seq:
  #   try_enter -> true or false
  #
  # Attempts to enter exclusive section.  Returns +false+ if lock fails.
  #
  def try_enter
    cur = Fiber.current
    unless @owner.equal?(cur)
      return false unless (@mutex || __mutex).try_lock
      @owner = cur
      @count = 0
    end
    @count += 1
    true
  end

  #
  # call-seq:
  #   enter -> nil
  #
  # Enters exclusive section.
  #
  def enter
    cur = Fiber.current
    unless @owner.equal?(cur)
      # Nothing can interrupt this thread between the mutex being
      # handed over and the owner being recorded: asynchronous interrupts
      # are delivered where a Ruby method or block is entered and on loop
      # back edges, and there is neither on the way. `try_lock` first:
      # it is all of `lock` when nobody holds the mutex.
      (m = @mutex || __mutex).try_lock || m.lock
      @owner = cur
      @count = 0
    end
    @count += 1
    nil
  end

  #
  # call-seq:
  #   exit -> nil
  #
  # Leaves exclusive section.
  #
  def exit
    unless @owner.equal?(Fiber.current)
      raise ThreadError, "current fiber not owner"
    end
    if @count == 1
      # The outermost `exit`. In C this cannot be interrupted half way;
      # here `Mutex#unlock` is a Ruby method, and a `Thread#raise` or a
      # kill delivered on its way in would leave a mutex that is still
      # locked behind a monitor that says nobody owns it: the thread's
      # next `enter` would report "deadlock; recursive locking" against
      # itself, and every other thread would wait for good. So the two
      # go together, and an interrupt that arrives meanwhile is delivered
      # once both are done (`Mutex#synchronize` releases the same way).
      # The two calls that bracket the section are native, so there is
      # no delivery point on the way into it either.
      Thread.__uninterruptible_begin
      begin
        @count = 0
        @owner = nil
        @mutex.unlock
      ensure
        Thread.__uninterruptible_end
      end
    else
      @count -= 1
    end
    nil
  end

  #
  # call-seq:
  #   synchronize { } -> result of the block
  #
  # Enters exclusive section and executes the block.  Leaves the exclusive
  # section automatically when the block exits.  See example under
  # +MonitorMixin+.
  #
  def synchronize
    # A C function's `rb_yield` reports a missing block in other words
    # than a Ruby method's `yield`.
    raise LocalJumpError, "no block given" unless block_given?
    # `enter` and `exit` spelled out, with the current fiber looked up
    # once for both, as monitor_synchronize does.
    cur = Fiber.current
    unless @owner.equal?(cur)
      (m = @mutex || __mutex).try_lock || m.lock
      @owner = cur
      @count = 0
    end
    @count += 1
    begin
      yield
    ensure
      unless @owner.equal?(cur)
        raise ThreadError, "current fiber not owner"
      end
      if @count == 1
        # See #exit. This one must not be a call to a Ruby method that
        # then opens the section: an interrupt delivered on the way into
        # that method would leave this `ensure` with the monitor held.
        Thread.__uninterruptible_begin
        begin
          @count = 0
          @owner = nil
          @mutex.unlock
        ensure
          Thread.__uninterruptible_end
        end
      else
        @count -= 1
      end
    end
  end

  # :stopdoc:

  # Internal methods for MonitorMixin.

  def mon_locked?
    (m = @mutex) ? m.locked? : false
  end

  def mon_check_owner
    unless @owner.equal?(Fiber.current)
      raise ThreadError, "current fiber not owner"
    end
    nil
  end

  def mon_owned?
    (m = @mutex) ? m.locked? && @owner.equal?(Fiber.current) : false
  end

  # Internal method for MonitorMixin::ConditionVariable: wait on `cond`
  # (a Thread::ConditionVariable) with the monitor given up, and take it
  # back with the same count, also when the wait is left by an exception.
  # `cond.wait` returns with the mutex held again either way.
  def wait_for_cond(cond, timeout)
    cur = Fiber.current
    count = @count
    @owner = nil
    @count = 0
    begin
      cond.wait(@mutex || __mutex, timeout) ? true : false
    ensure
      @owner = cur
      @count = count
    end
  end

  # :startdoc:
end

#
# In concurrent programming, a monitor is an object or module intended to be
# used safely by more than one thread. The defining characteristic of a
# monitor is that its methods are executed with mutual exclusion. That is, at
# each point in time, at most one thread may be executing any of its methods.
# This mutual exclusion greatly simplifies reasoning about the implementation
# of monitors compared to reasoning about parallel code that updates a data
# structure.
#
# You can read more about the general principles on the Wikipedia page for
# Monitors[https://en.wikipedia.org/wiki/Monitor_%28synchronization%29].
#
# == Examples
#
# === Simple object.extend
#
#   require 'monitor.rb'
#
#   buf = []
#   buf.extend(MonitorMixin)
#   empty_cond = buf.new_cond
#
#   # consumer
#   Thread.start do
#     loop do
#       buf.synchronize do
#         empty_cond.wait_while { buf.empty? }
#         print buf.shift
#       end
#     end
#   end
#
#   # producer
#   while line = ARGF.gets
#     buf.synchronize do
#       buf.push(line)
#       empty_cond.signal
#     end
#   end
#
# The consumer thread waits for the producer thread to push a line to buf
# while <tt>buf.empty?</tt>. The producer thread (main thread) reads a
# line from ARGF and pushes it into buf then calls <tt>empty_cond.signal</tt>
# to notify the consumer thread of new data.
#
# === Simple Class include
#
#   require 'monitor'
#
#   class SynchronizedArray < Array
#
#     include MonitorMixin
#
#     def initialize(*args)
#       super(*args)
#     end
#
#     alias :old_shift :shift
#     alias :old_unshift :unshift
#
#     def shift(n=1)
#       self.synchronize do
#         self.old_shift(n)
#       end
#     end
#
#     def unshift(item)
#       self.synchronize do
#         self.old_unshift(item)
#       end
#     end
#
#     # other methods ...
#   end
#
# +SynchronizedArray+ implements an Array with synchronized access to items.
# This Class is implemented as subclass of Array which includes the
# MonitorMixin module.
#
module MonitorMixin
  #
  # FIXME: This isn't documented in Nutshell.
  #
  # Since MonitorMixin.new_cond returns a ConditionVariable, and the example
  # above calls while_wait and signal, this class should be documented.
  #
  class ConditionVariable
    #
    # Releases the lock held in the associated monitor and waits; reacquires the lock on wakeup.
    #
    # If +timeout+ is given, this method returns after +timeout+ seconds passed,
    # even if no other thread doesn't signal.
    #
    def wait(timeout = nil)
      @monitor.mon_check_owner
      @monitor.wait_for_cond(@cond, timeout)
    end

    #
    # Calls wait repeatedly while the given block yields a truthy value.
    #
    def wait_while
      while yield
        wait
      end
    end

    #
    # Calls wait repeatedly until the given block yields a truthy value.
    #
    def wait_until
      until yield
        wait
      end
    end

    #
    # Wakes up the first thread in line waiting for this lock.
    #
    def signal
      @monitor.mon_check_owner
      @cond.signal
    end

    #
    # Wakes up all threads waiting for this lock.
    #
    def broadcast
      @monitor.mon_check_owner
      @cond.broadcast
    end

    private

    def initialize(monitor) # :nodoc:
      @monitor = monitor
      @cond = Thread::ConditionVariable.new
    end
  end

  def self.extend_object(obj) # :nodoc:
    super(obj)
    obj.__send__(:mon_initialize)
  end

  #
  # Attempts to enter exclusive section.  Returns +false+ if lock fails.
  #
  def mon_try_enter
    @mon_data.try_enter
  end
  # For backward compatibility
  alias try_mon_enter mon_try_enter

  #
  # Enters exclusive section.
  #
  def mon_enter
    @mon_data.enter
  end

  #
  # Leaves exclusive section.
  #
  def mon_exit
    mon_check_owner
    @mon_data.exit
  end

  #
  # Returns true if this monitor is locked by any thread
  #
  def mon_locked?
    @mon_data.mon_locked?
  end

  #
  # Returns true if this monitor is locked by current thread.
  #
  def mon_owned?
    @mon_data.mon_owned?
  end

  #
  # Enters exclusive section and executes the block.  Leaves the exclusive
  # section automatically when the block exits.  See example under
  # +MonitorMixin+.
  #
  def mon_synchronize(&b)
    @mon_data.synchronize(&b)
  end
  alias synchronize mon_synchronize

  #
  # Creates a new MonitorMixin::ConditionVariable associated with the
  # Monitor object.
  #
  def new_cond
    unless defined?(@mon_data)
      mon_initialize
      @mon_initialized_by_new_cond = true
    end
    return ConditionVariable.new(@mon_data)
  end

  private

  # Use <tt>extend MonitorMixin</tt> or <tt>include MonitorMixin</tt> instead
  # of this constructor.  Have look at the examples above to understand how to
  # use this module.
  def initialize(...)
    super
    mon_initialize
  end

  # Initializes the MonitorMixin after being included in a class or when an
  # object has been extended with the MonitorMixin
  def mon_initialize
    if defined?(@mon_data)
      if defined?(@mon_initialized_by_new_cond)
        return # already initialized.
      elsif @mon_data_owner_object_id == self.object_id
        raise ThreadError, "already initialized"
      end
    end
    @mon_data = ::Monitor.new
    @mon_data_owner_object_id = self.object_id
  end

  # Ensures that the MonitorMixin is owned by the current thread,
  # otherwise raises an exception.
  def mon_check_owner
    @mon_data.mon_check_owner
  end
end

# Use the Monitor class when you want to have a lock object for blocks with
# mutual exclusion.
#
#   require 'monitor'
#
#   lock = Monitor.new
#   lock.synchronize do
#     # exclusive access
#   end
#
class Monitor
  #
  # Creates a new MonitorMixin::ConditionVariable associated with the
  # Monitor object.
  #
  def new_cond
    ::MonitorMixin::ConditionVariable.new(self)
  end

  # for compatibility
  alias try_mon_enter try_enter
  alias mon_try_enter try_enter
  alias mon_enter enter
  alias mon_exit exit
  alias mon_synchronize synchronize
end

# Documentation comments:
#  - All documentation comes from Nutshell.
#  - MonitorMixin.new_cond appears in the example, but is not documented in
#    Nutshell.
#  - All the internals (internal modules Accessible and Initializable, class
#    ConditionVariable) appear in RDoc.  It might be good to hide them, by
#    making them private, or marking them :nodoc:, etc.
#  - RDoc doesn't recognise aliases, so we have mon_synchronize documented, but
#    not synchronize.
#  - mon_owner is in Nutshell, but appears as an accessor in a separate module
#    here, so is hard/impossible to RDoc.  Some other useful accessors
#    (mon_count and some queue stuff) are also in this module, and don't appear
#    directly in the RDoc output.
#  - in short, it may be worth changing the code layout in this file to make the
#    documentation easier
