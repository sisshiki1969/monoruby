# frozen_string_literal: true
#
# Monitor implementation for monoruby.
# In single-threaded mode, most operations are no-ops but
# we maintain correct interface and state tracking.
#

class Monitor
  def initialize
    @owner = nil
    @count = 0
  end

  def enter
    @owner = Thread.current
    @count += 1
    self
  end
  alias mon_enter enter

  def exit
    raise ThreadError, "current thread not owner" if @owner != Thread.current && @count > 0
    if @count > 0
      @count -= 1
      if @count == 0
        @owner = nil
      end
    end
    self
  end
  alias mon_exit exit

  def try_enter
    if @owner == Thread.current
      @count += 1
      true
    elsif @owner.nil?
      @owner = Thread.current
      @count = 1
      true
    else
      false
    end
  end
  alias try_mon_enter try_enter
  alias mon_try_enter try_enter

  def synchronize
    enter
    begin
      yield
    ensure
      exit
    end
  end
  alias mon_synchronize synchronize

  def owned?
    @owner == Thread.current
  end
  alias mon_owned? owned?

  def locked?
    @count > 0
  end
  alias mon_locked? locked?

  def new_cond
    ConditionVariable.new(self)
  end

  # Monitor::ConditionVariable
  class ConditionVariable
    def initialize(monitor)
      @monitor = monitor
    end

    def wait(timeout = nil)
      # In single-threaded mode, this is effectively a no-op
      # since no other thread can signal us
      nil
    end

    def wait_while
      while yield
        wait
      end
    end

    def wait_until
      until yield
        wait
      end
    end

    def signal
      # No-op in single-threaded mode
      nil
    end

    def broadcast
      # No-op in single-threaded mode
      nil
    end
  end
end

# MonitorMixin module - can be included in any class
module MonitorMixin
  def self.included(base)
    base.class_eval do
      alias_method :_mon_original_initialize, :initialize

      define_method(:initialize) do |*args, **kwargs, &block|
        mon_initialize
        if kwargs.empty?
          _mon_original_initialize(*args, &block)
        else
          _mon_original_initialize(*args, **kwargs, &block)
        end
      end
    end
  end

  def mon_initialize
    @mon_data = Monitor.new
  end

  def mon_enter
    @mon_data ||= Monitor.new
    @mon_data.enter
  end

  def mon_exit
    @mon_data.exit if @mon_data
  end

  def mon_try_enter
    @mon_data ||= Monitor.new
    @mon_data.try_enter
  end

  def mon_synchronize(&block)
    @mon_data ||= Monitor.new
    @mon_data.synchronize(&block)
  end
  alias synchronize mon_synchronize

  def mon_owned?
    @mon_data ? @mon_data.owned? : false
  end

  def mon_locked?
    @mon_data ? @mon_data.locked? : false
  end

  def new_cond
    @mon_data ||= Monitor.new
    @mon_data.new_cond
  end

  def self.extend_object(obj)
    super
    obj.mon_initialize
  end
end
