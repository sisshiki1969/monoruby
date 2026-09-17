# cool.io_ext.rb – monoruby's stand-in for cool.io_ext.so (the cool.io gem)
#
# The cool.io gem's Ruby half (`lib/cool.io/*.rb`: `Loop#run`, `Meta`'s
# `event_callback` / `watcher_delegate`, `IO`, `Socket`, `Listener`,
# `Server`, `AsyncWatcher`, `DNSResolver`) sits on a C extension that
# binds libev: `Coolio::Loop`'s `ev_loop_new` / `run_once` /
# `run_nonblock`, the `Watcher` base class with `IOWatcher`,
# `TimerWatcher` and `StatWatcher`, `Coolio::Buffer` and `Coolio::Utils`.
# The gem loads this file in its place (`cool_require "cool.io_ext"`
# finds it before the `.so`).
#
# The event loop is `IO.select` over the enabled IO watchers with a
# timeout cut to the next timer, the next stat poll and `run_once`'s own
# timeout, on monoruby's green threads (a blocked select parks the calling
# thread). What the extension does that this mirrors:
#
# - The loop's bookkeeping lives in the loop object's `@watchers` (watcher
#   => true) and `@active_watchers` (a count of enabled watchers), which
#   `Loop#run` in the Ruby half reads; `attach` / `detach` / `enable` /
#   `disable` keep them, with the extension's error texts ("not attached
#   to a loop", "already enabled", "already disabled").
# - `run_once(timeout)` waits for at least one event or the timeout and
#   answers the number of events dispatched; with nothing to wait on and
#   no timeout it returns at once, as libev does with no active watchers.
# - An IO watcher that is ready for both directions gets `on_readable`
#   only (the extension checks EV_READ first); a non-repeating timer fires
#   once and stops at the libev level while staying "enabled" in the
#   loop's count; `TimerWatcher#reset` re-arms a repeating timer and stops
#   a one-shot one.
# - A stat watcher polls `File.stat` at its interval (libev's 5 s default
#   for 0) and hands `on_change` two `Struct::StatInfo` values; a missing
#   file is all zeros.
# - `Buffer` is the extension's byte queue: `read` / `read_frame` /
#   `read_from` (non-blocking reads until the IO would block, nil at end
#   of file) / `write_to` (non-blocking writes until the IO would block).
#
# `on_readable`, `on_writable`, `on_timer` and `on_change` are defined on
# the classes themselves because the Ruby half's `event_callback`
# replaces them with `remove_method` + a block-storing version.

module Coolio
  StatInfo = if Struct.const_defined?(:StatInfo)
               Struct::StatInfo
             else
               Struct.new("StatInfo", :mtime, :ctime, :atime, :dev, :ino, :mode, :nlink,
                          :uid, :guid, :rdev, :size, :blksize, :blocks)
             end

  # The loop's C half: watcher sets, the pending event queue and the
  # one-iteration `run_once` / `run_nonblock`.
  class Loop
    DEFAULT_STAT_INTERVAL = 5.0

    def run_once(*args)
      if args.size > 1
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..1)"
      end
      timeout = args[0]
      unless timeout.nil?
        timeout = Coolio.__to_f(timeout)
        raise ArgumentError, "time interval must be positive" if timeout < 0
      end
      __ev_run(timeout, true)
    end

    def run_nonblock
      __ev_run(0.0, false)
    end

    # --- the parts the watchers call -----------------------------------------

    def __ev_io_start(w) # :nodoc:
      @_ev_ios[w] = true
    end

    def __ev_io_stop(w) # :nodoc:
      @_ev_ios.delete(w)
    end

    def __ev_timer_start(w, at) # :nodoc:
      @_ev_timers[w] = at
    end

    def __ev_timer_stop(w) # :nodoc:
      @_ev_timers.delete(w)
    end

    def __ev_timer_active?(w) # :nodoc:
      @_ev_timers.key?(w)
    end

    def __ev_timer_at(w) # :nodoc:
      @_ev_timers[w]
    end

    def __ev_stat_start(w, at) # :nodoc:
      @_ev_stats[w] = at
    end

    def __ev_stat_stop(w) # :nodoc:
      @_ev_stats.delete(w)
    end

    def __ev_now # :nodoc:
      Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    # A watcher that detaches while events are pending (from a callback
    # running in the same iteration, say) must not be dispatched to.
    def __ev_forget(w) # :nodoc:
      @_ev_events.each { |ev| ev[0] = nil if ev[0].equal?(w) } if @_ev_events
      @_ev_dispatching.each { |ev| ev[0] = nil if ev[0].equal?(w) } if @_ev_dispatching
    end

    private

    def ev_loop_new(flags)
      raise RuntimeError, "loop already initialized" if @_ev_initialized
      @_ev_initialized = true
      @_ev_ios = {}
      @_ev_timers = {}
      @_ev_stats = {}
      @_ev_events = []
      @_ev_dispatching = nil
      nil
    end

    # One libev iteration: wait for events (bounded by `timeout` when
    # given), queue them, dispatch them, answer their number.
    def __ev_run(timeout, blocking)
      raise RuntimeError, "loop not initialized" unless @_ev_initialized
      now = __ev_now
      deadline = timeout ? now + timeout + 0.0001 : nil
      loop do
        wait = deadline ? deadline - now : nil
        wait = 0.0 if wait && wait < 0
        wait = 0.0 unless blocking
        @_ev_timers.each_value do |at|
          d = at - now
          d = 0.0 if d < 0
          wait = d if wait.nil? || d < wait
        end
        @_ev_stats.each_value do |at|
          d = at - now
          d = 0.0 if d < 0
          wait = d if wait.nil? || d < wait
        end

        reads = []
        writes = []
        @_ev_ios.each_key do |w|
          io = w.__ev_io
          ev = w.__ev_events
          reads << io if ev == :r || ev == :rw
          writes << io if ev == :w || ev == :rw
        end

        if reads.empty? && writes.empty?
          if wait.nil?
            # nothing to wait for: libev returns at once
            return 0
          end
          sleep(wait) if wait > 0
          ready = nil
        else
          begin
            ready = ::IO.select(reads, writes, nil, wait)
          rescue IOError
            # a watched IO was closed underneath the loop: libev reports
            # the dead fd as an error event and stops watching it
            closed = @_ev_ios.keys.select { |w| w.__ev_io.closed? }
            closed.each do |w|
              @_ev_ios.delete(w)
              @_ev_events << [w, :read]
            end
            ready = nil
          end
        end

        now = __ev_now
        if ready
          rs, ws = ready
          @_ev_ios.each_key do |w|
            io = w.__ev_io
            if rs.include?(io) && (w.__ev_events == :r || w.__ev_events == :rw)
              @_ev_events << [w, :read]
            elsif ws.include?(io) && (w.__ev_events == :w || w.__ev_events == :rw)
              @_ev_events << [w, :write]
            end
          end
        end
        @_ev_timers.to_a.each do |w, at|
          next if at > now
          @_ev_events << [w, :timer]
          rep = w.__ev_repeat
          if rep > 0
            at += rep
            at = now + rep if at < now
            @_ev_timers[w] = at
          else
            @_ev_timers.delete(w)
          end
        end
        @_ev_stats.to_a.each do |w, at|
          next if at > now
          @_ev_stats[w] = now + w.__ev_interval
          @_ev_events << [w, :stat] if w.__ev_stat_poll
        end

        break unless @_ev_events.empty?
        break unless blocking
        break if deadline && now >= deadline
      end

      events = @_ev_events
      @_ev_events = []
      @_ev_dispatching = events
      n = events.size
      begin
        events.each do |w, kind|
          next if w.nil?
          w.__ev_dispatch(kind)
        end
      ensure
        @_ev_dispatching = nil
      end
      n
    end
  end

  class Watcher
    def initialize
      raise RuntimeError, "watcher base class should not be initialized directly"
    end

    def attach(loop)
      @_ev_enabled = true
      watchers = loop.instance_variable_get(:@watchers)
      if watchers.nil?
        watchers = {}
        loop.instance_variable_set(:@watchers, watchers)
      end
      watchers[self] = true
      active = loop.instance_variable_get(:@active_watchers)
      loop.instance_variable_set(:@active_watchers, active.nil? ? 1 : active + 1)
      self
    end

    def detach
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      watchers = @_ev_loop.instance_variable_get(:@watchers)
      watchers.delete(self) if watchers
      if @_ev_enabled
        @_ev_loop.instance_variable_set(:@active_watchers,
                                        @_ev_loop.instance_variable_get(:@active_watchers) - 1)
      end
      @_ev_enabled = false
      @_ev_loop.__ev_forget(self)
      @_ev_loop = nil
      self
    end

    def enable
      raise RuntimeError, "already enabled" if @_ev_enabled
      @_ev_enabled = true
      @_ev_loop.instance_variable_set(:@active_watchers,
                                      @_ev_loop.instance_variable_get(:@active_watchers) + 1)
      self
    end

    def disable
      raise RuntimeError, "already disabled" unless @_ev_enabled
      @_ev_enabled = false
      @_ev_loop.instance_variable_set(:@active_watchers,
                                      @_ev_loop.instance_variable_get(:@active_watchers) - 1)
      self
    end

    def evloop
      @_ev_loop
    end

    # The extension returns the C comparison's int as a VALUE, which is
    # `false` when detached and the Fixnum 0 (truthy) when attached; the
    # Ruby half only tests it, so this keeps the quirk.
    def attached?
      @_ev_loop.nil? ? false : 0
    end

    def enabled?
      @_ev_enabled ? true : false
    end

    private

    def __ev_check_loop(loop)
      unless loop.is_a?(Coolio::Loop)
        raise ArgumentError, "expected loop to be an instance of Coolio::Loop, not #{loop.inspect}"
      end
    end

    # Attaching an attached watcher: the extension stops its libev watcher
    # and then calls `rb_call_super(0, 0)` from inside `attach`, which is
    # `Watcher#attach` with no loop — an ArgumentError, with the watcher
    # left stopped but still bound to its old loop.
    def __ev_reattach_error
      yield
      raise ArgumentError, "wrong number of arguments (given 0, expected 1)"
    end
  end

  class IOWatcher < Watcher
    def initialize(*args)
      unless (1..2).cover?(args.size)
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1..2)"
      end
      io, flags = args
      flags_str = flags.nil? ? "r" : String(flags)
      @_ev_events = case flags_str
                    when "r" then :r
                    when "w" then :w
                    when "rw" then :rw
                    else
                      raise ArgumentError, "invalid event type: '#{flags_str}' (must be 'r', 'w', or 'rw')"
                    end
      @_ev_io = Coolio.__to_io(io)
      @_ev_loop = nil
      @_ev_enabled = false
    end

    def attach(loop)
      __ev_check_loop(loop)
      __ev_reattach_error { @_ev_loop.__ev_io_stop(self) } if @_ev_loop
      @_ev_loop = loop
      loop.__ev_io_start(self)
      super
    end

    def detach
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      @_ev_loop.__ev_io_stop(self)
      super
    end

    def enable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      @_ev_loop.__ev_io_start(self)
      self
    end

    def disable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      @_ev_loop.__ev_io_stop(self)
      self
    end

    def on_readable
      nil
    end

    def on_writable
      nil
    end

    def __ev_io # :nodoc:
      @_ev_io
    end

    def __ev_events # :nodoc:
      @_ev_events
    end

    def __ev_dispatch(kind) # :nodoc:
      case kind
      when :read then on_readable
      when :write then on_writable
      else raise RuntimeError, "unknown revents value for ev_io: #{kind}"
      end
    end
  end

  class TimerWatcher < Watcher
    def initialize(*args)
      unless (1..2).cover?(args.size)
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1..2)"
      end
      interval, repeating = args
      interval = Coolio.__to_f(interval)
      @interval = interval
      @repeating = repeating
      @_ev_loop = nil
      @_ev_enabled = false
      @_ev_remaining = interval
    end

    def attach(loop)
      __ev_check_loop(loop)
      __ev_reattach_error { __ev_stop } if @_ev_loop
      @_ev_loop = loop
      interval = Coolio.__to_f(@interval)
      @_ev_remaining = interval
      loop.__ev_timer_start(self, loop.__ev_now + interval)
      super
    end

    def detach
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      __ev_stop
      super
    end

    def enable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      @_ev_loop.__ev_timer_start(self, @_ev_loop.__ev_now + @_ev_remaining)
      self
    end

    def disable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      __ev_stop
      self
    end

    # libev's `ev_timer_again`: a repeating timer restarts from now, a
    # one-shot timer stops.
    def reset
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      if __ev_repeat > 0
        @_ev_loop.__ev_timer_start(self, @_ev_loop.__ev_now + __ev_repeat)
      else
        @_ev_loop.__ev_timer_stop(self)
      end
      self
    end

    def on_timer
      nil
    end

    def __ev_repeat # :nodoc:
      @repeating == true ? Coolio.__to_f(@interval) : 0.0
    end

    def __ev_dispatch(kind) # :nodoc:
      raise RuntimeError, "unknown revents value for ev_timer: #{kind}" unless kind == :timer
      on_timer
    end

    private

    def __ev_stop
      if @_ev_loop.__ev_timer_active?(self)
        remaining = @_ev_loop.__ev_timer_at(self) - @_ev_loop.__ev_now
        @_ev_remaining = remaining < 0 ? 0.0 : remaining
        @_ev_loop.__ev_timer_stop(self)
      end
    end
  end

  class StatWatcher < Watcher
    def initialize(*args)
      unless (1..2).cover?(args.size)
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1..2)"
      end
      path, interval = args
      interval = Coolio.__to_f(interval) unless interval.nil?
      @path = String(path)
      @_ev_interval = interval.nil? || interval == 0 ? Loop::DEFAULT_STAT_INTERVAL : interval
      @_ev_loop = nil
      @_ev_enabled = false
      @_ev_prev = nil
      @_ev_attr = nil
    end

    def attach(loop)
      __ev_check_loop(loop)
      __ev_reattach_error { @_ev_loop.__ev_stat_stop(self) } if @_ev_loop
      @_ev_loop = loop
      @_ev_attr = __ev_stat_now
      @_ev_prev = @_ev_attr
      loop.__ev_stat_start(self, loop.__ev_now + @_ev_interval)
      super
    end

    def detach
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      @_ev_loop.__ev_stat_stop(self)
      super
    end

    def enable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      @_ev_loop.__ev_stat_start(self, @_ev_loop.__ev_now + @_ev_interval)
      self
    end

    def disable
      raise RuntimeError, "not attached to a loop" if @_ev_loop.nil?
      super
      @_ev_loop.__ev_stat_stop(self)
      self
    end

    def on_change(previous, current)
      nil
    end

    def path
      @path
    end

    def __ev_interval # :nodoc:
      @_ev_interval
    end

    # Re-stat the path; true when something changed.
    def __ev_stat_poll # :nodoc:
      cur = __ev_stat_now
      if cur == @_ev_attr
        false
      else
        @_ev_prev = @_ev_attr
        @_ev_attr = cur
        true
      end
    end

    def __ev_dispatch(kind) # :nodoc:
      on_change(@_ev_prev, @_ev_attr)
    end

    private

    def __ev_stat_now
      st = File.stat(@path)
      StatInfo.new(Time.at(st.mtime.to_i), Time.at(st.ctime.to_i), Time.at(st.atime.to_i),
                   st.dev, st.ino, st.mode, st.nlink, st.uid, st.gid, st.rdev, st.size,
                   st.blksize || 0, st.blocks || 0)
    rescue SystemCallError
      StatInfo.new(Time.at(0), Time.at(0), Time.at(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    end
  end

  # A byte queue in front of a non-blocking IO.
  class Buffer
    MAX_SIZE = 0x40000000
    DEFAULT_NODE_SIZE = 16384

    @default_node_size = DEFAULT_NODE_SIZE

    class << self
      def default_node_size
        @default_node_size
      end

      def default_node_size=(size)
        size = Integer(size)
        raise ArgumentError, "invalid buffer size" if size < 1
        @default_node_size = size
      end
    end

    def initialize(*args)
      if args.size > 1
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..1)"
      end
      @node_size = args.empty? ? Buffer.default_node_size : Integer(args[0])
      @s = "".b
    end

    def clear
      @s = "".b
      nil
    end

    def size
      @s.bytesize
    end

    def empty?
      @s.empty?
    end

    def append(data)
      data = Coolio.__to_str(data)
      @s << data.b
      data
    end
    alias << append
    alias write append

    def prepend(data)
      data = Coolio.__to_str(data)
      @s = data.b + @s
      data
    end

    def read(*args)
      if args.size > 1
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..1)"
      end
      if args.empty?
        length = @s.bytesize
      else
        length = Integer(args[0])
        raise ArgumentError, "length must be greater than zero" if length < 1
        length = @s.bytesize if length > @s.bytesize
      end
      return "".b if @s.empty?
      out = @s.byteslice(0, length)
      @s = @s.byteslice(length, @s.bytesize - length) || "".b
      out
    end

    # Move bytes up to and including the first `mark` into `str`; true
    # when the mark was found.
    def read_frame(str, mark)
      mark = Integer(mark) & 0xff
      idx = @s.index(mark.chr(Encoding::BINARY))
      if idx
        str << @s.byteslice(0, idx + 1)
        @s = @s.byteslice(idx + 1, @s.bytesize - idx - 1) || "".b
        true
      else
        str << @s
        @s = "".b
        false
      end
    end

    def to_str
      @s.dup
    end

    # Non-blocking reads until the IO would block; the number of bytes
    # read, or nil at end of file.
    def read_from(io)
      io = Coolio.__to_io(io)
      total = 0
      loop do
        chunk = io.read_nonblock(@node_size, exception: false)
        return nil if chunk.nil?
        return total if chunk == :wait_readable
        @s << chunk.b
        total += chunk.bytesize
        return total if chunk.bytesize < @node_size
      end
    end

    # Non-blocking writes until the IO would block; the number of bytes
    # written.
    def write_to(io)
      io = Coolio.__to_io(io)
      total = 0
      until @s.empty?
        n = io.write_nonblock(@s, exception: false)
        return total if n == :wait_writable
        @s = @s.byteslice(n, @s.bytesize - n) || "".b
        total += n
      end
      total
    end
  end

  module Utils
    def self.ncpus
      if File.readable?("/proc/cpuinfo")
        File.foreach("/proc/cpuinfo").count { |l| l.start_with?("processor") }
      else
        require "etc"
        Etc.nprocessors
      end
    end

    def self.maxfds
      Process.getrlimit(:NOFILE)[0]
    end

    def self.maxfds=(max)
      Process.setrlimit(:NOFILE, Integer(max), Process.getrlimit(:NOFILE)[1])
      max
    end
  end

  # `rb_convert_type` with its messages (`Coolio::IO` shadows `IO` inside
  # this module, hence `::IO`).
  def self.__conv_name(v) # :nodoc:
    case v
    when nil then "nil"
    when true then "true"
    when false then "false"
    else v.class.name
    end
  end

  def self.__to_f(v) # :nodoc:
    return v if v.is_a?(Float)
    raise TypeError, "can't convert #{__conv_name(v)} into Float" unless v.respond_to?(:to_f)
    f = v.to_f
    raise TypeError, "can't convert #{v.class} to Float (#{v.class}#to_f gives #{f.class})" unless f.is_a?(Float)
    f
  end

  def self.__to_str(v) # :nodoc:
    return v if v.is_a?(String)
    raise TypeError, "no implicit conversion of #{__conv_name(v)} into String" unless v.respond_to?(:to_str)
    v.to_str
  end

  def self.__to_io(v) # :nodoc:
    return v if v.is_a?(::IO)
    raise TypeError, "no implicit conversion of #{__conv_name(v)} into IO" unless v.respond_to?(:to_io)
    io = v.to_io
    raise TypeError, "can't convert #{v.class} to IO (#{v.class}#to_io gives #{io.class})" unless io.is_a?(::IO)
    io
  end
end
