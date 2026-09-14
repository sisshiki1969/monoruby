# Ractor — a sequential-consistency emulation on top of monoruby's green
# threads.
#
# monoruby has no parallel execution and no object-isolation machinery;
# every Ractor here is a Thread with an inbox (`#send` / `Ractor.receive`)
# and an outbox (`Ractor.yield` / `#take`), and the block's return value is
# the thread's value (`#value` / `#take` after completion). Objects are
# passed by reference, never copied or moved; `Ractor.make_shareable`
# deep-freezes as CRuby does, so code that relies on it still gets frozen
# data, but the isolation errors CRuby raises for unshareable captures are
# not reproduced.
#
# What is covered is the API surface a program written for real Ractors
# touches when it just wants its work done: `Ractor.new(*args) { |*args| }`,
# `#value`, `#take`, `#join`, `#send` / `#<<`, `Ractor.receive` /
# `Ractor.recv`, `Ractor.yield`, `Ractor.current` / `.main` / `.main?` /
# `.count`, `Ractor.make_shareable` / `.shareable?`, `#name`, `#inspect`.
class Ractor
  class Error < StandardError; end
  class IsolationError < ArgumentError; end
  class MovedError < Error; end
  class ClosedError < StopIteration; end
  class UnsafeError < Error; end

  # Raised by `#value` / `#take` when the block raised; the original
  # exception is `#cause`, as in CRuby.
  class RemoteError < Error
    attr_reader :ractor

    def initialize(ractor, message = "thrown by remote Ractor.")
      super(message)
      @ractor = ractor
    end
  end

  class MovedObject < BasicObject
    def method_missing(*)
      ::Kernel.raise ::Ractor::MovedError, "can not send any methods to a moved object"
    end

    def respond_to_missing?(*)
      false
    end
  end

  @count = 0
  @main = nil
  @sequence = 0

  class << self
    # Every Ractor other than the main one runs its block on its own
    # (green) thread; the thread's value is the Ractor's value.
    def new(*args, name: nil, &block)
      raise ArgumentError, "must be called with a block" unless block
      unless name.nil? || name.is_a?(String)
        raise TypeError, "no implicit conversion of #{name.class} into String"
      end
      r = allocate
      r.__send__(:__ractor_init, args, name, block)
      r
    end

    def current
      Thread.current.thread_variable_get(:__ractor__) || main
    end

    def main
      @main ||= begin
        r = allocate
        r.__send__(:__ractor_init_main)
        r
      end
    end

    def main?
      current.equal?(main)
    end

    # Live (unfinished) ractors, the main one included.
    def count
      @count + 1
    end

    def receive
      current.__send__(:__receive)
    end
    alias recv receive

    def receive_if(&block)
      raise ArgumentError, "no block given" unless block
      current.__send__(:__receive_if, &block)
    end

    def yield(obj, move: false)
      current.__send__(:__yield, obj)
      nil
    end

    # Deep-freeze `obj` (its instance variables, elements, keys, values and
    # struct members) and return it. With `copy: true` a deep copy is
    # frozen instead and the original is left untouched.
    def make_shareable(obj, copy: false)
      obj = __deep_copy(obj, {}.compare_by_identity) if copy
      __deep_freeze(obj, {}.compare_by_identity)
      obj
    end

    # Immediates, frozen leaves, and objects whose entire reachable graph
    # is frozen. Modules and classes are shareable as in CRuby.
    def shareable?(obj)
      __shareable?(obj, {}.compare_by_identity)
    end

    def select(*ractors, yield_value: nil, move: false)
      raise ArgumentError, "specify at least one ractor or `yield_value`" if ractors.empty?
      loop do
        ractors.each do |r|
          if r.__send__(:__ready?)
            return [r, r.take]
          end
        end
        Thread.pass
      end
    end

    def __next_id
      @sequence += 1
    end

    def __started
      @count += 1
    end

    def __finished
      @count -= 1
    end

    private

    def __deep_freeze(obj, seen)
      return if seen.key?(obj)
      return if obj.is_a?(Module)
      seen[obj] = true
      case obj
      when Array
        obj.each { |e| __deep_freeze(e, seen) }
      when Hash
        obj.each { |k, v| __deep_freeze(k, seen); __deep_freeze(v, seen) }
      when Struct
        obj.each { |e| __deep_freeze(e, seen) }
      when Range
        __deep_freeze(obj.begin, seen)
        __deep_freeze(obj.end, seen)
      end
      if obj.respond_to?(:instance_variables)
        obj.instance_variables.each do |iv|
          __deep_freeze(obj.instance_variable_get(iv), seen)
        end
      end
      obj.freeze unless obj.frozen?
    end

    def __deep_copy(obj, seen)
      return obj if obj.is_a?(Module) || obj.is_a?(Symbol) || obj.is_a?(Numeric) ||
                    obj.nil? || obj == true || obj == false
      return seen[obj] if seen.key?(obj)
      copy = case obj
             when Array
               seen[obj] = obj.class.new
               obj.each { |e| seen[obj] << __deep_copy(e, seen) }
               seen[obj]
             when Hash
               h = seen[obj] = obj.class.new
               h.compare_by_identity if obj.compare_by_identity?
               obj.each { |k, v| h[__deep_copy(k, seen)] = __deep_copy(v, seen) }
               h
             when String
               seen[obj] = obj.dup
             else
               seen[obj] = obj.dup
             end
      if copy.respond_to?(:instance_variables)
        copy.instance_variables.each do |iv|
          copy.instance_variable_set(iv, __deep_copy(copy.instance_variable_get(iv), seen))
        end
      end
      copy
    end

    def __shareable?(obj, seen)
      return true if obj.is_a?(Module) || obj.is_a?(Symbol) || obj.is_a?(Numeric) ||
                     obj.nil? || obj == true || obj == false || obj.is_a?(Ractor)
      return true if seen.key?(obj)
      return false unless obj.frozen?
      seen[obj] = true
      case obj
      when Array, Struct
        return false unless obj.all? { |e| __shareable?(e, seen) }
      when Hash
        return false unless obj.all? { |k, v| __shareable?(k, seen) && __shareable?(v, seen) }
      when Range
        return false unless __shareable?(obj.begin, seen) && __shareable?(obj.end, seen)
      when Proc, Method, UnboundMethod, Binding
        return false
      end
      if obj.respond_to?(:instance_variables)
        obj.instance_variables.each do |iv|
          return false unless __shareable?(obj.instance_variable_get(iv), seen)
        end
      end
      true
    end
  end

  attr_reader :name

  def send(obj, move: false)
    raise ClosedError, "The incoming-port is already closed" if @inbox_closed
    @inbox.push(obj)
    self
  end
  alias << send

  # The next value the Ractor yielded, or its final value once the block
  # has returned. Raises `ClosedError` once the final value was consumed.
  def take
    raise ClosedError, "The outgoing-port is already closed" if @outbox_closed
    v = @outbox.pop
    if v.equal?(@terminator)
      @outbox_closed = true
      __final_value
    else
      v
    end
  end

  # The block's return value; raises `RemoteError` (with the original
  # exception as `cause`) when the block raised.
  def value
    __final_value
  end

  def join
    @thread.join if @thread
    self
  end

  def close_incoming
    was = @inbox_closed
    @inbox_closed = true
    was
  end

  def close_outgoing
    was = @outbox_closed
    @outbox_closed = true
    was
  end

  def alive?
    @thread.nil? ? true : @thread.alive?
  end

  def inspect
    loc = @loc ? " #{@loc}" : ""
    name = @name ? " #{@name}" : ""
    state = @thread.nil? ? "running" : (@thread.alive? ? "running" : "terminated")
    "#<Ractor:##{@id}#{name}#{loc} #{state}>"
  end
  alias to_s inspect

  def [](sym)
    (@locals ||= {})[sym.to_sym]
  end

  def []=(sym, val)
    (@locals ||= {})[sym.to_sym] = val
  end

  private

  def __ractor_init(args, name, block)
    @id = Ractor.__next_id
    @name = name
    @inbox = Thread::Queue.new
    @outbox = Thread::Queue.new
    @terminator = Object.new
    @inbox_closed = false
    @outbox_closed = false
    @loc = block.source_location&.join(":")
    ractor = self
    Ractor.__started
    @thread = Thread.new do
      Thread.current.thread_variable_set(:__ractor__, ractor)
      begin
        block.call(*args)
      ensure
        Ractor.__finished
        @outbox.push(@terminator)
      end
    end
    @thread.report_on_exception = false
  end

  def __ractor_init_main
    @id = Ractor.__next_id
    @name = nil
    @inbox = Thread::Queue.new
    @outbox = Thread::Queue.new
    @terminator = Object.new
    @inbox_closed = false
    @outbox_closed = false
    @thread = nil
  end

  def __final_value
    raise Error, "the main Ractor has no value" if @thread.nil?
    begin
      @thread.value
    rescue Exception => e
      raise RemoteError.new(self), "thrown by remote Ractor.", cause: e
    end
  end

  def __receive
    raise ClosedError, "The incoming-port is already closed" if @inbox_closed && @inbox.empty?
    @inbox.pop
  end

  def __receive_if
    kept = []
    begin
      loop do
        v = @inbox.pop
        return v if yield(v)
        kept << v
      end
    ensure
      kept.each { |v| @inbox.push(v) }
    end
  end

  def __yield(obj)
    raise ClosedError, "The outgoing-port is already closed" if @outbox_closed
    @outbox.push(obj)
  end

  def __ready?
    !@outbox.empty?
  end
end
