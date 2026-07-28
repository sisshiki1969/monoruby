# Minimal ObjectSpace stub for monoruby.
#
# monoruby has no support for weak references or general object iteration.
# Provide just enough surface area for libraries that defensively reference
# ObjectSpace constants (ActiveSupport::DescendantsTracker, ConnectionPool,
# weakref) — `WeakMap` is implemented as a strong-referenced hash so keys
# are never GCed while held by the map. That is semantically weaker than
# CRuby but correct enough for class loading and most non-GC-sensitive use.
module ObjectSpace
  class WeakMap
    include ::Enumerable if defined?(::Enumerable)

    def initialize
      @map = {}
    end

    def [](key)
      @map[key.object_id]&.first
    end

    def []=(key, value)
      @map[key.object_id] = [value, key]
      value
    end

    def key?(key)
      @map.key?(key.object_id)
    end
    alias include? key?
    alias member? key?

    def delete(key)
      pair = @map.delete(key.object_id)
      pair ? pair.first : nil
    end

    def keys
      @map.values.map { |pair| pair[1] }
    end

    def values
      @map.values.map { |pair| pair[0] }
    end

    def each
      return to_enum(:each) unless block_given?
      @map.each_value { |pair| yield pair[1], pair[0] }
      self
    end
    alias each_pair each

    def each_key
      return to_enum(:each_key) unless block_given?
      @map.each_value { |pair| yield pair[1] }
      self
    end

    def each_value
      return to_enum(:each_value) unless block_given?
      @map.each_value { |pair| yield pair[0] }
      self
    end

    def size
      @map.size
    end
    alias length size

    def inspect
      "#<ObjectSpace::WeakMap:#{format('0x%016x', object_id << 1)} size=#{size}>"
    end
  end

  def self.each_object(klass = nil)
    return to_enum(:each_object, klass) unless block_given?
    0
  end

  # Register a finalizer for +obj+. The finalizer (a callable or block,
  # invoked with the object's id) is run at program termination. monoruby
  # never runs finalizers asynchronously at GC time, which the spec
  # explicitly permits. The actual registry lives in the runtime; the
  # private +__register_finalizer+ primitive records the pair.
  def self.define_finalizer(obj, *args, &block)
    callable = block || args[0]
    if callable.nil?
      raise ArgumentError, "wrong number of arguments (given 1, expected 2)"
    end
    unless callable.respond_to?(:call)
      raise ArgumentError, "no _id2ref or finalizer is given; must respond to #call"
    end
    # The primitive returns the effective callable: the one already
    # registered when an equal finalizer was given before, else +callable+.
    [0, __register_finalizer(obj, callable)]
  end

  def self.undefine_finalizer(obj)
    __unregister_finalizer(obj)
  end

  def self.garbage_collect(**opts)
    GC.start(**opts)
  end

  def self._id2ref(id)
    raise RangeError, "0x#{id.to_s(16)} is not id value"
  end

  def self.count_objects(result_hash = {})
    result_hash[:TOTAL] = 0
    result_hash[:FREE] = 0
    result_hash
  end
end
