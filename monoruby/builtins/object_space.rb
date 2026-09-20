# ObjectSpace for monoruby.
#
# `WeakMap` is a real weak map: its storage is `ObjTy::WEAKMAP`
# (`src/value/rvalue/weakmap.rs`), which the collector never traces, and
# whose pairs it breaks as their halves die. The class, its allocator and
# the primitives below it are defined in `src/builtins/object_space.rs`;
# this file only adds the methods derived from them.
#
# Object iteration is still unsupported: `each_object` answers nothing.
module ObjectSpace
  class WeakMap
    include ::Enumerable if defined?(::Enumerable)

    alias include? key?
    alias member? key?
    alias length size

    # The pairs, as of the moment `each` was called. A block is free to
    # allocate — and so to collect, which breaks pairs — so iteration
    # walks a snapshot rather than the live map.
    def each
      return to_enum(:each) unless block_given?
      e = __entries
      i = 0
      while i < e.size
        yield e[i], e[i + 1]
        i += 2
      end
      self
    end
    alias each_pair each

    def each_key
      return to_enum(:each_key) unless block_given?
      keys.each { |k| yield k }
      self
    end

    def each_value
      return to_enum(:each_value) unless block_given?
      values.each { |v| yield v }
      self
    end

    def inspect
      "#<ObjectSpace::WeakMap:#{format('0x%016x', object_id << 1)} size=#{size}>"
    end

    private :__entries
  end

  # Yield every object on the heap, or every one that is `kind_of?`
  # +klass+, and answer how many there were.
  #
  # The walk itself is `__live_objects`, in Rust: the allocator knows
  # which cells hold an object, since a free one's header is a `next`
  # pointer. What comes back is a *snapshot*, and it has to be — the
  # block is free to allocate, and so to collect, which would move the
  # heap out from under a live walk. Holding the snapshot in an Array
  # also keeps everything in it alive for the duration, which is what
  # makes it safe to yield.
  #
  # The Array is dropped from the enumerable path before yielding
  # begins, so its own entry — which CRuby's walk would not have — is
  # skipped rather than reported.
  def self.each_object(klass = nil)
    return to_enum(:each_object, klass) unless block_given?
    objs = __live_objects(klass)
    count = 0
    i = 0
    n = objs.size
    while i < n
      o = objs[i]
      i += 1
      # The snapshot Array is an object too, and it exists only because
      # of this call.
      next if o.equal?(objs)
      count += 1
      yield o
    end
    count
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
