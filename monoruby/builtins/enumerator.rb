# monoruby-hidden-ivars: true

class Enumerator
  include Enumerable
end

# The remaining aliases need their *original* defined on the class, because
# CRuby keeps the original there too (the strict identity check compares the
# owner). monoruby inherits them, so we re-root the original on the class (a
# `super`-forwarding stub preserves behaviour) and then alias.
#
# NOT done here: Complex#quo == Complex#/. CRuby keeps both on Complex as one
# method; in monoruby `quo` (Rational-component result) and the inherited
# Numeric#/ (and the `/` *operator*, which has a separate fast path that is
# itself buggy — e.g. `Complex(1,2) / 2` => `(0+1i)`) are genuinely different
# implementations, so aliasing them changes results. Tracked as an issue.
# (Thread#to_s / #inspect are defined in startup.rb in CRuby's
# `#<Thread:0xADDR status>` format — no override needed here.)

class Enumerator
  # `e + other` — an `Enumerator::Chain` walking `self` then `other`.
  def +(other)
    Enumerator::Chain.new(self, other)
  end

  # `Enumerator.produce(initial = nil) { |prev| ... }` — an enumerator
  # that yields `initial` (or, when omitted, the first block result) and
  # then repeatedly the block applied to the previous value. Infinite
  # unless the block raises StopIteration.
  def self.produce(*init, **kw, &block)
    raise ArgumentError, "no block given" unless block
    if init.size > 1
      raise ArgumentError, "wrong number of arguments (given #{init.size}, expected 0..1)"
    end
    unknown = kw.keys.reject { |k| k == :size }
    unless unknown.empty?
      raise ArgumentError, "unknown keywords: #{unknown.map { |k| k.inspect }.join(', ')}"
    end
    size = kw.key?(:size) ? kw[:size] : Float::INFINITY
    has_init = !init.empty?
    first = init[0]
    Enumerator.new(size) do |y|
      value = has_init ? first : block.call(nil)
      # `loop` swallows the StopIteration the block may raise, which is
      # how CRuby's `producer_each` terminates.
      loop do
        y << value
        value = block.call(value)
      end
    end
  end

  # The Cartesian product of the given enumerables, as a lazily-iterated
  # `Enumerator::Product`. With a block, iterates immediately and returns
  # nil.
  def self.product(*enums, **kw, &block)
    unless kw.empty?
      raise ArgumentError, "unknown keywords: #{kw.keys.map { |k| k.inspect }.join(', ')}"
    end
    product = Enumerator::Product.new(*enums)
    return product unless block
    product.each(&block)
    nil
  end

  # `Enumerator::Product.new(a, b, ...)` enumerates the Cartesian product
  # of its arguments, yielding one Array per combination, rightmost
  # element varying fastest. Members are walked with `#each_entry` (as in
  # CRuby), so multi-value yields arrive gathered and an argument that
  # does not respond to it raises NoMethodError at iteration time — never
  # at construction time.
  #
  # It subclasses Enumerator and initializes the inherited payload to
  # "call `#__product_each` on myself", so `#next` / `#peek` work through
  # the usual external-iteration machinery.
  class Product < Enumerator
    def initialize(*enums)
      @__enums = enums
      __enum_init_method__(self, :__product_each, [], -> { size })
      self
    end
    private :initialize

    private def initialize_copy(other)
      # Self-copy is a no-op — checked before the frozen test, since
      # CRuby allows `frozen.send(:initialize_copy, frozen)`.
      return self if other.equal?(self)
      if frozen?
        raise FrozenError, "can't modify frozen #{self.class}"
      end
      unless other.instance_of?(self.class)
        raise TypeError, "initialize_copy should take same class object"
      end
      enums = other.__send__(:__product_enums)
      raise ArgumentError, "uninitialized product" if enums.nil?
      @__enums = enums
      __enum_init_method__(self, :__product_each, [], -> { size })
      self
    end

    def each(&block)
      return to_enum(:each) { size } unless block
      enums = @__enums
      raise ArgumentError, "uninitialized product" if enums.nil?
      __product_each_i(enums, 0, [], &block)
      self
    end

    # The members, for `initialize_copy` to take from the original.
    private def __product_enums
      @__enums
    end

    # `each`'s inherited-payload entry point (see the class comment).
    private def __product_each(&block)
      each(&block)
    end

    private def __product_each_i(enums, i, acc, &block)
      if i == enums.size
        block.call(acc.dup)
      else
        enums[i].each_entry do |e|
          acc.push(e)
          __product_each_i(enums, i + 1, acc, &block)
          acc.pop
        end
      end
    end

    # Product of the member sizes. A zero anywhere wins outright (the
    # product is empty however infinite the other members are); after
    # that, a nil or infinite member propagates, and a member whose size
    # is neither an Integer nor infinite (NaN, a Symbol, 1.0, …) makes
    # the whole size unknown.
    def size
      enums = @__enums
      return nil if enums.nil?
      sizes = []
      enums.each do |e|
        s = __check_funcall(e, :size, nil)
        return 0 if s == 0
        sizes << s
      end
      total = 1
      sizes.each do |s|
        return s if s.nil?
        return s if s.is_a?(Float) && s.infinite?
        return nil unless s.is_a?(Integer)
        total *= s
      end
      total
    end

    def rewind
      enums = @__enums
      return self if enums.nil?
      enums.each { |e| __check_funcall(e, :rewind, nil) }
      self
    end

    def inspect
      enums = @__enums
      return __sprintf_repr("#<#{self.class}: uninitialized>") if enums.nil?
      guard = (Thread.current[:__product_inspect] ||= [])
      return __sprintf_repr("#<#{self.class}: ...>") if guard.include?(object_id)
      guard.push(object_id)
      begin
        __sprintf_repr("#<#{self.class}: #{enums.inspect}>")
      ensure
        guard.pop
      end
    end
    alias to_s inspect
  end
end

class Enumerator
  # `e.with_index(offset = 0) { |x, i| … }` — CRuby's
  # `enumerator_with_index`: the source runs with a block that packs what
  # it yields into one value and hands it on with the running index.
  #
  # The general form of that is native (`__with_index`: the source is
  # called with an adapter block, which costs two trips through the
  # interpreter's generic call paths per element). When the source is a
  # builtin iterator written in Ruby that yields one value at a time
  # (`__direct_source`: `Array#each`, `Array#map`, an arithmetic
  # sequence's `each`), it is called here instead, with a block of this
  # method's own — a call the JIT inlines together with both blocks, so
  # `ary.each.with_index { }` compiles to the loop `ary.each_with_index { }`
  # is (#2020).
  def with_index(offset = (omitted = true; nil), &block)
    unless block_given?
      # The Enumerator records the argument as given, and only when one
      # was given; it is converted when the block form runs (#2034).
      return omitted ? to_enum(:with_index) { size } : to_enum(:with_index, offset) { size }
    end
    # `NIL_P(memo) ? 0 : rb_to_int(memo)`
    i = offset.nil? ? 0 : __to_int(offset)
    # 1: `Array#each`, 2: `Array#map`, 3: this sequence's `each`
    # (`DirectSource` in src/builtins/enumerator.rs).
    kind = __direct_source
    if kind == 1
      __source_receiver.each do |x|
        yield x, i
        i += 1
      end
    elsif kind == 2
      __source_receiver.map do |x|
        r = yield x, i
        i += 1
        r
      end
    elsif kind == 3
      each do |x|
        yield x, i
        i += 1
      end
      # What the call that made the sequence answers with a block:
      # `1.step(7, 3) { }` is 1.
      (r = @__receiver).nil? ? self : r
    else
      __with_index(i, &block)
    end
  end

  # CRuby roots `each_with_index` on Enumerator as `with_index` with no
  # offset (`enumerator_each_with_index`), so it returns the *source's*
  # return value and rejects arguments — unlike
  # `Enumerable#each_with_index`, which the plain inheritance would
  # otherwise supply.
  def each_with_index(&block)
    # Not a bare delegation: `with_index` would build the Enumerator and
    # record *its* name. CRuby answers `#<Enumerator: …:each_with_index>`.
    return to_enum(:each_with_index) { size } unless block_given?
    # `with_index(0, &block)`, spelled out: handing the block on would
    # put a call the JIT does not inline between it and the loop.
    i = 0
    kind = __direct_source
    if kind == 1
      __source_receiver.each do |x|
        yield x, i
        i += 1
      end
    elsif kind == 2
      __source_receiver.map do |x|
        r = yield x, i
        i += 1
        r
      end
    elsif kind == 3
      each do |x|
        yield x, i
        i += 1
      end
      (r = @__receiver).nil? ? self : r
    else
      __with_index(0, &block)
    end
  end

  # `e.with_object(memo) { |x, memo| … }` — as `with_index`, the memo in
  # place of the index, and the memo is what it answers.
  def with_object(memo, &block)
    return to_enum(__callee__, memo) { size } unless block_given?
    kind = __direct_source
    if kind == 1
      __source_receiver.each { |x| yield x, memo }
    elsif kind == 2
      __source_receiver.map { |x| yield x, memo }
    elsif kind == 3
      each { |x| yield x, memo }
    else
      __with_object(memo, &block)
    end
    memo
  end
  # One method under two names, as in CRuby (overrides Enumerable's).
  alias each_with_object with_object

  class Yielder
    # A C struct in CRuby, with no `_dump_data`: `Marshal.dump` refuses it.
    @__no_dump_data = true

    # `obj.each(&yielder)` — CRuby's `yielder_to_proc` wraps the yielder
    # so it can stand in for a block.
    def to_proc
      me = self
      proc { |*args| me.yield(*args) }
    end
  end
end
