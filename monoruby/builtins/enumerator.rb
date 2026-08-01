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
class Enumerator
  # identical to with_object; re-root on Enumerator (overrides Enumerable's).
  alias each_with_object with_object
end
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
      @enums = enums
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
      enums = other.instance_variable_get(:@enums)
      raise ArgumentError, "uninitialized product" if enums.nil?
      @enums = enums
      __enum_init_method__(self, :__product_each, [], -> { size })
      self
    end

    def each(&block)
      return to_enum(:each) { size } unless block
      enums = @enums
      raise ArgumentError, "uninitialized product" if enums.nil?
      __product_each_i(enums, 0, [], &block)
      self
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
      enums = @enums
      return nil if enums.nil?
      sizes = []
      enums.each do |e|
        s = e.respond_to?(:size) ? e.size : nil
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
      enums = @enums
      return self if enums.nil?
      enums.each { |e| e.rewind if e.respond_to?(:rewind) }
      self
    end

    def inspect
      enums = @enums
      return "#<#{self.class}: uninitialized>" if enums.nil?
      guard = (Thread.current[:__product_inspect] ||= [])
      return "#<#{self.class}: ...>" if guard.include?(object_id)
      guard.push(object_id)
      begin
        "#<#{self.class}: #{enums.inspect}>"
      ensure
        guard.pop
      end
    end
    alias to_s inspect
  end
end

class Enumerator
  # CRuby roots `each_with_index` on Enumerator as `with_index` with no
  # offset (`enumerator_with_index`), so it returns the *source's* return
  # value and rejects arguments — unlike `Enumerable#each_with_index`,
  # which the plain inheritance would otherwise supply.
  def each_with_index(&block)
    with_index(0, &block)
  end

  class Yielder
    # `obj.each(&yielder)` — CRuby's `yielder_to_proc` wraps the yielder
    # so it can stand in for a block.
    def to_proc
      me = self
      proc { |*args| me.yield(*args) }
    end
  end
end
