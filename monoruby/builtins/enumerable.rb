module Enumerable
  # Iterate `self.each`, gathering multi-value yields into a single
  # Array per iteration. CRuby's rule (block-arity-1 semantics): a
  # one-arg block called on a multi-value yield receives all values
  # as an Array; a one-value yield (Array or scalar) is passed
  # through unwrapped.
  #
  # `yield 1`        → 1
  # `yield [2]`      → [2]
  # `yield 3, 4`     → [3, 4]
  # `yield 5, 6, 7`  → [5, 6, 7]
  # `yield []`       → []
  # `yield nil`      → nil
  #
  # Used by every Enumerable method whose CRuby contract is "treats
  # multi-yields as a single gathered element" — `inject`, `map`,
  # `find`, `filter`, etc. Methods that consume the multi-yield via
  # an explicit multi-arity block (`zip`, `each_with_object`) still
  # use `self.each` directly.
  private def __gather_each
    return self.to_enum(:__gather_each) unless block_given?
    self.each do |*__ys|
      yield(__ys.size == 1 ? __ys[0] : __ys)
    end
    self
  end

  # `inject` (a.k.a. `reduce`) accepts five argument shapes:
  #   inject() { |acc, x| ... }            # first element seeds acc
  #   inject(init) { |acc, x| ... }        # `init` seeds acc
  #   inject(sym)                          # acc.send(sym, x) per step
  #   inject(init, sym)                    # `init` seeds; symbol form
  #   inject(str_or_to_str)                # same as sym; `to_str` coerced
  # Disambiguation of single-arg call: if a block is given, the arg is
  # `init` regardless of its type — matches CRuby's
  # `12.times.reduce("ン") { |c| c.succ }`, where `"ン"` seeds the
  # accumulator and is *not* treated as a method name.
  def inject(*args)
    case args.size
    when 0
      raise ArgumentError, "no block given" unless block_given?
      init = nil
      sym = nil
    when 1
      a = args[0]
      if block_given?
        init = a
        sym = nil
      elsif a.is_a?(Symbol) || a.is_a?(String) || a.respond_to?(:to_str)
        a = a.to_str if !a.is_a?(Symbol) && !a.is_a?(String)
        raise TypeError, "#{args[0].inspect} is not a symbol nor a string" if a.nil?
        init = nil
        sym = a.to_sym
      else
        raise ArgumentError, "no block given"
      end
    when 2
      init = args[0]
      a = args[1]
      a = a.to_str if !a.is_a?(Symbol) && !a.is_a?(String) && a.respond_to?(:to_str)
      raise TypeError, "#{args[1].inspect} is not a symbol nor a string" unless a.is_a?(Symbol) || a.is_a?(String)
      sym = a.to_sym
    else
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0..2)"
    end

    acc = init
    if sym
      if init.nil?
        first = true
        __gather_each do |x|
          if first
            acc = x
            first = false
          else
            acc = acc.send(sym, x)
          end
        end
      else
        __gather_each { |x| acc = acc.send(sym, x) }
      end
    else
      if init.nil?
        first = true
        __gather_each do |x|
          if first
            acc = x
            first = false
          else
            acc = yield(acc, x)
          end
        end
      else
        __gather_each { |x| acc = yield(acc, x) }
      end
    end
    acc
  end
  alias reduce inject

  def each_slice(n)
    if n == 0 || n < 0
      raise ArgumentError, "invalid slice size"
    end
    return self.to_enum(:each_slice, n) unless block_given?
    slice = []
    __gather_each do |x|
      slice << x
      if slice.size == n
        yield slice
        slice = []
      end
    end
    yield slice if !slice.empty?
    self
  end

  def each_with_index(*args)
    return self.to_enum(:each_with_index, *args) unless block_given?
    i = 0
    __gather_each do |x|
      yield x, i
      i += 1
    end
    self
  end

  def each_with_object(obj)
    return self.to_enum(:each_with_object) unless block_given?
    __gather_each do |x|
      yield x, obj
    end
    obj
  end

  def map
    return self.to_enum(:map) unless block_given?
    res = []
    __gather_each do |x|
      res << yield(x)
    end
    res
  end
  alias collect map

  def find(ifnone = nil)
    return self.to_enum(:find) unless block_given?
    __gather_each do |x|
      if yield(x)
        return x
      end
    end
    if ifnone.nil?
      nil
    else
      ifnone.call
    end
  end
  alias detect find

  def find_index(*args)
    if args.empty?
      return self.to_enum(:find_index) unless block_given?
      i = 0
      __gather_each do |x|
        return i if yield(x)
        i += 1
      end
      nil
    else
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1)" if args.size > 1
      # When given a value, block is ignored (matches CRuby) — search by ==.
      target = args[0]
      i = 0
      __gather_each do |x|
        return i if x == target
        i += 1
      end
      nil
    end
  end

  def filter
    return self.to_enum(:filter) unless block_given?
    res = []
    __gather_each do |x|
      if yield(x)
        res << x
      end
    end
    res
  end
  alias select filter
  alias find_all filter

  def filter_map
    return self.to_enum(:filter_map) unless block_given?
    res = []
    __gather_each do |x|
      y = yield(x)
      res << y if y
    end
    res
  end

  def take_while
    return self.to_enum(:take_while) unless block_given?
    res = []
    __gather_each do |x|
      break unless yield(x)
      res << x
    end
    res
  end

  def drop_while
    return self.to_enum(:drop_while) unless block_given?
    res = []
    dropping = true
    __gather_each do |x|
      if dropping
        next if yield(x)
        dropping = false
      end
      res << x
    end
    res
  end

  def take(n)
    n = n.to_int if !n.is_a?(Integer) && n.respond_to?(:to_int)
    unless n.is_a?(Integer)
      raise TypeError, "no implicit conversion of #{n.class} into Integer"
    end
    raise ArgumentError, "attempt to take negative size" if n < 0
    res = []
    return res if n == 0
    __gather_each do |x|
      res << x
      break if res.size >= n
    end
    res
  end

  def drop(n)
    n = n.to_int if !n.is_a?(Integer) && n.respond_to?(:to_int)
    unless n.is_a?(Integer)
      raise TypeError, "no implicit conversion of #{n.class} into Integer"
    end
    raise ArgumentError, "attempt to drop negative size" if n < 0
    res = []
    i = 0
    __gather_each do |x|
      res << x if i >= n
      i += 1
    end
    res
  end

  def any?(*pattern)
    if !pattern.empty?
      if pattern.size != 1
        raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
      end
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      __gather_each do |x|
        return true if pat === x
      end
    elsif block_given?
      __gather_each do |x|
        return true if yield(x)
      end
    else
      __gather_each do |x|
        return true if x
      end
    end
    false
  end

  def none?(*pattern)
    if !pattern.empty?
      if pattern.size != 1
        raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
      end
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      __gather_each do |x|
        return false if pat === x
      end
    elsif block_given?
      __gather_each do |x|
        return false if yield(x)
      end
    else
      __gather_each do |x|
        return false if x
      end
    end
    true
  end

  def one?(*pattern)
    n = 0
    if !pattern.empty?
      if pattern.size != 1
        raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
      end
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      __gather_each do |x|
        if pat === x
          n += 1
          return false if n > 1
        end
      end
    elsif block_given?
      __gather_each do |x|
        if yield(x)
          n += 1
          return false if n > 1
        end
      end
    elsif pattern.empty?
      __gather_each do |x|
        if x
          n += 1
          return false if n > 1
        end
      end
    else
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
    end
    n == 1
  end

  def min_by(n = nil)
    return self.to_enum(:min_by, n) unless block_given?
    if n.nil?
      elem = nil
      res = nil
      first = true
      __gather_each do |x|
        r = yield(x)
        if first
          elem = x
          res = r
          first = false
        elsif (r <=> res) < 0
          elem = x
          res = r
        end
      end
      elem
    else
      n = n.to_int unless n.is_a?(Integer)
      raise ArgumentError, "negative size (#{n})" if n < 0
      return [] if n == 0
      self.map { |x| [yield(x), x] }.sort_by { |pair| pair[0] }.first(n).map { |pair| pair[1] }
    end
  end

  def flat_map
    return self.to_enum(:flat_map) unless block_given?
    res = []
    __gather_each do |x|
      r = yield(x)
      if r.is_a?(Array)
        res.concat(r)
      else
        res << r
      end
    end
    res
  end
  alias collect_concat flat_map

  def tally(hash = nil)
    h = hash || {}
    __gather_each do |x|
      h[x] = (h[x] || 0) + 1
    end
    h
  end

  def chunk
    return self.to_enum(:chunk) unless block_given?
    result = []
    current_key = nil
    current_ary = nil
    first = true
    __gather_each do |x|
      key = yield(x)
      if first
        current_key = key
        current_ary = [x]
        first = false
      elsif key == current_key
        current_ary << x
      else
        result << [current_key, current_ary]
        current_key = key
        current_ary = [x]
      end
    end
    result << [current_key, current_ary] unless first
    result
  end

  def chunk_while
    return self.to_enum(:chunk_while) unless block_given?
    result = []
    current = nil
    prev = nil
    first = true
    __gather_each do |x|
      if first
        current = [x]
        prev = x
        first = false
      elsif yield(prev, x)
        current << x
        prev = x
      else
        result << current
        current = [x]
        prev = x
      end
    end
    result << current unless first
    result
  end

  def slice_when
    return self.to_enum(:slice_when) unless block_given?
    result = []
    current = nil
    prev = nil
    first = true
    __gather_each do |x|
      if first
        current = [x]
        prev = x
        first = false
      elsif yield(prev, x)
        result << current
        current = [x]
        prev = x
      else
        current << x
        prev = x
      end
    end
    result << current unless first
    result
  end

  def zip(*others)
    result = []
    arr = self.to_a
    others_arrays = others.map(&:to_a)
    arr.each_with_index do |x, i|
      entry = [x]
      others_arrays.each do |oa|
        entry << oa[i]
      end
      if block_given?
        yield entry
      else
        result << entry
      end
    end
    block_given? ? nil : result
  end

  def group_by
    return self.to_enum(:group_by) unless block_given?
    h = {}
    __gather_each do |x|
      key = yield(x)
      (h[key] ||= []) << x
    end
    h
  end

  def sort(&block)
    self.to_a.sort(&block)
  end

  def sort_by
    return self.to_enum(:sort_by) unless block_given?
    map { |x| [yield(x), x] }.sort { |a, b| a[0] <=> b[0] }.map { |x| x[1] }
  end

  def max_by(n = nil)
    return self.to_enum(:max_by, n) unless block_given?
    if n.nil?
      elem = nil
      res = nil
      first = true
      __gather_each do |x|
        r = yield(x)
        if first
          elem = x
          res = r
          first = false
        elsif (r <=> res) > 0
          elem = x
          res = r
        end
      end
      elem
    else
      n = n.to_int unless n.is_a?(Integer)
      raise ArgumentError, "negative size (#{n})" if n < 0
      return [] if n == 0
      self.map { |x| [yield(x), x] }.sort_by { |pair| pair[0] }.last(n).reverse.map { |pair| pair[1] }
    end
  end

  def count(*args)
    if block_given?
      n = 0
      __gather_each { |x| n += 1 if yield(x) }
      n
    elsif args.empty?
      n = 0
      __gather_each { |_| n += 1 }
      n
    else
      target = args[0]
      n = 0
      __gather_each { |x| n += 1 if x == target }
      n
    end
  end

  def sum(init = 0)
    if block_given?
      __gather_each { |x| init = init + yield(x) }
    else
      __gather_each { |x| init = init + x }
    end
    init
  end

  def include?(obj)
    __gather_each { |x| return true if x == obj }
    false
  end
  alias member? include?

  def first(n = nil)
    if n.nil?
      __gather_each { |x| return x }
      nil
    else
      res = []
      __gather_each do |x|
        break if res.size >= n
        res << x
      end
      res
    end
  end

  def to_a(*args)
    res = []
    self.each(*args) do |*ys|
      res << (ys.size == 1 ? ys[0] : ys)
    end
    res
  end
  alias entries to_a

  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end

  def to_h
    h = {}
    __gather_each do |x|
      if block_given?
        pair = yield(x)
      else
        pair = x
      end
      raise TypeError, "wrong element type #{pair.class}" unless pair.is_a?(Array)
      raise ArgumentError, "wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
      h[pair[0]] = pair[1]
    end
    h
  end

  def reject
    return self.to_enum(:reject) unless block_given?
    res = []
    __gather_each do |x|
      res << x unless yield(x)
    end
    res
  end

  def all?(*pattern)
    if !pattern.empty?
      if pattern.size != 1
        raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
      end
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      __gather_each { |x| return false unless pat === x }
    elsif block_given?
      __gather_each { |x| return false unless yield(x) }
    else
      __gather_each { |x| return false unless x }
    end
    true
  end

  def each_cons(n)
    raise ArgumentError, "invalid size" if n <= 0
    return self.to_enum(:each_cons, n) unless block_given?
    buf = []
    __gather_each do |x|
      buf << x
      buf.shift if buf.size > n
      yield buf.dup if buf.size == n
    end
    nil
  end

  def min(n = nil)
    if n.nil?
      m = nil
      first = true
      if block_given?
        __gather_each do |x|
          if first
            m = x
            first = false
          elsif yield(x, m) < 0
            m = x
          end
        end
      else
        __gather_each do |x|
          if first
            m = x
            first = false
          elsif (cmp = (x <=> m)).nil?
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed"
          elsif cmp < 0
            m = x
          end
        end
      end
      m
    else
      n = n.to_int unless n.is_a?(Integer)
      raise ArgumentError, "negative size (#{n})" if n < 0
      return [] if n == 0
      if block_given?
        self.to_a.sort { |a, b| yield(a, b) }.first(n)
      else
        self.to_a.sort.first(n)
      end
    end
  end

  def max(n = nil)
    if n.nil?
      m = nil
      first = true
      if block_given?
        __gather_each do |x|
          if first
            m = x
            first = false
          elsif yield(x, m) > 0
            m = x
          end
        end
      else
        __gather_each do |x|
          if first
            m = x
            first = false
          elsif (cmp = (x <=> m)).nil?
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed"
          elsif cmp > 0
            m = x
          end
        end
      end
      m
    else
      n = n.to_int unless n.is_a?(Integer)
      raise ArgumentError, "negative size (#{n})" if n < 0
      return [] if n == 0
      if block_given?
        self.to_a.sort { |a, b| yield(a, b) }.last(n).reverse
      else
        self.to_a.sort.last(n).reverse
      end
    end
  end

  def minmax(&block)
    if block
      mn = nil
      mx = nil
      first = true
      __gather_each do |x|
        if first
          mn = x
          mx = x
          first = false
        else
          mn = x if block.call(x, mn) < 0
          mx = x if block.call(x, mx) > 0
        end
      end
      [mn, mx]
    else
      [min, max]
    end
  end

  def uniq
    h = {}
    res = []
    if block_given?
      __gather_each do |x|
        key = yield(x)
        unless h.key?(key)
          h[key] = true
          res << x
        end
      end
    else
      __gather_each do |x|
        unless h.key?(x)
          h[x] = true
          res << x
        end
      end
    end
    res
  end

  def lazy
    Enumerator::Lazy.new(self)
  end

  ##
  ## --- Methods added by claude/enumerable-stage1-2 ---
  ##

  # Returns a new Array with nil elements removed.
  def compact
    res = []
    __gather_each { |x| res << x unless x.nil? }
    res
  end

  # Returns `[truthy_array, falsey_array]`.
  def partition
    return self.to_enum(:partition) unless block_given?
    yes_arr = []
    no_arr = []
    __gather_each do |x|
      if yield(x)
        yes_arr << x
      else
        no_arr << x
      end
    end
    [yes_arr, no_arr]
  end

  # Returns `[obj_with_smallest_score, obj_with_largest_score]` after
  # mapping each element through the block.
  def minmax_by
    return self.to_enum(:minmax_by) unless block_given?
    min_elem = nil
    max_elem = nil
    min_score = nil
    max_score = nil
    first = true
    __gather_each do |x|
      score = yield(x)
      if first
        min_elem = max_elem = x
        min_score = max_score = score
        first = false
      else
        if (score <=> min_score) < 0
          min_elem = x
          min_score = score
        end
        if (score <=> max_score) > 0
          max_elem = x
          max_score = score
        end
      end
    end
    first ? [nil, nil] : [min_elem, max_elem]
  end

  # Pass each element of `self` to the block, treating multi-value
  # yields as Arrays (matches CRuby's `each_entry`). Mostly useful on
  # enumerables whose `each` yields more than one value at a time.
  def each_entry(*args)
    return self.to_enum(:each_entry, *args) unless block_given?
    self.each(*args) do |*x|
      yield(x.size == 1 ? x[0] : x)
    end
    self
  end

  # Walk the enumerable in reverse order. Materialises via `to_a`
  # because a generic `Enumerable` has no random access. Objects with
  # an efficient native `reverse_each` (Array, String, …) override
  # this on their own class.
  def reverse_each(*args)
    return self.to_enum(:reverse_each, *args) unless block_given?
    self.to_a.reverse_each { |x| yield(x) }
    self
  end

  # Cycle through the enumerable `n` times, calling the block each
  # round. `nil` (the default) cycles forever. Non-positive `n`
  # returns `nil` without calling `each`.
  def cycle(n = nil)
    if n.nil?
      return self.to_enum(:cycle) unless block_given?
      cache = []
      __gather_each do |x|
        cache << x
        yield x
      end
      return nil if cache.empty?
      loop do
        cache.each { |x| yield x }
      end
    else
      n = n.to_int unless n.is_a?(Integer)
      return self.to_enum(:cycle, n) unless block_given?
      return nil if n <= 0
      cache = []
      __gather_each { |x| cache << x }
      n.times { cache.each { |x| yield x } }
      nil
    end
  end

  # Returns an Array of elements for which `pattern === element` is
  # truthy. With a block, transforms each match through the block.
  def grep(pattern)
    res = []
    __gather_each do |x|
      if pattern === x
        res << (block_given? ? yield(x) : x)
      end
    end
    res
  end

  # Inverse of `grep`: keeps elements where `pattern === element` is
  # false.
  def grep_v(pattern)
    res = []
    __gather_each do |x|
      unless pattern === x
        res << (block_given? ? yield(x) : x)
      end
    end
    res
  end

  # Splits `self` into Arrays whenever the boundary triggers. With a
  # `pattern` argument, the boundary fires before any element where
  # `pattern === x` is truthy. With a block, the boundary fires before
  # any element for which `yield(x)` is truthy. The two argument
  # forms are mutually exclusive (matches CRuby's ArgumentError).
  def slice_before(*args)
    if block_given?
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)" unless args.empty?
      pattern = nil
    else
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1)" if args.size != 1
      pattern = args[0]
    end
    res = []
    current = nil
    __gather_each do |x|
      hit = pattern.nil? ? yield(x) : (pattern === x)
      if hit
        res << current if current
        current = [x]
      else
        current ||= []
        current << x
      end
    end
    res << current if current
    res.each
  end

  # Mirror of `slice_before`: the boundary fires *after* an element
  # that matches. The first emitted chunk therefore always contains
  # the first element (no leading empty Array).
  def slice_after(*args)
    if block_given?
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)" unless args.empty?
      pattern = nil
    else
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1)" if args.size != 1
      pattern = args[0]
    end
    res = []
    current = []
    __gather_each do |x|
      current << x
      hit = pattern.nil? ? yield(x) : (pattern === x)
      if hit
        res << current
        current = []
      end
    end
    res << current unless current.empty?
    res.each
  end

  # Concatenates `self` with each enumerable in `enums`. Returns an
  # Enumerator that walks all of them in sequence. CRuby returns an
  # `Enumerator::Chain`, but a plain `Enumerator` satisfies every
  # `Enumerable::Chain` assertion except the class check.
  def chain(*enums)
    parts = [self, *enums]
    Enumerator.new do |y|
      parts.each do |part|
        part.each { |x| y << x }
      end
    end
  end
end

class Enumerator
  # Enumerator::Lazy is a minimal placeholder class.
  # Full lazy evaluation is blocked by a known block variable capture
  # limitation (see Issue #228). The class exists so that calling
  # .lazy on an Enumerable does not raise NoMethodError.
  #
  # Methods that would iterate (each, to_a, force, first) are intentionally
  # omitted to prevent stack overflows on infinite sequences.
  class Lazy
    def initialize(obj = nil, size = nil, &block)
      @obj = obj
    end

    def lazy
      self
    end

    def inspect
      if @obj
        "#<Enumerator::Lazy: \#{@obj.inspect}>"
      else
        "#<Enumerator::Lazy: ...>"
      end
    end

    def is_a?(klass)
      return true if klass == Enumerator::Lazy
      return true if klass == Enumerator
      super
    end
    alias kind_of? is_a?
  end

  # Enumerator::ArithmeticSequence is the value `Numeric#step` and
  # `Range#step` (and `Range#%`) return when called without a block:
  # a thin holder for `(begin, end, step, exclude_end?)` that
  # behaves like an Enumerator over the sequence and is also
  # accepted by `Array#[]` / `Array#slice` as a stride-aware index.
  #
  # We don't subclass Enumerator (its Fiber machinery only works
  # for Enumerators built from a real iterator), so `is_a?` /
  # `kind_of?` are overridden to keep `is_a?(Enumerator)` truthy
  # for spec compatibility.
  #
  # Phase 1 of the Rust-native migration moved storage off Ruby
  # ivars onto a dedicated `RValue` variant
  # (`ObjTy::ARITHMETIC_SEQUENCE`).  Phase 2 then moved every
  # accessor and the `inspect` / `to_s` / `first` / `last`
  # formatters into Rust builtins (registered in
  # `src/builtins/enumerator.rs`), so this Ruby class body only
  # needs to host the parts that still depend on Ruby semantics:
  #   * `is_a?` / `kind_of?` — lie about `Enumerator` ancestry
  #   * `each` — block-iteration kept in Ruby per design (Phase 2)
  #   * `to_a` / `entries` — thin `__each` wrapper
  #   * `size` — Numeric-type dispatch is easier in Ruby for now
  #   * `[]` — stride-aware Array slicer (Rust-ified in Phase 3)
  class ArithmeticSequence
    include Enumerable

    def is_a?(klass)
      return true if klass == Enumerator::ArithmeticSequence
      return true if klass == Enumerator
      return true if klass == Enumerable
      super
    end
    alias kind_of? is_a?

    def each(&block)
      return self.to_enum(:each) { size } unless block
      __each(&block)
      self
    end

    def to_a
      result = []
      __each { |v| result << v }
      result
    end
    alias entries to_a

    # Number of elements the sequence would yield. Returns
    # `Float::INFINITY` for unbounded sequences whose step matches
    # the open end (e.g. `(0..).step(1)`), `0` for empty / direction
    # mismatch, the integer count for finite sequences. Raises
    # `ArgumentError` when the step isn't `Numeric` (CRuby checks
    # this lazily on `.size`).
    def size
      b = __b
      e = __e
      s = __s
      excl = __excl?
      raise ArgumentError, "step must be numeric" unless s.is_a?(Numeric)
      raise ArgumentError, "step can't be 0" if s == 0
      if b.nil?
        # Beginless ⇒ no way to iterate to a finite end. CRuby
        # reports `Float::INFINITY`.
        return Float::INFINITY
      end
      if e.nil?
        # Endless: sequence runs forever in `step`'s direction.
        return Float::INFINITY
      end
      # Float arithmetic: `infinity` step yields at most one value
      # (the `begin`, if it satisfies the bound), so size is 1 or 0.
      if s.is_a?(Float) && s.infinite?
        if s > 0
          return excl ? (b < e ? 1 : 0) : (b <= e ? 1 : 0)
        else
          return excl ? (b > e ? 1 : 0) : (b >= e ? 1 : 0)
        end
      end
      diff = e - b
      # Direction mismatch (e.g. `1.step(0, 1).size`) ⇒ 0 yields.
      if (s > 0 && diff < 0) || (s < 0 && diff > 0)
        return 0
      end
      n = diff.to_f / s.to_f
      return 0 if n.nan?
      return 0 if n < 0
      # `e == ±Infinity` → infinite count of yields in step's
      # direction (already handled the direction-mismatch case
      # above). Avoid `Float::INFINITY.floor` (FloatDomainError).
      return Float::INFINITY if n.infinite?
      n_int = n.floor
      if excl
        last = b + n_int * s
        overshoot = s > 0 ? last >= e : last <= e
        n_int -= 1 if overshoot
      end
      n_int + 1
    end

    # `aseq[arr]` — extract the slice of `arr` whose indices are the
    # terms of this sequence. Independent of Array's own `[]`
    # implementation: Array#[] delegates here when its index argument
    # is an ArithmeticSequence, instead of carrying AS-specific code.
    #
    # Mirrors CRuby's `rb_ary_subseq_step` behaviour:
    #   * Resolve `begin` (nil → 0 for positive step, len-1 for
    #     negative; negative ints rebased against `len`).
    #   * Resolve `end` (nil → len-1 / 0; exclusive end ⇒ inner term).
    #   * Walk by `step`, collecting elements whose index is in
    #     `0...len`.
    #   * `RangeError` when the explicit end (or begin for negative
    #     step) is itself a term of the AS that lands outside the
    #     array — see comments inline.
    #   * Pure-`nil` return when `begin` is out of bounds for the
    #     `step == ±1` cases that fall back to plain Range slicing
    #     semantics.
    def [](arr)
      len = arr.length
      raw_b = __b
      raw_e = __e
      s = __s
      excl = __excl?
      return [] if s == 0

      if s > 0
        # ---- positive step ----------------------------------------
        b = raw_b.nil? ? 0 : raw_b
        b += len if b < 0
        # begin past the boundary: nil for stride 1 (matches plain
        # `arr[N..]`), RangeError for larger strides — CRuby
        # promotes the OOB to an error when the user explicitly
        # asked for a non-trivial stride.
        if !raw_b.nil? && b > len
          if s > 1
            raise RangeError,
                  "((#{raw_b.inspect}..#{raw_e.inspect}).step(#{s.inspect})) out of range"
          end
          return nil
        end
        # begin exactly at the boundary (== len): empty result.
        return [] if b == len
        return nil if b < 0

        if raw_e.nil?
          e = len - 1
          end_is_term = false
        else
          e_res = raw_e
          e_res += len if e_res < 0
          # `end` is a term of the sequence iff stepping from begin
          # lands exactly on it (and end is inclusive).
          end_is_term = !excl && b >= 0 && (e_res - b) % s == 0
          # CRuby raises when stride > 1 and the *user-supplied*
          # end is itself a yielded value past the array.
          if s > 1 && end_is_term && e_res >= len
            raise RangeError,
                  "((#{raw_b.inspect}..#{raw_e.inspect}).step(#{s.inspect})) out of range"
          end
          e = excl ? e_res - 1 : e_res
        end
        e = len - 1 if e >= len
        return [] if e < b
        result = []
        i = b
        while i <= e
          result << arr[i]
          i += s
        end
        result
      else
        # ---- negative step ----------------------------------------
        if raw_b.nil?
          b = len - 1
        else
          b = raw_b
          b += len if b < 0
          # begin past the array's last index: clip to `len - 1`,
          # but only if the gap is small enough — when
          # `begin - (len - 1) > |step|` CRuby treats the request
          # as out of range.
          if b > len - 1
            diff = b - (len - 1)
            if s.abs > 1 && diff > s.abs
              raise RangeError,
                    "((#{raw_b.inspect}..#{raw_e.inspect}).step(#{s.inspect})) out of range"
            end
            b = len - 1
          end
          return nil if b < 0
        end

        if raw_e.nil?
          e = 0
        else
          e_res = raw_e
          e_res += len if e_res < 0
          e = excl ? e_res + 1 : e_res
        end
        e = 0 if e < 0
        return [] if b < e
        result = []
        i = b
        while i >= e
          result << arr[i]
          i += s
        end
        result
      end
    end

    private

    def __each
      b = __b
      e = __e
      s = __s
      excl = __excl?
      raise TypeError, "step can't be 0" if s == 0
      raise ArgumentError, "#each for beginless arithmetic sequences is meaningless" if b.nil?
      cur = b
      if e.nil?
        loop do
          yield cur
          cur += s
        end
      elsif s > 0
        if excl
          while cur < e
            yield cur
            cur += s
          end
        else
          while cur <= e
            yield cur
            cur += s
          end
        end
      else
        if excl
          while cur > e
            yield cur
            cur += s
          end
        else
          while cur >= e
            yield cur
            cur += s
          end
        end
      end
    end
  end
end
