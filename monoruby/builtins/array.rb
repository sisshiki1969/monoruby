class Array
  include Enumerable

  # NOTE: no `def self.new` here. It used to be defined as
  # `o = allocate; o.__send__(:initialize, ...); o`, which predates the
  # Ruby-level `Class#new`. That override is now both slower and less
  # correct than inheriting `Class#new`:
  #   * `__send__` is registered with rest + kwrest, so the forwarded
  #     `initialize` call missed every JIT forwarding fast path and fell
  #     back to the generic argument re-parse plus an eagerly
  #     materialized rest Array per call (~8x slower than the inherited
  #     trampoline, which reaches `Array#initialize` — 0/2 positionals,
  #     no rest, no keywords — through the inline path); and
  #   * it dispatched the *public* `allocate`, so a user-defined
  #     `self.allocate` on an Array subclass was honoured. CRuby's
  #     `Array.new` bypasses it, which is what `Class#new` does via
  #     `__builtin_allocate__`.

  # NOTE: `sample`, `shuffle` and `shuffle!` are native
  # (`builtins/array.rs`). They share one `random:` resolver and one
  # `RAND_UPTO`, so the keyword behaves identically across all three.

  # Array#initialize
  # new(size = 0, val = nil) / new(ary) / new(size) {|index| ... }
  #
  # In Ruby so a hot `Array.new` site gets JIT-specialized argument
  # binding and, for the block form, an inlined `yield` per element
  # (the native version paid a native->Ruby block invocation each).
  # The heavy legs stay native: `__init_fill` (bulk fill + size
  # checks), `__init_from` (the `#to_ary` contents protocol),
  # and `__size_to_int` (implicit
  # `#to_int` coercion). This is the only definition — nothing in the
  # startup sequence constructs an Array with arguments before this
  # file loads, so no native fallback exists.
  #
  # The `#to_ary` probe is gated on the argument NOT being an Integer,
  # mirroring CRuby's `!FIXNUM_P` fast path in `rb_ary_initialize`:
  # defining `Integer#to_ary` never hijacks `Array.new(5)`. (For a
  # Bignum CRuby would still probe; monoruby's Integer is one class,
  # so a Bignum skips the probe too — `__init_fill`'s size checks then
  # reject it: ArgumentError within the i64 range, RangeError beyond,
  # matching CRuby's NUM2LONG + "array size too big" order.)
  private def initialize(size = (no_size = true; nil), val = (no_val = true; nil))
    if no_size
      __init_fill(0, nil)
      # CRuby: rb_warning — emitted only when $VERBOSE is true (-w/-W).
      __warn_caller "warning: given block not used" if block_given? && $VERBOSE
      return self
    end
    if size.is_a?(Integer)
      n = size
    else
      if no_val
        r = __init_from(size)
        return r if r
      end
      n = __size_to_int(size)
    end
    if n < 0
      # CRuby's NUM2LONG runs before its negative check, so a negative
      # Bignum below the `long` range is a RangeError, not ArgumentError.
      raise RangeError, "bignum too big to convert into 'long'" if n < -9223372036854775808
      raise ArgumentError, "negative array size"
    end
    if block_given?
      # CRuby: rb_warn — shown at the default warning level, silent
      # only under -W0 ($VERBOSE == nil).
      unless no_val || $VERBOSE.nil?
        __warn_caller "warning: block supersedes default value argument"
      end
      # `yield` here specializes against the literal block at the
      # user's `Array.new { .. }` site: `resolve_given_block` follows
      # the `Class#new` forwarding chain, so each element is an inlined
      # block call. Incremental push keeps CRuby's partial-contents
      # semantics when the block `break`s.
      # CRuby checks the size before running the block; mirror the
      # native cap (MAX_ARRAY_SIZE in __init_fill) and `long` range.
      if n > 1073741824
        raise RangeError, "bignum too big to convert into 'long'" if n > 9223372036854775807
        raise ArgumentError, "array size too big"
      end
      __init_fill(0, nil)
      i = 0
      while i < n
        self << yield(i)
        i += 1
      end
      return self
    end
    __init_fill(n, val)
    self
  end

  def each
    return self.to_enum(:each) { self.size } unless block_given?
    i = 0
    while i < self.size
      yield self[i]
      i += 1
    end
    self
  end

  def reverse_each
    return self.to_enum(:reverse_each) { self.size } unless block_given?
    len = self.size
    if len == 0
      return self
    end
    i = len - 1
    while i >= 0
      yield self[i]
      i -= 1
    end
    self
  end

  def each_with_index
    return self.to_enum(:each_with_index) { self.size } unless block_given?
    i = 0
    while i < self.size
      yield self[i], i
      i += 1
    end
    self
  end

  def map!
    return self.to_enum(:map!) { self.size } unless block_given?
    raise FrozenError, "can't modify frozen #{self.class}: #{self.inspect}" if frozen?
    i = 0
    while i < self.size
      self[i] = yield(self[i])
      i += 1
    end
    self
  end
  alias collect! map!

  def map
    return self.to_enum(:map) { self.size } unless block_given?
    res = Array.new(self.size)
    i = 0
    while i < self.size
      res[i] = yield(self[i])
      i += 1
    end
    res
  end
  alias collect map

  #def product(*lists)
  #  if lists.empty?
  #    return self.map {|x| [x] }
  #  end
  #  l = lists.shift
  #  res = []
  #  for e1 in self
  #    for e2 in l.product(*lists)
  #      res << [e1, *e2]
  #    end
  #  end
  #  res
  #end

  def bsearch
    return to_enum(:bsearch) unless block_given?
    low = 0
    high = size
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = self[mid]
      res = yield(val)

      if mode.nil?
        if res == true || res == false || res.nil?
          mode = :find_min
        elsif res.is_a?(Numeric)
          mode = :find_exact
        else
          raise TypeError, "wrong argument type #{res.class} (must be numeric, true, false or nil)"
        end
      end

      case mode
      when :find_min
        if res
          high = mid
        else
          low = mid + 1
        end
      when :find_exact
        if res.nil?
          low = mid + 1
        elsif res < 0
          low = mid + 1
        elsif res > 0
          high = mid
        else
          return val
        end
      end
    end
    mode == :find_min ? self[low] : nil
  end

  def dig(idx, *rest)
    val = self[idx]
    return val if rest.empty? || val.nil?
    raise TypeError, "#{val.class} does not have #dig method" unless val.respond_to?(:dig)
    val.dig(*rest)
  end

  # Array#sum is implemented in Rust (see builtins/array.rs::sum) with a
  # Fixnum fast-path. Falling back to the Ruby version below would skip
  # that fast-path, so it stays out of the Array re-opens.

  def tally
    h = {}
    each do |x|
      h[x] = (h[x] || 0) + 1
    end
    h
  end

  def filter_map
    return to_enum(:filter_map) { self.size } unless block_given?
    res = []
    each do |x|
      y = yield(x)
      res << y if y
    end
    res
  end

  def cycle(n = (no_n = true; nil))
    unless block_given?
      unless no_n || n.nil? || n.is_a?(Integer)
        raise TypeError, "no implicit conversion of #{n.class} into Integer" unless n.respond_to?(:to_int)
        n = n.to_int
        raise TypeError, "can't convert to Integer" unless n.is_a?(Integer)
      end
      # Size hint:
      #   * empty array            -> 0
      #   * cycle()/cycle(nil)     -> Float::INFINITY
      #   * cycle(n) with n >= 0   -> length * n
      #   * cycle(n) with n  < 0   -> 0
      args = no_n ? [] : [n]
      return to_enum(:cycle, *args) {
        len = self.length
        if len == 0
          0
        elsif no_n || n.nil?
          Float::INFINITY
        elsif n < 0
          0
        else
          len * n
        end
      }
    end
    return nil if empty?
    if n.nil?
      while true
        each { |x| yield x }
      end
    else
      unless n.is_a?(Integer)
        raise TypeError, "no implicit conversion of #{n.class} into Integer" unless n.respond_to?(:to_int)
        n = n.to_int
        raise TypeError, "can't convert to Integer" unless n.is_a?(Integer)
      end
      n.times do
        each { |x| yield x }
      end
    end
    nil
  end

  def combination(n)
    unless block_given?
      # Lazily compute the binomial coefficient C(self.size, k) the
      # same way CRuby's enumerator does: 0 for k < 0 or k > size, 1
      # for k == 0, otherwise the multiplicative form so we never
      # build the full factorial.
      return to_enum(:combination, n) {
        k = n.is_a?(Integer) ? n : n.to_int
        len = self.size
        if k < 0 || k > len
          0
        elsif k == 0
          1
        else
          k = len - k if k > len - k
          v = 1
          1.upto(k) { |i| v = v * (len - k + i) / i }
          v
        end
      }
    end
    n = n.to_int
    # Defensive copy: CRuby snapshots the receiver before iterating,
    # so mutations from the block don't change the generated combos.
    pool = self.dup
    len = pool.size
    if n == 0
      yield []
    elsif n == 1
      pool.each { |x| yield [x] }
    elsif n > 0 && n <= len
      # iterative combination generation
      indices = (0...n).to_a
      loop do
        yield indices.map { |i| pool[i] }
        # find rightmost index that can be incremented
        i = n - 1
        i -= 1 while i >= 0 && indices[i] == len - n + i
        break if i < 0
        indices[i] += 1
        (i + 1...n).each { |j| indices[j] = indices[j - 1] + 1 }
      end
    end
    self
  end

  def bsearch_index
    return to_enum(:bsearch_index) unless block_given?
    low = 0
    high = size
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = self[mid]
      res = yield(val)

      if mode.nil?
        if res == true || res == false || res.nil?
          mode = :find_min
        elsif res.is_a?(Numeric)
          mode = :find_exact
        else
          raise TypeError, "wrong argument type #{res.class} (must be numeric, true, false or nil)"
        end
      end

      case mode
      when :find_min
        if res
          high = mid
        else
          low = mid + 1
        end
      when :find_exact
        if res.nil?
          low = mid + 1
        elsif res < 0
          low = mid + 1
        elsif res > 0
          high = mid
        else
          return mid
        end
      end
    end
    mode == :find_min ? low < size ? low : nil : nil
  end

  def permutation(n = nil)
    unless block_given?
      # When called with no argument the Enumerator must use the
      # receiver's size *at iteration time*, not freeze it now — CRuby
      # reflects later mutations (`a.permutation; a << x; enum.to_a`).
      # Replay with no arg so `self.size` is recomputed; with an
      # explicit `n` the Enumerator is built with that fixed length.
      # Descending factorial size = n * (n-1) * ... * (n-k+1).
      enum_args = n.nil? ? [] : [n]
      return to_enum(:permutation, *enum_args) {
        k = n.nil? ? self.size : (n.is_a?(Integer) ? n : n.to_int)
        len = self.size
        if k < 0 || k > len
          0
        elsif k == 0
          1
        else
          v = 1
          k.times { |i| v *= (len - i) }
          v
        end
      }
    end
    n = n.nil? ? self.size : n.to_int
    if n == 0
      yield []
      return self
    end
    return self if n < 0 || n > size
    if n == size
      # Generate all permutations
      pool = self.dup
      indices = (0...n).to_a
      yield indices.map { |i| pool[i] }
      cycles = (size.downto(size - n + 1)).to_a
      loop do
        found = false
        (n - 1).downto(0) do |i|
          cycles[i] -= 1
          if cycles[i] == 0
            # Move index at i to end
            tmp = indices[i]
            (i...n - 1).each { |j| indices[j] = indices[j + 1] }
            indices[n - 1] = tmp
            cycles[i] = size - i
          else
            j = -cycles[i]
            indices[i], indices[j] = indices[j], indices[i]
            yield indices[0, n].map { |idx| pool[idx] }
            found = true
            break
          end
        end
        return self unless found
      end
    else
      pool = self.dup
      indices = (0...size).to_a
      cycles = (size.downto(size - n + 1)).to_a
      yield indices[0, n].map { |i| pool[i] }
      loop do
        found = false
        (n - 1).downto(0) do |i|
          cycles[i] -= 1
          if cycles[i] == 0
            tmp = indices[i]
            (i...size - 1).each { |j| indices[j] = indices[j + 1] }
            indices[size - 1] = tmp
            cycles[i] = size - i
          else
            j = -cycles[i]
            indices[i], indices[j] = indices[j], indices[i]
            yield indices[0, n].map { |idx| pool[idx] }
            found = true
            break
          end
        end
        return self unless found
      end
    end
    self
  end

  def at(index)
    self[index]
  end

  def to_ary
    self
  end

  def deconstruct
    self
  end

  def drop_while
    return to_enum(:drop_while) unless block_given?
    i = 0
    while i < size
      break unless yield(self[i])
      i += 1
    end
    self[i, size - i]
  end

  def fetch_values(*indexes)
    result = []
    if block_given?
      indexes.each do |i|
        idx = i.to_int
        if idx < -size || idx >= size
          result << yield(i)
        else
          result << self[idx]
        end
      end
    else
      indexes.each do |i|
        result << fetch(i)
      end
    end
    result
  end

  def rindex(val = (no_val = true; nil))
    unless no_val
      warn "warning: given block not used" if block_given?
      i = self.size - 1
      while i >= 0
        if i >= self.size
          i = self.size - 1
          next
        end
        # rb_equal, not a bare `==`: an element that *is* `val` is found
        # however its `==` answers (CRuby's identity step).
        return i if __rb_equal(self[i], val)
        i -= 1
      end
      return nil
    end
    if block_given?
      i = self.size - 1
      while i >= 0
        if i >= self.size
          i = self.size - 1
          next
        end
        return i if yield(self[i])
        i -= 1
      end
      return nil
    end
    return self.to_enum(:rindex)
  end

  def assoc(key)
    each do |elem|
      if elem.is_a?(Array)
        return elem if elem.size > 0 && __rb_equal(elem[0], key)
      elsif elem.respond_to?(:to_ary)
        ary = elem.to_ary
        return ary if ary.is_a?(Array) && ary.size > 0 && __rb_equal(ary[0], key)
      end
    end
    nil
  end

  def rassoc(key)
    each do |elem|
      if elem.is_a?(Array)
        return elem if elem.size > 1 && __rb_equal(elem[1], key)
      elsif elem.respond_to?(:to_ary)
        ary = elem.to_ary
        return ary if ary.is_a?(Array) && ary.size > 1 && __rb_equal(ary[1], key)
      end
    end
    nil
  end

  def each_index
    return self.to_enum(:each_index) { self.size } unless block_given?
    i = 0
    while i < self.size
      yield i
      i += 1
    end
    self
  end

  def repeated_permutation(n)
    unless block_given?
      # size**n; 0 when n < 0 (no permutations); 1 when n == 0
      # (the empty permutation), even for an empty receiver.
      return to_enum(:repeated_permutation, n) {
        k = n.is_a?(Integer) ? n : n.to_int
        if k < 0
          0
        elsif k == 0
          1
        else
          self.size ** k
        end
      }
    end
    n = n.to_int
    copy = self.dup
    len = copy.size
    if n == 0
      yield []
    elsif len == 0
      # nothing
    elsif n > 0
      indices = [0] * n
      loop do
        yield indices.map { |i| copy[i] }
        # Increment from the rightmost
        i = n - 1
        while i >= 0
          indices[i] += 1
          if indices[i] < len
            break
          end
          indices[i] = 0
          i -= 1
        end
        break if i < 0
      end
    end
    self
  end

  def repeated_combination(n)
    unless block_given?
      # C(size + k - 1, k); 0 for k < 0; 1 for k == 0; the full
      # multiplicative form to avoid factorials of large arrays.
      return to_enum(:repeated_combination, n) {
        k = n.is_a?(Integer) ? n : n.to_int
        len = self.size
        if k < 0
          0
        elsif k == 0
          1
        elsif len == 0
          0
        else
          # C(len + k - 1, k) using the multiplicative form.
          v = 1
          1.upto(k) { |i| v = v * (len + k - i) / i }
          v
        end
      }
    end
    n = n.to_int
    # Defensive copy (see #combination): ignore block mutations.
    pool = self.dup
    len = pool.size
    if n == 0
      yield []
    elsif n == 1
      pool.each { |x| yield [x] }
    elsif len > 0 && n > 0
      indices = [0] * n
      loop do
        yield indices.map { |i| pool[i] }
        # Increment
        i = n - 1
        while i >= 0
          indices[i] += 1
          if indices[i] < len
            # Fill all subsequent indices with the same value
            ((i + 1)...n).each { |j| indices[j] = indices[i] }
            break
          end
          i -= 1
        end
        break if i < 0
      end
    end
    self
  end
end
class Array
  def values_at(*selectors)
    result = []
    selectors.each do |s|
      if s.is_a?(Range)
        b = s.begin
        e = s.end
        b = b.nil? ? 0 : b.to_int
        endless = e.nil?
        e = endless ? size - 1 : e.to_int
        b += size if b < 0
        e += size if e < 0
        e -= 1 if !endless && s.exclude_end?
        next if b < 0
        i = b
        while i <= e
          result << self[i]
          i += 1
        end
      else
        result << self[s]
      end
    end
    result
  end

  # Pull up to +n+ leading items from +obj+ for Array#zip — CRuby's
  # `rb_ary_zip` / `take_items` protocol: collect with `each` and stop
  # early with a plain `break`. Unlike an `Enumerator#next` pull there is
  # no fiber in the path, and every exception the argument's `each`
  # raises — StopIteration included — reaches the caller instead of
  # being mistaken for end-of-iteration (issue #1080). A multi-value
  # yield packs into an Array, a single value stays bare, matching what
  # `#next` used to return.
  private def __zip_pull(obj, n)
    buf = []
    return buf if n == 0
    obj.each do |*x|
      buf << (x.size <= 1 ? x[0] : x)
      break if buf.size >= n
    end
    buf
  end
end
