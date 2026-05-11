module Enumerable
  def inject(init = nil)
    acc = init
    if init.nil?
      flag = true
      self.each do |x|
        if flag
          acc = x
          flag = false
        else
          acc = yield(acc, x)
        end
      end
    else
      self.each do |x|
        acc = yield(acc, x)
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
    self.each do |x|
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
    self.each do |x|
      yield x, i
      i += 1
    end
    self
  end

  def each_with_object(obj)
    return self.to_enum(:each_with_object) unless block_given?
    self.each do |x|
      yield x, obj
    end
    obj
  end

  def map
    return self.to_enum(:map) unless block_given?
    res = []
    self.each do |x|
      res << yield(x)
    end
    res
  end
  alias collect map

  def find(ifnone = nil)
    return self.to_enum(:find) unless block_given?
    self.each do |x|
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

  def filter
    return self.to_enum(:filter) unless block_given?
    res = []
    self.each do |x|
      if yield(x)
        res << x
      end
    end
    res
  end
  alias select filter

  def filter_map
    return self.to_enum(:filter_map) unless block_given?
    res = []
    self.each do |x|
      y = yield(x)
      res << y if y
    end
    res
  end

  def take_while
    return self.to_enum(:take_while) unless block_given?
    res = []
    self.each do |x|
      break unless yield(x)
      res << x
    end
    res
  end

  def drop_while
    return self.to_enum(:drop_while) unless block_given?
    res = []
    dropping = true
    self.each do |x|
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
    self.each do |x|
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
    self.each do |x|
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
      self.each do |x|
        return true if pat === x
      end
    elsif block_given?
      self.each do |x|
        return true if yield(x)
      end
    else
      self.each do |x|
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
      self.each do |x|
        return false if pat === x
      end
    elsif block_given?
      self.each do |x|
        return false if yield(x)
      end
    else
      self.each do |x|
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
      self.each do |x|
        if pat === x
          n += 1
          return false if n > 1
        end
      end
    elsif block_given?
      self.each do |x|
        if yield(x)
          n += 1
          return false if n > 1
        end
      end
    elsif pattern.empty?
      self.each do |x|
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
    return self.to_enum(:min_by) unless block_given?
    elem = nil
    res = nil
    self.each do |x|
      r = yield(x)
      if res.nil?
        elem = x
        res = r
      else
        if (r <=> res) < 0
          elem = x
          res = r
        end
      end
    end
    elem
  end

  def flat_map
    return self.to_enum(:flat_map) unless block_given?
    res = []
    self.each do |x|
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
    self.each do |x|
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
    self.each do |x|
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
    self.each do |x|
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
    self.each do |x|
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
    self.each do |x|
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
    return self.to_enum(:max_by) unless block_given?
    elem = nil
    res = nil
    self.each do |x|
      r = yield(x)
      if res.nil? || (r <=> res) > 0
        elem = x
        res = r
      end
    end
    elem
  end

  def count(*args)
    if block_given?
      n = 0
      self.each { |x| n += 1 if yield(x) }
      n
    elsif args.empty?
      n = 0
      self.each { |_| n += 1 }
      n
    else
      target = args[0]
      n = 0
      self.each { |x| n += 1 if x == target }
      n
    end
  end

  def sum(init = 0)
    if block_given?
      self.each { |x| init = init + yield(x) }
    else
      self.each { |x| init = init + x }
    end
    init
  end

  def include?(obj)
    self.each { |x| return true if x == obj }
    false
  end
  alias member? include?

  def first(n = nil)
    if n.nil?
      self.each { |x| return x }
      nil
    else
      res = []
      self.each do |x|
        break if res.size >= n
        res << x
      end
      res
    end
  end

  def to_a(*args)
    res = []
    self.each(*args) do |x|
      res << x
    end
    res
  end
  alias entries to_a

  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end

  def to_h
    h = {}
    self.each do |x|
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
    self.each do |x|
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
      self.each { |x| return false unless pat === x }
    elsif block_given?
      self.each { |x| return false unless yield(x) }
    else
      self.each { |x| return false unless x }
    end
    true
  end

  def each_cons(n)
    raise ArgumentError, "invalid size" if n <= 0
    return self.to_enum(:each_cons, n) unless block_given?
    buf = []
    self.each do |x|
      buf << x
      buf.shift if buf.size > n
      yield buf.dup if buf.size == n
    end
    nil
  end

  def min(n = nil)
    m = nil
    if block_given?
      self.each do |x|
        if m.nil? || yield(x, m) < 0
          m = x
        end
      end
    else
      self.each do |x|
        if m.nil? || (x <=> m) < 0
          m = x
        end
      end
    end
    m
  end

  def max(n = nil)
    m = nil
    if block_given?
      self.each do |x|
        if m.nil? || yield(x, m) > 0
          m = x
        end
      end
    else
      self.each do |x|
        if m.nil? || (x <=> m) > 0
          m = x
        end
      end
    end
    m
  end

  def minmax(&block)
    if block
      mn = nil
      mx = nil
      self.each do |x|
        if mn.nil?
          mn = x
          mx = x
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
      self.each do |x|
        key = yield(x)
        unless h.key?(key)
          h[key] = true
          res << x
        end
      end
    else
      self.each do |x|
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
  class ArithmeticSequence
    include Enumerable

    # Bypass the standard `Enumerator#new(&block)` requirement so
    # `Range#step` can stamp begin/end/step/exclude_end? on a fresh
    # instance without going through the Fiber path.
    def self.__build(b, e, step, exclude_end)
      o = allocate
      o.__send__(:__init, b, e, step, exclude_end)
      o
    end

    def __init(b, e, step, exclude_end)
      @begin = b
      @end = e
      @step = step
      @exclude_end = exclude_end ? true : false
    end
    private :__init

    attr_reader :begin, :end, :step

    # `first` follows Enumerator#first: with no arg returns the
    # first yielded value (== `begin` for a non-empty AS); with
    # `n` returns the first `n` yielded values.
    def first(n = nil)
      n.nil? ? @begin : take(n)
    end

    def exclude_end?
      @exclude_end
    end

    def is_a?(klass)
      return true if klass == Enumerator::ArithmeticSequence
      return true if klass == Enumerator
      return true if klass == Enumerable
      super
    end
    alias kind_of? is_a?

    # CRuby format: `((begin..end).step(step))` — or `%(step)` when
    # the sequence was produced via `Range#%`. `Range#%` overrides
    # `inspect` separately to hit the second form; this default is
    # the `step` form.
    def inspect
      lo = @begin.nil? ? "" : @begin.inspect
      hi = @end.nil?  ? "" : @end.inspect
      sep = @exclude_end ? "..." : ".."
      step_part = @step.nil? ? "" : @step.inspect
      "((#{lo}#{sep}#{hi}).step(#{step_part}))"
    end
    alias to_s inspect

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
      b = @begin
      e = @end
      s = @step
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
          return @exclude_end ? (b < e ? 1 : 0) : (b <= e ? 1 : 0)
        else
          return @exclude_end ? (b > e ? 1 : 0) : (b >= e ? 1 : 0)
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
      if @exclude_end
        last = b + n_int * s
        overshoot = s > 0 ? last >= e : last <= e
        n_int -= 1 if overshoot
      end
      n_int + 1
    end

    def last(n = nil)
      arr = to_a
      n.nil? ? arr.last : arr.last(n)
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
      s = @step
      return [] if s == 0

      if s > 0
        # ---- positive step ----------------------------------------
        b = @begin.nil? ? 0 : @begin
        b += len if b < 0
        # begin past the boundary: nil for stride 1 (matches plain
        # `arr[N..]`), RangeError for larger strides — CRuby
        # promotes the OOB to an error when the user explicitly
        # asked for a non-trivial stride.
        if !@begin.nil? && b > len
          if s > 1
            raise RangeError,
                  "((#{@begin.inspect}..#{@end.inspect}).step(#{s.inspect})) out of range"
          end
          return nil
        end
        # begin exactly at the boundary (== len): empty result.
        return [] if b == len
        return nil if b < 0

        if @end.nil?
          e = len - 1
          end_is_term = false
        else
          e_res = @end
          e_res += len if e_res < 0
          # `end` is a term of the sequence iff stepping from begin
          # lands exactly on it (and end is inclusive).
          end_is_term = !@exclude_end && b >= 0 && (e_res - b) % s == 0
          # CRuby raises when stride > 1 and the *user-supplied*
          # end is itself a yielded value past the array.
          if s > 1 && end_is_term && e_res >= len
            raise RangeError,
                  "((#{@begin.inspect}..#{@end.inspect}).step(#{s.inspect})) out of range"
          end
          e = @exclude_end ? e_res - 1 : e_res
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
        if @begin.nil?
          b = len - 1
        else
          b = @begin
          b += len if b < 0
          # begin past the array's last index: clip to `len - 1`,
          # but only if the gap is small enough — when
          # `begin - (len - 1) > |step|` CRuby treats the request
          # as out of range.
          if b > len - 1
            diff = b - (len - 1)
            if s.abs > 1 && diff > s.abs
              raise RangeError,
                    "((#{@begin.inspect}..#{@end.inspect}).step(#{s.inspect})) out of range"
            end
            b = len - 1
          end
          return nil if b < 0
        end

        if @end.nil?
          e = 0
        else
          e_res = @end
          e_res += len if e_res < 0
          e = @exclude_end ? e_res + 1 : e_res
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
    # Compatibility alias: the previous PR commit shipped Rust code
    # that calls `__as_slice` to do the Array slicing. Keep the old
    # name pointing at `[]` for one release so a half-applied
    # rollout (e.g. enumerable.rb landed but array.rs / runtime.rs
    # didn't yet) still has a valid method to invoke. Remove once
    # all callers have switched.
    alias __as_slice []

    private

    def __each
      b = @begin
      e = @end
      s = @step
      raise TypeError, "step can't be 0" if s == 0
      raise ArgumentError, "#each for beginless arithmetic sequences is meaningless" if b.nil?
      cur = b
      if e.nil?
        loop do
          yield cur
          cur += s
        end
      elsif s > 0
        if @exclude_end
          while cur < e
            yield cur
            cur += s
          end
        else
          while cur >= e
            yield cur
            cur += s
          end
        end
      else
        if @exclude_end
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
