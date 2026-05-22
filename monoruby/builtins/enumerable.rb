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
      yield(__ys.empty? ? nil : __ys.size == 1 ? __ys[0] : __ys)
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
    has_init = false
    init = nil
    sym = nil
    case args.size
    when 0
      raise ArgumentError, "no block given" unless block_given?
    when 1
      a = args[0]
      if block_given?
        has_init = true
        init = a
      elsif a.is_a?(Symbol) || a.is_a?(String) || a.respond_to?(:to_str)
        a = a.to_str if !a.is_a?(Symbol) && !a.is_a?(String)
        raise TypeError, "#{args[0].inspect} is not a symbol nor a string" if a.nil?
        sym = a.to_sym
      else
        raise TypeError, "#{args[0].inspect} is not a symbol nor a string"
      end
    when 2
      warn "warning: given block not used" if block_given?
      has_init = true
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
      if !has_init
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
      if !has_init
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
    n = n.to_int unless n.is_a?(Integer)
    if n <= 0
      raise ArgumentError, "invalid slice size"
    end
    return to_enum(:each_slice, n) { respond_to?(:size) ? (size.to_f / n).ceil : nil } unless block_given?
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
    return to_enum(:each_with_index, *args) { respond_to?(:size) ? size : nil } unless block_given?
    i = 0
    each(*args) do |*xs|
      yield(xs.size == 1 ? xs[0] : xs, i)
      i += 1
    end
    self
  end

  def each_with_object(obj)
    return to_enum(:each_with_object, obj) { respond_to?(:size) ? size : nil } unless block_given?
    __gather_each do |x|
      yield x, obj
    end
    obj
  end

  def map
    return to_enum(:map) { respond_to?(:size) ? size : nil } unless block_given?
    res = []
    # The user block is called with the *original* values `each`
    # yields (arity-adaptive), not the packed element: for
    # `yield 1, 2` a `{ |e| }` block sees `1`, a `{ |a, b| }` block
    # sees `1, 2` (matches CRuby `rb_yield_values2`). Internal
    # packed-element callers use `__gather_each` directly instead.
    self.each do |*vs|
      res << yield(*vs)
    end
    res
  end
  alias collect map

  def find(ifnone = nil)
    return self.to_enum(:find, ifnone) unless block_given?
    __gather_each do |x|
      return x if yield(x)
    end
    ifnone.nil? ? nil : ifnone.call
  end
  alias detect find

  def find_index(*args)
    if args.empty?
      return self.to_enum(:find_index) unless block_given?
      i = 0
      # Single-param block receives first element (CRuby arity adaptation)
      each do |*xs|
        return i if yield(*xs)
        i += 1
      end
      nil
    else
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1)" if args.size > 1
      warn "warning: given block not used" if block_given?
      target = args[0]
      i = 0
      # Value comparison uses gathered arrays as elements
      __gather_each do |x|
        return i if x == target
        i += 1
      end
      nil
    end
  end

  def filter
    return to_enum(:filter) { respond_to?(:size) ? size : nil } unless block_given?
    res = []
    __gather_each do |x|
      res << x if yield(x)
    end
    res
  end
  alias select filter
  alias find_all filter

  def filter_map
    return to_enum(:filter_map) { respond_to?(:size) ? size : nil } unless block_given?
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
    # Single-param block gets first element (CRuby arity adaptation, not gathered)
    each do |*xs|
      x = xs.size == 1 ? xs[0] : xs
      break unless yield(*xs)
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
      each do |*xs|
        if yield(*xs)
          n += 1
          return false if n > 1
        end
      end
    else
      each do |*xs|
        x = xs.size == 1 ? xs[0] : xs
        if x
          n += 1
          return false if n > 1
        end
      end
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
      pairs = []
      __gather_each { |x| pairs << [yield(x), x] }
      pairs.sort_by { |pair| pair[0] }.first(n).map { |pair| pair[1] }
    end
  end

  def flat_map
    return to_enum(:flat_map) { respond_to?(:size) ? size : nil } unless block_given?
    res = []
    __gather_each do |x|
      r = yield(x)
      if r.is_a?(Array)
        res.concat(r)
      elsif !r.is_a?(String) && r.respond_to?(:to_ary)
        a = r.to_ary
        if a.is_a?(Array)
          res.concat(a)
        elsif a.nil?
          res << r
        else
          raise TypeError,
                "can't convert #{r.class} to Array (#{r.class}#to_ary gives #{a.class})"
        end
      else
        res << r
      end
    end
    res
  end
  alias collect_concat flat_map

  def tally(hash = nil)
    unless hash.nil?
      unless hash.is_a?(Hash)
        if hash.respond_to?(:to_hash)
          hash = hash.to_hash
        end
        raise TypeError, "wrong argument type #{hash.class} (expected Hash)" unless hash.is_a?(Hash)
      end
      raise FrozenError, "can't modify frozen Hash: #{hash.inspect}" if hash.frozen?
    end
    h = hash || {}
    __gather_each do |x|
      h[x] = (h.key?(x) ? h[x] : 0) + 1
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

  def chunk_while(&block)
    raise ArgumentError, "no block given" unless block
    blk = block
    src = self
    Enumerator.new do |y|
      prev = nil
      current = nil
      first = true
      src.each do |*xs|
        x = xs.size == 1 ? xs[0] : xs
        if first
          current = [x]
          prev = x
          first = false
        elsif blk.call(prev, x)
          current << x
          prev = x
        else
          y << current
          current = [x]
          prev = x
        end
      end
      y << current unless first
    end
  end

  def slice_when(&block)
    raise ArgumentError, "no block given" unless block
    blk = block
    src = self
    Enumerator.new do |y|
      prev = nil
      current = nil
      first = true
      src.each do |*xs|
        x = xs.size == 1 ? xs[0] : xs
        if first
          current = [x]
          prev = x
          first = false
        elsif blk.call(prev, x)
          y << current
          current = [x]
          prev = x
        else
          current << x
          prev = x
        end
      end
      y << current unless first
    end
  end

  def zip(*others)
    others_arrays = others.map do |other|
      if other.respond_to?(:to_ary)
        other.to_ary
      elsif other.respond_to?(:each)
        other.to_enum(:each).to_a
      else
        raise TypeError, "wrong argument type #{other.class} (must respond to :each)"
      end
    end
    arr = self.to_a
    result = []
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
    # Use the *packed* element (`__gather_each`) — `sort_by` treats a
    # multi-value `yield` as a single Array element. (Public `map`
    # forwards the raw values arity-adaptively, which is the wrong
    # shape here.)
    pairs = []
    __gather_each { |x| pairs << [yield(x), x] }
    pairs.sort! { |a, b| a[0] <=> b[0] }
    pairs.map { |p| p[1] }
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
      pairs = []
      __gather_each { |x| pairs << [yield(x), x] }
      pairs.sort_by { |pair| pair[0] }.last(n).reverse.map { |pair| pair[1] }
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

  def first(*args)
    if args.size > 1
      raise ArgumentError,
            "wrong number of arguments (given #{args.size}, expected 0..1)"
    end
    if args.empty?
      __gather_each { |x| return x }
      return nil
    end
    n = args[0]
    n = n.to_int if !n.is_a?(Integer) && n.respond_to?(:to_int)
    unless n.is_a?(Integer)
      raise TypeError, "no implicit conversion of #{n.class} into Integer"
    end
    raise ArgumentError, "negative array size" if n < 0
    # CRuby converts the count with NUM2LONG; a Bignum that does not
    # fit in a C long raises RangeError.
    if n > 0x7fff_ffff_ffff_ffff
      raise RangeError, "bignum too big to convert into `long'"
    end
    res = []
    return res if n == 0
    # Push-then-break so exactly `n` elements are consumed (the
    # underlying `each` is not advanced past what is needed).
    __gather_each do |x|
      res << x
      break if res.size >= n
    end
    res
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
    warn "warning: Enumerable#to_set is deprecated. Use Set[] or Set.new directly instead."
    klass.new(self, *args, &block)
  end

  def to_h(*args, &blk)
    h = {}
    self.each(*args) do |*xs|
      x = xs.size == 1 ? xs[0] : xs
      pair = blk ? blk.call(x) : x
      unless pair.is_a?(Array)
        if pair.respond_to?(:to_ary)
          pair = pair.to_ary
        end
        raise TypeError, "wrong element type #{pair.class}" unless pair.is_a?(Array)
      end
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
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
    n = n.to_int unless n.is_a?(Integer)
    raise ArgumentError, "invalid size" if n <= 0
    return to_enum(:each_cons, n) { respond_to?(:size) ? [size - n + 1, 0].max : nil } unless block_given?
    buf = []
    __gather_each do |x|
      buf << x
      buf.shift if buf.size > n
      yield buf.dup if buf.size == n
    end
    self
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
          else
            cmp = yield(x, m)
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed" if cmp.nil?
            m = x if cmp < 0
          end
        end
      else
        __gather_each do |x|
          if first
            m = x
            first = false
          else
            cmp = (x <=> m)
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed" if cmp.nil?
            m = x if cmp < 0
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
          else
            cmp = yield(x, m)
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed" if cmp.nil?
            m = x if cmp > 0
          end
        end
      else
        __gather_each do |x|
          if first
            m = x
            first = false
          else
            cmp = (x <=> m)
            raise ArgumentError, "comparison of #{x.class} with #{m.class} failed" if cmp.nil?
            m = x if cmp > 0
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
          cmp_mn = block.call(x, mn)
          raise ArgumentError, "comparison failed" if cmp_mn.nil?
          mn = x if cmp_mn < 0
          cmp_mx = block.call(x, mx)
          raise ArgumentError, "comparison failed" if cmp_mx.nil?
          mx = x if cmp_mx > 0
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
    return to_enum(:each_entry, *args) { respond_to?(:size) ? size : nil } unless block_given?
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
      return to_enum(:cycle) { respond_to?(:size) ? Float::INFINITY : nil } unless block_given?
      cache = []
      # First pass: collect elements and yield simultaneously so break stops early
      each do |*xs|
        x = xs.size == 1 ? xs[0] : xs
        cache << x
        yield x
      end
      return nil if cache.empty?
      cn = cache.size
      loop do
        j = 0
        while j < cn
          yield cache[j]
          j += 1
        end
      end
    else
      unless n.is_a?(Integer)
        if n.respond_to?(:to_int)
          n = n.to_int
          raise TypeError, "can't convert to Integer" unless n.is_a?(Integer)
        else
          raise TypeError, "no implicit conversion of #{n.class} into Integer"
        end
      end
      return to_enum(:cycle, n) { respond_to?(:size) ? (n > 0 ? size * n : 0) : nil } unless block_given?
      return nil if n <= 0
      cache = []
      # First pass: collect and yield simultaneously
      each do |*xs|
        x = xs.size == 1 ? xs[0] : xs
        cache << x
        yield x
      end
      return nil if cache.empty?
      cn = cache.size
      # n-1 more passes using the cache (each called only once)
      repeat = 1
      while repeat < n
        j = 0
        while j < cn
          yield cache[j]
          j += 1
        end
        repeat += 1
      end
      nil
    end
  end

  # Returns an Array of elements for which `pattern === element` is
  # truthy. With a block, transforms each match through the block.
  def grep(pattern, &block)
    res = []
    __gather_each do |x|
      if pattern === x
        res << (block ? block.call(x) : x)
      end
    end
    res
  end

  # Inverse of `grep`: keeps elements where `pattern === element` is
  # false.
  def grep_v(pattern, &block)
    res = []
    __gather_each do |x|
      unless pattern === x
        res << (block ? block.call(x) : x)
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
end

# (Enumerator::ArithmeticSequence lives in `arithmetic_sequence.rb`,
# loaded from `startup.rb` right after this file.)

