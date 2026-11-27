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
      # CRuby checks the size, and allocates the Array, before running
      # the block: the `long` range, ARY_MAX_SIZE, then a NoMemoryError
      # when the allocator refuses it (as `__init_fill` does).
      if n > 16777216
        raise RangeError, "bignum too big to convert into 'long'" if n > 9223372036854775807
        raise ArgumentError, "array size too big" if n > 0x0fff_ffff_ffff_ffff
        __array_capa_check(n)
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

  # The methods below read and write the receiver with `__size`,
  # `__aref` and `__aset` (private, builtins/array.rs), never with
  # `size`, `[]` and `[]=`: those are ordinary method calls, which an
  # Array subclass may have overridden, and CRuby's C calls no method
  # to walk an Array (`RARRAY_LEN` / `RARRAY_AREF`). `__aref(i)` takes an
  # Integer (`rb_ary_entry`), and `__aref(start, len)` answers a plain
  # Array whatever the receiver's class, which is also how a working
  # copy is taken (`self.dup` would keep the subclass).
  #
  # The exception is the size of the Enumerator that `each_with_index`,
  # `partition`, `flat_map` and `filter_map` answer without a block:
  # those are Enumerable's methods in CRuby, whose `enum_size` does call
  # `size`.

  def each
    return self.to_enum(:each) { __size } unless block_given?
    i = 0
    while i < __size
      yield __aref(i)
      i += 1
    end
    self
  end

  # `each` as defined here, for code that walks an Array's storage
  # whatever its class's `each` says (`Set.new(ary)`).
  alias __builtin_each each
  private :__builtin_each

  def reverse_each
    return self.to_enum(:reverse_each) { __size } unless block_given?
    # `rb_ary_reverse_each`: the length is read again after every
    # element, so a block that shrinks the receiver moves the walk down
    # to what is left.
    i = __size - 1
    while i >= 0
      yield __aref(i)
      i -= 1
      i = __size - 1 if i >= __size
    end
    self
  end

  # `each_with_index`, `partition`, `flat_map` / `collect_concat`,
  # `filter_map`, `find_all` and `tally` are Enumerable's in CRuby, which
  # walk the elements with `each`: an Array subclass that overrides
  # `each` gets Enumerable's method (#2219). (`grep`, `group_by`,
  # `inject` / `reduce` and `sort_by`, written in Rust, make the same
  # test.)
  def each_with_index
    return super unless __each_basic?
    return self.to_enum(:each_with_index) { self.size } unless block_given?
    i = 0
    while i < __size
      yield __aref(i), i
      i += 1
    end
    self
  end

  def map!
    return self.to_enum(__callee__) { __size } unless block_given?
    raise FrozenError, "can't modify frozen #{self.class}: #{self.inspect}" if frozen?
    i = 0
    while i < __size
      __aset(i, yield(__aref(i)))
      i += 1
    end
    self
  end
  alias collect! map!

  def map
    # `__callee__`, not `:map`: the Enumerator records the name the
    # call site used, so `[1, 2].collect` reads `…:collect` (CRuby
    # names it with `rb_frame_this_func()`).
    return self.to_enum(__callee__) { __size } unless block_given?
    n = __size
    res = Array.new(n)
    i = 0
    while i < __size
      res[i] = yield(__aref(i))
      i += 1
    end
    # `rb_ary_collect` pushes one value per element it yielded: a block
    # that shrank the receiver leaves a shorter result, not one padded
    # with nil.
    res = res[0, i] if i < n
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
    high = __size
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = __aref(mid)
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
    mode == :find_min ? __aref(low) : nil
  end

  def dig(idx, *rest)
    val = __aref(idx)
    return val if rest.empty? || val.nil?
    # `rb_obj_dig`: the next level is asked by `rb_check_funcall`.
    return val.dig(*rest) if val.is_a?(::Array) || val.is_a?(::Hash)
    undef_ = __undef
    cur = val
    val = __check_funcall(cur, :dig, undef_, *rest)
    raise TypeError, "#{cur.class} does not have #dig method" if undef_.equal?(val)
    val
  end

  # Array#sum is implemented in Rust (see builtins/array.rs::sum) with a
  # Fixnum fast-path. Falling back to the Ruby version below would skip
  # that fast-path, so it stays out of the Array re-opens.

  def tally(hash = nil)
    return __tally_into(hash) unless hash.nil? && __each_basic?
    h = {}
    # `tally_up`: the element is the key as it is (see `__tally_into`).
    each { |x| __tally_up(h, x) }
    h
  end

  # `select` / `reject` / `partition` / `flat_map` are Ruby so that the
  # JIT compiles the block into the loop: written in Rust, every element
  # costs a call out to the block and back, and the collected values
  # have to be rooted by hand across it. Here they sit in the frame.
  def select
    return self.to_enum(__callee__) { __size } unless block_given?
    res = []
    i = 0
    while i < __size
      # The element is read again after the block, as `rb_ary_select`
      # reads it: what the block stored there (or nil, once the block
      # shrank the receiver) is what is collected.
      res << __aref(i) if yield(__aref(i))
      i += 1
    end
    res
  end
  alias filter select

  # Enumerable's in CRuby, unlike `select`: it collects each element as
  # `each` yielded it.
  def find_all
    return super unless __each_basic?
    return self.to_enum(:find_all) { self.size } unless block_given?
    res = []
    i = 0
    while i < __size
      x = __aref(i)
      res << x if yield(x)
      i += 1
    end
    res
  end

  # `none?`, `one?` and `take_while` are Array's own in CRuby
  # (`rb_ary_none_p`, `rb_ary_one_p`, `rb_ary_take_while`), which walk
  # the storage and never call `each` (#2219).
  def none?(*pattern)
    if pattern.size > 1
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
    end
    if !pattern.empty?
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      i = 0
      while i < __size
        return false if pat === __aref(i)
        i += 1
      end
    elsif block_given?
      i = 0
      while i < __size
        return false if yield(__aref(i))
        i += 1
      end
    else
      i = 0
      while i < __size
        return false if __aref(i)
        i += 1
      end
    end
    true
  end

  def one?(*pattern)
    if pattern.size > 1
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
    end
    return false if __size == 0
    found = false
    if !pattern.empty?
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      i = 0
      while i < __size
        if pat === __aref(i)
          return false if found
          found = true
        end
        i += 1
      end
    elsif block_given?
      i = 0
      while i < __size
        if yield(__aref(i))
          return false if found
          found = true
        end
        i += 1
      end
    else
      i = 0
      while i < __size
        if __aref(i)
          return false if found
          found = true
        end
        i += 1
      end
    end
    found
  end

  def take_while
    return self.to_enum(:take_while) unless block_given?
    i = 0
    while i < __size
      break unless yield(__aref(i))
      i += 1
    end
    # `rb_ary_take`: what the receiver holds now, up to that point.
    __aref(0, i)
  end

  def reject
    return self.to_enum(:reject) { __size } unless block_given?
    res = []
    i = 0
    while i < __size
      x = __aref(i)
      res << x unless yield(x)
      i += 1
    end
    res
  end

  def partition
    return super unless __each_basic?
    return self.to_enum(:partition) { self.size } unless block_given?
    first = []
    second = []
    i = 0
    while i < __size
      x = __aref(i)
      if yield(x)
        first << x
      else
        second << x
      end
      i += 1
    end
    [first, second]
  end

  def flat_map
    return super unless __each_basic?
    return self.to_enum(__callee__) { self.size } unless block_given?
    res = []
    i = 0
    while i < __size
      r = yield(__aref(i))
      if r.instance_of?(Array)
        # Up to three elements, what most blocks return, are pushed here:
        # reading and pushing one is some 40 to 50 instructions, the call
        # out to `__concat_to` about 200. Only for an Array proper, whose
        # `size` and `[]` are Array's own.
        n = r.size
        if n == 1
          res << r[0]
        elsif n == 2
          res << r[0]
          res << r[1]
        elsif n == 3
          res << r[0]
          res << r[1]
          res << r[2]
        elsif n != 0
          __concat_to(res, r)
        end
      elsif (a = __check_ary(r)).nil?
        res << r
      else
        __concat_to(res, a)
      end
      i += 1
    end
    res
  end
  alias collect_concat flat_map

  def filter_map
    return super unless __each_basic?
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
      # Size hint:
      #   * empty array            -> 0
      #   * cycle()/cycle(nil)     -> Float::INFINITY
      #   * cycle(n) with n >= 0   -> length * n
      #   * cycle(n) with n  < 0   -> 0
      # The count is converted (CRuby's NUM2LONG) only here and when the
      # Enumerator is iterated: `cycle("x")` itself raises nothing.
      args = no_n ? [] : [n]
      return to_enum(:cycle, *args) {
        len = __size
        if len == 0
          0
        elsif no_n || n.nil?
          Float::INFINITY
        else
          k = __num2long(n)
          k <= 0 ? 0 : len * k
        end
      }
    end
    # `rb_ary_cycle`: the length is read again for every round and for
    # every element, so a block that empties the receiver ends the walk.
    if n.nil?
      while __size > 0
        i = 0
        while i < __size
          yield __aref(i)
          i += 1
        end
      end
    else
      n = __num2long(n)
      while n > 0 && __size > 0
        i = 0
        while i < __size
          yield __aref(i)
          i += 1
        end
        n -= 1
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
        k = __num2long(n)
        len = __size
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
    n = __num2long(n)
    # Defensive copy: CRuby snapshots the receiver before iterating,
    # so mutations from the block don't change the generated combos.
    pool = __aref(0, __size)
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
    high = __size
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = __aref(mid)
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
    mode == :find_min ? low < __size ? low : nil : nil
  end

  # A count that is given as nil is "the whole length" to iterate, but
  # the Enumerator's size function converts whatever was given
  # (`rb_ary_permutation_size`): `permutation(nil).size` is a TypeError.
  def permutation(n = (no_n = true; nil))
    unless block_given?
      # When called with no argument the Enumerator must use the
      # receiver's size *at iteration time*, not freeze it now — CRuby
      # reflects later mutations (`a.permutation; a << x; enum.to_a`).
      # Replay with no arg so `self.size` is recomputed; with an
      # explicit `n` the Enumerator is built with that fixed length.
      # Descending factorial size = n * (n-1) * ... * (n-k+1).
      enum_args = no_n ? [] : [n]
      return to_enum(:permutation, *enum_args) {
        k = no_n ? __size : __num2long(n)
        len = __size
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
    n = n.nil? ? __size : __num2long(n)
    if n == 0
      yield []
      return self
    end
    return self if n < 0 || n > __size
    if n == __size
      # Generate all permutations
      pool = __aref(0, __size)
      indices = (0...n).to_a
      yield indices.map { |i| pool[i] }
      cycles = (__size.downto(__size - n + 1)).to_a
      loop do
        found = false
        (n - 1).downto(0) do |i|
          cycles[i] -= 1
          if cycles[i] == 0
            # Move index at i to end
            tmp = indices[i]
            (i...n - 1).each { |j| indices[j] = indices[j + 1] }
            indices[n - 1] = tmp
            cycles[i] = __size - i
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
      pool = __aref(0, __size)
      indices = (0...__size).to_a
      cycles = (__size.downto(__size - n + 1)).to_a
      yield indices[0, n].map { |i| pool[i] }
      loop do
        found = false
        (n - 1).downto(0) do |i|
          cycles[i] -= 1
          if cycles[i] == 0
            tmp = indices[i]
            (i...__size - 1).each { |j| indices[j] = indices[j + 1] }
            indices[__size - 1] = tmp
            cycles[i] = __size - i
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
    __aref(index)
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
    while i < __size
      break unless yield(__aref(i))
      i += 1
    end
    __aref(i, __size - i)
  end

  def fetch_values(*indexes)
    result = []
    if block_given?
      indexes.each do |i|
        idx = i.to_int
        if idx < -__size || idx >= __size
          result << yield(i)
        else
          result << __aref(idx)
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
      i = __size - 1
      while i >= 0
        if i >= __size
          i = __size - 1
          next
        end
        # rb_equal, not a bare `==`: an element that *is* `val` is found
        # however its `==` answers (CRuby's identity step).
        return i if __rb_equal(__aref(i), val)
        i -= 1
      end
      return nil
    end
    if block_given?
      i = __size - 1
      while i >= 0
        if i >= __size
          i = __size - 1
          next
        end
        return i if yield(__aref(i))
        i -= 1
      end
      return nil
    end
    return self.to_enum(:rindex)
  end

  def assoc(key)
    i = 0
    while i < __size
      elem = __aref(i)
      if elem.is_a?(Array)
        return elem if elem.size > 0 && __rb_equal(elem[0], key)
      elsif (ary = Array.try_convert(elem))
        return ary if ary.size > 0 && __rb_equal(ary[0], key)
      end
      i += 1
    end
    nil
  end

  def rassoc(key)
    i = 0
    while i < __size
      elem = __aref(i)
      if elem.is_a?(Array)
        return elem if elem.size > 1 && __rb_equal(elem[1], key)
      elsif (ary = Array.try_convert(elem))
        return ary if ary.size > 1 && __rb_equal(ary[1], key)
      end
      i += 1
    end
    nil
  end

  def each_index
    return self.to_enum(:each_index) { __size } unless block_given?
    i = 0
    while i < __size
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
        k = __num2long(n)
        if k < 0
          0
        elsif k == 0
          1
        else
          __size ** k
        end
      }
    end
    n = __num2long(n)
    copy = __aref(0, __size)
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
        k = __num2long(n)
        len = __size
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
    n = __num2long(n)
    # Defensive copy (see #combination): ignore block mutations.
    pool = __aref(0, __size)
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
        e = endless ? __size - 1 : e.to_int
        b += __size if b < 0
        e += __size if e < 0
        e -= 1 if !endless && s.exclude_end?
        next if b < 0
        i = b
        while i <= e
          result << __aref(i)
          i += 1
        end
      else
        result << __aref(s)
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
