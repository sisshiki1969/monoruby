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

  def any?(*pattern)
    if block_given?
      self.each do |x|
        return true if yield(x)
      end
    elsif pattern.size == 1
      pat = pattern[0]
      self.each do |x|
        return true if pat === x
      end
    else
      self.each do |x|
        return true if x
      end
    end
    false
  end

  def none?(*pattern)
    if block_given?
      self.each do |x|
        return false if yield(x)
      end
    elsif pattern.empty?
      self.each do |x|
        return false if x
      end
    elsif pattern.size == 1
      pat = pattern[0]
      self.each do |x|
        return false if pat === x
      end
    else
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
    end
    true
  end

  def one?(*pattern)
    n = 0
    if block_given?
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
    elsif pattern.size == 1
      pat = pattern[0]
      self.each do |x|
        if pat === x
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
    if block_given?
      self.each { |x| return false unless yield(x) }
    elsif pattern.empty?
      self.each { |x| return false unless x }
    elsif pattern.size == 1
      pat = pattern[0]
      self.each { |x| return false unless pat === x }
    else
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)"
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

  def minmax
    [min, max]
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
end