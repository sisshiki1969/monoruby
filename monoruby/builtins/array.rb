class Array
  include Enumerable

  def self.new(...)
    o = allocate
    o.initialize(...)
    o
  end

  def sample(n = (no_n = true; nil), random: nil)
    if no_n
      return nil if empty?
      if random
        r = random.rand(size)
        r = r.to_int unless r.is_a?(Integer)
        raise RangeError, "random number too big #{r}" if r < 0 || r >= size
        self[r]
      else
        self[rand(size)]
      end
    else
      n = n.to_int
      raise ArgumentError, "negative sample number" if n < 0
      return [] if empty? || n == 0
      n = size if n > size
      result = self.dup
      i = 0
      while i < n
        remaining = size - i
        if random
          r = random.rand(remaining)
          r = r.to_int unless r.is_a?(Integer)
          raise RangeError, "random number too big #{r}" if r < 0 || r >= remaining
          j = i + r
        else
          j = i + rand(remaining)
        end
        result[i], result[j] = result[j], result[i]
        i += 1
      end
      result[0, n]
    end
  end

  def shuffle(random: nil)
    result = Array.new(self)
    if random
      i = result.size
      while i > 1
        i -= 1
        r = random.rand(i + 1)
        r = r.to_int unless r.is_a?(Integer)
        raise RangeError, "random number too big #{r}" if r < 0 || r > i
        result[i], result[r] = result[r], result[i]
      end
      result
    else
      result.shuffle!
    end
  end

  def each
    return self.to_enum(:each) unless block_given?
    i = 0
    while i < self.size
      yield self[i]
      i += 1
    end
    self
  end

  def reverse_each
    return self.to_enum(:reverse_each) unless block_given?
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
    return self.to_enum(:each_with_index) unless block_given?
    i = 0
    while i < self.size
      yield self[i], i
      i += 1
    end
    self
  end

  def map!
    return self.to_enum(:map!) unless block_given?
    i = 0
    while i < self.size
      self[i] = yield(self[i])
      i += 1
    end
    self
  end
  alias collect! map!

  def map
    return self.to_enum(:map) unless block_given?
    res = self.dup
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
    # 判定モードを最初の呼び出しで決定
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = self[mid]
      res = yield(val)

      if mode.nil?
        if res == true || res == false
          mode = :find_min
        elsif res.is_a?(Numeric)
          mode = :find_exact
        else
          raise TypeError, "unexpected block result #{res.inspect}"
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
        if res < 0
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

  def sum(init = 0)
    if block_given?
      each { |x| init = init + yield(x) }
    else
      each { |x| init = init + x }
    end
    init
  end

  def tally
    h = {}
    each do |x|
      h[x] = (h[x] || 0) + 1
    end
    h
  end

  def filter_map
    return to_enum(:filter_map) unless block_given?
    res = []
    each do |x|
      y = yield(x)
      res << y if y
    end
    res
  end

  def cycle(n = nil)
    return to_enum(:cycle, n) unless block_given?
    return nil if empty?
    if n.nil?
      loop do
        each { |x| yield x }
      end
    else
      n = n.to_int
      n.times do
        each { |x| yield x }
      end
    end
    nil
  end

  def combination(n)
    return to_enum(:combination, n) unless block_given?
    n = n.to_int
    len = self.size
    if n == 0
      yield []
    elsif n == 1
      each { |x| yield [x] }
    elsif n > 0 && n <= len
      # iterative combination generation
      indices = (0...n).to_a
      loop do
        yield indices.map { |i| self[i] }
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
        if res == true || res == false
          mode = :find_min
        elsif res.is_a?(Numeric)
          mode = :find_exact
        else
          raise TypeError, "unexpected block result #{res.inspect}"
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
        if res < 0
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

  def permutation(n = self.size)
    return to_enum(:permutation, n) unless block_given?
    n = n.to_int
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

  def fetch_values(*indexes)
    indexes.map do |i|
      if block_given?
        idx = i.to_int
        if idx < -size || idx >= size
          yield i
        else
          self[idx]
        end
      else
        fetch(i)
      end
    end
  end

  def rindex(val = (no_val = true; nil))
    unless no_val
      i = self.size - 1
      while i >= 0
        if i >= self.size
          i = self.size - 1
          next
        end
        return i if self[i] == val
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
        return elem if elem.size > 0 && elem[0] == key
      elsif elem.respond_to?(:to_ary)
        ary = elem.to_ary
        return ary if ary.is_a?(Array) && ary.size > 0 && ary[0] == key
      end
    end
    nil
  end

  def rassoc(key)
    each do |elem|
      if elem.is_a?(Array)
        return elem if elem.size > 1 && elem[1] == key
      elsif elem.respond_to?(:to_ary)
        ary = elem.to_ary
        return ary if ary.is_a?(Array) && ary.size > 1 && ary[1] == key
      end
    end
    nil
  end

  def each_index
    return self.to_enum(:each_index) unless block_given?
    i = 0
    while i < self.size
      yield i
      i += 1
    end
    self
  end

  def repeated_permutation(n)
    return to_enum(:repeated_permutation, n) unless block_given?
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
    return to_enum(:repeated_combination, n) unless block_given?
    n = n.to_int
    len = self.size
    if n == 0
      yield []
    elsif n == 1
      each { |x| yield [x] }
    elsif len > 0 && n > 0
      indices = [0] * n
      loop do
        yield indices.map { |i| self[i] }
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