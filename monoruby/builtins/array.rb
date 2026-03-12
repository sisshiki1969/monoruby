class Array
  include Enumerable

  def self.new(...)
    o = allocate
    o.initialize(...)
    o
  end

  def sample
    return nil if self.empty?
    self[rand(self.size)]
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
end