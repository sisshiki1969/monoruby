class Enumerator
  include Enumerable
end

class CallerLocation
  def initialize(path, lineno, label)
    @path = path
    @lineno = lineno
    @label = label
  end
  attr_reader :path, :lineno, :label

  def base_label
    l = @label
    l = $1 while l =~ /\Ablock in (.+)\z/
    l
  end

  def to_s
    "#{@path}:#{@lineno}:in '#{@label}'"
  end
  alias inspect to_s
end

def caller_locations(start = 1, length = nil)
  frames = caller(start + 1)
  return nil if frames.nil?
  frames = frames[0, length] if length
  frames.map do |frame|
    if frame =~ /\A(.+):(\d+):in ['`](.+)'\z/
      CallerLocation.new($1, $2.to_i, $3)
    else
      CallerLocation.new(frame, 0, "")
    end
  end
end

class TrueClass
  def ^(other)
    !other
  end

  def |(other)
    true
  end

  def &(other)
    !!other
  end
end

class FalseClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end
end

class NilClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end

  def to_i
    0
  end
end

class Set
  include Enumerable
end

class Hash
  include Enumerable

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    return self unless block_given?
    h = {}
    self.each {|k, v|
      new_kv = yield k, v
      new_k = new_kv[0]
      new_v = new_kv[1]
      h[new_k] = new_v
    }
    h
  end

  def transform_keys(&block)
    return to_enum(:transform_keys) unless block
    h = {}
    each { |k, v| h[block.call(k)] = v }
    h
  end

  def transform_keys!(&block)
    return to_enum(:transform_keys!) unless block
    keys.each { |k| self[block.call(k)] = delete(k) }
    self
  end

  def transform_values(&block)
    return to_enum(:transform_values) unless block
    h = {}
    each { |k, v| h[k] = block.call(v) }
    h
  end

  def transform_values!(&block)
    return to_enum(:transform_values!) unless block
    each { |k, v| self[k] = block.call(v) }
    self
  end

  def slice(*keys)
    h = {}
    keys.each { |k| h[k] = self[k] if key?(k) }
    h
  end

  def except(*keys)
    h = dup
    keys.each { |k| h.delete(k) }
    h
  end

  def dig(key, *rest)
    val = self[key]
    return val if rest.empty? || val.nil?
    raise TypeError, "#{val.class} does not have #dig method" unless val.respond_to?(:dig)
    val.dig(*rest)
  end

  def each_with_object(obj)
    return to_enum(:each_with_object, obj) unless block_given?
    each { |k, v| yield [k, v], obj }
    obj
  end

  def any?
    if block_given?
      each { |k, v| return true if yield([k, v]) }
    else
      return !empty?
    end
    false
  end

  def all?
    if block_given?
      each { |k, v| return false unless yield([k, v]) }
    else
      each { |k, v| return false unless v }
    end
    true
  end

  def count(*args)
    if block_given?
      n = 0
      each { |k, v| n += 1 if yield([k, v]) }
      n
    elsif args.empty?
      size
    else
      n = 0
      target = args[0]
      each { |k, v| n += 1 if [k, v] == target }
      n
    end
  end

  def flat_map
    return to_enum(:flat_map) unless block_given?
    res = []
    each { |k, v|
      r = yield(k, v)
      if r.is_a?(Array)
        res.concat(r)
      else
        res << r
      end
    }
    res
  end

  def min_by
    return to_enum(:min_by) unless block_given?
    min_entry = nil
    min_val = nil
    each do |k, v|
      val = yield(k, v)
      if min_val.nil? || (val <=> min_val) < 0
        min_entry = [k, v]
        min_val = val
      end
    end
    min_entry
  end
end

class Module
  def ruby2_keywords(*)
    # no-op for compatibility
  end
end

class String
  def insert(index, other)
    if index < 0
      index = self.size + 1 + index
    end
    if index < 0 || index > self.size
      raise IndexError, "index #{index} out of string"
    end
    self[index, 0] = other
    self
  end
end

class Float
  def zero?
    self == 0.0
  end

  def positive?
    self > 0.0
  end

  def negative?
    self < 0.0
  end

  # truncate and ceil are defined in Rust for JIT compatibility

  def integer?
    false
  end

  def coerce(other)
    if other.is_a?(Integer)
      [other.to_f, self]
    elsif other.is_a?(Float)
      [other, self]
    else
      raise TypeError, "#{other.class} can't be coerced into Float"
    end
  end

  def remainder(other)
    r = self % other
    if r != 0 && (self < 0) != (other < 0)
      r - other
    else
      r
    end
  end

  def numerator
    if defined?(Rational)
      to_r.numerator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def denominator
    if defined?(Rational)
      to_r.denominator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def to_r
    if defined?(Rational)
      Rational(self)
    else
      self
    end
  end

  def rationalize(eps = nil)
    if defined?(Rational)
      Rational(self)
    else
      self
    end
  end

  def fdiv(other)
    self / other.to_f
  end
end

class Numeric
  def positive?
    self > 0
  end

  def negative?
    self < 0
  end

  def abs
    self < 0 ? -self : self
  end
  alias magnitude abs

  def to_int
    to_i
  end

  def ceil(ndigits = 0)
    to_f.ceil(ndigits)
  end

  def floor(ndigits = 0)
    to_f.floor(ndigits)
  end

  def round(ndigits = 0)
    to_f.round(ndigits)
  end

  def truncate(ndigits = 0)
    to_f.truncate(ndigits)
  end

  def step(limit = nil, step = nil, by: nil, to: nil)
    limit = to if to
    step = by if by
    step ||= 1
    raise ArgumentError, "step can't be 0" if step == 0
    return to_enum(:step, limit, step) unless block_given?
    i = self
    if step > 0
      while limit.nil? || i <= limit
        yield i
        i += step
      end
    else
      while limit.nil? || i >= limit
        yield i
        i += step
      end
    end
    self
  end
end

class Proc
  def curry(arity = nil)
    arity ||= self.arity
    arity = -arity - 1 if arity < 0
    return self if arity == 0
    orig = self
    curried = nil
    curried = lambda do |collected_args|
      lambda do |*args|
        new_args = collected_args + args
        if new_args.size >= arity
          orig.call(*new_args)
        else
          curried.call(new_args)
        end
      end
    end
    curried.call([])
  end
end

class Symbol
  def [](*args)
    to_s.[](*args)
  end

  def size
    to_s.size
  end

  def length
    to_s.length
  end

  def empty?
    to_s.empty?
  end

  def upcase
    to_s.upcase.to_sym
  end

  def downcase
    to_s.downcase.to_sym
  end

  def capitalize
    to_s.capitalize.to_sym
  end

  def swapcase
    to_s.swapcase.to_sym
  end

  def start_with?(*args)
    to_s.start_with?(*args)
  end

  def end_with?(*args)
    to_s.end_with?(*args)
  end

  def encoding
    to_s.encoding
  end

  def succ
    to_s.succ.to_sym
  end
  alias next succ

  def id2name
    to_s
  end

  def name
    to_s.freeze
  end

  def intern
    self
  end

  def to_sym
    self
  end

  def =~(other)
    to_s =~ other
  end

  def casecmp(other)
    return nil unless other.is_a?(Symbol)
    to_s.casecmp(other.to_s)
  end

  def casecmp?(other)
    return nil unless other.is_a?(Symbol)
    to_s.casecmp?(other.to_s)
  end

  def match(other)
    self.to_s.match(other)
  end

  def match?(other)
    self.to_s.match?(other)
  end

  def to_proc
    Proc.new do |*args|
      if args.size == 0
        raise ArgumentError, "no receiver given"
      end
      slf, *args = args
      slf.send(self, *args)
    end
  end
end

class String
  def +@
    self
  end
  def -@
    self
  end

  def freeze
    self
  end

  def frozen?
    true
  end

  def encode(*args, **opts)
    if opts[:xml] == :attr
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s = s.gsub("\"", "&quot;")
      "\"#{s}\""
    elsif opts[:xml] == :text
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s
    else
      self
    end
  end

  def squeeze(*args)
    if args.empty?
      gsub(/(.)\1+/, '\1')
    else
      chars_to_squeeze = args.join
      escaped = chars_to_squeeze.gsub(/[\\\[\]\-\^]/) { |c| "\\#{c}" }
      gsub(/([#{escaped}])\1+/, '\1')
    end
  end

  def squeeze!(*args)
    result = squeeze(*args)
    if result == self
      nil
    else
      replace(result)
      self
    end
  end
end
