require_relative 'monitor'

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

  def transform_keys(hash = nil, &block)
    return to_enum(:transform_keys) unless block || hash
    h = {}
    if hash
      each do |k, v|
        new_k = hash.key?(k) ? hash[k] : (block ? block.call(k) : k)
        h[new_k] = v
      end
    else
      each { |k, v| h[block.call(k)] = v }
    end
    h
  end

  def transform_keys!(hash = nil, &block)
    return to_enum(:transform_keys!) unless block || hash
    if hash
      keys.each do |k|
        if hash.key?(k)
          self[hash[k]] = delete(k)
        elsif block
          self[block.call(k)] = delete(k)
        end
      end
    else
      keys.each { |k| self[block.call(k)] = delete(k) }
    end
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

  def any?(*pattern)
    if block_given?
      each { |k, v| return true if yield([k, v]) }
    elsif pattern.size == 1
      pat = pattern[0]
      each { |k, v| return true if pat === [k, v] }
    else
      return !empty?
    end
    false
  end

  def all?(*pattern)
    if block_given?
      each { |k, v| return false unless yield([k, v]) }
    elsif pattern.size == 1
      pat = pattern[0]
      each { |k, v| return false unless pat === [k, v] }
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

class File
  class Stat
    def initialize(path)
      @path = path
      @mode = File.exist?(path) ? 0o755 : 0
    end

    def world_writable?
      (@mode & 0o002) != 0
    end

    def sticky?
      (@mode & 0o1000) != 0
    end

    def directory?
      File.directory?(@path)
    end

    def file?
      File.file?(@path)
    end

    def size
      File.size(@path)
    end
  end

  def self.stat(path)
    Stat.new(path)
  end

  def self.lstat(path)
    stat(path)
  end
end

class String
  def insert(index, other)
    index = index.is_a?(Integer) ? index : __to_int(index)
    if index < 0
      index = self.size + 1 + index
    end
    if index < 0 || index > self.size
      raise IndexError, "index #{index} out of string"
    end
    self[index, 0] = other
    self
  end

  def concat(*args)
    args.each do |arg|
      self << arg
    end
    self
  end
  alias append concat

  def prepend(*args)
    args.reverse_each do |arg|
      self[0, 0] = arg
    end
    self
  end

  def reverse
    chars.reverse.join
  end

  def reverse!
    replace(reverse)
  end

  def chop
    return "" if empty?
    if self[-1] == "\n" && length > 1 && self[-2] == "\r"
      self[0..-3]
    else
      self[0..-2]
    end
  end

  def chop!
    return nil if empty?
    result = chop
    replace(result)
    self
  end

  def squeeze(*args)
    result = dup
    result.squeeze!(*args)
    result
  end

  def squeeze!(*args)
    original = self.dup
    if args.empty?
      # Squeeze all repeated characters
      new_str = ""
      prev = nil
      each_char do |c|
        if c != prev
          new_str << c
          prev = c
        end
      end
    else
      chars_to_squeeze = args.map { |a| a.is_a?(String) ? a : __to_str(a) }.join
      new_str = ""
      prev = nil
      each_char do |c|
        if c == prev && chars_to_squeeze.include?(c)
          next
        end
        new_str << c
        prev = c
      end
    end
    if new_str == original
      nil
    else
      replace(new_str)
      self
    end
  end

  def delete_suffix(suffix)
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    if end_with?(s)
      self[0, length - s.length]
    else
      dup
    end
  end

  def delete_suffix!(suffix)
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    if end_with?(s)
      result = self[0, length - s.length]
      replace(result)
      self
    else
      nil
    end
  end

  def partition(sep)
    if sep.is_a?(Regexp)
      m = match(sep)
      if m
        [m.pre_match, m[0], m.post_match]
      else
        [self, "", ""]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = index(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        [self, "", ""]
      end
    end
  end

  def rpartition(sep)
    if sep.is_a?(Regexp)
      # Find the last match
      last_match = nil
      pos = 0
      while (m = match(sep, pos))
        last_match = m
        pos = m.begin(0) + 1
        break if pos > length
      end
      if last_match
        [last_match.pre_match, last_match[0], last_match.post_match]
      else
        ["", "", self]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = rindex(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        ["", "", self]
      end
    end
  end

  def each_byte(&block)
    return to_enum(:each_byte) unless block
    bytes.each(&block)
    self
  end

  def upto(max, exclusive = false, &block)
    return to_enum(:upto, max, exclusive) unless block
    current = self
    if exclusive
      while current < max && current.length <= max.length
        block.call(current)
        current = current.succ
      end
    else
      while (current <=> max) <= 0 && current.length <= max.length
        block.call(current)
        current = current.succ
      end
    end
    self
  end

  def codepoints
    each_codepoint.to_a
  end

  def each_codepoint(&block)
    return enum_for(:each_codepoint) unless block
    each_char { |c| block.call(c.ord) }
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
    return self if nan? || infinite?
    if defined?(Rational)
      to_r.numerator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def denominator
    return 0 if nan?
    return 1 if infinite?
    if defined?(Rational)
      to_r.denominator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def to_r
    raise FloatDomainError, "NaN" if nan?
    raise FloatDomainError, (self > 0 ? "Infinity" : "-Infinity") if infinite?
    if defined?(Rational)
      Rational.__float_to_rational(self)
    else
      self
    end
  end

  def rationalize(eps = nil)
    raise FloatDomainError, "NaN" if nan?
    raise FloatDomainError, (self > 0 ? "Infinity" : "-Infinity") if infinite?
    if eps
      return Rational.__float_find_simplest(self, eps)
    end
    if defined?(Rational)
      Rational.__float_to_rational(self)
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

  def to_c
    Complex(self, 0)
  end

  def to_r
    Rational(self)
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
  alias slice []

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

  def match(other, *args, &block)
    self.to_s.match(other, *args, &block)
  end

  def match?(other, pos = 0)
    self.to_s.match?(other, pos)
  end

  def to_proc
    m = self
    lambda do |recv, *args, &blk|
      recv.public_send(m, *args, &blk)
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

class Dir
  include Enumerable

  def self.open(path, encoding: nil, &block)
    dir = new(path)
    if block
      begin
        result = block.call(dir)
      ensure
        dir.close
      end
      result
    else
      dir
    end
  end

  def initialize(path)
    path = path.to_path if path.respond_to?(:to_path)
    path = path.to_str if path.respond_to?(:to_str)
    raise TypeError, "no implicit conversion of #{path.class} into String" unless path.is_a?(String)
    raise Errno::ENOENT, "No such file or directory @ dir_initialize - #{path}" unless File.directory?(path)
    @path = path
    @entries = Dir.entries(path)
    @pos = 0
    @closed = false
  end

  def read
    raise IOError, "closed directory" if @closed
    return nil if @pos >= @entries.length
    entry = @entries[@pos]
    @pos += 1
    entry
  end

  def each(&block)
    raise IOError, "closed directory" if @closed
    return to_enum(:each) unless block
    @entries.each { |e| block.call(e) }
    self
  end

  def children
    raise IOError, "closed directory" if @closed
    @entries.reject { |e| e == "." || e == ".." }
  end

  def each_child(&block)
    raise IOError, "closed directory" if @closed
    return to_enum(:each_child) unless block
    children.each { |e| block.call(e) }
    self
  end

  def rewind
    raise IOError, "closed directory" if @closed
    @pos = 0
    self
  end

  def pos
    raise IOError, "closed directory" if @closed
    @pos
  end

  alias tell pos

  def pos=(newpos)
    raise IOError, "closed directory" if @closed
    @pos = newpos
  end

  alias seek pos=

  def close
    @closed = true
    nil
  end

  def path
    @path
  end

  alias to_path path

  def inspect
    "#<Dir:#{@path}>"
  end

  def self.children(path)
    entries(path).reject { |e| e == "." || e == ".." }
  end

  def self.each_child(path, &block)
    return to_enum(:each_child, path) unless block
    children(path).each { |e| block.call(e) }
  end

  def self.empty?(path)
    children(path).empty?
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
        e = e.nil? ? size - 1 : e.to_int
        b += size if b < 0
        e += size if e < 0
        e -= 1 if s.exclude_end?
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
end

class File
  def self.zero?(path)
    s = File.size(path) rescue return false
    s == 0
  end
end

module FileTest
  def self.zero?(path)
    File.zero?(path)
  end
end
