require_relative 'monitor'

class Enumerator
  include Enumerable

  # Placeholder for CRuby's Enumerator::ArithmeticSequence. We treat it as
  # an alias of Enumerator so `Numeric#step` / `Range#step` can report the
  # conventional class without a dedicated implementation.
  ArithmeticSequence = self unless defined?(ArithmeticSequence)
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

class Hash
  include Enumerable

  def to_hash
    self
  end

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    return (self.instance_of?(Hash) ? self : Hash[self]) unless block_given?
    h = {}
    self.each {|k, v|
      pair = yield k, v
      pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
      raise TypeError, "wrong element type #{pair.class} (expected array)" unless pair.is_a?(Array)
      raise ArgumentError, "wrong array length (given #{pair.size}, expected 2)" unless pair.size == 2
      h[pair[0]] = pair[1]
    }
    h
  end

  def transform_keys(hash = nil, &block)
    return to_enum(:transform_keys) { size } unless block || hash
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
    return to_enum(:transform_keys!) { size } unless block || hash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
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
    return to_enum(:transform_values) { size } unless block
    h = {}
    h.compare_by_identity if compare_by_identity?
    each { |k, v| h[k] = block.call(v) }
    h
  end

  def transform_values!(&block)
    return to_enum(:transform_values!) { size } unless block
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    each { |k, v| self[k] = block.call(v) }
    self
  end

  def slice(*keys)
    h = {}
    h.compare_by_identity if compare_by_identity?
    keys.each { |k| h[k] = self[k] if key?(k) }
    h
  end

  def except(*keys)
    h = dup
    h.default = nil
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

  def has_value?(value)
    each_value { |v| return true if v == value }
    false
  end
  alias value? has_value?

  def deconstruct_keys(keys)
    self
  end

  def flatten(level = 1)
    level = level.to_int if level.respond_to?(:to_int) && !level.is_a?(Integer)
    raise TypeError, "no implicit conversion of #{level.class} into Integer" unless level.is_a?(Integer)
    to_a.flatten(level)
  end

  def fetch_values(*keys, &block)
    keys.map { |k| fetch(k, &block) }
  end

  def to_proc
    hash = self
    ->(k) { hash[k] }
  end

  def rehash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    pairs = to_a
    clear
    pairs.each { |k, v| self[k] = v }
    self
  end

  def self.ruby2_keywords_hash?(h)
    raise TypeError, "no implicit conversion of #{h.class} into Hash" unless h.is_a?(Hash)
    false
  end

  def self.ruby2_keywords_hash(h)
    raise TypeError, "no implicit conversion of #{h.class} into Hash" unless h.is_a?(Hash)
    h.dup
  end
end

class Module
  def ruby2_keywords(*)
    # no-op for compatibility
  end

  def deprecate_constant(*)
    self
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
