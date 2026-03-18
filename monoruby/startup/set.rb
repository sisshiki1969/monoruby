# Set stub for monoruby
# In Ruby 4.0, Set is built-in. This stub provides a minimal implementation.

class Set
  include Enumerable

  def initialize(enum = nil, &block)
    @hash = {}
    if enum
      if block
        enum.each { |o| add(block.call(o)) }
      else
        enum.each { |o| add(o) }
      end
    end
  end

  def self.[](*args)
    new(args)
  end

  def add(o)
    @hash[o] = true
    self
  end
  alias_method :<<, :add

  def add?(o)
    if include?(o)
      nil
    else
      add(o)
    end
  end

  def delete(o)
    @hash.delete(o)
    self
  end

  def delete?(o)
    if include?(o)
      delete(o)
    else
      nil
    end
  end

  def include?(o)
    @hash.key?(o)
  end
  alias_method :member?, :include?

  def each(&block)
    @hash.each_key(&block)
    self
  end

  def size
    @hash.size
  end
  alias_method :length, :size

  def empty?
    @hash.empty?
  end

  def clear
    @hash.clear
    self
  end

  def to_a
    @hash.keys
  end

  def |(other)
    Set.new(to_a + other.to_a)
  end
  alias_method :union, :|
  alias_method :+, :|

  def &(other)
    Set.new(to_a.select { |o| other.include?(o) })
  end
  alias_method :intersection, :&

  def -(other)
    Set.new(to_a.reject { |o| other.include?(o) })
  end
  alias_method :difference, :-

  def ^(other)
    (self | other) - (self & other)
  end

  def subset?(other)
    return false if size > other.size
    all? { |o| other.include?(o) }
  end

  def superset?(other)
    other.subset?(self)
  end

  def ==(other)
    return false unless other.is_a?(Set)
    return false unless size == other.size
    all? { |o| other.include?(o) }
  end

  def hash
    @hash.keys.map(&:hash).sort.hash
  end

  def eql?(other)
    self == other
  end

  def merge(enum)
    enum.each { |o| add(o) }
    self
  end

  def to_set
    self
  end

  def inspect
    "#<Set: {#{to_a.map(&:inspect).join(', ')}}>"
  end
  alias_method :to_s, :inspect

  def freeze
    @hash.freeze
    super
  end
end

module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end
