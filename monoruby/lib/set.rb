# frozen_string_literal: true
#
# Set class implementation for monoruby.
# Uses Hash as the backend storage (like CRuby).
#

class Set
  include Enumerable

  # --- Class methods ---

  def self.[](*elements)
    new(elements)
  end

  # --- Instance methods ---

  def initialize(enum = nil, &block)
    @hash = {}
    if enum
      if block
        enum.each { |o| add(block.call(o)) }
      else
        merge(enum)
      end
    end
  end

  # --- Core operations ---

  def add(o)
    @hash[o] = true
    self
  end
  alias << add

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
  alias member? include?
  alias === include?

  def clear
    @hash.clear
    self
  end

  def empty?
    @hash.empty?
  end

  def size
    @hash.size
  end
  alias length size

  # --- Enumerable ---

  def each(&block)
    return to_enum(:each) unless block
    @hash.each_key(&block)
    self
  end

  def map(&block)
    return to_enum(:map) unless block
    result = self.class.new
    each { |o| result.add(block.call(o)) }
    result
  end
  alias collect map

  def select(&block)
    return to_enum(:select) unless block
    result = self.class.new
    each { |o| result.add(o) if block.call(o) }
    result
  end
  alias filter select

  def reject(&block)
    return to_enum(:reject) unless block
    result = self.class.new
    each { |o| result.add(o) unless block.call(o) }
    result
  end

  def flat_map(&block)
    return to_enum(:flat_map) unless block
    result = self.class.new
    each do |o|
      val = block.call(o)
      if val.is_a?(Array) || val.is_a?(Set)
        val.each { |v| result.add(v) }
      else
        result.add(val)
      end
    end
    result
  end
  alias collect_concat flat_map

  # --- Set operations ---

  def &(other)
    result = self.class.new
    if size <= other.size
      each { |o| result.add(o) if other.include?(o) }
    else
      other.each { |o| result.add(o) if include?(o) }
    end
    result
  end
  alias intersection &

  def |(other)
    result = dup
    other.each { |o| result.add(o) }
    result
  end
  alias union |
  alias + |

  def -(other)
    result = self.class.new
    each { |o| result.add(o) unless other.include?(o) }
    result
  end
  alias difference -

  def ^(other)
    (self | other) - (self & other)
  end

  def merge(enum)
    if enum.is_a?(Set)
      enum.each { |o| add(o) }
    elsif enum.is_a?(Array)
      enum.each { |o| add(o) }
    else
      enum.each { |o| add(o) }
    end
    self
  end

  def subtract(enum)
    enum.each { |o| delete(o) }
    self
  end

  # --- Subset / Superset ---

  def subset?(other)
    return false if size > other.size
    each { |o| return false unless other.include?(o) }
    true
  end
  alias <= subset?

  def proper_subset?(other)
    return false if size >= other.size
    subset?(other)
  end
  alias < proper_subset?

  def superset?(other)
    other.subset?(self)
  end
  alias >= superset?

  def proper_superset?(other)
    other.proper_subset?(self)
  end
  alias > proper_superset?

  def intersect?(other)
    if size < other.size
      each { |o| return true if other.include?(o) }
    else
      other.each { |o| return true if include?(o) }
    end
    false
  end

  def disjoint?(other)
    !intersect?(other)
  end

  # --- Conversion ---

  def to_a
    @hash.keys
  end

  def to_set
    self
  end

  def dup
    result = self.class.new
    each { |o| result.add(o) }
    result
  end
  alias clone dup

  def freeze
    @hash.freeze
    super
  end

  # --- Comparison ---

  def ==(other)
    if other.is_a?(Set)
      size == other.size && subset?(other)
    else
      false
    end
  end

  alias eql? ==

  def hash
    @hash.keys.map(&:hash).sort.hash
  end

  # --- String representation ---

  def to_s
    "#<Set: {#{to_a.map(&:inspect).join(', ')}}>"
  end
  alias inspect to_s

  # --- Replace / keep_if / delete_if ---

  def replace(enum)
    clear
    merge(enum)
  end

  def keep_if(&block)
    return to_enum(:keep_if) unless block
    to_a.each { |o| delete(o) unless block.call(o) }
    self
  end

  def select!(&block)
    return to_enum(:select!) unless block
    before = size
    keep_if(&block)
    size == before ? nil : self
  end
  alias filter! select!

  def delete_if(&block)
    return to_enum(:delete_if) unless block
    to_a.each { |o| delete(o) if block.call(o) }
    self
  end

  def reject!(&block)
    return to_enum(:reject!) unless block
    before = size
    delete_if(&block)
    size == before ? nil : self
  end

  def classify(&block)
    return to_enum(:classify) unless block
    h = {}
    each do |o|
      key = block.call(o)
      (h[key] ||= self.class.new).add(o)
    end
    h
  end

  def divide(&block)
    return to_enum(:divide) unless block
    # Simple implementation: group by block result
    classify(&block).values.to_set
  end

  def count(*args, &block)
    if args.empty? && !block
      size
    else
      super(*args, &block)
    end
  end

  def reset
    # Rebuild the hash (useful if contained objects have changed their hash values)
    values = to_a
    clear
    merge(values)
    self
  end
end

# Add to_set to Enumerable
module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end
