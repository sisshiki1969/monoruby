# frozen_string_literal: true
#
# ActiveSupport::HashWithIndifferentAccess for monoruby.
# Treats string and symbol keys interchangeably.
#
# NOTE: Uses composition (wrapping a Hash) rather than inheritance
# due to monoruby Hash subclass alias limitations.
#

module ActiveSupport
  class HashWithIndifferentAccess
    include Enumerable

    def initialize(constructor = nil)
      @data = {}
      if constructor.is_a?(Hash)
        constructor.each do |k, v|
          @data[convert_key(k)] = convert_value(v)
        end
      end
    end

    def self.[](*args)
      h = new
      if args.length == 1 && args[0].is_a?(Hash)
        args[0].each { |k, v| h[k] = v }
      elsif args.length.even?
        i = 0
        while i < args.length
          h[args[i]] = args[i + 1]
          i += 2
        end
      end
      h
    end

    def []=(key, value)
      @data[convert_key(key)] = convert_value(value)
    end
    alias store []=

    def [](key)
      @data[convert_key(key)]
    end

    def fetch(key, *args, &block)
      k = convert_key(key)
      if @data.key?(k)
        @data[k]
      elsif args.length > 0
        args[0]
      elsif block
        block.call(key)
      else
        raise KeyError, "key not found: #{key.inspect}"
      end
    end

    def key?(key)
      @data.key?(convert_key(key))
    end
    alias include? key?
    alias has_key? key?
    alias member? key?

    def delete(key, &block)
      @data.delete(convert_key(key), &block)
    end

    def each(&block)
      @data.each(&block)
    end
    alias each_pair each

    def keys
      @data.keys
    end

    def values
      @data.values
    end

    def size
      @data.size
    end
    alias length size

    def empty?
      @data.empty?
    end

    def clear
      @data.clear
      self
    end

    def to_hash
      result = {}
      @data.each do |k, v|
        result[k] = v.is_a?(HashWithIndifferentAccess) ? v.to_hash : v
      end
      result
    end
    alias to_h to_hash

    def merge(other_hash, &block)
      dup.update(other_hash, &block)
    end

    def merge!(other_hash, &block)
      update(other_hash, &block)
    end

    def update(other_hash, &block)
      other_hash.each do |key, value|
        k = convert_key(key)
        if block && @data.key?(k)
          @data[k] = block.call(k, @data[k], convert_value(value))
        else
          @data[k] = convert_value(value)
        end
      end
      self
    end

    def reverse_merge(other_hash)
      self.class.new(other_hash).update(self)
    end

    def reverse_merge!(other_hash)
      other_hash.each do |key, value|
        k = convert_key(key)
        @data[k] = convert_value(value) unless @data.key?(k)
      end
      self
    end

    def replace(other_hash)
      @data.clear
      other_hash.each do |k, v|
        @data[convert_key(k)] = convert_value(v)
      end
      self
    end

    def dup
      result = self.class.new
      @data.each { |k, v| result.store(k, v) }
      result
    end

    def except(*keys)
      dup.tap do |hash|
        keys.each { |k| hash.delete(k) }
      end
    end

    def slice(*keys)
      result = self.class.new
      keys.each do |k|
        ck = convert_key(k)
        result.store(ck, @data[ck]) if @data.key?(ck)
      end
      result
    end

    def extract!(*keys)
      result = self.class.new
      keys.each do |key|
        ck = convert_key(key)
        if @data.key?(ck)
          result.store(ck, @data.delete(ck))
        end
      end
      result
    end

    def select(&block)
      return to_enum(:select) unless block
      result = self.class.new
      @data.each do |k, v|
        result.store(k, v) if block.call(k, v)
      end
      result
    end

    def reject(&block)
      return to_enum(:reject) unless block
      result = self.class.new
      @data.each do |k, v|
        result.store(k, v) unless block.call(k, v)
      end
      result
    end

    def map(&block)
      @data.map(&block)
    end

    def any?(&block)
      if block
        @data.any?(&block)
      else
        !@data.empty?
      end
    end

    def dig(key, *rest)
      val = self[key]
      if rest.empty? || val.nil?
        val
      elsif val.respond_to?(:dig)
        val.dig(*rest)
      else
        raise TypeError, "#{val.class} does not have #dig method"
      end
    end

    def stringify_keys
      dup
    end

    def stringify_keys!
      self
    end

    def symbolize_keys
      h = {}
      @data.each { |key, value| h[key.to_sym] = value }
      h
    end

    def deep_symbolize_keys
      _deep_transform(symbolize: true)
    end

    def to_options
      self
    end
    alias to_options! to_options

    def respond_to?(method_name, include_private = false)
      super || @data.respond_to?(method_name, include_private)
    end

    def inspect
      @data.inspect
    end

    def to_s
      @data.to_s
    end

    def ==(other)
      if other.is_a?(HashWithIndifferentAccess)
        @data == other.to_hash
      elsif other.is_a?(Hash)
        @data == self.class.new(other).to_hash
      else
        false
      end
    end

    def is_a?(klass)
      klass == Hash || klass == HashWithIndifferentAccess || super
    end
    alias kind_of? is_a?

    # Allow Hash-like method_missing delegation
    def method_missing(method_name, *args, &block)
      if @data.respond_to?(method_name)
        @data.send(method_name, *args, &block)
      else
        super
      end
    end

    private

    def convert_key(key)
      key.is_a?(Symbol) ? key.to_s : key
    end

    def convert_value(value)
      case value
      when HashWithIndifferentAccess
        value
      when Hash
        self.class.new(value)
      when Array
        value.map { |v| convert_value(v) }
      else
        value
      end
    end

    def _deep_transform(symbolize: false)
      result = {}
      @data.each do |k, v|
        new_key = symbolize ? k.to_sym : k
        new_value = case v
                    when HashWithIndifferentAccess
                      v._deep_transform(symbolize: symbolize)
                    when Hash
                      self.class.new(v)._deep_transform(symbolize: symbolize)
                    else
                      v
                    end
        result[new_key] = new_value
      end
      result
    end

    protected :_deep_transform
  end
end

# Alias at top level for convenience
HashWithIndifferentAccess = ActiveSupport::HashWithIndifferentAccess unless defined?(HashWithIndifferentAccess)
