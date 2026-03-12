# frozen_string_literal: true
#
# ActiveSupport::HashWithIndifferentAccess for monoruby.
# A Hash subclass that treats string and symbol keys interchangeably.
#

module ActiveSupport
  class HashWithIndifferentAccess < Hash
    def initialize(constructor = nil)
      super()
      if constructor.is_a?(Hash)
        update(constructor)
      end
    end

    def self.[](*args)
      h = new
      if args.length == 1 && args[0].is_a?(Hash)
        h.update(args[0])
      elsif args.length.even?
        i = 0
        while i < args.length
          h[args[i]] = args[i + 1]
          i += 2
        end
      end
      h
    end

    def default(key = nil)
      if key.is_a?(Symbol)
        super(key.to_s)
      else
        super
      end
    end

    def []=(key, value)
      super(convert_key(key), convert_value(value))
    end
    alias store []=

    def [](key)
      super(convert_key(key))
    end

    def fetch(key, *args, &block)
      super(convert_key(key), *args, &block)
    end

    def key?(key)
      super(convert_key(key))
    end
    alias include? key?
    alias has_key? key?
    alias member? key?

    def delete(key, &block)
      super(convert_key(key), &block)
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

    def merge(other_hash, &block)
      dup.update(other_hash, &block)
    end

    def merge!(other_hash, &block)
      update(other_hash, &block)
    end

    def update(other_hash, &block)
      if other_hash.is_a?(Hash)
        other_hash.each do |key, value|
          k = convert_key(key)
          if block && key?(k)
            value = block.call(k, self[k], convert_value(value))
            super_store(k, value)
          else
            super_store(k, convert_value(value))
          end
        end
      end
      self
    end

    def reverse_merge(other_hash)
      other = self.class.new(other_hash)
      other.update(self)
    end

    def reverse_merge!(other_hash)
      replace(reverse_merge(other_hash))
    end

    def replace(other_hash)
      super(self.class.new(other_hash))
    end

    def dup
      self.class.new.tap do |new_hash|
        each do |key, value|
          new_hash.super_store(key, value)
        end
        new_hash.default = default
      end
    end

    def to_hash
      h = {}
      each do |key, value|
        h[key] = value.is_a?(HashWithIndifferentAccess) ? value.to_hash : value
      end
      h
    end

    def select(&block)
      return to_enum(:select) unless block
      dup.tap do |hash|
        hash.clear
        each do |key, value|
          hash.super_store(key, value) if block.call(key, value)
        end
      end
    end

    def reject(&block)
      return to_enum(:reject) unless block
      dup.tap do |hash|
        hash.clear
        each do |key, value|
          hash.super_store(key, value) unless block.call(key, value)
        end
      end
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
        result.super_store(ck, self[ck]) if key?(ck)
      end
      result
    end

    def stringify_keys
      dup
    end

    def stringify_keys!
      self
    end

    def symbolize_keys
      h = {}
      each { |key, value| h[key.to_sym] = value }
      h
    end

    def to_options
      self
    end
    alias to_options! to_options

    # Internal helper to bypass convert_key
    def super_store(key, value)
      super(key, value)
    end

    private

    def convert_key(key)
      key.is_a?(Symbol) ? key.to_s : key
    end

    def convert_value(value)
      case value
      when Hash
        if value.is_a?(HashWithIndifferentAccess)
          value
        else
          self.class.new(value)
        end
      when Array
        value.map { |v| convert_value(v) }
      else
        value
      end
    end
  end
end

# Alias at top level for convenience
HashWithIndifferentAccess = ActiveSupport::HashWithIndifferentAccess unless defined?(HashWithIndifferentAccess)
