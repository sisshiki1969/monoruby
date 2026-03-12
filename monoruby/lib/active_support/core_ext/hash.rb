# frozen_string_literal: true
#
# ActiveSupport core extensions for Hash.
#

require 'active_support/hash_with_indifferent_access'

class Hash
  def stringify_keys
    result = {}
    each { |key, value| result[key.to_s] = value }
    result
  end

  def stringify_keys!
    keys.each do |key|
      val = delete(key)
      self[key.to_s] = val
    end
    self
  end

  def symbolize_keys
    result = {}
    each { |key, value| result[key.to_sym] = value }
    result
  end
  alias to_options symbolize_keys

  def symbolize_keys!
    keys.each do |key|
      val = delete(key)
      self[key.to_sym] = val
    end
    self
  end
  alias to_options! symbolize_keys!

  def deep_symbolize_keys
    _deep_transform_keys { |key| key.to_sym }
  end

  def deep_symbolize_keys!
    _deep_transform_keys_in_place! { |key| key.to_sym }
    self
  end

  def deep_stringify_keys
    _deep_transform_keys { |key| key.to_s }
  end

  def deep_stringify_keys!
    _deep_transform_keys_in_place! { |key| key.to_s }
    self
  end

  def reverse_merge(other_hash)
    other_hash.merge(self)
  end
  alias with_defaults reverse_merge

  def reverse_merge!(other_hash)
    replace(reverse_merge(other_hash))
  end
  alias with_defaults! reverse_merge!

  unless method_defined?(:except)
    def except(*keys)
      dup.tap do |hash|
        keys.each { |key| hash.delete(key) }
      end
    end
  end

  unless method_defined?(:slice)
    def slice(*keys)
      result = {}
      keys.each do |k|
        result[k] = self[k] if key?(k)
      end
      result
    end
  end

  def extract!(*keys)
    result = {}
    keys.each do |key|
      if key?(key)
        result[key] = delete(key)
      end
    end
    result
  end

  def with_indifferent_access
    ActiveSupport::HashWithIndifferentAccess.new(self)
  end

  def deep_merge(other_hash, &block)
    dup.deep_merge!(other_hash, &block)
  end

  def deep_merge!(other_hash, &block)
    other_hash.each do |key, other_value|
      this_value = self[key]
      if this_value.is_a?(Hash) && other_value.is_a?(Hash)
        self[key] = this_value.deep_merge(other_value, &block)
      elsif block && key?(key)
        self[key] = block.call(key, this_value, other_value)
      else
        self[key] = other_value
      end
    end
    self
  end

  private

  def _deep_transform_keys(&block)
    result = {}
    each do |key, value|
      new_key = block.call(key)
      new_value = if value.is_a?(Hash)
                    value._deep_transform_keys(&block)
                  elsif value.is_a?(Array)
                    value.map { |v| v.is_a?(Hash) ? v._deep_transform_keys(&block) : v }
                  else
                    value
                  end
      result[new_key] = new_value
    end
    result
  end

  def _deep_transform_keys_in_place!(&block)
    keys.each do |key|
      value = delete(key)
      new_key = block.call(key)
      if value.is_a?(Hash)
        value._deep_transform_keys_in_place!(&block)
      end
      self[new_key] = value
    end
  end

  # Make these accessible for deep transforms
  protected :_deep_transform_keys
  protected :_deep_transform_keys_in_place!
end
