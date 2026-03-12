# frozen_string_literal: true
#
# ActiveSupport core extensions for Object.
#

class Object
  # An object is blank if it's false, empty, or a whitespace string.
  def blank?
    respond_to?(:empty?) ? !!empty? : !self
  end

  def present?
    !blank?
  end

  def presence
    self if present?
  end

  def try(method_name = nil, *args, &block)
    if method_name.nil? && block
      if block.arity == 0
        instance_eval(&block)
      else
        yield self
      end
    elsif respond_to?(method_name)
      send(method_name, *args, &block)
    end
  end

  def try!(method_name = nil, *args, &block)
    if method_name.nil? && block
      if block.arity == 0
        instance_eval(&block)
      else
        yield self
      end
    else
      send(method_name, *args, &block)
    end
  end

  def in?(another_object)
    another_object.include?(self)
  rescue NoMethodError
    raise ArgumentError, "The parameter passed to #in? must respond to #include?"
  end
end

class NilClass
  def blank?
    true
  end

  def try(*args, &block)
    nil
  end

  def try!(*args, &block)
    nil
  end
end

class FalseClass
  def blank?
    true
  end
end

class TrueClass
  def blank?
    false
  end
end

class Numeric
  def blank?
    false
  end
end

class String
  def blank?
    empty? || !(/[^\s]/ === self)
  end
end

class Array
  def blank?
    empty?
  end
end

class Hash
  def blank?
    empty?
  end
end
