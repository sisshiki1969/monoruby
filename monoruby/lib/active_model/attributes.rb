# frozen_string_literal: true
#
# ActiveModel::Attributes - Attribute declaration DSL with type casting.
#
# Provides the `attribute` class method for declaring typed attributes
# on model classes.

module ActiveModel
  module Attributes
    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_attribute_definitions, {})
      base.instance_variable_set(:@_attribute_defaults, {})
    end

    module ClassMethods
      def inherited(subclass)
        super
        # Copy attribute definitions to subclass
        parent_defs = instance_variable_get(:@_attribute_definitions) || {}
        parent_defaults = instance_variable_get(:@_attribute_defaults) || {}
        subclass.instance_variable_set(:@_attribute_definitions, parent_defs.dup)
        subclass.instance_variable_set(:@_attribute_defaults, parent_defaults.dup)
      end

      def attribute_definitions
        @_attribute_definitions ||= {}
      end

      def attribute_defaults
        @_attribute_defaults ||= {}
      end

      def attribute_names
        attribute_definitions.keys.map(&:to_s)
      end

      # DSL: attribute :name, :string, default: ""
      def attribute(name, type = :value, default: nil, **options)
        name = name.to_sym
        attribute_definitions[name] = { type: type, options: options }
        attribute_defaults[name] = default

        # Define getter
        define_method(name) do
          val = instance_variable_get(:"@_attr_#{name}")
          if val.nil?
            self.class.attribute_defaults[name]
          else
            val
          end
        end

        # Define setter with type casting
        define_method(:"#{name}=") do |value|
          casted = _cast_attribute(name, value)
          instance_variable_set(:"@_attr_#{name}", casted)
        end
      end
    end

    # Initialize attributes from a hash
    def initialize(attrs = {})
      _init_attributes(attrs)
    end

    def attributes
      result = {}
      self.class.attribute_definitions.each do |name, _|
        result[name.to_s] = send(name)
      end
      result
    end

    def attribute_names
      self.class.attribute_names
    end

    private

    def _init_attributes(attrs = {})
      attrs.each do |key, value|
        setter = :"#{key}="
        if respond_to?(setter)
          send(setter, value)
        end
      end
    end

    def _cast_attribute(name, value)
      return nil if value.nil?

      defn = self.class.attribute_definitions[name]
      return value unless defn

      case defn[:type]
      when :string, "string", "String"
        value.to_s
      when :integer, "integer", "Integer"
        _cast_to_integer(value)
      when :float, "float", "Float"
        _cast_to_float(value)
      when :boolean, "boolean", "Boolean"
        _cast_to_boolean(value)
      when :decimal, "decimal", "Decimal"
        _cast_to_float(value)
      when :datetime, "datetime", "DateTime"
        value
      when :date, "date", "Date"
        value
      when :time, "time", "Time"
        value
      else
        value
      end
    end

    def _cast_to_integer(value)
      case value
      when Integer
        value
      when Float
        value.to_i
      when String
        value.empty? ? nil : value.to_i
      when true
        1
      when false
        0
      else
        value.to_i
      end
    end

    def _cast_to_float(value)
      case value
      when Float
        value
      when Integer
        value.to_f
      when String
        value.empty? ? nil : value.to_f
      else
        value.to_f
      end
    end

    def _cast_to_boolean(value)
      case value
      when true, 1, "1", "t", "true", "yes", "on", "TRUE", "True"
        true
      when false, 0, "0", "f", "false", "no", "off", "FALSE", "False", nil
        false
      else
        !!value
      end
    end
  end
end
