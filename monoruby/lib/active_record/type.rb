# frozen_string_literal: true
#
# ActiveRecord::Type - Type casting system for database columns.

module ActiveRecord
  module Type
    class Value
      def cast(value)
        value
      end

      def serialize(value)
        value
      end

      def deserialize(value)
        cast(value)
      end

      def type
        :value
      end
    end

    class String < Value
      def cast(value)
        return nil if value.nil?
        value.to_s
      end

      def type
        :string
      end
    end

    class Text < String
      def type
        :text
      end
    end

    class Integer < Value
      def cast(value)
        return nil if value.nil?
        case value
        when ::Integer
          value
        when ::Float
          value.to_i
        when ::String
          return nil if value.empty?
          value.to_i
        when true then 1
        when false then 0
        else
          value.to_i
        end
      end

      def type
        :integer
      end
    end

    class Float < Value
      def cast(value)
        return nil if value.nil?
        case value
        when ::Float
          value
        when ::Integer
          value.to_f
        when ::String
          return nil if value.empty?
          value.to_f
        else
          value.to_f
        end
      end

      def type
        :float
      end
    end

    class Boolean < Value
      def cast(value)
        return nil if value.nil?
        case value
        when true, 1, "1", "t", "true", "yes", "TRUE", "True"
          true
        when false, 0, "0", "f", "false", "no", "FALSE", "False"
          false
        else
          !!value
        end
      end

      def serialize(value)
        return nil if value.nil?
        value ? 1 : 0
      end

      def type
        :boolean
      end
    end

    class DateTime < Value
      def cast(value)
        return nil if value.nil?
        return value if value.is_a?(::Time)
        value.to_s
      end

      def type
        :datetime
      end
    end

    class Date < Value
      def cast(value)
        return nil if value.nil?
        value.to_s
      end

      def type
        :date
      end
    end

    class Decimal < Float
      def type
        :decimal
      end
    end

    class Binary < Value
      def type
        :binary
      end
    end

    # Map SQL type strings to Type objects
    REGISTRY = {}

    def self.register(type_name, type_class)
      REGISTRY[type_name.to_s.downcase] = type_class
    end

    def self.lookup(type_name)
      type_str = type_name.to_s.downcase
      # Try exact match first
      if REGISTRY.key?(type_str)
        return REGISTRY[type_str].new
      end
      # Try partial match for SQL types like VARCHAR(255)
      REGISTRY.each do |key, klass|
        if type_str.include?(key)
          return klass.new
        end
      end
      Value.new
    end

    # Register default types
    register "string", String
    register "text", Text
    register "varchar", String
    register "char", String
    register "integer", Integer
    register "int", Integer
    register "bigint", Integer
    register "smallint", Integer
    register "tinyint", Integer
    register "float", Float
    register "real", Float
    register "double", Float
    register "numeric", Decimal
    register "decimal", Decimal
    register "boolean", Boolean
    register "bool", Boolean
    register "datetime", DateTime
    register "timestamp", DateTime
    register "date", Date
    register "blob", Binary
    register "binary", Binary
  end
end
