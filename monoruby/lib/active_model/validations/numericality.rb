# frozen_string_literal: true

module ActiveModel
  module Validations
    class NumericalityValidator
      attr_reader :attributes, :options

      def initialize(options = {})
        @attributes = Array(options.delete(:attributes))
        @options = options
      end

      def validate(record)
        @attributes.each do |attribute|
          value = record.send(attribute)
          next if value.nil? && options[:allow_nil]
          next if value.respond_to?(:empty?) && value.empty? && options[:allow_blank]

          raw = value

          # Check if it's a number
          unless _is_number?(raw)
            record.errors.add(attribute, :not_a_number,
              **options.reject { |k, _| _option_key?(k) })
            next
          end

          num = _to_number(raw)

          if options[:only_integer]
            unless _is_integer?(raw)
              record.errors.add(attribute, :not_an_integer,
                **options.reject { |k, _| _option_key?(k) })
              next
            end
            num = num.to_i
          end

          if options[:greater_than]
            unless num > options[:greater_than]
              record.errors.add(attribute, :greater_than, count: options[:greater_than],
                **options.reject { |k, _| _option_key?(k) })
            end
          end

          if options[:greater_than_or_equal_to]
            unless num >= options[:greater_than_or_equal_to]
              record.errors.add(attribute, :greater_than_or_equal_to, count: options[:greater_than_or_equal_to],
                **options.reject { |k, _| _option_key?(k) })
            end
          end

          if options[:less_than]
            unless num < options[:less_than]
              record.errors.add(attribute, :less_than, count: options[:less_than],
                **options.reject { |k, _| _option_key?(k) })
            end
          end

          if options[:less_than_or_equal_to]
            unless num <= options[:less_than_or_equal_to]
              record.errors.add(attribute, :less_than_or_equal_to, count: options[:less_than_or_equal_to],
                **options.reject { |k, _| _option_key?(k) })
            end
          end

          if options[:equal_to]
            unless num == options[:equal_to]
              record.errors.add(attribute, :equal_to, count: options[:equal_to],
                **options.reject { |k, _| _option_key?(k) })
            end
          end

          if options[:other_than]
            unless num != options[:other_than]
              record.errors.add(attribute, :other_than, count: options[:other_than],
                **options.reject { |k, _| _option_key?(k) })
            end
          end
        end
      end

      private

      def _option_key?(k)
        [:only_integer, :greater_than, :greater_than_or_equal_to,
         :less_than, :less_than_or_equal_to, :equal_to, :other_than,
         :if, :unless, :on, :allow_nil, :allow_blank].include?(k)
      end

      def _is_number?(value)
        case value
        when Integer, Float
          true
        when String
          !!(value =~ /\A[+-]?\d+(\.\d+)?\z/)
        else
          value.respond_to?(:to_f)
        end
      end

      def _is_integer?(value)
        case value
        when Integer
          true
        when Float
          value == value.to_i.to_f
        when String
          !!(value =~ /\A[+-]?\d+\z/)
        else
          false
        end
      end

      def _to_number(value)
        case value
        when Integer, Float
          value
        when String
          value.include?('.') ? value.to_f : value.to_i
        else
          value.to_f
        end
      end
    end
  end
end
