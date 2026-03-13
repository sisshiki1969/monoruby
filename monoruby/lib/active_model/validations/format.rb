# frozen_string_literal: true

module ActiveModel
  module Validations
    class FormatValidator
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

          value_str = value.to_s

          if options[:with]
            pattern = options[:with]
            unless pattern.match(value_str)
              record.errors.add(attribute, :invalid,
                **options.reject { |k, _| [:with, :without, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
            end
          end

          if options[:without]
            pattern = options[:without]
            if pattern.match(value_str)
              record.errors.add(attribute, :invalid,
                **options.reject { |k, _| [:with, :without, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
            end
          end
        end
      end
    end
  end
end
