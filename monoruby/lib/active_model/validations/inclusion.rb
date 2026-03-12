# frozen_string_literal: true

module ActiveModel
  module Validations
    class InclusionValidator
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

          list = options[:in] || options[:within]
          unless list && list.include?(value)
            record.errors.add(attribute, :inclusion,
              **options.reject { |k, _| [:in, :within, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
          end
        end
      end
    end
  end
end
