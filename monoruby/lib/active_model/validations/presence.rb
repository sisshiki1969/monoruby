# frozen_string_literal: true

module ActiveModel
  module Validations
    class PresenceValidator
      attr_reader :attributes, :options

      def initialize(options = {})
        @attributes = Array(options.delete(:attributes))
        @options = options
      end

      def validate(record)
        @attributes.each do |attribute|
          value = record.send(attribute)
          if value.nil? || (value.respond_to?(:empty?) && value.empty?)
            unless options[:if] && !_evaluate_condition(record, options[:if])
              unless options[:unless] && _evaluate_condition(record, options[:unless])
                record.errors.add(attribute, :blank, **options.reject { |k, _| [:if, :unless, :on].include?(k) })
              end
            end
          end
        end
      end

      private

      def _evaluate_condition(record, condition)
        case condition
        when Symbol
          record.send(condition)
        when Proc
          condition.call(record)
        else
          condition
        end
      end
    end
  end
end
