# frozen_string_literal: true

module ActiveModel
  module Validations
    class LengthValidator
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

          length = value.nil? ? 0 : value.respond_to?(:length) ? value.length : value.to_s.length

          if options[:minimum] && length < options[:minimum]
            record.errors.add(attribute, :too_short, count: options[:minimum],
              **options.reject { |k, _| [:minimum, :maximum, :in, :within, :is, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
          end

          if options[:maximum] && length > options[:maximum]
            record.errors.add(attribute, :too_long, count: options[:maximum],
              **options.reject { |k, _| [:minimum, :maximum, :in, :within, :is, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
          end

          if options[:is] && length != options[:is]
            record.errors.add(attribute, :wrong_length, count: options[:is],
              **options.reject { |k, _| [:minimum, :maximum, :in, :within, :is, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
          end

          range = options[:in] || options[:within]
          if range
            if length < range.first
              record.errors.add(attribute, :too_short, count: range.first,
                **options.reject { |k, _| [:minimum, :maximum, :in, :within, :is, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
            elsif length > range.last
              record.errors.add(attribute, :too_long, count: range.last,
                **options.reject { |k, _| [:minimum, :maximum, :in, :within, :is, :if, :unless, :on, :allow_nil, :allow_blank].include?(k) })
            end
          end
        end
      end
    end
  end
end
