# frozen_string_literal: true
#
# ActiveModel::Validations - Validation framework for models.
#
# Provides `validates`, `validate`, `valid?`, `invalid?`, and `errors`.

require 'active_model/errors'
require 'active_model/validations/presence'
require 'active_model/validations/length'
require 'active_model/validations/format'
require 'active_model/validations/numericality'
require 'active_model/validations/inclusion'
require 'active_model/validations/exclusion'

module ActiveModel
  module Validations
    VALIDATOR_MAP = {
      presence: PresenceValidator,
      length: LengthValidator,
      format: FormatValidator,
      numericality: NumericalityValidator,
      inclusion: InclusionValidator,
      exclusion: ExclusionValidator,
    }

    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_validators, [])
      base.instance_variable_set(:@_custom_validators, [])
    end

    module ClassMethods
      def inherited(subclass)
        super
        parent_validators = instance_variable_get(:@_validators) || []
        parent_custom = instance_variable_get(:@_custom_validators) || []
        subclass.instance_variable_set(:@_validators, parent_validators.dup)
        subclass.instance_variable_set(:@_custom_validators, parent_custom.dup)
      end

      def _validators_list
        @_validators ||= []
      end

      def _custom_validators_list
        @_custom_validators ||= []
      end

      # validates :name, presence: true, length: { minimum: 2 }
      def validates(*attributes, **options)
        # Extract condition options that apply to all validators
        condition_opts = {}
        condition_opts[:if] = options.delete(:if) if options.key?(:if)
        condition_opts[:unless] = options.delete(:unless) if options.key?(:unless)
        condition_opts[:on] = options.delete(:on) if options.key?(:on)
        condition_opts[:allow_nil] = options.delete(:allow_nil) if options.key?(:allow_nil)
        condition_opts[:allow_blank] = options.delete(:allow_blank) if options.key?(:allow_blank)

        options.each do |validator_key, validator_options|
          validator_class = VALIDATOR_MAP[validator_key]
          next unless validator_class

          # Build options for this validator
          vopts = case validator_options
                  when Hash
                    validator_options.merge(condition_opts)
                  when true
                    condition_opts.dup
                  else
                    condition_opts.merge(validator_key => validator_options)
                  end

          vopts[:attributes] = attributes
          _validators_list << validator_class.new(vopts)
        end
      end

      # validate :custom_method
      # validate { |record| record.errors.add(:base, "invalid") if ... }
      def validate(method_name = nil, **options, &block)
        if block
          _custom_validators_list << { block: block, options: options }
        elsif method_name
          _custom_validators_list << { method: method_name, options: options }
        end
      end

      # Collect all validators including ancestors
      def _all_validators
        validators = []
        # Walk up the ancestor chain collecting validators
        ancestors.reverse.each do |ancestor|
          if ancestor.respond_to?(:_validators_list, true) && ancestor != self
            validators.concat(ancestor._validators_list)
          end
          if ancestor.respond_to?(:_custom_validators_list, true) && ancestor != self
            validators.concat(ancestor._custom_validators_list.map { |cv| cv })
          end
        end
        validators.concat(_validators_list)
        validators
      end

      def validators
        _validators_list
      end

      def validators_on(attribute)
        _validators_list.select { |v| v.respond_to?(:attributes) && v.attributes.include?(attribute) }
      end
    end

    def errors
      @_errors ||= Errors.new(self)
    end

    def valid?(context = nil)
      errors.clear
      _run_validations(context)
      errors.empty?
    end

    def invalid?(context = nil)
      !valid?(context)
    end

    def validate!(context = nil)
      unless valid?(context)
        raise "Validation failed: %s" % [errors.full_messages.join(', ')]
      end
      true
    end

    private

    def _run_validations(context = nil)
      # Run class-level validators
      self.class._validators_list.each do |validator|
        if validator.is_a?(Hash)
          _run_custom_validator(validator, context)
        else
          _run_standard_validator(validator, context)
        end
      end

      # Run custom validators
      self.class._custom_validators_list.each do |cv|
        _run_custom_validator(cv, context)
      end
    end

    def _run_standard_validator(validator, context)
      opts = validator.options
      return if opts[:on] && opts[:on].to_s != context.to_s && context

      if opts[:if]
        return unless _evaluate_condition(opts[:if])
      end
      if opts[:unless]
        return if _evaluate_condition(opts[:unless])
      end

      validator.validate(self)
    end

    def _run_custom_validator(cv, context)
      opts = cv[:options] || {}
      return if opts[:on] && opts[:on].to_s != context.to_s && context

      if opts[:if]
        return unless _evaluate_condition(opts[:if])
      end
      if opts[:unless]
        return if _evaluate_condition(opts[:unless])
      end

      if cv[:block]
        cv[:block].call(self)
      elsif cv[:method]
        send(cv[:method])
      end
    end

    def _evaluate_condition(condition)
      case condition
      when Symbol
        send(condition)
      when Proc
        condition.call(self)
      else
        condition
      end
    end
  end
end
