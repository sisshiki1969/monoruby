# frozen_string_literal: true
#
# ActiveModel::Callbacks - Lifecycle callbacks for models.
#
# Provides define_model_callbacks to set up before/after/around hooks.
# This is a simplified implementation that works without ActiveSupport::Callbacks
# when it's not available, but integrates with it when present.

module ActiveModel
  module Callbacks
    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_model_callbacks, {})
    end

    module ClassMethods
      def inherited(subclass)
        super
        parent_cbs = instance_variable_get(:@_model_callbacks) || {}
        # Deep dup callback definitions
        new_cbs = {}
        parent_cbs.each do |name, hooks|
          new_cbs[name] = {
            before: (hooks[:before] || []).dup,
            after: (hooks[:after] || []).dup,
            around: (hooks[:around] || []).dup,
          }
        end
        subclass.instance_variable_set(:@_model_callbacks, new_cbs)
      end

      def _model_callbacks
        @_model_callbacks ||= {}
      end

      # define_model_callbacks :save, :create, :update, :destroy
      def define_model_callbacks(*callbacks, **options)
        only = Array(options[:only]) if options[:only]

        callbacks.each do |callback|
          _model_callbacks[callback] = { before: [], after: [], around: [] }

          types = only || [:before, :after, :around]

          if types.include?(:before)
            # Define before_<callback> class method
            _define_callback_registrar(:before, callback)
          end

          if types.include?(:after)
            _define_callback_registrar(:after, callback)
          end

          if types.include?(:around)
            _define_callback_registrar(:around, callback)
          end
        end
      end

      private

      def _define_callback_registrar(kind, callback)
        method_name = "#{kind}_#{callback}"
        cb_name = callback
        cb_kind = kind

        # Use define_method on the singleton class
        sc = (class << self; self; end)
        sc.define_method(method_name.to_sym) do |*methods, **opts, &block|
          methods.each do |method|
            _model_callbacks[cb_name][cb_kind] << { method: method, options: opts }
          end
          if block
            _model_callbacks[cb_name][cb_kind] << { block: block, options: opts }
          end
        end
      end
    end

    private

    # Run callbacks around a block:
    #   run_callbacks(:save) { actually_save }
    def run_callbacks(kind, &block)
      cbs = self.class._model_callbacks[kind]
      return block.call if cbs.nil?

      # Run before callbacks
      (cbs[:before] || []).each do |cb|
        result = _execute_callback(cb)
        # If a before callback explicitly returns false, halt the chain
        if result == false
          return false
        end
      end

      # Run around callbacks (simplified: just nest them)
      around_cbs = cbs[:around] || []
      result = if around_cbs.empty?
        block.call
      else
        _run_around_callbacks(around_cbs, 0, block)
      end

      # Run after callbacks (in reverse order, like Rails)
      (cbs[:after] || []).reverse.each do |cb|
        _execute_callback(cb)
      end

      result
    end

    def _run_around_callbacks(callbacks, index, block)
      if index >= callbacks.length
        block.call
      else
        cb = callbacks[index]
        if cb[:method]
          send(cb[:method]) { _run_around_callbacks(callbacks, index + 1, block) }
        elsif cb[:block]
          cb[:block].call(self) { _run_around_callbacks(callbacks, index + 1, block) }
        end
      end
    end

    def _execute_callback(cb)
      opts = cb[:options] || {}

      if opts[:if]
        condition = opts[:if]
        case condition
        when Symbol
          return unless send(condition)
        when Proc
          return unless condition.call(self)
        end
      end

      if opts[:unless]
        condition = opts[:unless]
        case condition
        when Symbol
          return if send(condition)
        when Proc
          return if condition.call(self)
        end
      end

      if cb[:method]
        send(cb[:method])
      elsif cb[:block]
        cb[:block].call(self)
      end
    end
  end
end
