# frozen_string_literal: true
#
# ActiveSupport::Concern for monoruby.
# Provides the `included` block and `class_methods` DSL.
#
# NOTE: monoruby doesn't support `append_features` override, `self.extended`,
# `define_singleton_method`, or `class_eval` on `Module.new`.
# We work around all of these limitations.
#

module ActiveSupport
  module Concern
    class MultipleIncludedBlocks < StandardError
      def initialize
        super "Cannot define multiple 'included' blocks for a Concern"
      end
    end

    # These are instance methods of Concern, which become singleton methods
    # when a module does `extend Concern`

    def included(base = nil, &block)
      if block
        # Being called as DSL: `included do ... end`
        raise MultipleIncludedBlocks if instance_variable_defined?(:@_included_block)
        @_included_block = block
        _install_concern_hook
      elsif base
        # Normal included callback - forward to super if not overridden
      end
    end

    def class_methods(&block)
      # Create ClassMethods module if it doesn't exist
      unless const_defined?(:ClassMethods, false)
        # Use string class_eval since Module.new doesn't work with class_eval in monoruby
        class_eval("module ClassMethods; end")
      end
      cm = const_get(:ClassMethods)
      # Evaluate the block in the context of ClassMethods
      cm.class_eval(&block)
      _install_concern_hook
    end

    private

    def _install_concern_hook
      return if instance_variable_defined?(:@_concern_hook_installed)
      @_concern_hook_installed = true

      concern_mod = self

      # Override self.included via singleton_class.define_method
      singleton_class.define_method(:included) do |base|
        # Extend class methods
        if concern_mod.const_defined?(:ClassMethods, false)
          base.extend(concern_mod.const_get(:ClassMethods))
        end

        # Run included block
        if concern_mod.instance_variable_defined?(:@_included_block)
          base.class_eval(&concern_mod.instance_variable_get(:@_included_block))
        end
      end
    end
  end
end
