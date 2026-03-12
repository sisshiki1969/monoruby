# frozen_string_literal: true
#
# ActiveSupport::Concern for monoruby.
# Provides the `included` block and `class_methods` DSL
# with dependency auto-resolution.
#

module ActiveSupport
  module Concern
    class MultipleIncludedBlocks < StandardError
      def initialize
        super "Cannot define multiple 'included' blocks for a Concern"
      end
    end

    def self.extended(base)
      base.instance_variable_set(:@_dependencies, [])
    end

    def append_features(base)
      if base.instance_variable_defined?(:@_dependencies)
        base.instance_variable_get(:@_dependencies) << self
        false
      else
        return false if base < self

        # Include dependencies first
        if instance_variable_defined?(:@_dependencies)
          @_dependencies.each do |dep|
            base.include(dep)
          end
        end

        super

        # Extend class methods
        if const_defined?(:ClassMethods, false)
          base.extend(const_get(:ClassMethods))
        end

        # Run included block
        if instance_variable_defined?(:@_included_block)
          base.class_eval(&@_included_block)
        end

        true
      end
    end

    def included(base = nil, &block)
      if base
        super
      else
        raise MultipleIncludedBlocks if instance_variable_defined?(:@_included_block)
        @_included_block = block
      end
    end

    def class_methods(&block)
      mod = if const_defined?(:ClassMethods, false)
              const_get(:ClassMethods)
            else
              mod = Module.new
              const_set(:ClassMethods, mod)
              mod
            end
      mod.module_eval(&block)
    end
  end
end
