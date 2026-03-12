# frozen_string_literal: true
#
# ActiveSupport::DescendantsTracker for monoruby.
# Tracks descendants of a class for efficient lookup.
#
# NOTE: monoruby doesn't support `inherited` hooks.
# Classes must manually register via `register_descendant` or
# this module provides a polling-based `descendants` that walks
# known registered classes.
#

module ActiveSupport
  module DescendantsTracker
    @_direct_descendants = {}

    class << self
      def direct_descendants(klass)
        @_direct_descendants[klass] || []
      end

      def descendants(klass)
        result = []
        direct = direct_descendants(klass)
        direct.each do |d|
          result << d
          result.concat(descendants(d))
        end
        result
      end

      def clear
        @_direct_descendants.each do |klass, descs|
          @_direct_descendants[klass] = descs.select { |d| d.name }
        end
      end

      def store_inherited(klass, descendant)
        (@_direct_descendants[klass] ||= []) << descendant
      end
    end

    # Since monoruby doesn't support `inherited` hooks, provide
    # a manual registration method
    def register_descendant(descendant)
      DescendantsTracker.store_inherited(self, descendant)
    end

    def direct_descendants
      DescendantsTracker.direct_descendants(self)
    end

    def descendants
      DescendantsTracker.descendants(self)
    end

    def subclasses
      direct_descendants
    end
  end
end
