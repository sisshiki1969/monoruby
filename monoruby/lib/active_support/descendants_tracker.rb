# frozen_string_literal: true
#
# ActiveSupport::DescendantsTracker for monoruby.
# Tracks descendants of a class for efficient lookup.
#

module ActiveSupport
  module DescendantsTracker
    @@direct_descendants = {}

    class << self
      def direct_descendants(klass)
        @@direct_descendants[klass] || []
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
        @@direct_descendants.each do |klass, descendants|
          @@direct_descendants[klass] = descendants.select { |d| d.name }
        end
      end

      def store_inherited(klass, descendant)
        (@@direct_descendants[klass] ||= []) << descendant
      end
    end

    def inherited(base)
      DescendantsTracker.store_inherited(self, base)
      super
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
