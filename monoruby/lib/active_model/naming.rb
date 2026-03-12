# frozen_string_literal: true
#
# ActiveModel::Naming - Model name introspection.
#
# Provides model_name which returns a Name object with singular, plural,
# element, collection, etc.

module ActiveModel
  class Name
    attr_reader :name, :singular, :plural, :element, :collection,
                :singular_route_key, :route_key, :param_key,
                :i18n_key, :human

    def initialize(klass, namespace = nil, name = nil)
      @name = name || klass.name || ""
      _compute_names
    end

    def to_s
      @name
    end

    def to_str
      @name
    end

    def ==(other)
      case other
      when String
        to_s == other
      when Name
        @name == other.name
      else
        false
      end
    end

    def eql?(other)
      self == other
    end

    def hash
      @name.hash
    end

    private

    def _compute_names
      # Handles simple names like "User" and namespaced like "Admin::User"
      base = @name.split("::").last || @name

      @element = _underscore(base)
      @singular = _underscore(@name.gsub("::", "_"))
      @plural = _pluralize(@singular)
      @collection = _underscore(@name.gsub("::", "/"))
      @collection = _pluralize(@collection)
      @singular_route_key = @singular
      @route_key = @plural
      @param_key = @singular
      @i18n_key = @singular.to_sym
      @human = _humanize(base)
    end

    def _underscore(str)
      result = str.dup
      # Handle :: -> /
      result = result.gsub("::", "/")
      # Insert underscore between camelCase boundaries
      # Handle ABCDef -> abc_def
      result = result.gsub(/([A-Z]+)([A-Z][a-z])/) { "#{$1}_#{$2}" }
      # Handle abcDef -> abc_def
      result = result.gsub(/([a-z\d])([A-Z])/) { "#{$1}_#{$2}" }
      result = result.tr("-", "_")
      result.downcase
    end

    def _pluralize(str)
      # Simple English pluralization rules sufficient for model names
      return str if str.empty?

      if str.end_with?("ss") || str.end_with?("us") || str.end_with?("is")
        str + "es"
      elsif str.end_with?("s") || str.end_with?("x") || str.end_with?("z") ||
            str.end_with?("ch") || str.end_with?("sh")
        str + "es"
      elsif str.end_with?("y") && !_vowel?(str[-2])
        str[0..-2] + "ies"
      else
        str + "s"
      end
    end

    def _vowel?(char)
      return false if char.nil?
      "aeiou".include?(char.downcase)
    end

    def _humanize(str)
      s = _underscore(str).tr('_', ' ')
      s.empty? ? s : (s[0].upcase + s[1..-1].to_s)
    end
  end

  module Naming
    def self.included(base)
      base.extend(ClassMethods)
    end

    module ClassMethods
      def model_name
        @_model_name ||= ActiveModel::Name.new(self)
      end
    end

    def model_name
      self.class.model_name
    end
  end
end
