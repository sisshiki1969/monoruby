# frozen_string_literal: true
#
# ActiveModel::Conversion - Provides to_model, to_key, to_param, to_partial_path.

module ActiveModel
  module Conversion
    def to_model
      self
    end

    def to_key
      key = respond_to?(:id) && id
      key ? [key] : nil
    end

    def to_param
      key = to_key
      key ? key.join("-") : nil
    end

    def to_partial_path
      klass_name = self.class.name || ""
      element = _conversion_underscore(klass_name.split("::").last || "")
      collection = _conversion_pluralize(element)
      "%s/_%s" % [collection, element]
    end

    private

    def _conversion_underscore(str)
      result = str.dup
      result = result.gsub(/([A-Z]+)([A-Z][a-z])/) { "#{$1}_#{$2}" }
      result = result.gsub(/([a-z\d])([A-Z])/) { "#{$1}_#{$2}" }
      result = result.tr("-", "_")
      result.downcase
    end

    def _conversion_pluralize(str)
      return str if str.empty?
      if str.end_with?("ss") || str.end_with?("us") || str.end_with?("is")
        str + "es"
      elsif str.end_with?("s") || str.end_with?("x") || str.end_with?("z") ||
            str.end_with?("ch") || str.end_with?("sh")
        str + "es"
      elsif str.end_with?("y") && str.length > 1 && !"aeiou".include?(str[-2].downcase)
        str[0..-2] + "ies"
      else
        str + "s"
      end
    end
  end
end
