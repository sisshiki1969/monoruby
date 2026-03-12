# frozen_string_literal: true
#
# ActiveSupport core extensions for String.
# Requires ActiveSupport::Inflector to be loaded.
#

require 'active_support/inflector'
require 'active_support/core_ext/object'

class String
  def camelize(first_letter = :upper)
    if first_letter == :upper
      ActiveSupport::Inflector.camelize(self, true)
    else
      ActiveSupport::Inflector.camelize(self, false)
    end
  end
  alias camelcase camelize

  def underscore
    ActiveSupport::Inflector.underscore(self)
  end

  def pluralize
    ActiveSupport::Inflector.pluralize(self)
  end

  def singularize
    ActiveSupport::Inflector.singularize(self)
  end

  def tableize
    ActiveSupport::Inflector.tableize(self)
  end

  def classify
    ActiveSupport::Inflector.classify(self)
  end

  def humanize(options = {})
    ActiveSupport::Inflector.humanize(self, options)
  end

  def titleize
    ActiveSupport::Inflector.titleize(self)
  end
  alias titlecase titleize

  def demodulize
    ActiveSupport::Inflector.demodulize(self)
  end

  def deconstantize
    ActiveSupport::Inflector.deconstantize(self)
  end

  def constantize
    ActiveSupport::Inflector.constantize(self)
  end

  def safe_constantize
    ActiveSupport::Inflector.safe_constantize(self)
  end

  def foreign_key(separate_class_name_and_id_with_underscore = true)
    ActiveSupport::Inflector.foreign_key(self, separate_class_name_and_id_with_underscore)
  end

  def dasherize
    ActiveSupport::Inflector.dasherize(self)
  end

  def squish
    dup.squish!
  end

  def squish!
    s = gsub(/[[:space:]]+/, " ").strip
    replace(s)
    self
  end

  # Strip tags - simple implementation
  def remove(pattern)
    gsub(pattern, "")
  end

  def truncate(truncate_at, options = {})
    return dup if length <= truncate_at
    omission = options[:omission] || "..."
    length_with_room_for_omission = truncate_at - omission.length
    if length_with_room_for_omission < 0
      length_with_room_for_omission = 0
    end
    stop = length_with_room_for_omission
    self[0, stop] + omission
  end

  def starts_with?(*prefixes)
    prefixes.any? { |prefix| start_with?(prefix) }
  end

  def ends_with?(*suffixes)
    suffixes.any? { |suffix| end_with?(suffix) }
  end
end
