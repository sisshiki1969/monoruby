# frozen_string_literal: true
#
# ActiveSupport::Inflector for monoruby.
# Provides English inflection rules for pluralization, singularization,
# and various string transformations needed by ActiveRecord.
#

# Polyfills for missing String methods in monoruby
unless "".respond_to?(:capitalize)
  class String
    def capitalize
      return self if empty?
      self[0].upcase + (self[1..-1] || "").downcase
    end
  end
end

unless "".respond_to?(:capitalize!)
  class String
    def capitalize!
      result = capitalize
      if result == self
        nil
      else
        replace(result)
        self
      end
    end
  end
end

module ActiveSupport
  module Inflector
    class Inflections
      attr_reader :plurals, :singulars, :uncountables, :humans, :acronyms, :acronym_regex

      def initialize
        @plurals = []
        @singulars = []
        @uncountables = []
        @humans = []
        @acronyms = {}
        @acronym_regex = /(?=a)b/  # matches nothing by default
      end

      def self.instance(locale = :en)
        @__instance__ ||= {}
        @__instance__[locale] ||= new
      end

      def acronym(word)
        @acronyms[word.downcase] = word
        regex_str = @acronyms.values.join("|")
        @acronym_regex = Regexp.new(regex_str)
      end

      def plural(rule, replacement)
        @plurals.unshift([rule, replacement])
      end

      def singular(rule, replacement)
        @singulars.unshift([rule, replacement])
      end

      def irregular(singular, plural)
        s0 = singular[0]
        srest = singular[1..-1] || ""
        p0 = plural[0]
        prest = plural[1..-1] || ""

        if s0.upcase == p0.upcase
          plural(Regexp.new("(#{s0})#{srest}$", Regexp::IGNORECASE), "\\1" + prest)
          plural(Regexp.new("(#{p0})#{prest}$", Regexp::IGNORECASE), "\\1" + prest)
          singular(Regexp.new("(#{p0})#{prest}$", Regexp::IGNORECASE), "\\1" + srest)
        else
          plural(Regexp.new("#{s0.upcase}(?i)#{srest}$"), p0.upcase + prest)
          plural(Regexp.new("#{s0.downcase}(?i)#{srest}$"), p0.downcase + prest)
          plural(Regexp.new("#{p0.upcase}(?i)#{prest}$"), p0.upcase + prest)
          plural(Regexp.new("#{p0.downcase}(?i)#{prest}$"), p0.downcase + prest)
          singular(Regexp.new("#{p0.upcase}(?i)#{prest}$"), s0.upcase + srest)
          singular(Regexp.new("#{p0.downcase}(?i)#{prest}$"), s0.downcase + srest)
        end
      end

      def uncountable(*words)
        @uncountables += words.flatten.map(&:downcase)
      end

      def human(rule, replacement)
        @humans.unshift([rule, replacement])
      end

      def clear(scope = :all)
        case scope
        when :all
          @plurals = []
          @singulars = []
          @uncountables = []
          @humans = []
          @acronyms = {}
          @acronym_regex = /(?=a)b/
        else
          instance_variable_set("@#{scope}", [])
        end
      end
    end

    def self.inflections(locale = :en)
      if block_given?
        yield Inflections.instance(locale)
      else
        Inflections.instance(locale)
      end
    end

    def self.pluralize(word)
      return word if word.nil? || word.empty?
      return word if inflections.uncountables.include?(word.downcase)

      result = word.dup
      inflections.plurals.each do |rule, replacement|
        changed = result.sub(rule, replacement)
        if changed != result
          return changed
        end
      end
      result
    end

    def self.singularize(word)
      return word if word.nil? || word.empty?
      return word if inflections.uncountables.include?(word.downcase)

      result = word.dup
      inflections.singulars.each do |rule, replacement|
        changed = result.sub(rule, replacement)
        if changed != result
          return changed
        end
      end
      result
    end

    def self.camelize(term, uppercase_first_letter = true)
      string = term.to_s
      if uppercase_first_letter
        string = string.sub(/^[a-z\d]*/) { |match|
          acronyms = inflections.acronyms
          acronyms[match] || match.capitalize
        }
      else
        string = string.sub(/^(?:#{inflections.acronym_regex}(?=\b|[A-Z_])|\w)/) { |match|
          match.downcase
        }
      end
      string = string.gsub(/(?:_|(\/))([a-z\d]*)/) { |_|
        sep = $1
        word = $2 || ""
        acronyms = inflections.acronyms
        result = acronyms[word] || word.capitalize
        if sep
          "::" + result
        else
          result
        end
      }
      string.gsub("/", "::")
    end

    def self.underscore(camel_cased_word)
      return camel_cased_word unless camel_cased_word
      word = camel_cased_word.to_s.dup
      word = word.gsub("::", "/")
      word = word.gsub(/([A-Z\d]+)([A-Z][a-z])/) { $1 + "_" + $2 }
      word = word.gsub(/([a-z\d])([A-Z])/) { $1 + "_" + $2 }
      word = word.tr("-", "_")
      word.downcase
    end

    def self.humanize(lower_case_and_underscored_word, options = {})
      result = lower_case_and_underscored_word.to_s.dup

      inflections.humans.each do |rule, replacement|
        break if result.sub!(rule, replacement)
      end

      result = result.sub(/\A_+/, "")
      result = result.sub(/_id\z/, "")
      result = result.tr("_", " ")

      if options[:capitalize] != false
        result = result.sub(/\A\w/) { |match| match.upcase }
      end

      result
    end

    def self.titleize(word)
      humanize(underscore(word)).gsub(/\b('?\w)/) { |match|
        match.capitalize
      }
    end

    def self.tableize(class_name)
      pluralize(underscore(class_name))
    end

    def self.classify(table_name)
      camelize(singularize(table_name.to_s.sub(/.*\./, "")))
    end

    def self.dasherize(underscored_word)
      underscored_word.tr("_", "-")
    end

    def self.demodulize(path)
      path = path.to_s
      if i = path.rindex("::")
        path[(i + 2)..-1]
      else
        path
      end
    end

    def self.deconstantize(path)
      path.to_s[0, path.rindex("::") || 0]
    end

    def self.foreign_key(class_name, separate_class_name_and_id_with_underscore = true)
      underscore(demodulize(class_name)) +
        (separate_class_name_and_id_with_underscore ? "_id" : "id")
    end

    def self.constantize(camel_cased_word)
      names = camel_cased_word.split("::")

      # Handle absolute paths (::Foo::Bar)
      if names.empty? || (names.size == 1 && names.first.empty?)
        raise NameError, "wrong constant name #{camel_cased_word}"
      end

      # Remove leading empty string from absolute path
      if names.first.empty?
        names.shift
        object = Object
      else
        object = Object
      end

      names.each do |name|
        if name.empty?
          raise NameError, "wrong constant name #{camel_cased_word}"
        end
        object = object.const_get(name)
      end

      object
    end

    def self.safe_constantize(camel_cased_word)
      constantize(camel_cased_word)
    rescue NameError
      nil
    end

    def self.ordinal(number)
      abs_number = number.to_i.abs

      if (11..13).include?(abs_number % 100)
        "th"
      else
        case abs_number % 10
        when 1 then "st"
        when 2 then "nd"
        when 3 then "rd"
        else "th"
        end
      end
    end

    def self.ordinalize(number)
      "#{number}#{ordinal(number)}"
    end

    # Setup default inflection rules
    inflections(:en) do |inflect|
      inflect.plural(/$/, "s")
      inflect.plural(/s$/i, "s")
      inflect.plural(/^(ax|test)is$/i, "\\1es")
      inflect.plural(/(octop|vir)us$/i, "\\1i")
      inflect.plural(/(octop|vir)i$/i, "\\1i")
      inflect.plural(/(alias|status)$/i, "\\1es")
      inflect.plural(/(bu|mis|gas)s$/i, "\\1ses")
      inflect.plural(/(buffal|tomat)o$/i, "\\1oes")
      inflect.plural(/([ti])um$/i, "\\1a")
      inflect.plural(/([ti])a$/i, "\\1a")
      inflect.plural(/sis$/i, "ses")
      inflect.plural(/(?:([^f])fe|([lr])f)$/i, "\\1\\2ves")
      inflect.plural(/(hive)$/i, "\\1s")
      inflect.plural(/([^aeiouy]|qu)y$/i, "\\1ies")
      inflect.plural(/(x|ch|ss|sh)$/i, "\\1es")
      inflect.plural(/(matr|vert|append)ix|ex$/i, "\\1ices")
      inflect.plural(/^(m|l)ouse$/i, "\\1ice")
      inflect.plural(/^(m|l)ice$/i, "\\1ice")
      inflect.plural(/^(ox)$/i, "\\1en")
      inflect.plural(/^(oxen)$/i, "\\1")
      inflect.plural(/(quiz)$/i, "\\1zes")

      inflect.singular(/s$/i, "")
      inflect.singular(/(ss)$/i, "\\1")
      inflect.singular(/(n)ews$/i, "\\1ews")
      inflect.singular(/([ti])a$/i, "\\1um")
      inflect.singular(/((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$/i, "\\1sis")
      inflect.singular(/(^analy)(sis|ses)$/i, "\\1sis")
      inflect.singular(/([^f])ves$/i, "\\1fe")
      inflect.singular(/(hive)s$/i, "\\1")
      inflect.singular(/(tive)s$/i, "\\1")
      inflect.singular(/([lr])ves$/i, "\\1f")
      inflect.singular(/([^aeiouy]|qu)ies$/i, "\\1y")
      inflect.singular(/(s)eries$/i, "\\1eries")
      inflect.singular(/(m)ovies$/i, "\\1ovie")
      inflect.singular(/(x|ch|ss|sh)es$/i, "\\1")
      inflect.singular(/^(m|l)ice$/i, "\\1ouse")
      inflect.singular(/(bus)(es)?$/i, "\\1")
      inflect.singular(/(o)es$/i, "\\1")
      inflect.singular(/(shoe)s$/i, "\\1")
      inflect.singular(/(cris|test)(is|es)$/i, "\\1is")
      inflect.singular(/^(a)x[ie]s$/i, "\\1xis")
      inflect.singular(/(octop|vir)(us|i)$/i, "\\1us")
      inflect.singular(/(alias|status)(es)?$/i, "\\1")
      inflect.singular(/^(ox)en/i, "\\1")
      inflect.singular(/(vert|ind)ices$/i, "\\1ex")
      inflect.singular(/(matr)ices$/i, "\\1ix")
      inflect.singular(/(quiz)zes$/i, "\\1")
      inflect.singular(/(database)s$/i, "\\1")

      inflect.irregular("person", "people")
      inflect.irregular("man", "men")
      inflect.irregular("child", "children")
      inflect.irregular("sex", "sexes")
      inflect.irregular("move", "moves")
      inflect.irregular("zombie", "zombies")

      inflect.uncountable(%w(equipment information rice money species series fish sheep jeans police))
    end
  end
end
