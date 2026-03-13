# frozen_string_literal: true
#
# ActiveModel::Errors - Collection of validation errors for a model.

module ActiveModel
  class Error
    attr_reader :attribute, :type, :options

    def initialize(base, attribute, type = :invalid, **options)
      @base = base
      @attribute = attribute
      @type = type
      @options = options
    end

    def message
      if @options[:message]
        @options[:message]
      else
        case @type
        when :blank
          "can't be blank"
        when :invalid
          "is invalid"
        when :too_short
          "is too short (minimum is %s characters)" % [@options[:count].to_s]
        when :too_long
          "is too long (maximum is %s characters)" % [@options[:count].to_s]
        when :wrong_length
          "is the wrong length (should be %s characters)" % [@options[:count].to_s]
        when :not_a_number
          "is not a number"
        when :not_an_integer
          "must be an integer"
        when :greater_than
          "must be greater than %s" % [@options[:count].to_s]
        when :greater_than_or_equal_to
          "must be greater than or equal to %s" % [@options[:count].to_s]
        when :less_than
          "must be less than %s" % [@options[:count].to_s]
        when :less_than_or_equal_to
          "must be less than or equal to %s" % [@options[:count].to_s]
        when :equal_to
          "must be equal to %s" % [@options[:count].to_s]
        when :other_than
          "must be other than %s" % [@options[:count].to_s]
        when :inclusion
          "is not included in the list"
        when :exclusion
          "is reserved"
        else
          @type.to_s
        end
      end
    end

    def full_message
      if @attribute == :base
        message
      else
        attr_s = @attribute.to_s.tr('_', ' ')
        attr_name = attr_s.empty? ? attr_s : (attr_s[0].upcase + attr_s[1..-1].to_s)
        "%s %s" % [attr_name, message]
      end
    end

    def ==(other)
      other.is_a?(Error) &&
        @attribute == other.attribute &&
        @type == other.type
    end
  end

  class Errors
    include Enumerable

    def initialize(base)
      @base = base
      @errors = []
    end

    def add(attribute, type = :invalid, **options)
      error = Error.new(@base, attribute, type, **options)
      @errors << error
      error
    end

    def delete(attribute, type: nil)
      if type
        @errors.reject! { |e| e.attribute == attribute && e.type == type }
      else
        @errors.reject! { |e| e.attribute == attribute }
      end
    end

    def [](attribute)
      messages_for(attribute)
    end

    def messages_for(attribute)
      @errors.select { |e| e.attribute == attribute }.map(&:message)
    end

    def where(attribute, type = nil)
      result = @errors.select { |e| e.attribute == attribute }
      result = result.select { |e| e.type == type } if type
      result
    end

    def each(&block)
      @errors.each(&block)
    end

    def size
      @errors.size
    end
    alias count size
    alias length size

    def empty?
      @errors.empty?
    end

    def any?
      if block_given?
        @errors.any? { |e| yield e }
      else
        !@errors.empty?
      end
    end

    def clear
      @errors.clear
    end

    def messages
      hash = {}
      @errors.each do |error|
        (hash[error.attribute] ||= []) << error.message
      end
      hash
    end

    def full_messages
      @errors.map(&:full_message)
    end
    alias to_a full_messages

    def full_messages_for(attribute)
      @errors.select { |e| e.attribute == attribute }.map(&:full_message)
    end

    def include?(attribute)
      @errors.any? { |e| e.attribute == attribute }
    end
    alias has_key? include?
    alias key? include?

    def of_kind?(attribute, type = nil)
      if type
        @errors.any? { |e| e.attribute == attribute && e.type == type }
      else
        include?(attribute)
      end
    end

    def added?(attribute, type = :invalid, **options)
      @errors.any? { |e| e.attribute == attribute && e.type == type }
    end

    def to_hash
      messages
    end

    def to_s
      full_messages.join(", ")
    end

    def inspect
      "#<ActiveModel::Errors [%s]>" % [@errors.map(&:inspect).join(", ")]
    end

    def copy!(other)
      other.each do |error|
        add(error.attribute, error.type, **error.options)
      end
    end

    def merge!(other)
      copy!(other)
    end

    # Provides backward-compat: errors.add(:name, "is too short")
    # where type is actually a full message string
    alias_method :_original_add, :add
    def add(attribute, type = :invalid, **options)
      if type.is_a?(String)
        options[:message] = type
        type = :invalid
      end
      _original_add(attribute, type, **options)
    end
  end
end
