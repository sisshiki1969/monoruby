# frozen_string_literal: true
#
# ActiveSupport core extensions for Array.
#

class Array
  def extract_options!
    if last.is_a?(Hash) && !last.is_a?(ActiveSupport::HashWithIndifferentAccess)
      pop
    else
      {}
    end
  end

  def self.wrap(object)
    if object.nil?
      []
    elsif object.respond_to?(:to_ary)
      result = object.to_ary
      result.nil? ? [object] : result
    else
      [object]
    end
  end

  def in_groups_of(number, fill_with = nil, &block)
    if number <= 0
      raise ArgumentError, "Group size must be a positive integer, was #{number}"
    end

    collection = self.dup
    # Pad if needed
    remainder = collection.size % number
    if remainder > 0 && !fill_with.nil?
      padding = number - remainder
      padding.times { collection << fill_with }
    end

    groups = []
    index = 0
    while index < collection.size
      groups << collection[index, number]
      index += number
    end

    if block
      groups.each(&block)
    else
      groups
    end
  end

  def in_groups(number, fill_with = nil, &block)
    division = size / number
    modulo = size % number

    groups = []
    start = 0
    number.times do |index|
      length = division + (modulo > 0 && modulo > index ? 1 : 0)
      groups << last_group = slice(start, length)
      if fill_with && modulo > 0 && length == division
        last_group << fill_with
      end
      start += length
    end

    if block
      groups.each(&block)
    else
      groups
    end
  end

  # Returns second, third, etc elements
  def second
    self[1]
  end

  def third
    self[2]
  end

  def fourth
    self[3]
  end

  def fifth
    self[4]
  end

  def forty_two
    self[41]
  end

  def to_sentence(options = {})
    default_connectors = {
      words_connector: ", ",
      two_words_connector: " and ",
      last_word_connector: ", and "
    }
    options = default_connectors.merge(options)

    case length
    when 0
      ""
    when 1
      self[0].to_s.dup
    when 2
      "#{self[0]}#{options[:two_words_connector]}#{self[1]}"
    else
      "#{self[0...-1].join(options[:words_connector])}#{options[:last_word_connector]}#{self[-1]}"
    end
  end
end
