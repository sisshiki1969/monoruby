class Set
  include Enumerable

  def initialize(enum = nil, &block)
    if enum.nil?
      # empty set
    elsif enum.respond_to?(:each_entry)
      if block
        enum.each_entry { |o| add(block.call(o)) }
      else
        enum.each_entry { |o| add(o) }
      end
    elsif enum.respond_to?(:each)
      if block
        enum.each { |o| add(block.call(o)) }
      else
        enum.each { |o| add(o) }
      end
    else
      raise ArgumentError, "value must be enumerable"
    end
  end
  private :initialize

  def to_set
    self
  end
end

module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end
