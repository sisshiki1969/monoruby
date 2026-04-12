class Set
  include Enumerable

  def initialize(enum = nil, &block)
    @hash = {}
    if enum
      if block
        enum.each { |o| add(block.call(o)) }
      else
        enum.each { |o| add(o) }
      end
    end
  end

  def to_set
    self
  end
end

module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end
