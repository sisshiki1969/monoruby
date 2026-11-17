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
  def to_set(klass = nil, *args, &block)
    # Ruby 4.0 deprecates only the *set-class* form; a bare `to_set` is
    # still the ordinary way to build a Set. `warn(uplevel:)` supplies
    # the "file:line: warning: " prefix itself.
    warn "Enumerable#to_set is deprecated. Use Set[] or Set.new directly instead.", uplevel: 1 if klass
    (klass || Set).new(self, *args, &block)
  end
end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Set
  alias < proper_subset?
  alias > proper_superset?
  alias << add
  alias eql? ==
  # The rest of what `Init_Set` defines with `rb_define_alias`, so that
  # `Method#original_name` and `#inspect` name the original
  # (`#<UnboundMethod: Set#===(include?)(_)>`, #2017).
  alias === include?
  alias member? include?
  alias + |
  alias union |
  alias <= subset?
  alias >= superset?
  alias difference -
  alias intersection &
  alias filter! select!
  alias map! collect!
  alias length size
  alias to_s inspect
end
