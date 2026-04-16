# Complex class is now implemented in Rust (builtins/numeric/complex.rs).
# This file adds Ruby-side helpers and visibility tweaks that are
# easier to express in Ruby.

class Complex
  # marshal_dump is a private instance method per CRuby.
  private :marshal_dump

  # Complex has no ordering beyond the <=> behaviour already implemented,
  # and CRuby explicitly undefines negative?/positive? on Complex. Our
  # Numeric#negative?/positive? are defined in Ruby, so we need to both
  # shadow them with raisers and then undef them.
  def negative?
    raise NoMethodError, "undefined method 'negative?' for an instance of Complex"
  end
  def positive?
    raise NoMethodError, "undefined method 'positive?' for an instance of Complex"
  end
  undef_method :negative?
  undef_method :positive?

  class << self
    undef_method :new
  end
end
