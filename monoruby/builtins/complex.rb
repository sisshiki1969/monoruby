# Complex class is now implemented in Rust (builtins/numeric/complex.rs).
# This file adds Ruby-side helpers and visibility tweaks that are
# easier to express in Ruby.

class Complex
  # marshal_dump is a private instance method per CRuby.
  private :marshal_dump

  # What `Init_Complex` undefines: the Numeric methods that need an
  # ordering or a rounding, and Comparable's — a Complex answers `<=>`
  # only for a real value, and none of these.
  undef_method :%, :div, :divmod, :floor, :ceil, :modulo, :remainder,
               :round, :step, :truncate, :i
  undef_method :positive?, :negative?
  undef_method :<, :<=, :>, :>=, :between?, :clamp

  class << self
    undef_method :allocate
    undef_method :new
  end
end
