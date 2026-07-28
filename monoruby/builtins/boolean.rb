# TrueClass / FalseClass / NilClass: pure-Ruby surface of the three
# singleton-value classes.

class TrueClass
  class << self
    undef_method :new
  end
  TRUE_TO_S = "true".freeze
  def to_s
    TRUE_TO_S
  end
end

class FalseClass
  class << self
    undef_method :new
  end
  FALSE_TO_S = "false".freeze
  def to_s
    FALSE_TO_S
  end
end

class NilClass
  class << self
    undef_method :new
  end
  NIL_TO_S = "".freeze
  def to_s
    NIL_TO_S
  end

  def to_a
    []
  end

  def to_i
    0
  end

  def to_f
    0.0
  end

  def to_h
    {}
  end

  def =~(_other)
    nil
  end

  def to_c
    Complex(0, 0)
  end

  def to_r
    Rational(0)
  end

  def rationalize(*args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 0..1)"
    end
    Rational(0)
  end
end

# `^`, `|`, `&` for `true` and `false` live on the internal `Boolean`
# parent class so that `true.method(:&) == false.method(:&)` and the
# JIT inline cache can treat the receiver as `BOOL_CLASS` regardless of
# which boolean was observed first.

class NilClass
  def |(other)
    !!other
  end
  # ruby/spec core/nil/xor_spec.rb: ^ is an alias of |.
  alias ^ |

  def &(other)
    false
  end

  def to_i
    0
  end
end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class TrueClass
  # core/true/inspect_spec.rb: inspect is an alias of to_s (to_s on TrueClass).
  alias inspect to_s
end

class FalseClass
  # core/false/inspect_spec.rb: inspect is an alias of to_s (to_s on FalseClass).
  alias inspect to_s
  # core/false/xor_spec.rb: ^ is an alias of |. Both `|` and `^` are rooted
  # directly on FalseClass (registered by bool_class.rs on FALSE_CLASS, not an
  # inherited parent), so just re-point `^` at the existing `|`. For false,
  # `false ^ x` and `false | x` are both `!!x`, so behaviour is unchanged.
  # (`true` keeps distinct `^`/`|` on the shared Boolean, where they differ.)
  alias ^ |
end
