class Encoding
  class CompatibilityError < EncodingError; end
  class InvalidByteSequenceError < EncodingError; end
  class UndefinedConversionError < EncodingError; end
  class ConverterNotFoundError < EncodingError; end
  class Converter; end

  def self.default_internal
    $DEFAULT_INTERNAL
  end
end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Encoding
  alias to_s name
end
