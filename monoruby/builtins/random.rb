# Random::Formatter wiring.
#
# CRuby wires the (initially method-less) `Random::Formatter` module into
# `Random` at boot (random.c): both instances and the class itself dispatch
# through it, and requiring `random/formatter` (directly, or via
# `securerandom`) fills the module in with the formatting helpers —
# `hex`, `base64`, `alphanumeric`, `uuid`, … The primitives those helpers
# build on (`rand` / `random_number`) are native (`src/builtins/random.rs`),
# defined directly on Random, so they win over the module for Random itself
# while still backing SecureRandom, which only extends the module.
class Random
  module Formatter
    # CRuby implements these two in C (random.c) for any receiver that
    # provides `bytes` — that is what lets `SecureRandom` (which only
    # `extend`s this module and defines `bytes`) answer `random_number` /
    # `alphanumeric` / `uuid`, …  Random itself never reaches these: its
    # native `rand` / `random_number` are defined directly on the class
    # (src/builtins/random.rs) and win over the included module.
    def random_number(n = nil)
      if n.is_a?(Integer)
        raise ArgumentError, "negative size" if n.negative?
        return __formatter_float if n.zero?
        # Rejection sampling over the minimal bit width keeps the result
        # uniform in 0...n (mirrors rb_random_ulong_limited).
        bits = (n - 1).bit_length
        nbytes = (bits + 7) / 8
        mask = (1 << bits) - 1
        loop do
          v = bytes(nbytes).unpack1("H*").to_i(16) & mask
          return v if v < n
        end
      elsif n.nil?
        __formatter_float
      elsif n.is_a?(Numeric)
        raise ArgumentError, "negative size" if n.negative?
        __formatter_float * n
      else
        raise ArgumentError, "invalid argument - #{n}"
      end
    end
    alias rand random_number

    private def __formatter_float
      # 53 random bits -> float in [0, 1), the standard IEEE-754 recipe.
      (bytes(8).unpack1("Q>") >> 11) * (1.0 / (1 << 53))
    end
  end

  include Formatter
  class << self
    include Random::Formatter
  end
end
