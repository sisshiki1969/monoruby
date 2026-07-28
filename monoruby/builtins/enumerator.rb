class Enumerator
  include Enumerable
end

# The remaining aliases need their *original* defined on the class, because
# CRuby keeps the original there too (the strict identity check compares the
# owner). monoruby inherits them, so we re-root the original on the class (a
# `super`-forwarding stub preserves behaviour) and then alias.
#
# NOT done here: Complex#quo == Complex#/. CRuby keeps both on Complex as one
# method; in monoruby `quo` (Rational-component result) and the inherited
# Numeric#/ (and the `/` *operator*, which has a separate fast path that is
# itself buggy — e.g. `Complex(1,2) / 2` => `(0+1i)`) are genuinely different
# implementations, so aliasing them changes results. Tracked as an issue.
class Enumerator
  # identical to with_object; re-root on Enumerator (overrides Enumerable's).
  alias each_with_object with_object
end
# (Thread#to_s / #inspect are defined in startup.rb in CRuby's
# `#<Thread:0xADDR status>` format — no override needed here.)
