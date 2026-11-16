class Symbol
  include Comparable

  # Everything that works on the characters — `[]` / `slice`, `=~`,
  # `match`, `match?`, `start_with?`, `end_with?`, `length`, `empty?`,
  # `encoding`, the case mappings, `succ`, `casecmp` — is native (see
  # builtins/symbol.rs). They run on the name itself, as CRuby's do, so a
  # redefined `Symbol#to_s` changes none of them (#1929); and the
  # matching ones must set the frame-local `$~` on the *caller's* LEP,
  # which a pure-Ruby `to_s`-delegation can't do.

  def id2name
    to_s
  end

  def intern
    self
  end

  def to_sym
    self
  end
end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Symbol
  alias === ==
  alias id2name to_s
  alias intern to_sym
end
