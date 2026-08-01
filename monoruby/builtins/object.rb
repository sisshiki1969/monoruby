class Object
  # NOTE: no `def initialize` here — the default constructor is the
  # native `BasicObject#initialize` (arity 0), matching CRuby's owner
  # and its strict arity (`Object.new(1)` raises ArgumentError). A
  # Ruby-level `def initialize(...)` would silently accept any
  # arguments and also defeat the JIT's forwarding specialization for
  # argument-less `Class#new`.

  def itself
    self
  end

  def then
    return to_enum(:then) { 1 } unless block_given?
    yield self
  end
  alias yield_self then

  def <=>(other)
    return 0 if equal?(other)
    # The `self == other` fallback would recurse infinitely when
    # `==` is `Comparable#==` (a Comparable class that did not
    # override `==`): that `==` calls `<=>`, which reaches here
    # again. Skip the fallback in that case (covers both a missing
    # `<=>` and a user `<=>` that calls `super`). Net behaviour
    # matches CRuby, which uses an equivalent recursion guard.
    return nil if self.class.instance_method(:==).owner == Comparable
    0 if (self == other)
  end
end
