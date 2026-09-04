class BasicObject
  private

  # The default constructor: a no-op that returns nil, with CRuby's strict
  # zero arity (`Object.new(1)` raises ArgumentError). Written in Ruby, not
  # as a native builtin, so bytecodegen tags it `ISeqHint::ConstReturn(nil)`
  # and the JIT's trivial-body fold deletes the call at every `Foo.new` site
  # whose class does not override `initialize` — `Class#new` then reduces to
  # the inline allocation alone, with no callee frame.
  def initialize
  end

  def singleton_method_added(name)
  end

  def singleton_method_removed(name)
  end

  def singleton_method_undefined(name)
  end
end
