class Class
  # The canonical object constructor, written in Ruby. `__builtin_allocate__`
  # is the private, non-overridable allocator (users override `allocate`,
  # which `new` deliberately bypasses, as in CRuby); `__builtin_initialize__`
  # is the privileged spelling that dispatches to `initialize` bypassing its
  # `private` visibility. The `(...)` forward is optimized end-to-end: the
  # interpreter defers the rest-Array via the lazy-forwarding marker, and a
  # specialized JIT compile source-routes the caller's argument slots
  # straight into `initialize`'s frame (D1), so construction allocates
  # nothing but the object itself.
  def new(...)
    o = __builtin_allocate__
    o.__builtin_initialize__(...)
    o
  end

  private
  def inherited(subclass)
  end
end
