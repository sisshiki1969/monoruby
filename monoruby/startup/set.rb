# Set stub for monoruby.
#
# In Ruby 4.0 Set is built-in, so `require "set"` is effectively a no-op.
# monoruby's built-in Set class already provides the full API; this file
# only exists so that `require "set"` (as emitted by older gems, rbs, etc.)
# succeeds, and so that `Enumerable#to_set` is available for code that
# expects the classic `set` library to have defined it.
#
# Do NOT reopen the built-in `Set` class here: doing so would replace the
# built-in methods with pure-Ruby versions that reference ivars the built-in
# implementation does not set, and would break any Set instances that had
# already been constructed before `require "set"` ran.

module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end
