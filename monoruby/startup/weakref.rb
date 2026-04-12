# WeakRef stub for monoruby.
#
# monoruby does not implement ObjectSpace::WeakMap, so the real weakref.rb
# (which wraps WeakMap) cannot load. This stub provides a WeakRef class
# that behaves like the real one from the caller's perspective — it
# responds to `new`, `__getobj__`, and `weakref_alive?` — but strong-holds
# its referent. That is semantically weaker than a real weakref (objects
# won't be GCed while referenced) but is correct enough for ActiveRecord's
# descendants_tracker and connection_pool::reaper to function.

class WeakRef
  VERSION = "0.1.4"

  class RefError < StandardError
  end

  def initialize(orig)
    @__monoruby_obj = orig
  end

  def __getobj__
    @__monoruby_obj
  end

  def __setobj__(obj)
    @__monoruby_obj = obj
  end

  def weakref_alive?
    !@__monoruby_obj.nil?
  end

  def method_missing(name, *args, &block)
    if @__monoruby_obj.respond_to?(name)
      @__monoruby_obj.__send__(name, *args, &block)
    else
      super
    end
  end

  def respond_to_missing?(name, include_private = false)
    @__monoruby_obj.respond_to?(name, include_private) || super
  end
end
