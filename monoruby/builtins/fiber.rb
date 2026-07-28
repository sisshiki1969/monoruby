class Fiber
  @@main_fiber = nil
  def self.current
    # Inside a fiber body the executor knows which Fiber it is running as;
    # at a thread's root context there is none, so fall back to a shared
    # main-fiber object (distinct from any explicit Fiber, which is all that
    # per-Fiber mutex ownership needs — cross-thread checks key on the
    # Thread).
    __current_fiber || (@@main_fiber ||= Fiber.new {})
  end

  # --- Fiber-local storage (CRuby 3.2+) --------------------------------
  #
  # `Fiber.current.storage` is a Hash whose keys must be Symbols. The
  # `Fiber.[key]` / `Fiber.[]=key, val` class-method sugar reads/writes
  # the *current* fiber's storage. Pure-Ruby implementation here —
  # monoruby tracks an `@storage` ivar per Fiber, lazily materialised.

  # Returns the storage Hash. CRuby returns `nil` until `[]=` /
  # `storage=` materialises one (no lazy init on bare read), so do
  # not auto-create here.
  def storage
    @storage
  end

  # Assigns the storage Hash. `nil` clears it. Non-Hash raises
  # TypeError; non-Symbol keys raise TypeError.
  def storage=(hash)
    if hash.nil?
      @storage = nil
      return nil
    end
    raise TypeError, "no implicit conversion of #{hash.class} into Hash" \
      unless hash.is_a?(Hash)
    hash.each_key do |k|
      raise TypeError, "wrong argument type #{k.class} (expected Symbol)" \
        unless k.is_a?(Symbol)
    end
    @storage = hash
  end

  # Class-method sugar over `Fiber.current` storage. Symbol-keyed.
  # `Fiber[:k]` on a fiber with no storage returns nil (CRuby).
  def self.[](key)
    raise TypeError, "wrong argument type #{key.class} (expected Symbol)" \
      unless key.is_a?(Symbol)
    s = current.storage
    s ? s[key] : nil
  end

  # `Fiber[:k] = v` materialises an empty storage on first write.
  def self.[]=(key, value)
    raise TypeError, "wrong argument type #{key.class} (expected Symbol)" \
      unless key.is_a?(Symbol)
    cur = current
    cur.storage = {} if cur.storage.nil?
    cur.storage[key] = value
  end

  # --- Scheduler -------------------------------------------------------
  #
  # monoruby has no fiber scheduler, so `set_scheduler` is effectively
  # a no-op store. CRuby allows `nil` to clear the scheduler, so accept
  # that too. `scheduler` returns whatever was set (default nil).

  @@scheduler = nil

  def self.scheduler
    @@scheduler
  end

  def self.set_scheduler(scheduler)
    @@scheduler = scheduler
  end


  def self.current_scheduler
    # CRuby: nil when the current fiber is blocking. monoruby fibers
    # are always blocking, so this is always nil.
    nil
  end

  # --- Blocking / non-blocking ----------------------------------------
  #
  # CRuby contracts (3.2+):
  #   `Fiber.blocking?`         returns `1` when the *current* fiber
  #                             is blocking, `false` otherwise.
  #   `Fiber#blocking?`         instance form: `true` / `false`.
  # monoruby has no non-blocking fibers, so the class form is `1`
  # and the instance form is `true`.

  def self.blocking?
    1
  end

  def blocking?
    true
  end
end
