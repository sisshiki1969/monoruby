class Module
  # Partial ordering on the include / inherit graph, matching CRuby's
  # `rb_mod_cmp`:
  #
  # | relationship                                    | result |
  # | ----------------------------------------------- | ------ |
  # | `self` is `other`                               | `0`    |
  # | `self` is a subclass of, or includes, `other`   | `-1`   |
  # | `other` is a subclass of, or includes, `self`   | `+1`   |
  # | unrelated                                       | `nil`  |
  # | `other` is not a Module / Class                 | `nil`  |
  #
  # Defined on Module so `<` / `>` / `<=` / `>=` (provided by
  # `Comparable` in CRuby; we synthesise them) all reduce to this.
  def <=>(other)
    return nil unless other.is_a?(Module)
    return 0 if equal?(other)
    if ancestors.include?(other)
      -1
    elsif other.ancestors.include?(self)
      +1
    else
      nil
    end
  end

  private
  def extended(mod)
  end

  def prepended(mod)
  end

  def included(mod)
  end

  # CRuby keeps these as private instance methods of Module so subclasses
  # override them with `def method_added(name); …; end` (i.e. via the
  # default-private visibility inside a `class M`). `public` here would
  # surface them as `Module.public_instance_methods` entries, which the
  # spec explicitly disallows ("is a private instance method").
  def method_added(name)
  end

  def method_removed(name)
  end

  def method_undefined(name)
  end

  def const_added(name)
  end

  public
  def const_missing(name)
    # Drop the implicit `Object::` prefix so a top-level miss reads
    # `uninitialized constant Foo` (not `Object::Foo`), matching CRuby.
    # For a qualified miss, CRuby crafts the prefix from the module's
    # `#name`, falling back to `#inspect` when the module is anonymous.
    if self.equal?(Object)
      qual = name.to_s
    else
      prefix = self.name || self.inspect
      qual = "#{prefix}::#{name}"
    end
    raise NameError.new("uninitialized constant #{qual}", name)
  end

  def include?(mod)
    # CRuby `Module#include?` accepts only true Modules, not Classes (even
    # though `Class < Module`). And the receiver is *not* counted as one of
    # its own included modules — `M.include?(M)` is `false`.
    if !mod.is_a?(Module) || mod.is_a?(Class)
      raise TypeError, "wrong argument type #{mod.class} (expected Module)"
    end
    return false if equal?(mod)
    ancestors.include?(mod)
  end

  # `Module.used_refinements` returns the refinements active in the
  # current scope. monoruby has no refinement support, so this returns
  # an empty Array as a permissive mock — gems and code that
  # defensively read this list (RSpec, Sorbet) won't crash. Defined in
  # Ruby (not Rust) so the user can override it in specs that actually
  # exercise refinements. CRuby only exposes the class form (no
  # `Module#used_refinements` instance method), so we follow suit.
  def self.used_refinements
    []
  end
end
