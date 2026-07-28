# ENV is a singleton object (not a class), so its methods live on its
# singleton class. ruby/spec checks several of them for strict alias identity
# (`ENV.method(:a) == ENV.method(:b)`). monoruby registered these as separate
# Rust builtins sharing one fn (distinct FuncId), so re-point them as real
# aliases here. The originals (`include?`, `value?`, `[]=`, `merge!`) are all
# registered by `hash.rs` before this file loads.
class << ENV
  alias has_key? include?
  alias key? include?
  alias member? include?
  alias has_value? value?
  alias store []=
  alias update merge!

  # core/env/clone_spec.rb / dup_spec.rb: unlike a plain Hash, ENV cannot be
  # copied — CRuby raises TypeError from `#clone`/`#dup`. `#clone` still
  # validates its `freeze:` keyword first (ArgumentError for a non-boolean
  # value or an unknown keyword), matching Kernel#clone's signature.
  def clone(freeze: nil, **rest)
    unless rest.empty?
      raise ArgumentError, "unknown keyword: #{rest.keys.first.inspect}"
    end
    unless freeze.nil? || freeze == true || freeze == false
      raise ArgumentError, "unexpected value for freeze: #{freeze.class}"
    end
    raise TypeError, "Cannot clone ENV, use ENV.to_h to get a copy of ENV as a hash"
  end

  def dup
    raise TypeError, "Cannot dup ENV, use ENV.to_h to get a copy of ENV as a hash"
  end

  # Hash#except is implemented via #dup, which ENV now forbids. CRuby's
  # ENV.except returns a plain Hash snapshot, so route it through #to_h.
  def except(*keys)
    to_h.except(*keys)
  end
end
