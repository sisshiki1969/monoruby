# ENV is a plain Object extended with Enumerable, as in CRuby: `all?`,
# `map`, `sort`, `first`, ... are Enumerable's own over `ENV.each`, which
# yields the `[name, value]` pairs of the moment as frozen copies.
ENV.extend(Enumerable)

# ENV's own methods live on its singleton class. ruby/spec checks several
# of them for strict alias identity (`ENV.method(:a) == ENV.method(:b)`).
# monoruby registered these as separate Rust builtins sharing one fn
# (distinct FuncId), so re-point them as real aliases here. The originals
# (`include?`, `value?`, `[]=`, `merge!`) are all registered by `hash.rs`
# before this file loads.
class << ENV
  alias has_key? include?
  alias key? include?
  alias member? include?
  alias has_value? value?
  alias store []=
  alias update merge!

  # core/env/clone_spec.rb / dup_spec.rb: ENV cannot be
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

  # CRuby's ENV.except answers a plain Hash: a copy of the environment
  # without the given names.
  def except(*keys)
    to_h.except(*keys)
  end

  # ENV keeps no state of its own: every access reads the process
  # environment, which native code changes with setenv(3) behind
  # monoruby's back.
  def inspect
    to_h.inspect
  end

  def freeze
    raise TypeError, "cannot freeze ENV"
  end
end
