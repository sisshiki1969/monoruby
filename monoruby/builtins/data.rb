# Data class (Ruby 3.2+): immutable value objects with a keyword-based
# initializer. The class itself, the `Data.define` class factory
# (`__define_class`), slot storage + freeze (`__data_init`) and the
# identity-sensitive primitives (inspect/==/eql?/hash/members/deconstruct)
# are implemented in Rust (`builtins/data_class.rs`) on the Struct slot
# machinery, so defined classes are *real* `Data` subclasses. This file
# layers the pure protocol logic on top: member validation, `new`/`[]`
# positional-to-keyword coercion, keyword validation, `with`, `to_h` and
# `deconstruct_keys`.
class Data
  def self.define(*members, &block)
    members = members.map do |m|
      case m
      when Symbol then m
      when String then m.to_sym
      else raise TypeError, "#{m} is not a symbol"
      end
    end
    seen = {}
    members.each do |m|
      raise ArgumentError, "duplicate member: #{m}" if seen.key?(m)
      seen[m] = true
    end
    # `self` (not literally ::Data) so `define` called on an already-defined
    # class produces a subclass of that class.
    klass = ::Data.__define_class(self, members)
    klass.class_eval do
      # `new` / `[]` accept positional *or* keyword arguments; positional
      # ones are zipped onto the members, then `initialize` (possibly
      # user-overridden) is dispatched with keywords.
      def self.new(*args, **kw)
        ::Data.__data_alloc_init(self, args, kw)
      end
      class << self
        alias_method :[], :new
      end
    end
    klass.class_eval(&block) if block
    klass
  end

  # The base initializer, reachable as `Data.instance_method(:initialize)`
  # (used by e.g. marshalling libraries to populate an allocated instance).
  def initialize(**kw)
    __data_init(::Data.__data_values(self.class.members, kw))
  end

  alias_method :to_s, :inspect

  def to_h
    ms = self.class.members
    vs = deconstruct
    h = {}
    ms.each_with_index { |m, i| h[m] = vs[i] }
    return h unless block_given?
    r = {}
    h.each do |k, v|
      pair = yield k, v
      pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
      raise TypeError, "wrong element type #{pair.class} (expected Array)" unless pair.is_a?(Array)
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
      r[pair[0]] = pair[1]
    end
    r
  end

  # Returns a frozen copy with the given members replaced. Allocates
  # and initializes directly rather than going through `new`, matching
  # CRuby (a redefined `new` must not affect `with`).
  def with(**kw)
    return self if kw.empty?
    norm = {}
    kw.each { |k, v| norm[k.is_a?(::String) ? k.to_sym : k] = v }
    copy = self.class.allocate
    copy.send(:initialize, **to_h.merge(norm))
    copy
  end

  # `deconstruct_keys(keys)` for pattern matching: `nil` returns all
  # members; otherwise the requested keys (Symbol / String / `#to_str`)
  # are looked up, stopping at the first non-member.
  def deconstruct_keys(keys)
    return to_h if keys.nil?
    unless keys.is_a?(::Array)
      raise TypeError, "wrong argument type #{keys.class} (expected Array or nil)"
    end
    ms = self.class.members
    return {} if keys.size > ms.size
    result = {}
    keys.each do |k|
      sym, rkey =
        case k
        when ::Symbol then [k, k]
        when ::String then [k.to_sym, k]
        else
          if k.respond_to?(:to_str)
            s = k.to_str
            unless s.is_a?(::String)
              raise TypeError, "can't convert #{k.class} into String"
            end
            [s.to_sym, s]
          else
            raise TypeError, "#{k} is not a symbol nor a string"
          end
        end
      break unless ms.include?(sym)
      result[rkey] = send(sym)
    end
    result
  end

  def self.__data_alloc_init(klass, args, kw)
    ms = klass.members
    unless args.empty?
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)" unless kw.empty?
      unless args.size == ms.size
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 0)"
      end
      kw = ::Hash[ms.zip(args)]
    end
    obj = klass.allocate
    obj.send(:initialize, **kw)
    obj
  end

  # Validate `kw` against `members` (converting String / `#to_str` keys to
  # Symbols) and return the member values in declaration order.
  def self.__data_values(members, kw)
    norm = {}
    unknown = []
    kw.each do |k, v|
      key, disp = __data_key(k)
      if members.include?(key)
        norm[key] = v
      else
        unknown << disp
      end
    end
    missing = members.reject { |m| norm.key?(m) }
    unless missing.empty?
      s = missing.size == 1 ? "" : "s"
      raise ArgumentError, "missing keyword#{s}: #{missing.map { |m| ":#{m}" }.join(", ")}"
    end
    unless unknown.empty?
      s = unknown.size == 1 ? "" : "s"
      raise ArgumentError, "unknown keyword#{s}: #{unknown.join(", ")}"
    end
    members.map { |m| norm[m] }
  end

  # Normalize a keyword key to `[symbol, display]`, where `display` is how
  # the key appears in an "unknown keyword" message (`:sym` / `"str"`).
  def self.__data_key(k)
    case k
    when Symbol then [k, ":#{k}"]
    when String then [k.to_sym, k.inspect]
    else
      if k.respond_to?(:to_str)
        s = k.to_str
        raise TypeError, "can't convert #{k.class} into String" unless s.is_a?(String)
        [s.to_sym, s.inspect]
      else
        raise TypeError, "#{k} is not a symbol nor a string"
      end
    end
  end
end
