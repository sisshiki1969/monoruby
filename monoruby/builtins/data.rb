# Data class (Ruby 3.2+): immutable value objects with a keyword-based
# initializer. Built on top of Struct for storage, attribute readers,
# equality, hashing and inspection; the keyword initializer, argument
# validation, `new`/`[]` positional-to-keyword coercion, frozen instances
# and `with` are layered on here.
class Data
  def self.define(*members, &block)
    members = members.map do |m|
      case m
      when Symbol then m
      when String then m.to_sym
      else raise TypeError, "#{m} is not a symbol"
      end
    end
    klass = ::Struct.new(*members)
    klass.class_eval do
      # CRuby renders `Data` instances as `#<data ...>` rather than
      # `#<struct ...>`. Reuse Struct's (correct) inspect — which already
      # handles qualified/anonymous class names and bypasses an overridden
      # `#name` — and only relabel the prefix.
      def inspect
        ::Struct.instance_method(:inspect).bind(self).call.sub(/\A#<struct/, '#<data')
      end
      alias_method :to_s, :inspect

      # The keyword initializer is layered in a module so a user-defined
      # `initialize` (from the `define` block) can `super` into it.
      include(::Module.new do
        def initialize(*args, **kw)
          ms = self.class.members
          kw = ::Hash[ms.zip(args)].merge(kw) unless args.empty?
          values = ::Data.__data_values(ms, kw)
          super(*values)
          freeze
        end
      end)

      # `new` / `[]` accept positional *or* keyword arguments; positional
      # ones are zipped onto the members, then `initialize` (possibly
      # user-overridden) is dispatched with keywords.
      def self.new(*args, **kw)
        ::Data.__data_alloc_init(self, args, kw)
      end
      class << self
        alias_method :[], :new
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
    end
    klass.class_eval(&block) if block
    klass
  end

  # The base initializer, reachable as `Data.instance_method(:initialize)`
  # (used by e.g. marshalling libraries to populate an allocated instance).
  def initialize(**kw)
    ms = self.class.members
    values = ::Data.__data_values(ms, kw)
    ms.each_with_index { |m, i| send("#{m}=", values[i]) }
    freeze
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
end unless defined?(::Data)
