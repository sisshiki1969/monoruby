class Enumerator
  include Enumerable
end

class CallerLocation
  def initialize(path, lineno, label)
    @path = path
    @lineno = lineno
    @label = label
  end
  attr_reader :path, :lineno, :label

  def base_label
    l = @label
    l = $1 while l =~ /\Ablock in (.+)\z/
    l
  end

  def to_s
    "#{@path}:#{@lineno}:in '#{@label}'"
  end
  alias inspect to_s
end

def caller_locations(start = 1, length = nil)
  frames = caller(start + 1)
  return nil if frames.nil?
  frames = frames[0, length] if length
  frames.map do |frame|
    if frame =~ /\A(.+):(\d+):in ['`](.+)'\z/
      CallerLocation.new($1, $2.to_i, $3)
    else
      CallerLocation.new(frame, 0, "")
    end
  end
end

class TrueClass
  def ^(other)
    !other
  end

  def |(other)
    true
  end

  def &(other)
    !!other
  end
end

class FalseClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end
end

class NilClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end

  def to_i
    0
  end
end

class Hash
  include Enumerable

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    return self unless block_given?
    h = {}
    self.each {|k, v|
      new_kv = yield k, v
      new_k = new_kv[0]
      new_v = new_kv[1]
      h[new_k] = new_v
    }
    h
  end

  def transform_keys(&block)
    return to_enum(:transform_keys) unless block
    h = {}
    each { |k, v| h[block.call(k)] = v }
    h
  end

  def transform_keys!(&block)
    return to_enum(:transform_keys!) unless block
    keys.each { |k| self[block.call(k)] = delete(k) }
    self
  end

  def transform_values(&block)
    return to_enum(:transform_values) unless block
    h = {}
    each { |k, v| h[k] = block.call(v) }
    h
  end

  def transform_values!(&block)
    return to_enum(:transform_values!) unless block
    each { |k, v| self[k] = block.call(v) }
    self
  end
end

class Module
  def ruby2_keywords(*)
    # no-op for compatibility
  end
end

class Float
  def zero?
    self == 0.0
  end
end

class Symbol
  def match(other)
    self.to_s.match(other)
  end

  def match?(other)
    self.to_s.match?(other)
  end

  def to_proc
    Proc.new do |*args|
      if args.size == 0
        raise ArgumentError, "no receiver given"
      end
      slf, *args = args
      slf.send(self, *args)
    end
  end

  def to_sym
    self
  end
end

class String
  def +@
    self
  end
  def -@
    self
  end

  def encode(**opts)
    if opts[:xml] == :attr
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s = s.gsub("\"", "&quot;")
      "\"#{s}\""
    elsif opts[:xml] == :text
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s
    else
      self
    end
  end
end
