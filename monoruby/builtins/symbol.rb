class Symbol
  include Comparable

  # `[]` / `slice`, `=~`, `match`, `start_with?` are native (see
  # builtins/symbol.rs): they must set the frame-local `$~` on the
  # *caller's* LEP, which a pure-Ruby `to_s`-delegation can't do.

  def size
    to_s.size
  end

  def length
    to_s.length
  end

  def empty?
    to_s.empty?
  end

  def upcase
    to_s.upcase.to_sym
  end

  def downcase
    to_s.downcase.to_sym
  end

  def capitalize
    to_s.capitalize.to_sym
  end

  def swapcase
    to_s.swapcase.to_sym
  end

  def end_with?(*args)
    to_s.end_with?(*args)
  end

  def encoding
    to_s.encoding
  end

  def succ
    to_s.succ.to_sym
  end
  alias next succ

  def id2name
    to_s
  end

  def intern
    self
  end

  def to_sym
    self
  end

  def casecmp(other)
    return nil unless other.is_a?(Symbol)
    to_s.casecmp(other.to_s)
  end

  def casecmp?(other)
    return nil unless other.is_a?(Symbol)
    to_s.casecmp?(other.to_s)
  end

  def match?(other, pos = 0)
    self.to_s.match?(other, pos)
  end
end
