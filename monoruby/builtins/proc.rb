class Proc
  def >>(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    f = self
    if f.lambda?
      lambda { |*a, &b| g.call(f.call(*a, &b)) }
    else
      proc { |*a, &b| g.call(f.call(*a, &b)) }
    end
  end

  def <<(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    f = self
    if g.is_a?(Proc) && !g.lambda?
      proc { |*a, &b| f.call(g.call(*a, &b)) }
    else
      lambda { |*a, &b| f.call(g.call(*a, &b)) }
    end
  end
end
