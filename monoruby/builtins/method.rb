class Method
  def >>(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    me = self
    lambda { |*a, &b| g.call(me.call(*a, &b)) }
  end

  def <<(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    me = self
    if g.is_a?(Proc) && !g.lambda?
      proc { |*a, &b| me.call(g.call(*a, &b)) }
    else
      lambda { |*a, &b| me.call(g.call(*a, &b)) }
    end
  end

  def curry(arity = nil)
    pr = to_proc
    return pr.curry(self.arity) if arity.nil?
    params = parameters
    req = params.count { |t, _| t == :req }
    opt = params.count { |t, _| t == :opt }
    has_rest = params.any? { |t, _| t == :rest }
    max = has_rest ? Float::INFINITY : req + opt
    if arity < req || arity > max
      raise ArgumentError, "wrong number of arguments (given #{arity}, expected #{req})"
    end
    pr.curry(arity)
  end
end
