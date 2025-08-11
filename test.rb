class S
  attr_accessor :foo
end

class C < S
  def foo=(x)
    super
  end
end

C.new.foo = 42