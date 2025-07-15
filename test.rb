class C
  def foo(*x, **y)
    p "#{x.inspect} #{y.inspect}"
  end
end

C.new.foo(1, 2, a: 3, b: 4)
