class A < Array
  def f
    @a = @b = @c = @d = @e = @f = 6
  end
  def c
    @a = 7
    @c
  end
end

A.new.f
30.times do puts A.new.c end
