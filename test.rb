class A < Array
  def f(x)
    @a = 1
    @b = 2
    @c = 3
    @a + @b + @c
    if x % 2 == 0
      @d = 4
      @e = 5
      @f = 6
    end
    @f
  end
end

30.times do |x| puts A.new.f(x) end