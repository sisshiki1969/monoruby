class C
  def f(x)
    puts x
  end
end

m = C.new.method(:f)
m[3]