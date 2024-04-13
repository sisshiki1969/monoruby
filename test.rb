class C
  def f
    yield 100
  end
end

c = C.new

20.times {
  puts c.f {|x| x * 2}
}