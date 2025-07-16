class C
  def foo(*x, a:100, **y)
    puts "#{x.inspect} #{a.inspect} #{y.inspect}"
  end
end

100.times {
  C.new.send(:foo, 1, 2, a: 3, c: 5)
}