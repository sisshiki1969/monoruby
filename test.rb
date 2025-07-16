class C
  def foo(*x, **y)
    puts "#{x.inspect} #{y.inspect}"
  end
end

100.times {
  C.new.send(:foo, 1, 2, a: 3, b: 4)
}