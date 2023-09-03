class C
  def foo(*x)
    puts "#{x}"
  end
end

c = C.new
for i in 0..10
  c.send(:foo, 1, 2)
end