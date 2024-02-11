class C
  def f(x,y,z)
    puts "#{x},#{y},#{z}"
  end
end

15.times do
  C.new.send(:f, 1, 2, 3)
end